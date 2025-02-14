use crate::lexer::position::Position;
use crate::parser::expr::StructExpr;
use std::collections::HashMap;

use crate::context::key::ContextKey;
use crate::context::value::ContextValue;
use crate::context::Context;
use crate::parser::declaration::VariableDeclaration;
use crate::parser::expr::{Argument, Expr, ExprNode};
use crate::parser::function::Function;
use crate::parser::module::Module;
use crate::parser::r#struct::{Struct, StructField};
use crate::parser::r#type::Type;
use crate::parser::stmt::{IfBranchStmt, Stmt, StmtNode};
use crate::postprocessor::id::id;
use std::sync::{Arc, RwLock};
use slotmap::DefaultKey;

pub struct TypePostprocessor {
    module: Module,
}
impl TypePostprocessor {
    pub fn new() -> Self {
        Self {
            module: Module::new("unknown"),
        }
    }
    pub fn process(&mut self, module: DefaultKey,ctx:&Context) -> Module {
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let mut module = module_slot_map.get_mut(module).unwrap().clone();

        drop(module_slot_map);
        self.module.set_name(module.get_name());
        let submodule = module.get_submodules();
        let mut names = vec![];
        for (name, _) in submodule {
            names.push(name.clone());
        }
        for name in names {
            module.merge_into_main(ctx, &name);
        }
        dbg!(&module);
        self.process_module(&module,ctx);
        self.module.clone()
    }
    fn process_module(&mut self, module: &Module,ctx:&Context){

        let mut symbols = HashMap::new();
        let ctx = Context::with_value(
            &ctx,
            ContextKey::SymbolType,
            ContextValue::SymbolType(Arc::new(RwLock::new(symbols.clone()))),
        );
        let alias = HashMap::new();
        let ctx = Context::with_value(
            &ctx,
            ContextKey::AliasType,
            ContextValue::AliasType(Arc::new(RwLock::new(alias))),
        );
        for module_name in module.get_submodules().keys(){
            ctx.set_symbol_type(module_name.into(),Type::Module)
        }
        for (name, st) in module.get_structs() {
            ctx.set_alias_type(name.clone(), st.get_type())
        }
        for (name, st) in module.get_structs() {
            let st = self.process_struct(st, &ctx);
            self.module.register_struct(name, st.clone());
        }
        let mut functions = module.get_functions();
        for (_, f) in &mut functions.iter_mut() {
            if f.has_binding() {
                f.insert_arg(
                    0,
                    VariableDeclaration::new("this")
                        .with_type(ctx.get_alias_type(f.get_binding()).unwrap()),
                )
            }
        }
        for (name, f) in functions.iter() {
            let mut ty = f.get_type();
            let return_type = ty.get_function_return_type().unwrap();
            if return_type.is_alias() {
                let alias = return_type.get_alias_name().unwrap();
                let new_return_ty = ctx.get_alias_type(alias).unwrap();
                ty = ty.with_return_type(new_return_ty)
            }
            ctx.set_symbol_type(name.clone(), ty)
        }
        for (name, f) in functions.iter() {
            let mut new_f;
            if f.is_extern {
                new_f = f.clone();
            } else {
                let mut symbol_types = HashMap::new();
                let mut locals = vec![];
                for i in f.args() {
                    let ty = i.r#type().unwrap();
                    symbol_types.insert(i.name(), ty);
                    locals.push(i.name());
                }
                let ctx = Context::with_value(
                    &ctx,
                    ContextKey::SymbolType,
                    ContextValue::SymbolType(Arc::new(RwLock::new(symbol_types))),
                );
                let ctx = Context::with_local(&ctx, locals);
                let mut v = vec![];
                for i in f.body() {
                    let stmt = self.process_stmt(i, &ctx);
                    v.push(stmt)
                }
                new_f = f.clone();
                new_f.set_body(v);
            }
            let ret = ctx
                .get_symbol_type(name)
                .unwrap()
                .get_function_return_type()
                .unwrap();
            new_f.set_return_type(ret);
            self.module.register_function(name, new_f);
            symbols.insert(name.clone(), f.get_type());
        }
        let ctx = Context::with_local(&ctx, vec![]);
        for stmt in module.get_global_block().iter() {
            let stmt = self.process_stmt(&stmt, &ctx);
            self.module.add_stmt(stmt);
        }
    }

    fn process_struct(&self, s: &Struct, ctx: &Context) -> Struct {
        let mut fields = vec![];
        for i in s.get_fields() {
            let ty = self.process_type(&i.field_type, ctx);
            fields.push(StructField::new(i.name.clone(), ty));
        }
        Struct::new(s.name.clone(), fields)
    }
    fn process_type(&self, ty: &Type, ctx: &Context) -> Type {
        match ty {
            Type::Alias(name) => {
                let ty = ctx.get_alias_type(name).unwrap();
                self.process_type(&ty, ctx)
            }
            Type::Struct(name, s) => {
                let mut fields = vec![];
                for (name, v) in s.iter() {
                    let ty = self.process_type(v, ctx);
                    fields.push((name.clone(), ty.clone()))
                }
                Type::Struct(name.clone(), fields)
            }
            Type::Array(ty) => {
                let ty = self.process_type(ty, ctx);
                Type::Array(Box::new(ty))
            }
            _ => ty.clone(),
        }
    }
    fn process_expr(&mut self, expr: &ExprNode, ctx: &Context) -> ExprNode {
        match expr.get_expr() {
            Expr::Closure(mut l, body, _) => {
                let mut new_body = vec![];
                let mut local = vec![];
                let ctx = Context::with_capture(ctx);
                let mut param_type = vec![];
                for i in &l {
                    local.push(i.name());
                    param_type.push(i.r#type().unwrap());
                    ctx.set_symbol_type(i.name(), i.r#type().unwrap())
                }
                let ctx = Context::with_local(&ctx, local);
                for i in &body {
                    let stmt = self.process_stmt(i, &ctx);
                    new_body.push(stmt)
                }
                let captures = ctx.get_captures().unwrap();
                let actual =
                    ExprNode::new(Expr::Closure(l.clone(), new_body.clone(), captures.clone()))
                        .with_type(Type::Closure {
                            ptr: (Box::new(Type::Unit), param_type),
                            env: captures.clone(),
                        });
                let mut env_props = HashMap::new();
                let mut fields = vec![];
                for (name, ty) in &captures {
                    env_props.insert(
                        name.clone(),
                        ExprNode::new(Expr::Variable(name.clone())).with_type(ty.clone()),
                    );
                    fields.push(StructField::new(name.clone(), ty.clone()))
                }
                let env_var_name = format!("Env{}", id());
                self.module.register_struct(
                    &env_var_name,
                    Struct {
                        name: env_var_name.clone(),
                        fields,
                    },
                );
                // t0 是闭包的类型
                let t0 = actual.get_type().unwrap();
                let env_ty = t0.get_env_type().unwrap();
                let env = Expr::Struct(StructExpr {
                    name: env_var_name,
                    props: env_props,
                });
                let env_node = ExprNode::new(env).with_type(env_ty.clone());
                l.push(
                    VariableDeclaration::new("env")
                        .with_type(env_ty.clone())
                        .with_default(env_node.clone()),
                );
                // 构造一个闭包函数 接受所有捕获变量为一个结构体env,然后将val a赋值为一个包含函数指针和环境变量的结构体，a的类型为闭包
                for (name, ty) in captures {
                    new_body.insert(
                        0,
                        StmtNode::new(
                            Stmt::ValDecl(
                                VariableDeclaration::new(name.clone())
                                    .with_default(
                                        ExprNode::new(Expr::Member(
                                            Box::new(
                                                ExprNode::new(Expr::Variable("env".into()))
                                                    .with_type(env_ty.clone()),
                                            ),
                                            name,
                                        ))
                                        .with_type(ty.clone()),
                                    )
                                    .with_type(ty),
                            ),
                            Position::none(),
                        ),
                    );
                }
                let closure_var_name = format!("Closure{}", id());
                let lambda_function =
                    Function::new(closure_var_name.clone(), Type::Unit, l, new_body, false);
                dbg!(&lambda_function);
                self.module
                    .register_function(&closure_var_name, lambda_function);
                let mut closure_struct = HashMap::new();
                let closure_fn_gen_type = t0.get_closure_fn_gen_type().unwrap();
                closure_struct.insert(
                    "ptr".into(),
                    ExprNode::new(Expr::Variable(closure_var_name.clone()))
                        .with_type(closure_fn_gen_type.clone()),
                );
                closure_struct.insert("env".into(), env_node);
                let mut closure_struct_type0 = vec![];
                closure_struct_type0.push(("ptr".into(), closure_fn_gen_type.clone()));
                closure_struct_type0.push(("env".into(), env_ty.clone()));

                let mut closure_fields = vec![];
                closure_fields.push(StructField::new("ptr".into(), closure_fn_gen_type));
                closure_fields.push(StructField::new("env".into(), env_ty));
                self.module.register_struct(
                    &closure_var_name,
                    Struct {
                        name: closure_var_name.clone(),
                        fields: closure_fields,
                    },
                );
                let closure_sturct_type =
                    Type::Struct(Some(closure_var_name.clone()), closure_struct_type0);

                ctx.set_symbol_type(closure_var_name.clone(), closure_sturct_type.clone());
                ExprNode::new(Expr::Struct(StructExpr {
                    name: closure_var_name,
                    props: closure_struct,
                }))
                .with_type(closure_sturct_type.clone())
            }
            Expr::FnCall(fc) => {
                let mut fc_name = fc.name.clone();
                let mut fc_args = fc.args.clone();
                if fc.is_method {
                    let this = fc.args.first().unwrap();
                    let this = self.process_expr(&this.value, ctx);
                    let this_type = this.get_type().unwrap();
                    if this_type.is_module(){
                        fc_args.remove(0);
                        fc_name = format!("{}:{}", this.get_variable_name().unwrap(), fc_name);
                    }else{
                        let this_type_name = this_type.get_struct_name().unwrap();
                        fc_name = format!("{}.{}", this_type_name, fc_name);
                    }
                }
                let mut new_fc = fc.clone();
                let mut args = vec![];
                dbg!(&fc_name);
                let fc_type = ctx.get_symbol_type(&fc_name).unwrap();
                let mut new_generics = vec![];
                for i in &fc.generics {
                    if i.is_alias() {
                        // 当前解析函数是模版的话获取不到别名就跳过
                        let ty = ctx
                            .get_alias_type(i.get_alias_name().unwrap())
                            .unwrap_or(i.clone());
                        new_generics.push(ty)
                    } else {
                        new_generics.push(i.clone())
                    }
                }
                let mut fc_return_type = fc_type.get_function_return_type().unwrap();
                if !fc.generics.is_empty() && &fc.name != "sizeof" {
                    for i in &fc.generics {
                        fc_name = format!("{}${:?}", fc_name, i);
                    }
                    let mut template = self.module.get_function(&fc.name).unwrap().clone();

                    let mut new_body = vec![];
                    let mut symbol_types = HashMap::new();
                    let mut locals = vec![];
                    for i in template.args() {
                        let ty = i.r#type().unwrap();
                        symbol_types.insert(i.name(), ty);
                        locals.push(i.name());
                    }
                    let ctx = Context::with_value(
                        &ctx,
                        ContextKey::SymbolType,
                        ContextValue::SymbolType(Arc::new(RwLock::new(symbol_types))),
                    );
                    let ctx = Context::with_local(&ctx, locals);
                    for (index, i) in template.generic_list.iter().enumerate() {
                        ctx.set_alias_type(i.get_alias_name().unwrap(), fc.generics[index].clone());
                    }
                    for stmt in template.body() {
                        let new_stmt = self.process_stmt(stmt, &ctx);
                        new_body.push(new_stmt)
                    }
                    fc_return_type = self.process_type(template.return_type(), &ctx);
                    template.set_name(fc_name.clone());
                    template.is_template = false;
                    template.generic_list = vec![];
                    template.set_return_type(fc_return_type.clone());
                    template.set_body(new_body);
                    self.module.register_function(&fc_name, template);
                }
                let arg_count = fc_type.get_function_arg_count();
                for (idx, arg) in fc_args.iter().enumerate() {
                    if idx == arg_count - 1 {
                        let ty0 = fc_type.get_function_arg_type(idx).unwrap();
                        if ty0.is_array_vararg() {
                            let mut array_vararg_args = vec![];
                            for arg0 in fc_args.iter().skip(idx) {
                                let arg0 = self.process_expr(&arg0.value, ctx);
                                array_vararg_args.push(arg0);
                            }
                            args.push(Argument::new(
                                ExprNode::new(Expr::Array(array_vararg_args)).with_type(ty0),
                            ));
                            break;
                        }
                    }
                    let mut new_arg = arg.clone();
                    let arg0 = self.process_expr(&arg.value, ctx);
                    new_arg.value = arg0;
                    args.push(new_arg);
                }
                new_fc.args = args;
                new_fc.name = fc_name;
                new_fc.generics = new_generics;

                ExprNode::new(Expr::FnCall(new_fc)).with_type(fc_return_type)
            }
            Expr::Binary(op, l, r) => {
                let l = self.process_expr(&l, ctx);
                let r = self.process_expr(&r, ctx);
                let ty = l.get_type().unwrap();
                if ty == r.get_type().unwrap() {
                    ExprNode::new(Expr::Binary(op, Box::new(l), Box::new(r))).with_type(ty)
                } else {
                    dbg!(&l, &r);
                    panic!("type mismatch")
                }
            }
            Expr::String(s) => ExprNode::new(Expr::String(s)).with_type(Type::String),
            Expr::Int(i) => ExprNode::new(Expr::Int(i)).with_type(Type::Int64),
            Expr::Variable(name) => {
                dbg!(&name);
                let ty = ctx.get_symbol_type(&name).unwrap();
                if !ctx.is_local_variable(&name) && !ty.is_function() &&!ty.is_module() {
                    ctx.add_capture(name.clone(), ty.clone())
                }
                ExprNode::new(Expr::Variable(name)).with_type(ty)
            }
            Expr::Struct(se) => {
                let mut fields = HashMap::new();
                for (name, v) in se.get_props().iter() {
                    let i = self.process_expr(v, ctx);
                    fields.insert(name.clone(), i);
                }
                let ty = self.module.get_struct(&se.name).unwrap().get_type();
                let ty = self.process_type(&ty, ctx);
                ExprNode::new(Expr::Struct(StructExpr {
                    name: se.name.clone(),
                    props: fields,
                }))
                .with_type(ty)
            }
            Expr::Array(v0) => {
                let mut v = vec![];
                for i in v0.iter() {
                    let i = self.process_expr(i, ctx);
                    v.push(i)
                }
                ExprNode::new(Expr::Array(v)).with_type(Type::Array(Box::new(Type::Int32)))
            }
            Expr::Index(target, index) => {
                let target = self.process_expr(&target, ctx);
                let index = self.process_expr(&index, ctx);
                let ty = target.get_type().unwrap();
                let ty = ty.get_element_type().unwrap();
                dbg!(ty);
                ExprNode::new(Expr::Index(Box::new(target), Box::new(index))).with_type(ty.clone())
            }
            Expr::Member(target, name) => {
                let target = self.process_expr(&target, ctx);
                let ty = target.get_type().unwrap();
                if ty.is_module(){
                    let name = format!("{}:{}",target.get_variable_name().unwrap(),name);
                    let variable = ExprNode::new(Expr::Variable(name));
                    return self.process_expr(&variable,ctx)
                }
                let (_, ty) = ty.get_struct_field(name.clone()).unwrap();
                ExprNode::new(Expr::Member(Box::new(target), name)).with_type(ty.clone())
            }
            _ => expr.clone(),
        }
    }
    fn process_stmt(&mut self, stmt: &StmtNode, ctx: &Context) -> StmtNode {
        match stmt.get_stmt() {
            Stmt::ValDecl(decl) => {
                ctx.try_add_local(decl.name());
                let expect = decl.declaration_type.clone();
                let actual = decl.get_default().unwrap();
                let actual = self.process_expr(actual, ctx);
                let actual_type = actual.get_type().unwrap();
                if expect == None {
                    ctx.set_symbol_type(decl.name.clone(), actual_type.clone());
                    let mut new_decl = decl.clone();
                    new_decl.set_default(actual);
                    new_decl.set_type(actual_type);
                    return StmtNode::new(Stmt::ValDecl(new_decl), stmt.position());
                }
                if expect == Some(actual_type.clone()) {
                    let mut new_decl = decl.clone();
                    ctx.set_symbol_type(decl.name.clone(), expect.unwrap());
                    new_decl.set_default(actual);
                    StmtNode::new(Stmt::ValDecl(new_decl), stmt.position())
                } else {
                    dbg!(actual_type, expect);
                    panic!("type mismatch")
                }
            }

            Stmt::Return(expr) => {
                let expr = self.process_expr(&expr, ctx);
                StmtNode::new(Stmt::Return(Box::new(expr)), stmt.position())
            }
            Stmt::EvalExpr(expr) => {
                let expr = self.process_expr(&expr, ctx);
                StmtNode::new(Stmt::EvalExpr(Box::new(expr)), stmt.position())
            }
            Stmt::If(if_stmt) => {
                let mut branches = vec![];
                for i in if_stmt.get_branches() {
                    let condition = self.process_expr(i.get_condition(), ctx);
                    let body = i
                        .get_body()
                        .iter()
                        .map(|x| self.process_stmt(x, ctx))
                        .collect::<Vec<_>>();
                    let new_branch = IfBranchStmt::new(condition, body);
                    branches.push(new_branch)
                }
                let else_body = if let Some(else_body) = if_stmt.get_else_body() {
                    Some(
                        else_body
                            .iter()
                            .map(|x| self.process_stmt(x, ctx))
                            .collect::<Vec<_>>(),
                    )
                } else {
                    None
                };
                let mut new_if_stmt = if_stmt.clone();
                new_if_stmt.set_branches(branches);
                new_if_stmt.set_else_body(else_body);
                StmtNode::new(Stmt::If(new_if_stmt), stmt.position())
            }
            Stmt::Assign(target, expr) => {
                let target = self.process_expr(&target, ctx);
                let expr = self.process_expr(&expr, ctx);
                StmtNode::new(
                    Stmt::Assign(Box::new(target), Box::new(expr)),
                    stmt.position(),
                )
            }
            _ => stmt.clone(),
        }
    }
}
