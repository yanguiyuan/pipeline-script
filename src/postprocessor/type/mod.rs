use crate::context::key::ContextKey;
use crate::context::value::ContextValue;
use crate::context::Context;
use crate::lexer::position::Position;
use crate::parser::declaration::VariableDeclaration;
use crate::parser::expr::StructExpr;
use crate::parser::expr::{Expr, ExprNode};
use crate::parser::module::Module;
use crate::parser::r#struct::{Struct, StructField};
use crate::parser::r#type::Type;
use crate::parser::stmt::{IfBranchStmt, Stmt, StmtNode};
use crate::postprocessor::id::id;
use slotmap::DefaultKey;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

mod helper;
pub struct TypePostprocessor {
    module: Module,
}
impl TypePostprocessor {
    pub fn new() -> Self {
        Self {
            module: Module::new("unknown"),
        }
    }
    pub fn process(&mut self, module: DefaultKey, ctx: &Context) -> Module {
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let mut module = module_slot_map.get_mut(module).unwrap().clone();

        drop(module_slot_map);
        self.module.set_name(module.get_name());
        let submodule = module.get_submodules();
        let mut names = vec![];
        for name in submodule.keys() {
            names.push(name.clone());
        }
        for name in names {
            module.merge_into_main(ctx, &name);
        }
        module.sort_global_block();
        self.process_module(&module, ctx);
        self.module.clone()
    }
    fn process_module(&mut self, module: &Module, ctx: &Context) {
        let mut symbols = HashMap::new();
        let ctx = Context::with_value(
            ctx,
            ContextKey::SymbolType,
            ContextValue::SymbolType(Arc::new(RwLock::new(symbols.clone()))),
        );
        let alias = HashMap::new();
        let ctx = Context::with_value(
            &ctx,
            ContextKey::AliasType,
            ContextValue::AliasType(Arc::new(RwLock::new(alias))),
        );
        for module_name in module.get_submodules().keys() {
            ctx.set_symbol_type(module_name.into(), Type::Module)
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
                let new_return_ty = ctx
                    .get_alias_type(&alias)
                    .expect(format!("alias {alias} not found").as_str());
                ty = ty.with_return_type(new_return_ty)
            }
            ctx.set_symbol_type(name.clone(), ty)
        }
        for (name, f) in functions.iter() {
            if f.is_template && name != "sizeof" {
                continue;
            }
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
            let stmt = self.process_stmt(stmt, &ctx);
            self.module.add_stmt(stmt);
        }
    }

    fn process_struct(&self, s: &Struct, ctx: &Context) -> Struct {
        let mut fields = vec![];
        for i in s.get_fields() {
            let ty = Self::process_type(&i.field_type, ctx);
            fields.push(StructField::new(i.name.clone(), ty));
        }
        Struct::new(s.name.clone(), fields, s.generics.clone())
    }
    fn process_type(ty: &Type, ctx: &Context) -> Type {
        match ty {
            Type::Alias(name) => {
                let ty = ctx.get_alias_type(name).unwrap();
                Self::process_type(&ty, ctx)
            }
            Type::Struct(name, s) => {
                let mut fields = vec![];
                for (name, v) in s.iter() {
                    let ty = Self::process_type(v, ctx);
                    fields.push((name.clone(), ty.clone()))
                }
                Type::Struct(name.clone(), fields)
            }
            Type::Array(ty) => {
                let ty = Self::process_type(ty, ctx);
                Type::Array(Box::new(ty))
            }
            _ => ty.clone(),
        }
    }
    fn process_expr(&mut self, expr: &ExprNode, ctx: &Context) -> ExprNode {
        match expr.get_expr() {
            Expr::Closure(mut l, body, _) => {
                let (new_body, captures, param_type) = self.process_closure_body(&l, &body, ctx);
                let closure_var_name = format!("Closure{}", id());
                let actual = self.create_closure_expr(
                    &l,
                    &new_body,
                    &captures,
                    &param_type,
                    &closure_var_name,
                );

                let (_, env_node) = self.create_closure_env(&captures, &actual);
                self.add_env_parameter(&mut l, &env_node);

                let new_body = self.add_capture_initializations(&new_body, &captures, &env_node);
                self.register_closure_function(&closure_var_name, &l, &new_body);

                let closure_struct =
                    self.create_closure_struct(&closure_var_name, &actual, &env_node);
                ctx.set_symbol_type(closure_var_name.clone(), actual.get_type().unwrap());

                ExprNode::new(Expr::Struct(StructExpr::new(
                    closure_var_name.clone(),
                    closure_struct,
                )))
                .with_type(actual.get_type().unwrap())
            }
            Expr::FnCall(fc) => {
                // 处理方法调用的情况
                let (fc_name, fc_args) = self.process_method_call(&fc, ctx);

                // 创建新的函数调用对象
                let mut new_fc = fc.clone();
                // let mut args = vec![];

                // 获取函数类型信息
                let (mut fc_type, fc_return_type) = self.resolve_function_type(&fc_name, ctx);

                // 处理泛型参数
                let new_generics = self.process_generics(&fc.generics, ctx);

                // 处理闭包函数
                if fc_type.is_closure() {
                    self.handle_closure_function(&fc_name, &fc_type, ctx);
                    fc_type = fc_type
                        .get_closure_fn_gen_type()
                        .expect("Failed to get closure function type");
                }

                // 处理模板函数实例化
                let fc_return_type = if !fc.generics.is_empty() && &fc.name != "sizeof" {
                    self.instantiate_template_function(&fc, &fc_name, ctx)
                } else {
                    fc_return_type
                };

                // 处理函数参数
                let args = self.process_function_arguments(&fc_args, &fc_type, ctx);

                // 构建新的函数调用表达式
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
            Expr::Int(i) => {
                let context_value = ctx.get(ContextKey::Type("default".into()));
                if let Some(ContextValue::Type(ty)) = context_value {
                    if ty.is_integer() {
                        return ExprNode::new(Expr::Int(i)).with_type(ty);
                    }
                }
                ExprNode::new(Expr::Int(i)).with_type(Type::Int64)
            }
            Expr::Variable(name) => {
                let ty = ctx.get_symbol_type(&name);
                let ty = ty.unwrap();
                if !ctx.is_local_variable(&name) && !ty.is_function() && !ty.is_module() {
                    ctx.add_capture(name.clone(), ty.clone())
                }
                ExprNode::new(Expr::Variable(name)).with_type(ty)
            }
            Expr::Struct(se) => {
                // 提前获取结构体名称和泛型信息
                let struct_name = se.name.clone();
                let generics = se.get_generics().clone();

                // 分离结构体数据获取和后续操作
                let (generic_map, fields_info) = {
                    let struct_val = self.module.get_struct(&struct_name).unwrap();
                    let gm = struct_val
                        .get_generics()
                        .iter()
                        .zip(generics.iter())
                        .map(|(gen, rep)| (gen.get_alias_name().unwrap(), rep.clone()))
                        .collect::<HashMap<_, _>>();

                    let fi = se
                        .get_props()
                        .iter()
                        .map(|(name, v)| {
                            let mut field_type = struct_val.get_field_type(name).unwrap().clone();
                            field_type.try_replace_alias(&gm); // 在内部作用域使用gm
                            (name.clone(), (field_type, v.clone()))
                        })
                        .collect::<Vec<_>>();

                    (gm, fi)
                }; // 这里struct_val的借用结束

                // 处理结构体字段（此时已释放struct_val的借用）
                let fields = fields_info
                    .into_iter()
                    .map(|(name, (field_type, v))| {
                        let ctx = Context::with_type(ctx, "default".into(), field_type);
                        let processed = self.process_expr(&v, &ctx);
                        (name, processed)
                    })
                    .collect::<HashMap<_, _>>();

                // 处理泛型实例化
                let (new_name, need_register) = if !generics.is_empty() {
                    let type_params = generics
                        .iter()
                        .map(Type::as_str)
                        .collect::<Vec<_>>()
                        .join(",");
                    let new_name = format!("{}<{}>", struct_name, type_params);

                    // 重新获取结构体定义来处理字段（短期借用）
                    let struct_val = self.module.get_struct(&struct_name).unwrap();
                    let new_fields = struct_val
                        .get_fields()
                        .iter()
                        .map(|field| {
                            let mut new_field = field.clone();
                            if let Some(alias) = new_field.field_type.get_alias_name() {
                                new_field.field_type = generic_map[&alias].clone();
                            } else {
                                new_field.field_type.try_replace_alias(&generic_map);
                            }
                            new_field
                        })
                        .collect::<Vec<_>>();

                    // 注册新结构体（此时struct_val的借用已释放）
                    self.module.register_struct(
                        &new_name,
                        Struct::new(new_name.clone(), new_fields, vec![]),
                    );
                    (new_name, true)
                } else {
                    (struct_name.clone(), false)
                };

                // 获取最终类型
                let final_type = if need_register {
                    let concrete_struct = self.module.get_struct(&new_name).unwrap();
                    Self::process_type(&concrete_struct.get_type(), ctx)
                } else {
                    let struct_val = self.module.get_struct(&struct_name).unwrap();
                    Self::process_type(&struct_val.get_type(), ctx)
                };

                ExprNode::new(Expr::Struct(StructExpr::new(new_name, fields))).with_type(final_type)
            }
            Expr::Array(v0) => {
                let mut v = vec![];
                let context_value = ctx.get(ContextKey::Type("default".into()));
                if let Some(ContextValue::Type(ty)) = context_value {
                    let element_type = ty.get_element_type().unwrap();
                    let ctx = Context::with_type(ctx, "default".into(), element_type.clone());
                    for i in v0.iter() {
                        let i = self.process_expr(i, &ctx);
                        v.push(i)
                    }
                    return ExprNode::new(Expr::Array(v))
                        .with_type(Type::Array(Box::new(element_type.clone())));
                }
                for i in v0.iter() {
                    let i = self.process_expr(i, &ctx);
                    v.push(i)
                }
                ExprNode::new(Expr::Array(v)).with_type(Type::Array(Box::new(Type::Int64)))
            }
            Expr::Index(target, index) => {
                let target = self.process_expr(&target, ctx);
                let index = self.process_expr(&index, ctx);
                let ty = target.get_type().unwrap();
                let ty = ty.get_element_type().unwrap();
                ExprNode::new(Expr::Index(Box::new(target), Box::new(index))).with_type(ty.clone())
            }
            Expr::Member(target, name) => {
                let target = self.process_expr(&target, ctx);
                let ty = target.get_type().unwrap();
                if ty.is_module() {
                    let name = format!("{}:{}", target.get_variable_name().unwrap(), name);
                    let variable = ExprNode::new(Expr::Variable(name));
                    return self.process_expr(&variable, ctx);
                }
                let ty = if ty.is_array() && name == "length" {
                    Type::Int64
                } else {
                    ty.get_struct_field(name.clone()).unwrap().1.clone()
                };
                ExprNode::new(Expr::Member(Box::new(target), name)).with_type(ty)
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
                match expect {
                    None => {
                        ctx.set_symbol_type(decl.name.clone(), actual_type.clone());
                        let mut new_decl = decl.clone();
                        new_decl.set_default(actual);
                        new_decl.set_type(actual_type);
                        StmtNode::new(Stmt::ValDecl(new_decl), stmt.position())
                    }
                    Some(ty) => {
                        let mut new_decl = decl.clone();
                        ctx.set_symbol_type(decl.name.clone(), ty);
                        new_decl.set_default(actual);
                        StmtNode::new(Stmt::ValDecl(new_decl), stmt.position())
                    }
                }
            }
            Stmt::VarDecl(decl) => {
                ctx.try_add_local(decl.name());
                let expect = decl.declaration_type.clone();
                let actual = decl.get_default().unwrap();
                let actual = self.process_expr(actual, ctx);
                let actual_type = actual.get_type().unwrap();
                match expect {
                    None => {
                        ctx.set_symbol_type(decl.name.clone(), actual_type.clone());
                        let mut new_decl = decl.clone();
                        new_decl.set_default(actual);
                        new_decl.set_type(actual_type);
                        StmtNode::new(Stmt::ValDecl(new_decl), stmt.position())
                    }
                    Some(ty) => {
                        let mut new_decl = decl.clone();
                        ctx.set_symbol_type(decl.name.clone(), ty);
                        new_decl.set_default(actual);
                        StmtNode::new(Stmt::ValDecl(new_decl), stmt.position())
                    }
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
                let else_body = if_stmt.get_else_body().map(|else_body| {
                    else_body
                        .iter()
                        .map(|x| self.process_stmt(x, ctx))
                        .collect::<Vec<_>>()
                });
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
            Stmt::ForIn(var, expr, body) => {
                let expr = self.process_expr(&expr, ctx);
                let ty = expr.get_type().unwrap();
                let element_type = ty.get_element_type().unwrap();
                ctx.try_add_local(var.clone());
                ctx.set_symbol_type(var.clone(), element_type.clone());
                let body = body
                    .iter()
                    .map(|x| self.process_stmt(x, ctx))
                    .collect::<Vec<_>>();
                StmtNode::new(
                    Stmt::ForIn(var.clone(), Box::new(expr), body),
                    stmt.position(),
                )
            }
            _ => stmt.clone(),
        }
    }
}
