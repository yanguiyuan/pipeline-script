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
use crate::parser::function::Function;
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
        let ctx = self.create_module_context(ctx);
        self.process_module_symbols(module, &ctx);
        self.process_module_structs(module, &ctx);
        self.process_module_functions(module, &ctx);
        self.process_global_block(module, &ctx);
    }

    fn create_module_context(&self, ctx: &Context) -> Context {
        let symbols = HashMap::new();
        let ctx = Context::with_value(
            ctx,
            ContextKey::SymbolType,
            ContextValue::SymbolType(Arc::new(RwLock::new(symbols))),
        );
        let alias = HashMap::new();
        Context::with_value(
            &ctx,
            ContextKey::AliasType,
            ContextValue::AliasType(Arc::new(RwLock::new(alias))),
        )
    }

    fn process_module_symbols(&self, module: &Module, ctx: &Context) {
        // 处理子模块
        for module_name in module.get_submodules().keys() {
            ctx.set_symbol_type(module_name.into(), Type::Module);
        }

        // 处理结构体类型别名
        for (name, st) in module.get_structs() {
            ctx.set_alias_type(name.clone(), st.get_type());
        }
    }

    fn process_module_structs(&mut self, module: &Module, ctx: &Context) {
        for (name, st) in module.get_structs() {
            let st = self.process_struct(st, ctx);
            self.module.register_struct(name, st.clone());
        }
    }

    fn process_module_functions(&mut self, module: &Module, ctx: &Context) {
        let mut functions = module.get_functions();
        self.process_function_bindings(&mut functions, ctx);
        self.process_function_types(&functions, ctx);
        self.register_processed_functions(&functions, ctx);
    }

    fn process_function_bindings(&self, functions: &mut HashMap<String, Function>, ctx: &Context) {
        for f in functions.values_mut() {
            if f.has_binding() {
                f.insert_arg(
                    0,
                    VariableDeclaration::new("this")
                        .with_type(ctx.get_alias_type(f.get_binding()).unwrap()),
                );
            }
        }
    }

    fn process_function_types(&self, functions: &HashMap<String, Function>, ctx: &Context) {
        for (name, f) in functions.iter() {
            let mut ty = f.get_type();
            if let Some(return_type) = ty.get_function_return_type() {
                if return_type.is_alias() {
                    if let Some(alias) = return_type.get_alias_name() {
                        if let Some(new_return_ty) = ctx.get_alias_type(&alias) {
                            ty = ty.with_return_type(new_return_ty);
                        }
                    }
                }
            }
            ctx.set_symbol_type(name.clone(), ty);
        }
    }

    fn register_processed_functions(&mut self, functions: &HashMap<String, Function>, ctx: &Context) {
        for (name, f) in functions.iter() {
            if f.is_template && name != "sizeof" {
                continue;
            }

            let new_f = if f.is_extern {
                f.clone()
            } else {
                self.process_non_extern_function(f, ctx)
            };

            if let Some(ret) = ctx.get_symbol_type(name)
                .and_then(|ty| ty.get_function_return_type()) {
                let mut new_f = new_f;
                new_f.set_return_type(ret);
                self.module.register_function(name, new_f);
            }
        }
    }

    fn process_non_extern_function(&mut self, f: &Function, ctx: &Context) -> Function {
        let mut symbol_types = HashMap::new();
        let mut locals = vec![];

        for arg in f.args() {
            if let Some(ty) = arg.r#type() {
                symbol_types.insert(arg.name(), ty);
                locals.push(arg.name());
            }
        }

        let ctx = Context::with_value(
            ctx,
            ContextKey::SymbolType,
            ContextValue::SymbolType(Arc::new(RwLock::new(symbol_types))),
        );
        let ctx = Context::with_local(&ctx, locals);

        let mut new_f = f.clone();
        let processed_body: Vec<_> = f.body()
            .iter()
            .map(|stmt| self.process_stmt(stmt, &ctx))
            .collect();
        new_f.set_body(processed_body);
        new_f
    }

    fn process_global_block(&mut self, module: &Module, ctx: &Context) {
        let ctx = Context::with_local(ctx, vec![]);
        for stmt in module.get_global_block() {
            let stmt = self.process_stmt(stmt, &ctx);
            self.module.add_stmt(stmt);
        }
    }

    fn process_struct(&self, s: &Struct, ctx: &Context) -> Struct {
        let fields = s.get_fields()
            .iter()
            .map(|i| {
                let ty = Self::process_type(&i.field_type, ctx);
                StructField::new(i.name.clone(), ty)
            })
            .collect();
        
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

                // 获取函数类型信息
                let (mut fc_type, mut fc_return_type) = self.resolve_function_type(&fc_name, ctx);

                // 处理泛型参数
                let new_generics = self.process_generics(&fc.generics, ctx);

                // 处理闭包函数
                if fc_type.is_closure() {
                    self.handle_closure_function(&fc_name, &fc_type, ctx);
                    fc_type = fc_type
                        .get_closure_fn_gen_type()
                        .unwrap_or_else(|| {
                            eprintln!("无法获取闭包函数 '{}' 的类型", fc_name);
                            panic!("闭包函数类型获取失败");
                        });
                }

                // 处理模板函数实例化
                if !fc.generics.is_empty() && &fc.name != "sizeof" {
                    fc_return_type = self.instantiate_template_function(&fc, &fc_name, ctx);
                }

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
                // 获取左右操作数的类型
                let l_type = l.get_type().unwrap();
                let r_type = r.get_type().unwrap();
                
                // 如果类型相同，直接返回
                if l_type == r_type {
                    return ExprNode::new(Expr::Binary(op, Box::new(l), Box::new(r))).with_type(l_type);
                }
                
                // 处理数值类型之间的转换
                if l_type.is_integer() && r_type.is_integer() {
                    // 整数类型间的转换 - 选择更大的类型
                    let result_type = self.get_wider_integer_type(&l_type, &r_type);
                    return ExprNode::new(Expr::Binary(op, Box::new(l), Box::new(r))).with_type(result_type);
                } else if (l_type.is_integer() && (r_type == Type::Float || r_type == Type::Double)) ||
                          ((l_type == Type::Float || l_type == Type::Double) && r_type.is_integer()) {
                    // 整数和浮点数之间的转换 - 选择浮点类型
                    let result_type = if l_type == Type::Double || r_type == Type::Double {
                        Type::Double
                    } else {
                        Type::Float
                    };
                    return ExprNode::new(Expr::Binary(op, Box::new(l), Box::new(r))).with_type(result_type);
                } else if l_type == Type::Float && r_type == Type::Double {
                    // Float 和 Double 之间的转换 - 选择 Double
                    return ExprNode::new(Expr::Binary(op, Box::new(l), Box::new(r))).with_type(Type::Double);
                } else if l_type == Type::Double && r_type == Type::Float {
                    // Double 和 Float 之间的转换 - 选择 Double
                    return ExprNode::new(Expr::Binary(op, Box::new(l), Box::new(r))).with_type(Type::Double);
                }
                
                // 对于不支持的类型组合，输出更详细的错误信息
                eprintln!("类型不匹配: 左操作数类型 {:?}, 右操作数类型 {:?}, 操作符 {:?}", 
                          l_type, r_type, op);
                panic!("二元操作符类型不匹配: 无法对 {:?} 和 {:?} 执行 {:?} 操作", 
                       l_type, r_type, op);
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
                let final_type = {
                    let struct_name_to_lookup = if need_register { &new_name } else { &struct_name };
                    let struct_val = self.module.get_struct(struct_name_to_lookup).unwrap();
                    Self::process_type(&struct_val.get_type(), ctx)
                };

                ExprNode::new(Expr::Struct(StructExpr::new(new_name, fields))).with_type(final_type)
            }
            Expr::Array(v0) => {
                // 如果数组为空，直接使用上下文中的类型或默认为Int64
                if v0.is_empty() {
                    let element_type = match ctx.get(ContextKey::Type("default".into())) {
                        Some(ContextValue::Type(ty)) => match ty.get_element_type() {
                            Some(element_ty) => element_ty.clone(),
                            None => Type::Int64
                        },
                        _ => Type::Int64
                    };
                    
                    return ExprNode::new(Expr::Array(vec![])).with_type(Type::Array(Box::new(element_type)));
                }
                
                let mut v = vec![];
                
                // 获取元素类型（从上下文或默认为Int64）
                let element_type = match ctx.get(ContextKey::Type("default".into())) {
                    Some(ContextValue::Type(ty)) => ty.get_element_type().unwrap().clone(),
                    _ => Type::Int64
                };
                
                // 使用带有正确元素类型的上下文处理数组元素
                let ctx = Context::with_type(ctx, "default".into(), element_type.clone());
                
                // 处理所有数组元素
                for i in v0.iter() {
                    let i = self.process_expr(i, &ctx);
                    v.push(i);
                }
                
                ExprNode::new(Expr::Array(v)).with_type(Type::Array(Box::new(element_type)))
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
                        ctx.set_symbol_type(decl.name.clone(), Type::Pointer(Box::new(actual_type.clone())));
                        let mut new_decl = decl.clone();
                        new_decl.set_default(actual);
                        new_decl.set_type(Type::Pointer(Box::new(actual_type.clone())));
                        StmtNode::new(Stmt::VarDecl(new_decl), stmt.position())
                    }
                    Some(ty) => {
                        let mut new_decl = decl.clone();
                        ctx.set_symbol_type(decl.name.clone(), Type::Pointer(Box::new(ty)));
                        new_decl.set_default(actual);
                        dbg!(&new_decl);
                        StmtNode::new(Stmt::VarDecl(new_decl), stmt.position())
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
            Stmt::While(expr, body) => {
                let expr = self.process_expr(&expr, ctx);
                let body = body
                    .iter()
                    .map(|x| self.process_stmt(x, ctx))
                    .collect::<Vec<_>>();
                StmtNode::new(
                    Stmt::While(Box::new(expr), body),
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
    // 获取两个整数类型中更宽的类型
    fn get_wider_integer_type(&self, t1: &Type, t2: &Type) -> Type {
        if !t1.is_integer() || !t2.is_integer() {
            panic!("get_wider_integer_type 只能用于整数类型");
        }
        
        // 按照位宽排序: Int64 > Int32 > Int16 > Int8
        if *t1 == Type::Int64 || *t2 == Type::Int64 {
            Type::Int64
        } else if *t1 == Type::Int32 || *t2 == Type::Int32 {
            Type::Int32
        } else if *t1 == Type::Int16 || *t2 == Type::Int16 {
            Type::Int16
        } else {
            Type::Int8
        }
    }
}
