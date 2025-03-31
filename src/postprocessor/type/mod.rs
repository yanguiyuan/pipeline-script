use crate::ast::expr::Expr;
use crate::ast::expr::StructExpr;
use crate::ast::function::Function;
use crate::ast::module::Module;
use crate::ast::r#struct::{Struct, StructField};
use crate::ast::r#type::Type;
use crate::ast::stmt::{IfBranchStmt, MatchBranch, Stmt, StmtNode};
use crate::context::key::ContextKey;
use crate::context::value::ContextValue;
use crate::context::Context;
use crate::lexer::position::Position;
use slotmap::DefaultKey;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
mod expr;
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
        dbg!(&module);
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
        for (name, st) in module.get_functions() {
            ctx.set_symbol_type(name.clone(), st.get_type());
        }
        for (name, st) in module.get_type_aliases() {
            ctx.set_alias_type(name.clone(), st.get_type().clone());
        }
    }

    fn process_module_structs(&mut self, module: &Module, ctx: &Context) {
        for (name, st) in module.get_structs() {
            if !st.generics.is_empty() {
                continue;
            }
            let st = self.process_struct(st, ctx);
            self.module.register_struct(name, st.clone());
            ctx.set_alias_type(name.clone(), st.get_type().clone());
        }
    }

    fn process_module_functions(&mut self, module: &Module, ctx: &Context) {
        let mut functions = module.get_functions();
        self.process_function_types(&mut functions, ctx);
        self.register_processed_functions(&functions, ctx);
    }

    fn process_function_types(&mut self, functions: &mut HashMap<String, Function>, ctx: &Context) {
        for (name, f) in functions.iter_mut() {
            let mut ty = f.get_type();
            if let Some(return_type) = ty.get_function_return_type() {
                ty = Self::process_type(&ty, ctx);
                // 返回类型为泛型，需要实例化
                if let Type::Generic(b, l) = return_type {
                    if let Some(alias) = b.get_alias_name() {
                        // 获取类型模版
                        if let Some(return_alias) = ctx.get_type_alias(&alias) {
                            let mut alias_map = HashMap::new();
                            for (index, generic) in
                                return_alias.get_generic_list().iter().enumerate()
                            {
                                alias_map.insert(generic.clone(), l.get(index).unwrap().clone());
                            }

                            // 实例化所有绑定函数
                            let functions = ctx.get_type_binding_functions(alias.as_str());
                            self.instantiate_binding_functions(
                                ctx,
                                alias.as_str(),
                                &functions,
                                &l,
                                &alias_map,
                            );
                            let mut new_return_type = return_alias.get_type().clone();
                            new_return_type.try_replace_alias(&alias_map);
                            let new_name = format!(
                                "{}<{}>",
                                new_return_type.get_composite_type_name().unwrap(),
                                l.iter().map(|t| t.as_str()).collect::<Vec<_>>().join(",")
                            );

                            new_return_type.set_composite_type_name(Some(new_name.clone()));
                            // 注册一个新实例类型，并且实例化所有绑定的成员函数
                            self.module.register_type_alias(
                                new_name.as_str(),
                                new_return_type.clone(),
                                vec![],
                            );

                            ty = ty.with_return_type(new_return_type);
                        }
                    }
                }
            }
            for i in f.args_mut() {
                i.set_type(Self::process_type(&i.r#type().unwrap(), ctx));
            }
            ctx.set_symbol_type(name.clone(), ty);
        }
    }

    fn register_processed_functions(
        &mut self,
        functions: &HashMap<String, Function>,
        ctx: &Context,
    ) {
        for (name, f) in functions.iter() {
            if f.is_template && name != "sizeof" {
                continue;
            }

            let new_f = if f.is_extern {
                f.clone()
            } else {
                self.process_non_extern_function(f, ctx)
            };
            if let Some(ret) = ctx
                .get_symbol_type(name)
                .and_then(|ty| ty.get_function_return_type())
            {
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

        // 处理函数参数的默认值
        for i in 0..new_f.args().len() {
            let arg = &new_f.args()[i];

            // 先检查参数是否有默认值和类型
            if arg.has_default() && arg.r#type().is_some() {
                let param_type = arg.r#type().unwrap();
                let param_name = arg.name();

                // 创建类型上下文
                let ctx_with_type = Context::with_value(
                    &ctx,
                    ContextKey::Type("default".into()),
                    ContextValue::Type(param_type.clone()),
                );

                // 获取默认值表达式并处理
                let default_expr = arg.clone_default().unwrap();
                let processed_expr = self.process_expr(&default_expr, &ctx_with_type);

                // 检查类型是否匹配
                let default_type = processed_expr.get_type().unwrap();
                if default_type != param_type {
                    if param_type.is_integer() && default_type.is_integer() {
                        // 整数类型之间可以进行隐式转换，继续处理
                    } else {
                        eprintln!(
                            "函数 '{}' 参数 '{}' 的默认值类型 {:?} 与参数类型 {:?} 不匹配",
                            f.name(),
                            param_name,
                            default_type,
                            param_type
                        );
                        panic!("参数默认值类型不匹配");
                    }
                }

                // 更新默认值表达式
                let arg = &mut new_f.args_mut()[i];
                if let Some(default_expr_mut) = arg.get_mut_default() {
                    *default_expr_mut = processed_expr;
                }
            }
        }

        let processed_body: Vec<_> = f
            .body()
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
        dbg!(&s);
        let fields = s
            .get_fields()
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
            Type::Generic(name, n) => {
                if let Some(alias) = name.get_alias_name() {
                    if let Some(alias) = ctx.get_alias_type(&alias) {
                        let mut processed_type = Self::process_type(&alias, ctx);
                        let processed_type0 = n
                            .iter()
                            .map(|i| Self::process_type(i, ctx))
                            .collect::<Vec<_>>();
                        if processed_type.has_name() {
                            let new_name = format!(
                                "{}<{}>",
                                processed_type.get_composite_type_name().unwrap(),
                                processed_type0
                                    .iter()
                                    .map(|i| i.as_str())
                                    .collect::<Vec<_>>()
                                    .join(",")
                            );
                            processed_type.set_name(new_name)
                        }
                        return Type::GenericInstance {
                            template: Box::new(Type::Generic(name.clone(), processed_type0)),
                            instance: Box::new(processed_type),
                        };
                    }
                }
                ty.clone()
            }
            Type::Alias(name) => {
                let ty = ctx.get_alias_type(name).unwrap();
                ty
            }
            Type::Struct(name, s) => {
                let mut fields = vec![];
                for (name, v) in s.iter() {
                    let ty = Self::process_type(v, ctx);
                    fields.push((name.clone(), ty.clone()))
                }
                Type::Struct(name.clone(), fields)
            }
            Type::Pointer(ty) => {
                let ty = Self::process_type(ty, ctx);
                Type::Pointer(Box::new(ty))
            }
            Type::Array(ty) => {
                let ty = Self::process_type(ty, ctx);
                Type::Array(Box::new(ty))
            }
            Type::Enum(name, variants) => {
                let mut processed_variants = vec![];
                for (name, v) in variants.iter() {
                    match v {
                        None => {
                            processed_variants.push((name.clone(), None));
                            continue;
                        }
                        Some(v) => {
                            let ty = Self::process_type(v, ctx);
                            processed_variants.push((name.clone(), Some(ty.clone())))
                        }
                    }
                }
                Type::Enum(name.clone(), processed_variants)
            }
            Type::Ref(ty) => {
                let ty = Self::process_type(ty, ctx);
                Type::Ref(Box::new(ty))
            }
            Type::Function(ret, args) => {
                let ret = Self::process_type(ret, ctx);
                let args = args
                    .iter()
                    .map(|i| Self::process_type(i, ctx))
                    .collect::<Vec<_>>();
                Type::Function(Box::new(ret), args)
            }
            _ => ty.clone(),
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
                        ctx.set_symbol_type(
                            decl.name.clone(),
                            Type::Ref(Box::new(actual_type.clone())),
                        );
                        let mut new_decl = decl.clone();
                        new_decl.set_default(actual);
                        new_decl.set_type(Type::Ref(Box::new(actual_type.clone())));
                        StmtNode::new(Stmt::VarDecl(new_decl), stmt.position())
                    }
                    Some(ty) => {
                        let mut new_decl = decl.clone();
                        ctx.set_symbol_type(decl.name.clone(), Type::Ref(Box::new(ty.clone())));
                        new_decl.set_default(actual);
                        new_decl.set_type(Type::Ref(Box::new(ty)));
                        StmtNode::new(Stmt::VarDecl(new_decl), stmt.position())
                    }
                }
            }

            Stmt::Return(expr) => {
                let expr = self.process_expr(expr, ctx);
                StmtNode::new(Stmt::Return(Box::new(expr)), stmt.position())
            }
            Stmt::EvalExpr(expr) => {
                let expr = self.process_expr(expr, ctx);
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
                let target = self.process_expr(target, ctx);
                let expr = self.process_expr(expr, ctx);
                StmtNode::new(
                    Stmt::Assign(Box::new(target), Box::new(expr)),
                    stmt.position(),
                )
            }
            Stmt::While(expr, body) => {
                let expr = self.process_expr(expr, ctx);
                let body = body
                    .iter()
                    .map(|x| self.process_stmt(x, ctx))
                    .collect::<Vec<_>>();
                StmtNode::new(Stmt::While(Box::new(expr), body), stmt.position())
            }
            Stmt::ForIn(var, expr, body) => {
                let expr = self.process_expr(expr, ctx);
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
            Stmt::Match(expr, branches) => {
                // 处理匹配的表达式
                let processed_expr = self.process_expr(expr, ctx);
                let ctx =
                    Context::with_type(ctx, "default".into(), processed_expr.get_type().unwrap());
                // 处理匹配分支
                let mut processed_branches = vec![];
                for branch in branches {
                    let ctx = Context::with_flag(&ctx, "pattern", true);
                    // 处理模式
                    let processed_pattern = self.process_expr(branch.get_pattern(), &ctx);

                    // 创建新的上下文，用于处理分支体
                    let branch_ctx = ctx.clone();

                    // 如果模式是枚举变体，并且有关联值，将关联值添加到上下文中
                    if let Expr::EnumVariant(_, _, Some(binding)) = &processed_pattern.get_expr() {
                        if let Expr::Variable(name) = &binding.get_expr() {
                            // 获取关联值的类型
                            if let Some(Type::Enum(_, variants)) = processed_expr.get_type() {
                                // 查找变体的关联类型
                                for (variant_name, variant_type) in variants {
                                    if let Expr::EnumVariant(_, pattern_variant, _) =
                                        &processed_pattern.get_expr()
                                    {
                                        if &variant_name == pattern_variant {
                                            if let Some(ty) = variant_type {
                                                // 将关联值添加到上下文中
                                                branch_ctx
                                                    .set_symbol_type(name.clone(), ty.clone());
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // 处理分支体
                    let mut processed_body = vec![];
                    for body_stmt in branch.get_body() {
                        processed_body.push(self.process_stmt(body_stmt, &branch_ctx));
                    }

                    // 创建处理后的分支
                    processed_branches.push(MatchBranch::new(processed_pattern, processed_body));
                }

                // 创建处理后的Match语句
                StmtNode::new(
                    Stmt::Match(Box::new(processed_expr), processed_branches),
                    stmt.position(),
                )
            }
            Stmt::IfLet(pattern, expr, body, else_body) => {
                // 处理模式和表达式
                let processed_expr = self.process_expr(expr, ctx);
                let ctx =
                    Context::with_type(ctx, "default".into(), processed_expr.get_type().unwrap());
                let ctx = Context::with_flag(&ctx, "pattern", true);
                let processed_pattern = self.process_expr(pattern, &ctx);
                ctx.set_flag("pattern", false);

                // 创建新的上下文，用于处理if分支体
                let if_ctx = ctx.clone();

                // 如果模式是枚举变体，并且有关联值，将关联值添加到上下文中
                if let Expr::EnumVariant(_, _, Some(binding)) = &processed_pattern.get_expr() {
                    if let Expr::Variable(name) = &binding.get_expr() {
                        // 获取关联值的类型
                        if let Some(Type::Enum(_, variants)) = processed_expr.get_type() {
                            // 查找变体的关联类型
                            for (variant_name, variant_type) in variants {
                                if let Expr::EnumVariant(_, pattern_variant, _) =
                                    &processed_pattern.get_expr()
                                {
                                    if &variant_name == pattern_variant {
                                        if let Some(ty) = variant_type {
                                            // 将关联值添加到上下文中
                                            if_ctx.set_symbol_type(name.clone(), ty.clone());
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // 处理if分支体
                let mut processed_body = vec![];
                for body_stmt in body {
                    processed_body.push(self.process_stmt(body_stmt, &if_ctx));
                }

                // 处理else分支体（如果有）
                let processed_else_body = if let Some(else_stmts) = else_body {
                    let mut processed_else = vec![];
                    for else_stmt in else_stmts {
                        processed_else.push(self.process_stmt(else_stmt, &ctx));
                    }
                    Some(processed_else)
                } else {
                    None
                };

                // 创建处理后的IfLet语句
                StmtNode::new(
                    Stmt::IfLet(
                        Box::new(processed_pattern),
                        Box::new(processed_expr),
                        processed_body,
                        processed_else_body,
                    ),
                    stmt.position(),
                )
            }
            Stmt::IfConst(pattern, expr, body, else_body) => {
                // 处理模式和表达式
                let processed_expr = self.process_expr(expr, ctx);
                let ctx =
                    Context::with_type(ctx, "default".into(), processed_expr.get_type().unwrap());
                let ctx = Context::with_flag(&ctx, "pattern", true);
                let processed_pattern = self.process_expr(pattern, &ctx);
                ctx.set_flag("pattern", false);

                // 创建新的上下文，用于处理if分支体
                let if_ctx = ctx.clone();

                // 如果模式是枚举变体，并且有关联值，将关联值添加到上下文中
                if let Expr::EnumVariant(_, _, Some(binding)) = &processed_pattern.get_expr() {
                    if let Expr::Variable(name) = &binding.get_expr() {
                        // 获取关联值的类型
                        if let Some(Type::Enum(_, variants)) = processed_expr.get_deep_type() {
                            // 查找变体的关联类型
                            for (variant_name, variant_type) in variants {
                                if let Expr::EnumVariant(_, pattern_variant, _) =
                                    &processed_pattern.get_expr()
                                {
                                    if &variant_name == pattern_variant {
                                        if let Some(ty) = variant_type {
                                            // 将关联值添加到上下文中
                                            if_ctx.set_symbol_type(name.clone(), ty.clone());
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // 处理if分支体
                let mut processed_body = vec![];
                for body_stmt in body {
                    processed_body.push(self.process_stmt(body_stmt, &if_ctx));
                }

                // 处理else分支体（如果有）
                let processed_else_body = if let Some(else_stmts) = else_body {
                    let mut processed_else = vec![];
                    for else_stmt in else_stmts {
                        processed_else.push(self.process_stmt(else_stmt, &ctx));
                    }
                    Some(processed_else)
                } else {
                    None
                };

                // 创建处理后的IfLet语句
                StmtNode::new(
                    Stmt::IfConst(
                        Box::new(processed_pattern),
                        Box::new(processed_expr),
                        processed_body,
                        processed_else_body,
                    ),
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

    fn instantiate_binding_functions(
        &mut self,
        ctx: &Context,
        base_type_name: &str,
        functions: &[Function],
        concrete_types: &[Type],
        alias_map: &HashMap<String, Type>,
    ) {
        // 构建实例化后的类型名称
        let instantiated_type_name = format!(
            "{}<{}>",
            base_type_name,
            concrete_types
                .iter()
                .map(|t| t.as_str())
                .collect::<Vec<_>>()
                .join(",")
        );
        // self.process_function_bindings(functions,ctx)
        for function in functions {
            let function_name = function.name();
            // 解析原始函数名，分离类型名和方法名
            let parts: Vec<&str> = function_name.split('.').collect();
            if parts.len() != 2 {
                continue; // 不是标准的类型方法格式
            }

            let method_name = parts[1];

            // 构建实例化后的函数名
            let instantiated_function_name = format!("{}.{}", instantiated_type_name, method_name);

            // 克隆函数以创建新实例
            let mut instantiated_function = function.clone();

            // 更新函数名
            instantiated_function.set_name(instantiated_function_name.clone());

            // 处理函数参数中的类型别名
            for arg in instantiated_function.args_mut() {
                if arg.name() == "self" {
                    let self_type = arg.r#type().unwrap();
                    let alias = self_type.get_alias_name().unwrap();
                    let type_alias = ctx.get_type_alias(&alias).unwrap();
                    let mut alias_type = type_alias.get_type().clone();
                    if alias_type.is_enum() {
                        alias_type.set_enum_name(instantiated_type_name.clone());
                    }
                    alias_type.try_replace_alias(alias_map);
                    arg.set_type(alias_type);
                    continue;
                }
                if let Some(arg_type) = arg.r#type() {
                    let mut new_type = arg_type.clone();
                    new_type.try_replace_alias(alias_map);
                    arg.set_type(new_type);
                }
            }

            // 处理函数返回类型中的类型别名
            let mut return_type = instantiated_function.return_type().clone();
            return_type.try_replace_alias(alias_map);
            instantiated_function.set_return_type(return_type);
            instantiated_function.set_template(false);
            let mut new_body = vec![];
            let ctx = Context::with_local(ctx, vec![]);
            for i in instantiated_function.args() {
                ctx.set_symbol_type(i.name(), i.r#type().unwrap());
                ctx.try_add_local(i.name())
            }

            for i in instantiated_function.body() {
                let new_stmt = self.process_stmt(i, &ctx);
                new_body.push(new_stmt);
            }
            instantiated_function.set_body(new_body);
            let function_type = instantiated_function.get_type();
            ctx.set_symbol_type(instantiated_function_name.clone(), function_type);
            // 将实例化后的函数注册到模块
            self.module
                .register_function(&instantiated_function_name, instantiated_function);
        }
    }
}
