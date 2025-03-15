use crate::context::key::ContextKey;
use crate::context::value::ContextValue;
use crate::context::Context;
use crate::ast::declaration::VariableDeclaration;
use crate::ast::expr::FnCallExpr;
use crate::ast::expr::{Argument, Expr, ExprNode};
use crate::ast::function::Function;
use crate::ast::r#struct::{Struct, StructField};
use crate::ast::r#type::Type;
use crate::ast::stmt::{Stmt, StmtNode};
use crate::postprocessor::id::id;
use crate::postprocessor::r#type::Position;
use crate::postprocessor::r#type::StructExpr;
use crate::postprocessor::r#type::TypePostprocessor;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

impl TypePostprocessor {
    // 处理方法调用,返回处理后的函数名和参数列表
    pub(crate) fn process_method_call(
        &mut self,
        fc: &FnCallExpr,
        ctx: &Context,
    ) -> (String, Vec<Argument>) {
        let mut fc_name = fc.name.clone();
        let mut fc_args = fc.args.clone();

        if fc.is_method {
            let this = fc
                .args
                .first()
                .expect("Method call must have 'this' argument");
            let this = self.process_expr(&this.value, ctx);
            dbg!(&this);
            let this_type = this
                .get_type()
                .expect("Failed to get type for 'this' argument");

            if this_type.is_module() {
                // 模块方法调用
                fc_args.remove(0);
                fc_name = format!("{}:{}", this.get_variable_name().unwrap(), fc_name);
            } else {
                // 结构体方法调用
                let this_type_name = this_type
                    .get_composite_type_name()
                    .expect("Failed to get composite type name");
                fc_name = format!("{}.{}", this_type_name, fc_name);
            }
        }
        (fc_name, fc_args)
    }

    // 处理泛型参数
    pub(crate) fn process_generics(&self, generics: &[Type], ctx: &Context) -> Vec<Type> {
        generics
            .iter()
            .map(|generic| {
                if generic.is_alias() {
                    // 处理别名类型
                    ctx.get_alias_type(generic.get_alias_name().unwrap())
                        .unwrap_or_else(|| generic.clone())
                } else {
                    generic.clone()
                }
            })
            .collect()
    }

    // 处理闭包函数
    pub(crate) fn handle_closure_function(&self, fc_name: &str, fc_type: &Type, ctx: &Context) {
        if !ctx.is_local_variable(fc_name) {
            ctx.add_capture(fc_name.to_string(), fc_type.clone());
        }
    }

    // 处理函数参数
    pub(crate) fn process_function_arguments(
        &mut self,
        fc_args: &[Argument],
        fc_type: &Type,
        ctx: &Context,
    ) -> Vec<Argument> {
        let mut args = vec![];
        let arg_count = fc_type.get_function_arg_count();
        // 直接返回参数，不进行命名参数处理
        // 命名参数的处理将在add_default_arguments中完成
        for (idx, arg) in fc_args.iter().enumerate() {
            if idx < arg_count {
                let mut new_arg = arg.clone();
                new_arg.value = self.process_expr(&arg.value, ctx);
                args.push(new_arg);
            }
        }

        args
    }

    // 实例化模板函数
    pub(crate) fn instantiate_template_function(
        &mut self,
        fc: &FnCallExpr,
        fc_name: &str,
        ctx: &Context,
    ) -> (String, Type) {
        // 生成实例化后的函数名
        let instance_name = fc
            .generics
            .iter()
            .fold(fc_name.to_string(), |name, ty| format!("{}${:?}", name, ty));
        dbg!(&fc);
        // 获取模板函数定义
        let mut template = get_function_from_context(ctx, fc_name)
            .expect("Template function not found")
            .clone();
        dbg!(&template);
        // 创建新的上下文
        // 处理泛型参数
        for (index, generic) in template.generic_list.iter().enumerate() {
            ctx.set_alias_type(
                generic.get_alias_name().unwrap(),
                fc.generics[index].clone(),
            );
        }
        let (symbol_types, locals) = self.prepare_template_context(&template, ctx);
        let ctx = self.create_template_context(ctx, symbol_types, locals);

        // 处理函数体
        let new_body = template
            .body()
            .iter()
            .map(|stmt| self.process_stmt(stmt, &ctx))
            .collect();

        // 处理返回类型
        let return_type = Self::process_type(template.return_type(), &ctx);
        // 处理函数参数泛型
        for var in template.args_mut() {
            let old_type = var.r#type().unwrap();
            if old_type.is_alias() {
                let alias_name = old_type.get_alias_name().unwrap();
                let new_type = ctx.get_alias_type(alias_name).unwrap();
                var.set_type(new_type);
            }
        }
        // 更新模板函数
        template.set_name(instance_name.clone());
        template.is_template = false;
        template.generic_list = vec![];
        template.set_return_type(return_type.clone());
        template.set_body(new_body);

        // 注册实例化后的函数
        self.module.register_function(&instance_name, template);

        (instance_name, return_type)
    }

    // 准备模板函数上下文
    pub(crate) fn prepare_template_context(
        &self,
        template: &Function,
        ctx: &Context,
    ) -> (HashMap<String, Type>, Vec<String>) {
        let mut symbol_types = HashMap::new();
        let mut locals = vec![];

        for arg in template.args() {
            let mut ty = arg.r#type().unwrap();
            if ty.is_alias() {
                ty = ctx.get_alias_type(ty.get_alias_name().unwrap()).unwrap();
            }
            symbol_types.insert(arg.name(), ty);
            locals.push(arg.name());
        }

        (symbol_types, locals)
    }

    // 创建模板函数上下文
    pub(crate) fn create_template_context(
        &self,
        parent_ctx: &Context,
        symbol_types: HashMap<String, Type>,
        locals: Vec<String>,
    ) -> Context {
        let ctx = Context::with_value(
            parent_ctx,
            ContextKey::SymbolType,
            ContextValue::SymbolType(Arc::new(RwLock::new(symbol_types))),
        );

        Context::with_local(&ctx, locals)
    }

    // 解析函数类型
    pub(crate) fn resolve_function_type(&self, fc_name: &str, ctx: &Context) -> (Type, Type) {
        dbg!(&fc_name);
        let fc_type = ctx
            .get_symbol_type(fc_name)
            .expect("Failed to get function type");
        let fc_return_type = fc_type
            .get_function_return_type()
            .expect("Failed to get function return type");
        (fc_type, fc_return_type)
    }
    pub(crate) fn process_closure_body(
        &mut self,
        l: &[VariableDeclaration],
        body: &[StmtNode],
        ctx: &Context,
    ) -> (Vec<StmtNode>, Vec<(String, Type)>, Vec<Type>) {
        let mut new_body = vec![];
        let mut local = vec![];
        let ctx = Context::with_capture(ctx);
        let mut param_type = vec![];

        for i in l {
            local.push(i.name());
            param_type.push(i.r#type().unwrap());
            ctx.set_symbol_type(i.name(), i.r#type().unwrap())
        }

        let ctx = Context::with_local(&ctx, local);
        for i in body {
            let stmt = self.process_stmt(i, &ctx);
            new_body.push(stmt)
        }

        let captures = ctx.get_captures().unwrap();
        let captures_vec: Vec<(String, Type)> = captures.into_iter().collect();
        (new_body, captures_vec, param_type)
    }

    pub(crate) fn create_closure_expr(
        &self,
        l: &[VariableDeclaration],
        new_body: &[StmtNode],
        captures: &[(String, Type)],
        param_type: &[Type],
        closure_var_name: &str,
    ) -> ExprNode {
        let captures_map: HashMap<String, Type> = captures.iter().cloned().collect();
        ExprNode::new(Expr::Closure(
            l.to_vec(),
            new_body.to_vec(),
            captures_map.clone().into_iter().collect(),
        ))
        .with_type(Type::Closure {
            name: Some(closure_var_name.to_string()),
            ptr: (Box::new(Type::Unit), param_type.to_vec()),
            env: captures_map.clone().into_iter().collect(),
        })
    }

    pub(crate) fn create_closure_env(
        &mut self,
        captures: &[(String, Type)],
        actual: &ExprNode,
    ) -> (String, ExprNode) {
        let mut env_props = HashMap::new();
        let mut fields = vec![];

        for (name, ty) in captures {
            env_props.insert(
                name.clone(),
                ExprNode::new(Expr::Variable(name.clone())).with_type(ty.clone()),
            );
            fields.push(StructField::new(name.clone(), ty.clone()))
        }

        let env_var_name = format!("Env{}", id());
        self.module.register_struct(
            &env_var_name,
            Struct::new(env_var_name.clone(), fields, vec![]),
        );

        let t0 = actual.get_type().unwrap();
        let env_ty = t0.get_env_type().unwrap();
        let env = Expr::Struct(StructExpr::new(env_var_name.clone(), env_props));
        let env_node = ExprNode::new(env).with_type(env_ty.clone());

        (env_var_name, env_node)
    }

    pub(crate) fn add_env_parameter(&self, l: &mut Vec<VariableDeclaration>, env_node: &ExprNode) {
        let env_ty = env_node.get_type().unwrap();
        l.push(
            VariableDeclaration::new("env")
                .with_type(env_ty.clone())
                .with_default(env_node.clone()),
        );
    }

    pub(crate) fn add_capture_initializations(
        &self,
        new_body: &[StmtNode],
        captures: &[(String, Type)],
        env_node: &ExprNode,
    ) -> Vec<StmtNode> {
        let mut result = new_body.to_vec();
        let env_ty = env_node.get_type().unwrap();

        for (name, ty) in captures {
            result.insert(
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
                                    name.clone(),
                                ))
                                .with_type(ty.clone()),
                            )
                            .with_type(ty.clone()),
                    ),
                    Position::none(),
                ),
            );
        }

        result
    }

    pub(crate) fn register_closure_function(
        &mut self,
        closure_var_name: &str,
        l: &[VariableDeclaration],
        new_body: &[StmtNode],
    ) {
        let lambda_function = Function::new(
            closure_var_name.to_string(),
            Type::Unit,
            l.to_vec(),
            new_body.to_vec(),
            false,
        );
        self.module
            .register_function(closure_var_name, lambda_function);
    }

    pub(crate) fn create_closure_struct(
        &mut self,
        closure_var_name: &str,
        actual: &ExprNode,
        env_node: &ExprNode,
    ) -> HashMap<String, ExprNode> {
        let t0 = actual.get_type().unwrap();
        let closure_fn_gen_type = t0.get_closure_fn_gen_type().unwrap();
        let env_ty = t0.get_env_type().unwrap();

        let mut closure_struct = HashMap::new();
        closure_struct.insert(
            "ptr".into(),
            ExprNode::new(Expr::Variable(closure_var_name.to_string()))
                .with_type(closure_fn_gen_type.clone()),
        );
        closure_struct.insert("env".into(), env_node.clone());

        let closure_fields = vec![
            StructField::new("ptr".into(), closure_fn_gen_type),
            StructField::new("env".into(), env_ty),
        ];

        self.module.register_struct(
            closure_var_name,
            Struct::new(closure_var_name.to_string(), closure_fields, vec![]),
        );

        closure_struct
    }
}

fn get_function_from_context(ctx: &Context, name: &str) -> Option<Function> {
    let modules = ctx.get_module_slot_map();
    let readable_modules = modules.read().unwrap();
    for (_, module) in readable_modules.iter() {
        let function = module.get_function(name);
        if function.is_some() {
            return function;
        }
    }
    None
}
