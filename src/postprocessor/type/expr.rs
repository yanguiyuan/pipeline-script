use crate::ast::expr::{Expr, ExprNode, StructExpr};
use crate::ast::r#struct::Struct;
use crate::ast::r#type::Type;
use crate::context::key::ContextKey;
use crate::context::value::ContextValue;
use crate::context::Context;
use crate::postprocessor::id::id;
use crate::postprocessor::r#type::helper::get_struct_from_context;
use crate::postprocessor::r#type::TypePostprocessor;
use std::collections::HashMap;

impl TypePostprocessor {
    pub(crate) fn process_expr(&mut self, expr: &ExprNode, ctx: &Context) -> ExprNode {
        match expr.get_expr().clone() {
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
                let (mut fc_name, fc_args, caller_type) = self.process_method_call(&fc, ctx);
                if !fc.type_generics.is_empty() || fc_name.contains('<') {
                    let function_type;
                    // 实例化函数
                    (fc_name, function_type) =
                        self.instantiate_template_function(&fc, &fc_name, ctx, caller_type);
                    ctx.set_symbol_type(fc_name.clone(), function_type);
                }
                // 创建新的函数调用对象
                let mut new_fc = fc.clone();

                // 获取函数类型信息
                let (mut fc_type, mut fc_return_type) = self.resolve_function_type(&fc_name, ctx);

                // 处理泛型参数
                let new_generics = self.process_generics(&fc.generics, ctx);

                // 处理闭包函数
                if fc_type.is_closure() {
                    self.handle_closure_function(&fc_name, &fc_type, ctx);
                    fc_type = fc_type.get_closure_fn_gen_type().unwrap_or_else(|| {
                        eprintln!("无法获取闭包函数 '{}' 的类型", fc_name);
                        panic!("闭包函数类型获取失败");
                    });
                }
                // 处理模板函数实例化
                if !fc.generics.is_empty() && &fc.name != "sizeof" {
                    (fc_name, fc_return_type) =
                        self.instantiate_template_function(&fc, &fc_name, ctx, None);
                }

                // 处理函数参数
                let args = self.process_function_arguments(&fc_args, &fc_type, ctx);

                // 构建新的函数调用表达式
                new_fc.args = args;
                new_fc.name = fc_name.clone();
                new_fc.generics = new_generics;

                ExprNode::new(Expr::FnCall(new_fc)).with_type(fc_return_type)
            }
            Expr::Binary(op, l, r) => {
                let l = self.process_expr(&l, ctx);
                let r = self.process_expr(&r, ctx);
                // 获取左右操作数的类型
                let mut l_type = l.get_type().unwrap();
                let mut r_type = r.get_type().unwrap();

                // 如果类型相同，直接返回
                if l_type == r_type {
                    return ExprNode::new(Expr::Binary(op.clone(), Box::new(l), Box::new(r)))
                        .with_type(l_type);
                }

                // 处理数值类型之间的转换
                if r_type.is_ref() {
                    r_type = r_type.get_element_type().unwrap().clone();
                }
                // 处理数值类型之间的转换
                if l_type.is_ref() {
                    l_type = l_type.get_element_type().unwrap().clone();
                }
                if l_type.is_integer() && r_type.is_integer() {
                    // 整数类型间的转换 - 选择更大的类型
                    let result_type = self.get_wider_integer_type(&l_type, &r_type);
                    return ExprNode::new(Expr::Binary(op.clone(), Box::new(l), Box::new(r)))
                        .with_type(result_type);
                } else if (l_type.is_integer() && (r_type == Type::Float || r_type == Type::Double))
                    || ((l_type == Type::Float || l_type == Type::Double) && r_type.is_integer())
                {
                    // 整数和浮点数之间的转换 - 选择浮点类型
                    let result_type = if l_type == Type::Double || r_type == Type::Double {
                        Type::Double
                    } else {
                        Type::Float
                    };
                    return ExprNode::new(Expr::Binary(op.clone(), Box::new(l), Box::new(r)))
                        .with_type(result_type);
                } else if l_type == Type::Float && r_type == Type::Double {
                    // Float 和 Double 之间的转换 - 选择 Double
                    return ExprNode::new(Expr::Binary(op.clone(), Box::new(l), Box::new(r)))
                        .with_type(Type::Double);
                } else if l_type == Type::Double && r_type == Type::Float {
                    // Double 和 Float 之间的转换 - 选择 Double
                    return ExprNode::new(Expr::Binary(op.clone(), Box::new(l), Box::new(r)))
                        .with_type(Type::Double);
                }

                // 对于不支持的类型组合，输出更详细的错误信息
                eprintln!(
                    "类型不匹配: 左操作数类型 {:?}, 右操作数类型 {:?}, 操作符 {:?}",
                    l_type, r_type, op
                );
                panic!(
                    "二元操作符类型不匹配: 无法对 {:?} 和 {:?} 执行 {:?} 操作",
                    l_type, r_type, op
                );
            }
            Expr::String(s) => ExprNode::new(Expr::String(s)).with_type(Type::String),
            Expr::Int(i) => {
                let context_value = ctx.get(ContextKey::Type("default".into()));
                if let Some(ContextValue::Type(ty)) = context_value {
                    if ty.is_integer() {
                        return ExprNode::new(Expr::Int(i)).with_type(ty.clone());
                    }
                }
                ExprNode::new(Expr::Int(i)).with_type(Type::Int64)
            }
            Expr::Float(f) => ExprNode::new(Expr::Float(f)).with_type(Type::Float),
            Expr::Variable(name) => {
                dbg!(&name);
                let ty = ctx.get_symbol_type(&name);
                let ty = ty.unwrap();
                if !ctx.is_local_variable(&name) && !ty.is_function() && !ty.is_module() {
                    ctx.add_capture(name.clone(), ty.clone())
                }
                ExprNode::new(Expr::Variable(name.clone())).with_type(ty)
            }
            Expr::Struct(se) => {
                // 提前获取结构体名称和泛型信息
                let struct_name = se.name.clone();
                let generics = se
                    .get_generics()
                    .iter()
                    .map(|ty| Self::process_type(ty, ctx))
                    .collect::<Vec<_>>();
                let struct_val = get_struct_from_context(ctx, &struct_name).unwrap();
                // 分离结构体数据获取和后续操作
                let (generic_map, fields_info) = {
                    // 获取模版结构体

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
                            field_type = Self::process_type(&field_type, ctx);
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
                    let struct_name_to_lookup = if need_register {
                        &new_name
                    } else {
                        &struct_name
                    };
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
                            None => Type::Int64,
                        },
                        _ => Type::Int64,
                    };

                    return ExprNode::new(Expr::Array(vec![]))
                        .with_type(Type::Array(Box::new(element_type)));
                }

                let mut v = vec![];

                // 获取元素类型（从上下文或默认为Int64）
                let element_type = match ctx.get(ContextKey::Type("default".into())) {
                    Some(ContextValue::Type(ty)) => ty.get_element_type().unwrap().clone(),
                    _ => {
                        let first = v0.first().unwrap();
                        let first = self.process_expr(first, ctx);
                        first.get_type().unwrap()
                    }
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
                let mut ty = target.get_type().unwrap();
                if ty.is_ref() {
                    ty = ty.unwrap_ref();
                }
                let ty = ty.get_element_type().unwrap();
                ExprNode::new(Expr::Index(Box::new(target), Box::new(index))).with_type(ty.clone())
            }
            Expr::Member(target, name) => {
                let target = self.process_expr(&target, ctx);
                dbg!(&target);
                let mut ty = target.get_type().unwrap();
                if ty.is_ref() {
                    ty = ty.unwrap_ref();
                }
                if ty.is_module() {
                    let name = format!("{}:{}", target.get_variable_name().unwrap(), name);
                    let variable = ExprNode::new(Expr::Variable(name));
                    return self.process_expr(&variable, ctx);
                }

                let ty = if ty.is_array() && name == "length" {
                    Type::Int64
                } else {
                    dbg!(&ty);
                    dbg!(&name);
                    ty.get_struct_field(name.clone()).unwrap().1.clone()
                };
                ExprNode::new(Expr::Member(Box::new(target), name.clone())).with_type(ty)
            }
            Expr::Boolean(b) => ExprNode::new(Expr::Boolean(b)).with_type(Type::Bool),
            Expr::EnumVariant(enum_name, variant_name, value) => {
                // 克隆枚举名称
                let enum_name_clone = enum_name.clone();
                let is_pattern = ctx.get_flag("pattern").unwrap_or(false);
                // 获取枚举类型
                let enum_type = if is_pattern {
                    ctx.get(ContextKey::Type("default".into()))
                        .unwrap_or_else(|| {
                            panic!("未找到枚举类型: {}", enum_name_clone);
                        })
                        .as_type()
                } else {
                    ctx.get_type_alias(&enum_name_clone)
                        .unwrap_or_else(|| {
                            panic!("未找到枚举类型: {}", enum_name_clone);
                        })
                        .get_type()
                        .clone()
                };

                // 处理关联值
                let processed_value = match value {
                    Some(v) => {
                        // v目前只能是变量，直接处理

                        let processed = if is_pattern {
                            let inner_type =
                                enum_type.get_enum_variant_type(&variant_name).unwrap();
                            ctx.try_add_local(v.get_variable_name().unwrap());
                            v.clone().with_type(inner_type)
                        } else {
                            self.process_expr(&v, ctx)
                        };

                        let value_type = processed.get_type().unwrap();

                        // 检查是否需要泛型实例化
                        if let Type::Enum(Some(name), variants) = &enum_type {
                            // 查找变体的关联类型
                            let mut variant_type = None;
                            for (var_name, var_type) in variants {
                                if var_name == &variant_name {
                                    variant_type = var_type.clone();
                                    break;
                                }
                            }

                            // 如果变体有关联类型，并且是泛型参数
                            if let Some(Type::Alias(type_param)) = variant_type {
                                // 创建泛型映射
                                let mut generic_map = HashMap::new();
                                generic_map.insert(type_param.clone(), value_type.clone());

                                // 创建实例化后的枚举类型名称
                                let new_enum_name = format!("{}<{}>", name, value_type.as_str());

                                // 实例化变体类型
                                let mut new_variants = vec![];
                                for (var_name, var_type) in variants {
                                    let new_var_type = if let Some(ty) = var_type {
                                        if let Type::Alias(param) = ty {
                                            if param == &type_param {
                                                Some(value_type.clone())
                                            } else {
                                                var_type.clone()
                                            }
                                        } else {
                                            var_type.clone()
                                        }
                                    } else {
                                        None
                                    };
                                    new_variants.push((var_name.clone(), new_var_type));
                                }

                                // 创建实例化后的枚举类型
                                let new_enum_type =
                                    Type::Enum(Some(new_enum_name.clone()), new_variants);
                                // 注册新的枚举类型
                                self.module.register_type_alias(
                                    &new_enum_name,
                                    new_enum_type.clone(),
                                    vec![],
                                );

                                // 返回实例化后的枚举变体表达式
                                return ExprNode::new(Expr::EnumVariant(
                                    new_enum_name,
                                    variant_name.clone(),
                                    Some(Box::new(processed.clone())),
                                ))
                                .with_type(new_enum_type);
                            }
                        }

                        Some(Box::new(processed.clone()))
                    }
                    None => None,
                };

                // 创建枚举变体表达式
                ExprNode::new(Expr::EnumVariant(
                    enum_name.clone(),
                    variant_name.clone(),
                    processed_value,
                ))
                .with_type(enum_type)
            }
            _ => expr.clone(),
        }
    }
}
