use crate::compiler::Compiler;
use crate::context::key::ContextKey;
use crate::context::Context;

use crate::ast::expr::{Expr, ExprNode, Op};
use crate::ast::r#type::Type;
use crate::llvm::global::Global;
use crate::llvm::value::int::Int32Value;
use crate::llvm::value::penum::EnumVariantValue;
use crate::llvm::value::pstruct::StructValue;
use crate::llvm::value::LLVMValue;
use std::collections::HashMap;

impl Compiler {
    pub(crate) fn compile_expr_with_ptr(&self, expr: &ExprNode, ctx: &Context) -> LLVMValue {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let ty0 = expr.get_type().unwrap();

        match expr.get_expr() {
            Expr::Variable(name) => self.compile_variable_ptr(name, ctx),
            Expr::Index(target, index) => self.compile_index_ptr(target, index, &ty0, ctx),
            Expr::Member(target, field_name) => self.compile_member_ptr(target, field_name, ctx),
            Expr::Struct(_) => {
                let val = self.compile_expr(expr, ctx);
                let element_ty = ty0.get_element_type().unwrap();
                let ptr = builder.build_alloca("", &self.compile_type(element_ty));
                ptr.as_reference().unwrap().store(ctx, val);
                // builder.build_store(ptr.clone(), val);
                ptr
            }
            Expr::FnCall(ref fc) => self.compile_fn_call(fc, ctx),
            Expr::Address(target) => self.compile_expr_with_ptr(target, ctx),
            // Expr::EnumVariant(_, ref variant_name, ref value) => {
            //     self.compile_enum_variant_ptr(variant_name, value, ctx)
            // }
            Expr::None => LLVMValue::Unit,
            t => todo!("Unknown expr for ptr: {:?}", t),
        }
    }

    pub(crate) fn compile_expr(&self, expr: &ExprNode, ctx: &Context) -> LLVMValue {
        let builder = ctx.get_builder();
        let ty0 = expr.get_type().unwrap();
        match expr.get_expr() {
            Expr::FnCall(ref fc) => self.compile_fn_call(fc, ctx),
            Expr::Int(i) => self.compile_int_literal(ctx, *i),
            Expr::Float(f) => Global::const_float(*f),
            Expr::String(s, ..) => {
                let ptr = builder.build_global_string("", s);
                ptr
            }
            Expr::Binary(op, l, r) => self.compile_binary_op(op, l, r, ctx),
            Expr::Variable(name) => self.compile_variable(name, ctx),
            Expr::Array(v) => self.compile_array(v, &ty0, ctx),
            Expr::Index(target, index) => self.compile_index(target, index, ctx),
            Expr::Struct(s) => LLVMValue::Struct(self.compile_struct(s, ctx)),
            Expr::Member(target, field_name) => self.compile_member(target, field_name, ctx),
            Expr::Address(target) => self.compile_expr_with_ptr(target, ctx),
            Expr::EnumVariant(enum_name, ref variant_name, ref value) => LLVMValue::EnumVariant(
                self.compile_enum_variant(enum_name, variant_name, value, &ty0, ctx),
            ),
            Expr::None => LLVMValue::Unit,
            Expr::Boolean(b) => Global::const_bool(*b).into(),
            t => todo!("Unknown expr: {:?}", t),
        }
    }

    // ===== 指针相关编译函数 =====

    fn compile_variable_ptr(&self, name: &str, ctx: &Context) -> LLVMValue {
        ctx.get_symbol(name).unwrap()
    }

    fn compile_index_ptr(
        &self,
        target: &ExprNode,
        index: &ExprNode,
        ty0: &Type,
        ctx: &Context,
    ) -> LLVMValue {
        let v = self.compile_expr(target, ctx);
        let index = self.compile_expr(index, ctx);
        let builder = ctx.get_builder();
        let array_ptr = builder.build_array_gep(self.compile_type(ty0), v, index);
        array_ptr
    }

    fn compile_member_ptr(&self, target: &ExprNode, field_name: &str, ctx: &Context) -> LLVMValue {
        let v = self.compile_expr_with_ptr(target, ctx);
        let element = v.as_reference().unwrap().get_value(ctx);
        if element.is_pointer() {
            return element
                .as_pointer()
                .unwrap()
                .get_struct_field_ptr(ctx, field_name)
                .unwrap();
        }
        v.as_reference()
            .unwrap()
            .get_struct_field_ptr(ctx, field_name)
            .unwrap()
    }

    // fn compile_enum_variant_ptr(
    //     &self,
    //     variant_name: &str,
    //     value: &Option<Box<ExprNode>>,
    //     ctx: &Context,
    // ) -> LLVMValue {
    //     // 获取枚举类型
    //     let enum_type = ty0.clone();
    //
    //     // 创建枚举实例
    //     let builder = ctx.get_builder();
    //
    //     // 分配内存
    //     let enum_ptr = builder.build_alloca("enum_instance", &enum_type.as_llvm_type());
    //
    //     // 获取变体索引
    //     let mut variant_index = 0;
    //     if let Type::Enum(_, variants) = &enum_type {
    //         for (i, (name, _)) in variants.iter().enumerate() {
    //             if name == variant_name {
    //                 variant_index = i;
    //                 break;
    //             }
    //         }
    //     }
    //
    //     // 设置标签字段（第一个字段）
    //     let tag_ptr = builder.build_struct_gep(enum_type.as_llvm_type(), enum_ptr, 0);
    //     builder.build_store(tag_ptr, Global::const_i32(variant_index as i32));
    //
    //     // 如果有关联值，设置数据字段（第二个字段）
    //     if let Some(val) = value {
    //         // 编译关联值
    //         let val_value = self.compile_expr(val, ctx);
    //
    //         // 设置数据字段
    //         let data_ptr = builder.build_struct_gep(enum_type.as_llvm_type(), enum_ptr.clone(), 1);
    //         builder.build_store(data_ptr, val_value);
    //     }
    //
    //     // 返回枚举实例
    //     enum_ptr
    // }

    // ===== 值相关编译函数 =====

    fn compile_variable(&self, name: &str, ctx: &Context) -> LLVMValue {
        // 查找全局符号或函数
        let ptr = ctx.get_symbol(name).unwrap();
        if ptr.is_reference() {
            return ptr.as_reference().unwrap().get_value(ctx);
        }
        ptr
    }

    fn compile_int_literal(&self, ctx: &Context, i: i64) -> LLVMValue {
        let ty = ctx.get(ContextKey::Type("default".into()));
        if let Some(ty) = ty {
            let t = ty.as_type();
            return match t {
                Type::Int8 => Global::const_i8(i as i8).into(),
                Type::Int16 => Global::const_i16(i as i16).into(),
                Type::Int32 => Global::const_i32(i as i32).into(),
                Type::Int64 => Global::const_i64(i).into(),
                _ => Global::const_i64(i).into(),
            };
        }
        Global::const_i64(i).into()
    }

    fn compile_binary_op(&self, op: &Op, l: &ExprNode, r: &ExprNode, ctx: &Context) -> LLVMValue {
        let l = self.compile_expr(l, ctx);
        let r = self.compile_expr(r, ctx);
        let builder = ctx.get_builder();
        match op {
            Op::Plus => self.compile_add(l, r, ctx),
            Op::Minus => builder.build_sub(l, r),
            Op::Mul => builder.build_mul(l, r),
            Op::Equal => builder
                .build_eq(l.as_llvm_value_ref(), r.as_llvm_value_ref())
                .into(),
            Op::NotEqual => builder.build_neq(l, r),
            Op::Less => builder.build_less(l, r),
            Op::Greater => builder.build_greater(l, r),
            _ => todo!("compile binary op: {:?}", op),
        }
    }

    fn compile_array(&self, v: &[ExprNode], ty0: &Type, ctx: &Context) -> LLVMValue {
        let builder = ctx.get_builder();
        let mut llvm_args = vec![];
        let mut ty0 = ty0.clone();
        let ty_temp = ctx.get(ContextKey::Type("default".into()));
        if let Some(ty) = ty_temp {
            let t = ty.as_type();
            ty0 = t.clone();
        }
        let t = ty0.get_element_type().unwrap();
        let ty = self.get_type(ctx, t);
        for arg in v {
            let mut v = self.compile_expr(arg, ctx);
            if t.is_any() {
                let temp = builder.build_struct_insert(
                    ty.get_undef(),
                    0,
                    &Global::const_i32(t.id()).into(),
                );
                v = builder.build_struct_insert(temp, 1, &v);
            }
            llvm_args.push(v);
        }
        builder.build_array(ty, llvm_args)
    }

    fn compile_index(&self, target: &ExprNode, index: &ExprNode, ctx: &Context) -> LLVMValue {
        let v = self.compile_expr(target, ctx);
        let element_type = if v.is_array() {
            v.as_array().unwrap().get_element_type()
        } else if v.is_pointer() {
            v.as_pointer().unwrap().get_element_type(ctx)
        } else {
            panic!("索引操作的目标必须是数组或指针类型");
        };
        let index = self.compile_expr(index, ctx);
        let builder = ctx.get_builder();
        let v = builder.build_array_get_in_bounds(element_type, v, index);
        v
    }

    fn compile_struct(&self, s: &crate::ast::expr::StructExpr, ctx: &Context) -> StructValue {
        let builder = ctx.get_builder();
        let llvm_module = self.llvm_module.read().unwrap();
        let (field_index_map, _) = llvm_module.get_struct(s.get_name()).unwrap();
        let mut props: Vec<(usize, LLVMValue)> = s
            .get_props()
            .iter()
            .map(|(field_name, p)| {
                let field_index = field_index_map.get(field_name).unwrap();
                let ty = p.get_type().unwrap();
                if ty.is_pointer() {
                    let v = self.compile_expr_with_ptr(p, ctx);
                    return (*field_index, v);
                }
                let v = self.compile_expr(p, ctx);
                (*field_index, v)
            })
            .collect::<Vec<(usize, LLVMValue)>>();
        props.sort_by(|a, b| a.0.cmp(&b.0));
        let props = props
            .into_iter()
            .map(|(_, v)| v)
            .collect::<Vec<LLVMValue>>();
        // let struct_type = ctx.(s.get_name()).unwrap();
        // dbg!(struct_type);
        let struct_ast = self.module.get_struct(s.get_name()).unwrap();
        let struct_type = self.get_type(ctx, &struct_ast.get_type());
        let mut val = struct_type.get_undef();

        for (idx, v) in props.iter().enumerate() {
            val = builder.build_struct_insert(val, idx, v);
        }
        StructValue::new(
            val.as_llvm_value_ref(),
            s.get_name().to_string(),
            field_index_map.clone(),
            props,
        )
    }
    // target 有可能是结构体指针，也有可能是结构体
    fn compile_member(&self, target: &ExprNode, field_name: &str, ctx: &Context) -> LLVMValue {
        let mut v = self.compile_expr(target, ctx);
        if v.is_pointer() {
            v = v.as_pointer().unwrap().get_element(ctx);
        }
        if v.is_reference() {
            v = v.as_reference().unwrap().get_value(ctx);
        }
        if v.is_struct() {
            v.as_struct().unwrap().get_field(ctx, field_name)
        } else {
            panic!("成员访问的类型不是结构体: {:?}", v.get_llvm_type(ctx));
        }
    }

    fn compile_enum_variant(
        &self,
        enum_name: &str,
        variant_name: &str,
        value: &Option<Box<ExprNode>>,
        ty0: &Type,
        ctx: &Context,
    ) -> EnumVariantValue {
        // 获取枚举类型
        let enum_type = ty0.clone();
        let builder = ctx.get_builder();

        // 获取变体索引
        let mut variant_index = 0;
        if let Type::Enum(_, variants) = &enum_type {
            for (i, (name, _)) in variants.iter().enumerate() {
                if name == variant_name {
                    variant_index = i;
                    break;
                }
            }
        }
        // 设置标签字段（第一个字段）
        let enum_val = self.compile_type(ty0).get_undef();
        let mut enum_val = builder.build_struct_insert(
            enum_val,
            0,
            &LLVMValue::Int32(Global::const_i32(variant_index as i32)),
        );
        let val_value = value.clone().map(|v| Box::new(self.compile_expr(&v, ctx)));
        // 如果有关联值，设置数据字段（第二个字段）
        if let Some(val) = val_value.clone() {
            enum_val = builder.build_struct_insert(enum_val, 1, &val);
        }

        // 返回枚举实例
        EnumVariantValue::new(
            enum_val.as_llvm_value_ref(),
            enum_name.to_string(),
            variant_name.to_string(),
            Int32Value::new(enum_val.as_llvm_value_ref()),
            val_value,
        )
    }

    // ===== 函数调用处理 =====

    fn compile_fn_call(&self, fc: &crate::ast::expr::FunctionCall, ctx: &Context) -> LLVMValue {
        let name = fc.name.clone();
        let args = &fc.args;
        // 获取函数声明
        let function_value = ctx.get_symbol(&name).unwrap();
        let function_decl = self.get_function_declaration(&name, ctx);
        let index_map = function_value.as_function().unwrap().get_param_index_map();
        // 获取函数定义
        let mut arg_values = self.process_function_args(fc, args, &index_map, &function_decl, ctx);
        // 处理特殊内建函数
        if let Some(result) =
            self.handle_builtin_functions(&name, &fc.generics, &mut arg_values, ctx)
        {
            return result;
        }
        // 处理闭包和符号类型
        let symbol = ctx.get_symbol(&name).unwrap();
        symbol.as_function().unwrap().call(ctx, arg_values)
    }

    fn get_function_declaration(&self, name: &str, ctx: &Context) -> Type {
        if let Some(function) = ctx.get_function(name) {
            function.get_type()
        } else {
            let current_function = ctx.get_current_function();
            let function_index = current_function.get_param_index(name).unwrap();
            let function_decl = ctx
                .get_current_function_type()
                .get_function_arg_type(function_index)
                .unwrap();
            function_decl
        }
    }
    fn process_function_args(
        &self,
        fc: &crate::ast::expr::FunctionCall,
        args: &[crate::ast::expr::Argument],
        param_name_to_index: &HashMap<String, usize>,
        function_decl: &Type,
        ctx: &Context,
    ) -> Vec<Option<LLVMValue>> {
        let arg_count = function_decl.get_function_arg_count();
        let mut arg_values = vec![None; arg_count];
        // 处理命名参数
        for arg in args.iter() {
            if let Some(arg_name) = arg.get_name() {
                if let Some(&idx) = param_name_to_index.get(arg_name) {
                    let mut v = self.compile_expr(&arg.value, ctx);
                    let t = function_decl.get_function_arg_type(idx).unwrap();
                    // 处理Any类型
                    if t.is_any() {
                        v = self.convert_to_any_type(v, &t, ctx);
                    }

                    arg_values[idx] = Some(v);
                } else {
                    panic!("函数 '{}' 没有名为 '{}' 的参数", fc.name, arg_name);
                }
            }
        }

        // 处理位置参数
        let mut pos_idx = 0;
        for arg in args.iter() {
            if arg.has_name() {
                continue; // 跳过命名参数
            }

            // 找到下一个未赋值的位置
            while pos_idx < arg_count && arg_values[pos_idx].is_some() {
                pos_idx += 1;
            }

            if pos_idx < arg_count {
                let t = function_decl.get_function_arg_type(pos_idx).unwrap();
                let ctx = Context::with_type(ctx, "default".into(), t.clone());
                let mut v = if t.is_ref() {
                    self.compile_expr_with_ptr(&arg.value, &ctx)
                } else {
                    self.compile_expr(&arg.value, &ctx)
                };
                // 处理Any类型
                if t.is_any() {
                    v = self.convert_to_any_type(v, &t, &ctx);
                }

                arg_values[pos_idx] = Some(v);
                pos_idx += 1;
            }
        }

        // 处理默认参数
        self.process_default_args(arg_values, fc, ctx)
    }

    fn process_default_args(
        &self,
        mut arg_values: Vec<Option<LLVMValue>>,
        fc: &crate::ast::expr::FunctionCall,
        ctx: &Context,
    ) -> Vec<Option<LLVMValue>> {
        // 获取函数值
        let function_value = ctx.get_symbol(&fc.name).unwrap().as_function().unwrap();
        let args_count = function_value.get_args_count();
        dbg!(&function_value);
        // 处理默认参数
        for (i, arg_val) in arg_values.iter_mut().enumerate() {
            if arg_val.is_none() && i < args_count {
                if !function_value.is_arg_undef(i) {
                    *arg_val = Some(function_value.get_param_by_index(i).unwrap());
                } else {
                    panic!(
                        "函数 '{}' 的参数 '{}' 没有提供值且没有默认值",
                        fc.name,
                        function_value.get_arg_name(i).unwrap()
                    );
                }
            }
        }
        arg_values
    }

    fn convert_to_any_type(&self, v: LLVMValue, t: &Type, ctx: &Context) -> LLVMValue {
        let builder = ctx.get_builder();
        let mut val = v;
        if val.is_reference() {
            val = val.as_reference().unwrap().get_value(ctx);
        }

        let ty = self.get_type(ctx, t);
        let a = builder.build_alloca("any", &ty);
        let any_struct = Global::struct_type(
            "Any".into(),
            vec![
                ("id".into(), Global::i32_type()),
                ("data".into(), Global::pointer_type(Global::i8_type())),
            ],
        )
        .get_undef();
        let any_struct =
            builder.build_struct_insert(any_struct, 0, &Global::const_i32(val.id()).into());
        let any_struct = builder.build_struct_insert(any_struct, 1, &val);
        a.as_reference().unwrap().store(ctx, any_struct);
        // builder.build_store(a.clone(), any_struct);
        a
    }

    fn handle_builtin_functions(
        &self,
        name: &str,
        generics: &[Type],
        arg_values: &mut [Option<LLVMValue>],
        ctx: &Context,
    ) -> Option<LLVMValue> {
        let builder = ctx.get_builder();

        match name {
            "int16" => {
                let val = arg_values.first().unwrap().as_ref().unwrap().clone();
                let v = builder.build_zext(val, Global::i16_type());
                Some(v)
            }
            "int32" => {
                let val = arg_values.first().unwrap().as_ref().unwrap().clone();
                let v = builder.build_zext(val, Global::i32_type());
                Some(v)
            }
            "int64" => {
                let val = arg_values.first().unwrap().as_ref().unwrap().clone();
                let v = builder.build_zext(val, Global::i64_type());
                Some(v)
            }
            "sizeof" => {
                if generics.len() != 1 {
                    panic!("sizeof must have one generic");
                }
                let generic = &generics[0];
                let v = Global::sizeof(self.get_type(ctx, generic));
                Some(v)
            }
            _ => None,
        }
    }
    // ===== 辅助方法 =====

    fn compile_add(&self, l: LLVMValue, r: LLVMValue, ctx: &Context) -> LLVMValue {
        let builder = ctx.get_builder();
        let ty = l.get_llvm_type(ctx);
        if ty.is_float() {
            return builder.build_fadd(l, r);
        }
        builder.build_add(l, r)
    }
}
