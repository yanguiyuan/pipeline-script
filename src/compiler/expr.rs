use crate::compiler::Compiler;
use crate::context::key::ContextKey;
use crate::context::Context;

use crate::llvm::global::Global;
use crate::llvm::value::LLVMValue;

use crate::ast::expr::{Expr, ExprNode, Op};
use crate::ast::r#type::Type;
use crate::compiler::helper::check_type_match;
use crate::core::value::Value;
use std::collections::HashMap;

impl Compiler {
    pub(crate) fn compile_expr_with_ptr(&self, expr: &ExprNode, ctx: &Context) -> Value {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let mut ty0 = expr.get_type().unwrap();
        let ty = ctx.get(ContextKey::Type("default".into()));
        if let Some(ty) = ty {
            let t = ty.as_type();
            ty0 = t.clone();
        }
        match expr.get_expr() {
            Expr::Variable(name) => self.compile_variable_ptr(name, &ty0, ctx),
            Expr::Index(target, index) => self.compile_index_ptr(target, index, &ty0, ctx),
            Expr::Member(target, field_name) => {
                self.compile_member_ptr(target, field_name, &ty0, ctx)
            }
            Expr::Struct(_) => {
                let val = self.compile_expr(expr, ctx);
                let element_ty = ty0.get_element_type().unwrap();
                let ptr = builder.build_alloca("", &self.compile_type(element_ty));
                builder.build_store(ptr, val.get_value());
                Value::new(ptr, ty0)
            }
            Expr::FnCall(ref fc) => self.compile_fn_call(fc, ctx),
            Expr::Address(target) => self.compile_expr_with_ptr(target, ctx),
            Expr::EnumVariant(_, ref variant_name, ref value) => {
                self.compile_enum_variant_ptr(variant_name, value, &ty0, ctx)
            }
            Expr::None => Value::new(Global::const_unit(), ty0),
            t => todo!("Unknown expr for ptr: {:?}", t),
        }
    }

    pub(crate) fn compile_expr(&self, expr: &ExprNode, ctx: &Context) -> Value {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let mut ty0 = expr.get_type().unwrap();
        let ty = ctx.get(ContextKey::Type("default".into()));
        if let Some(ty) = ty {
            let t = ty.as_type();
            ty0 = t.clone();
        }
        match expr.get_expr() {
            Expr::FnCall(ref fc) => self.compile_fn_call(fc, ctx),
            Expr::Int(i) => self.compile_int_literal(*i, &ty0),
            Expr::Float(f) => Value::new(Global::const_float(*f), ty0),
            Expr::String(s, ..) => {
                let ptr = builder.build_global_string("", s);
                Value::new(ptr, ty0)
            }
            Expr::Binary(op, l, r) => self.compile_binary_op(op, l, r, &ty0, ctx),
            Expr::Variable(name) => self.compile_variable(name, &ty0, ctx),
            Expr::Array(v) => self.compile_array(v, &ty0, ctx),
            Expr::Index(target, index) => self.compile_index(target, index, &ty0, ctx),
            Expr::Struct(s) => self.compile_struct(s, &ty0, ctx),
            Expr::Member(target, field_name) => self.compile_member(target, field_name, &ty0, ctx),
            Expr::Address(target) => self.compile_expr_with_ptr(target, ctx),
            Expr::EnumVariant(_, ref variant_name, ref value) => {
                self.compile_enum_variant(variant_name, value, &ty0, ctx)
            }
            Expr::None => Value::new(Global::const_unit(), ty0),
            Expr::Boolean(b) => Value::new(Global::const_bool(*b), ty0),
            t => todo!("Unknown expr: {:?}", t),
        }
    }

    // ===== 指针相关编译函数 =====

    fn compile_variable_ptr(&self, name: &str, ty0: &Type, ctx: &Context) -> Value {
        let scope = ctx.get_scope();
        if scope.has(name) {
            return scope.get(name).unwrap();
        }
        let function = ctx.get_current_function();
        let a = function.get_param(name);
        if let Some(a) = a {
            let builder = ctx.get_builder();
            if ty0.is_struct() {
                let ptr = builder.build_alloca(name, &self.compile_type(ty0));
                builder.build_store(ptr, a);
                ctx.get_scope()
                    .set(name.to_string(), Value::new(ptr, ty0.clone()));
                return Value::new(ptr, ty0.clone());
            }
            return Value::new(a, ty0.clone());
        }
        ctx.get_symbol(name).unwrap()
    }

    fn compile_index_ptr(
        &self,
        target: &ExprNode,
        index: &ExprNode,
        ty0: &Type,
        ctx: &Context,
    ) -> Value {
        let v = self.compile_expr(target, ctx);
        let index = self.compile_expr(index, ctx);
        let builder = ctx.get_builder();
        let array_ptr =
            builder.build_array_gep(self.compile_type(ty0), v.get_value(), index.get_value());
        Value::new(array_ptr, ty0.clone())
    }

    fn compile_member_ptr(
        &self,
        target: &ExprNode,
        field_name: &str,
        ty0: &Type,
        ctx: &Context,
    ) -> Value {
        let v = self.compile_expr_with_ptr(target, ctx);
        let ty = v.get_type();
        let (idx, _) = ty
            .get_struct_field(field_name)
            .unwrap_or_else(|| panic!("未定义的字段: {}", field_name));
        let builder = ctx.get_builder();
        let target_ty = self.compile_type(target.get_type().unwrap().get_element_type().unwrap());
        let val = builder.build_struct_gep(target_ty, v.get_value(), idx);
        Value::new(val, ty0.clone())
    }

    fn compile_enum_variant_ptr(
        &self,
        variant_name: &str,
        value: &Option<Box<ExprNode>>,
        ty0: &Type,
        ctx: &Context,
    ) -> Value {
        // 获取枚举类型
        let enum_type = ty0.clone();

        // 创建枚举实例
        let builder = ctx.get_builder();

        // 分配内存
        let enum_ptr = builder.build_alloca("enum_instance", &enum_type.as_llvm_type());

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
        let tag_ptr = builder.build_struct_gep(enum_type.as_llvm_type(), enum_ptr, 0);
        builder.build_store(tag_ptr, Global::const_i32(variant_index as i32));

        // 如果有关联值，设置数据字段（第二个字段）
        if let Some(val) = value {
            // 编译关联值
            let val_value = self.compile_expr(val, ctx);

            // 设置数据字段
            let data_ptr = builder.build_struct_gep(enum_type.as_llvm_type(), enum_ptr, 1);
            builder.build_store(data_ptr, val_value.value);
        }

        // 返回枚举实例
        Value::new(enum_ptr, enum_type)
    }

    // ===== 值相关编译函数 =====

    fn compile_variable(&self, name: &str, ty0: &Type, ctx: &Context) -> Value {
        let builder = ctx.get_builder();
        // 1. 首先检查局部作用域
        let scope = ctx.get_scope();
        if scope.has(name) {
            let ptr = scope.get(name).unwrap();
            if ptr.get_type() == *ty0 {
                return ptr;
            }
            return Value::new(
                builder.build_load(self.get_type(ctx, ty0), ptr.get_value()),
                ty0.clone(),
            );
        }

        // 2. 检查函数参数
        if let Some(v) = ctx.get_current_function().get_param(name) {
            return Value::new(v, ty0.clone());
        }

        // 3. 查找全局符号或函数
        let ptr = ctx.get_symbol(name).unwrap_or_else(|| {
            let f = self.llvm_module.get_function(name).unwrap().as_ref();
            Value::new(LLVMValue::Pointer(f), ty0.clone())
        });
        // 4. 处理类型转换和引用
        let ptr_type = ptr.get_type();
        // 如果需要引用类型，或者符号本身是引用类型，需要加载其值
        if !ty0.is_ref() && (ptr_type.is_ref() || ptr_type.is_pointer()) {
            let element_type = if ty0.is_ref() {
                ty0.get_element_type().unwrap()
            } else {
                ptr_type.get_element_type().unwrap()
            };
            let v0 = builder.build_load(self.compile_type(element_type), ptr.value);
            return Value::new(v0, element_type.clone());
        }
        // 类型匹配，直接返回
        if check_type_match(&ptr_type, ty0) {
            return ptr;
        }

        // 需要加载非指针值
        if !ty0.is_pointer() && ptr_type.is_pointer() {
            let v0 = builder.build_load(self.compile_type(&ptr.ty), ptr.value);
            return Value::new(v0, ty0.clone());
        }

        ptr
    }

    fn compile_int_literal(&self, i: i64, ty0: &Type) -> Value {
        let v = match ty0 {
            Type::Int8 => Global::const_i8(i as i8),
            Type::Int16 => Global::const_i16(i as i16),
            Type::Int32 => Global::const_i32(i as i32),
            Type::Int64 => Global::const_i64(i),
            _ => panic!("Unknown type for int literal: {:?}", ty0),
        };
        Value::new(v, ty0.clone())
    }

    fn compile_binary_op(
        &self,
        op: &Op,
        l: &ExprNode,
        r: &ExprNode,
        ty0: &Type,
        ctx: &Context,
    ) -> Value {
        let l = self.compile_expr(l, ctx).get_value();
        let r = self.compile_expr(r, ctx).get_value();
        let builder = ctx.get_builder();
        let v = match op {
            Op::Plus => self.compile_add(l, r, ctx),
            Op::Minus => builder.build_sub(l, r),
            Op::Mul => builder.build_mul(l, r),
            Op::Equal => builder.build_eq(l, r),
            Op::NotEqual => builder.build_neq(l, r),
            Op::Less => builder.build_less(l, r),
            Op::Greater => builder.build_greater(l, r),
            _ => todo!("compile binary op: {:?}", op),
        };
        Value::new(v, ty0.clone())
    }

    fn compile_array(&self, v: &[ExprNode], ty0: &Type, ctx: &Context) -> Value {
        let builder = ctx.get_builder();
        let mut llvm_args = vec![];
        let t = ty0.get_element_type().unwrap();
        let ty = self.get_type(ctx, t);
        for arg in v {
            let mut v = self.compile_expr(arg, ctx);
            if t.is_any() {
                let val = v.get_value();
                let temp = builder.build_struct_insert(
                    Global::undef(ty.clone()),
                    0,
                    &Global::const_i32(v.get_type().id()),
                );
                let r = builder.build_struct_insert(temp, 1, &val);
                v = Value::new(r, t.clone());
            }
            llvm_args.push(v.get_value());
        }

        let v1 = builder.build_array(ty, llvm_args);
        Value::new(v1, ty0.clone())
    }

    fn compile_index(
        &self,
        target: &ExprNode,
        index: &ExprNode,
        ty0: &Type,
        ctx: &Context,
    ) -> Value {
        let v = self.compile_expr(target, ctx);
        let index = self.compile_expr(index, ctx);
        let builder = ctx.get_builder();
        let v = builder.build_array_get_in_bounds(
            self.compile_type(ty0),
            v.get_value(),
            index.get_value(),
        );
        Value::new(v, ty0.clone())
    }

    fn compile_struct(&self, s: &crate::ast::expr::StructExpr, ty0: &Type, ctx: &Context) -> Value {
        let builder = ctx.get_builder();
        let (field_index_map, _) = self.llvm_module.get_struct(s.get_name()).unwrap();
        let mut props: Vec<(usize, Value)> = s
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
            .collect::<Vec<(usize, Value)>>();
        props.sort_by(|a, b| a.0.cmp(&b.0));
        let props = props
            .into_iter()
            .map(|(_, v)| v.get_value())
            .collect::<Vec<LLVMValue>>();
        let mut val = Global::undef(self.compile_type(ty0));
        for (idx, v) in props.iter().enumerate() {
            val = builder.build_struct_insert(val, idx, v);
        }

        Value::new(val, ty0.clone())
    }
    // target 有可能是结构体指针，也有可能是结构体
    fn compile_member(
        &self,
        target: &ExprNode,
        field_name: &str,
        ty0: &Type,
        ctx: &Context,
    ) -> Value {
        let v = self.compile_expr(target, ctx);
        let ty = v.get_type();
        let mut val = v.get_value();
        let builder = ctx.get_builder();

        if ty.is_pointer() {
            let ty = ty.get_element_type().unwrap();
            let llvm_type = self.compile_type(ty);
            val = builder.build_load(llvm_type, v.get_value());
        }
        let (idx, _) = ty.get_struct_field(field_name).unwrap();
        if ty.is_ref() {
            let llvm_type = self.compile_type(ty.get_element_type().unwrap());
            val = builder.build_struct_gep(llvm_type, val, idx);
            val = builder.build_load(ty0.as_llvm_type(), val);
            return Value::new(val, ty0.clone());
        }
        if ty.is_struct() {
            val = builder.build_struct_get(val, idx);
            return Value::new(val, ty0.clone());
        }
        if !ty0.is_ref() && !ty0.is_pointer() {
            val = builder.build_load(ty0.as_llvm_type(), val);
        }
        Value::new(val, ty0.clone())
    }

    fn compile_enum_variant(
        &self,
        variant_name: &str,
        value: &Option<Box<ExprNode>>,
        ty0: &Type,
        ctx: &Context,
    ) -> Value {
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
        let enum_val = Global::undef(self.compile_type(ty0));
        let mut enum_val =
            builder.build_struct_insert(enum_val, 0, &Global::const_i32(variant_index as i32));

        // 如果有关联值，设置数据字段（第二个字段）
        if let Some(val) = value {
            // 编译关联值
            let val_value = self.compile_expr(val, ctx);
            enum_val = builder.build_struct_insert(enum_val, 1, &val_value.value);
        }

        // 返回枚举实例
        Value::new(enum_val, enum_type)
    }

    // ===== 函数调用处理 =====

    fn compile_fn_call(&self, fc: &crate::ast::expr::FunctionCall, ctx: &Context) -> Value {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let mut name = fc.name.clone();
        let args = &fc.args;
        let mut llvm_args = vec![];

        // 处理闭包和符号类型
        let symbol_type = ctx.get_symbol(&name);
        if let Some(symbol) = symbol_type {
            let symbol_type = symbol.get_type();
            if symbol_type.is_closure() {
                name = symbol_type.get_closure_name().unwrap().to_string();
            }
        }

        // 获取函数声明
        let (function_decl, is_fn_param) = self.get_function_declaration(&name, ctx);

        // 获取函数定义
        let func = ctx.get_function(&name).unwrap();
        let func_args = func.args();
        // 构建参数映射和参数值数组
        let param_name_to_index = self.build_param_index_map(func_args);
        let mut arg_values =
            self.process_function_args(fc, args, &param_name_to_index, &function_decl, ctx);

        // 处理特殊内建函数
        if let Some(result) =
            self.handle_builtin_functions(&name, &fc.generics, &mut arg_values, ctx)
        {
            return result;
        }

        // 处理函数指针调用
        if is_fn_param {
            return self.call_function_pointer(&name, function_decl, &mut arg_values, ctx);
        }

        // 普通函数调用
        let ty = func.return_type();
        let llvm_func = self.llvm_module.get_function(&name).unwrap();

        // 构建LLVM参数列表
        for arg_val in arg_values {
            if let Some(val) = arg_val {
                llvm_args.push(val);
            } else {
                panic!("函数 '{}' 的某个参数没有值", name);
            }
        }

        let v = builder.build_call(llvm_func, llvm_args.as_mut_slice(), "");
        if name == "panic" || name == "exit" {
            builder.build_unreachable();
        }
        Value::new(v, ty.clone())
    }

    fn get_function_declaration(&self, name: &str, ctx: &Context) -> (Type, bool) {
        if let Some(function) = ctx.get_function(name) {
            (function.get_type(), false)
        } else {
            let current_function = ctx.get_current_function();
            dbg!(name);
            let function_index = current_function.get_param_index(name).unwrap();
            let function_decl = ctx
                .get_current_function_type()
                .get_function_arg_type(function_index)
                .unwrap();
            (function_decl, true)
        }
    }

    fn build_param_index_map(
        &self,
        func_args: &[crate::ast::declaration::VariableDeclaration],
    ) -> HashMap<String, usize> {
        let mut param_name_to_index = HashMap::new();
        for (i, param) in func_args.iter().enumerate() {
            param_name_to_index.insert(param.name.clone(), i);
        }
        param_name_to_index
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
                    let t = function_decl.get_function_arg_type(idx).unwrap();
                    let mut v = self.compile_expr(&arg.value, ctx);

                    // 处理Any类型
                    if t.is_any() {
                        v = self.convert_to_any_type(v, &t, ctx);
                    }

                    arg_values[idx] = Some(v.get_value());
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
                let mut v = self.compile_expr(&arg.value, &ctx);

                // 处理Any类型
                if t.is_any() {
                    v = self.convert_to_any_type(v, &t, &ctx);
                }

                arg_values[pos_idx] = Some(v.get_value());
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
        // 获取函数定义
        let func = ctx.get_function(&fc.name).unwrap();
        let func_args = func.args();

        // 处理默认参数
        for (i, arg_val) in arg_values.iter_mut().enumerate() {
            if arg_val.is_none() && i < func_args.len() {
                let param = &func_args[i];
                if param.has_default() {
                    let d = param.get_default().unwrap();
                    let v = if d.get_type().unwrap().is_pointer() {
                        self.compile_expr_with_ptr(d, ctx)
                    } else {
                        self.compile_expr(d, ctx)
                    };
                    *arg_val = Some(v.get_value());
                } else {
                    panic!(
                        "函数 '{}' 的参数 '{}' 没有提供值且没有默认值",
                        fc.name,
                        param.name()
                    );
                }
            }
        }

        arg_values
    }

    fn convert_to_any_type(&self, v: Value, t: &Type, ctx: &Context) -> Value {
        let builder = ctx.get_builder();
        let mut val = v.get_value();

        if !(v.ty.is_pointer() || v.ty.is_string()) {
            let ty = self.compile_type(&v.ty);
            val = builder.build_alloca("", &ty);
            builder.build_store(val, v.value);
        }

        let ty = self.get_type(ctx, t);
        let a = builder.build_alloca("any", &ty);
        let any_struct = Global::undef(Global::struct_type(vec![
            Global::i32_type(),
            Global::pointer_type(Global::i8_type()),
        ]));
        let any_struct = builder.build_struct_insert(
            any_struct,
            0,
            &Global::const_i32(Type::from(v.value.get_type()).id()),
        );
        let any_struct = builder.build_struct_insert(any_struct, 1, &val);
        builder.build_store(a, any_struct);

        Value::new(a, t.clone())
    }

    fn handle_builtin_functions(
        &self,
        name: &str,
        generics: &[Type],
        arg_values: &mut [Option<LLVMValue>],
        ctx: &Context,
    ) -> Option<Value> {
        let builder = ctx.get_builder();

        match name {
            "int32" => {
                let val = *arg_values.first().unwrap().as_ref().unwrap();
                let v = builder.build_zext(val, Global::i32_type());
                Some(Value::new(v, Type::Int32))
            }
            "int64" => {
                let val = *arg_values.first().unwrap().as_ref().unwrap();
                let v = builder.build_zext(val, Global::i64_type());
                Some(Value::new(v, Type::Int64))
            }
            "sizeof" => {
                if generics.len() != 1 {
                    panic!("sizeof must have one generic");
                }
                let generic = &generics[0];
                let v = Global::sizeof(self.get_type(ctx, generic));
                Some(Value::new(v, Type::Int64))
            }
            _ => None,
        }
    }

    fn call_function_pointer(
        &self,
        name: &str,
        function_decl: Type,
        arg_values: &mut [Option<LLVMValue>],
        ctx: &Context,
    ) -> Value {
        let builder = ctx.get_builder();
        let fn_struct = ctx.get_current_function().get_param(name).unwrap();
        let fn_ptr = builder.build_struct_get(fn_struct, 0);
        let extra_param = builder.build_struct_get(fn_struct, 1);

        let mut llvm_args = Vec::new();
        for arg_val in arg_values.iter() {
            if let Some(val) = arg_val {
                llvm_args.push(*val);
            } else {
                panic!("函数指针调用缺少参数值");
            }
        }

        llvm_args.push(extra_param);
        let v = builder.build_call_fn_ptr(
            self.get_type(ctx, &function_decl),
            fn_ptr,
            llvm_args.as_mut_slice(),
            "",
        );

        Value::new(v, function_decl.get_function_return_type().unwrap())
    }

    // ===== 辅助方法 =====

    fn compile_add(&self, l: LLVMValue, r: LLVMValue, ctx: &Context) -> LLVMValue {
        let builder = ctx.get_builder();
        let ty = l.get_type();
        if ty.is_float() {
            return builder.build_fadd(l, r);
        }
        builder.build_add(l, r)
    }
}
