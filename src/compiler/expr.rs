use crate::compiler::Compiler;
use crate::context::key::ContextKey;
use crate::context::Context;

use crate::llvm::global::Global;
use crate::llvm::value::LLVMValue;

use crate::core::value::Value;
use crate::parser::expr::{Expr, ExprNode, Op};
use crate::parser::r#type::Type;

impl Compiler {
    pub(crate) fn compile_expr_with_ptr(&self, expr: &ExprNode, ctx: &Context) -> Value {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let ty0 = expr.get_type().unwrap();
        match expr.get_expr() {
            Expr::Variable(name) => {
                let scope = ctx.get_scope();
                if scope.has(&name) {
                    return scope.get(name).unwrap();
                }
                let function = ctx.get_current_function();
                let a = function.get_param(name.as_ref());
                if a.is_some() {
                    let a = a.unwrap();
                    if ty0.is_struct() {
                        let ptr = builder.build_alloca(&name, &self.compile_type(&ty0));
                        builder.build_store(ptr, a);
                        ctx.get_scope().set(name, Value::new(ptr, ty0.clone()));
                        return Value::new(ptr, ty0);
                    }
                    return Value::new(a, ty0);
                }
                ctx.get_symbol(name).unwrap()
            }
            Expr::Index(target, index) => {
                let v = self.compile_expr(&target, ctx);
                let index = self.compile_expr(&index, ctx);
                let array_ptr = builder.build_array_gep(
                    self.compile_type(&ty0), 
                    v.get_value(), 
                    index.get_value()
                );
                Value::new(array_ptr, ty0)
            }
            Expr::Member(target, field_name) => {
                let v = self.compile_expr_with_ptr(&target, ctx);
                let ty = v.get_type();
                let (idx, _) = ty
                    .get_struct_field(&field_name)
                    .unwrap_or_else(|| panic!("未定义的字段: {}", field_name));
                let target_ty = self.compile_type(&target.get_type().unwrap().get_element_type().unwrap());
                let val = builder.build_struct_gep(
                    target_ty,
                    v.get_value(),
                    idx,
                );
                Value::new(val, ty0)
            }
            Expr::Struct(_) => {
                let val = self.compile_expr(expr, ctx);
                let element_ty = ty0.get_element_type().unwrap();
                let ptr = builder.build_alloca("", &self.compile_type(element_ty));
                builder.build_store(ptr, val.get_value());
                Value::new(ptr, ty0)
            }
            Expr::FnCall(fc) => {
                let mut name = fc.name;
                let args = &fc.args;
                let mut llvm_args = vec![];
                let function_decl;
                let is_fn_param;

                let symbol_type = ctx.get_symbol(&name);
                if symbol_type.is_some() {
                    let symbol_type = symbol_type.unwrap().get_type();
                    if symbol_type.is_closure() {
                        name = symbol_type.get_closure_name().unwrap();
                    }
                }
                if let Some(function) = self.module.get_function(&name) {
                    function_decl = function.get_type();

                    is_fn_param = false;
                } else {
                    let current_function = ctx.get_current_function();
                    dbg!(&name);
                    let function_index = current_function.get_param_index(&name).unwrap();
                    function_decl = ctx
                        .get_current_function_type()
                        .get_function_arg_type(function_index)
                        .unwrap();

                    is_fn_param = true;
                }
                
                // 获取函数定义
                let func = self.module.get_function(name.clone()).unwrap();
                let func_args = func.args();
                
                // 创建参数名到索引的映射
                let mut param_name_to_index = std::collections::HashMap::new();
                for (i, param) in func_args.iter().enumerate() {
                    param_name_to_index.insert(param.name(), i);
                }
                
                // 创建参数值数组，初始化为None
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
                                let mut val = v.get_value();
                                dbg!(&v.ty);
                                if !(v.ty.is_pointer() || v.ty.is_string()) {
                                    let ty = self.compile_type(&v.ty);
                                    val = builder.build_alloca("", &ty);
                                    builder.build_store(val, v.value);
                                }
                                let a = builder.build_alloca("any", &self.compile_type(&t));
                                let any_struct = Global::undef(Global::struct_type(vec![Global::i32_type(), Global::pointer_type(Global::i8_type())]));
                                let any_struct = builder.build_struct_insert(any_struct, 0, &Global::const_i32(v.get_type().id()));
                                let any_struct = builder.build_struct_insert(any_struct, 1, &val);
                                builder.build_store(a, any_struct);
                                v = Value::new(a, t);
                            }
                            
                            arg_values[idx] = Some(v.get_value());
                        } else {
                            eprintln!("函数 '{}' 没有名为 '{}' 的参数", name, arg_name);
                            panic!("未知的参数名");
                        }
                    }
                }
                
                // 处理位置参数
                let mut pos_idx = 0;
                for arg in args.iter() {
                    if arg.has_name() {
                        continue;  // 跳过命名参数
                    }
                    
                    // 找到下一个未赋值的位置
                    while pos_idx < arg_count && arg_values[pos_idx].is_some() {
                        pos_idx += 1;
                    }
                    
                    if pos_idx < arg_count {
                        let t = function_decl.get_function_arg_type(pos_idx).unwrap();
                        let mut v = self.compile_expr(&arg.value, ctx);
                        
                        // 处理Any类型
                        if t.is_any() {
                            let mut val = v.get_value();
                            if !(v.ty.is_pointer() || v.ty.is_string()) {
                                let ty = self.compile_type(&v.ty);
                                val = builder.build_alloca("", &ty);
                                builder.build_store(val, v.value);
                            }
                            let a = builder.build_alloca("any", &self.compile_type(&t));
                            let any_struct = Global::undef(Global::struct_type(vec![Global::i32_type(), Global::pointer_type(Global::i8_type())]));
                            let any_struct = builder.build_struct_insert(any_struct, 0, &Global::const_i32(v.get_type().id()));
                            let any_struct = builder.build_struct_insert(any_struct, 1, &val);
                            builder.build_store(a, any_struct);
                            v = Value::new(a, t);
                        }
                        
                        arg_values[pos_idx] = Some(v.get_value());
                        pos_idx += 1;
                    }
                }
                
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
                            eprintln!("函数 '{}' 的参数 '{}' 没有提供值且没有默认值", name, param.name());
                            panic!("缺少必需参数");
                        }
                    }
                }
                
                // 构建LLVM参数列表
                for arg_val in arg_values {
                    if let Some(val) = arg_val {
                        llvm_args.push(val);
                    } else {
                        eprintln!("函数 '{}' 的某个参数没有值", name);
                        panic!("参数值缺失");
                    }
                }
                
                // 特殊函数转换为指令
                match name.as_str() {
                    "int32" => {
                        let val = *llvm_args.first().unwrap();
                        let v = builder.build_zext(val, Global::i32_type());
                        return Value::new(v, Type::Int32);
                    }
                    "int64" => {
                        let val = *llvm_args.first().unwrap();
                        let v = builder.build_zext(val, Global::i64_type());
                        return Value::new(v, Type::Int64);
                    }
                    "sizeof" => {
                        if fc.generics.len() != 1 {
                            panic!("sizeof must have one generic");
                        }
                        let generic = &fc.generics[0];
                        let v = Global::sizeof(generic.as_llvm_type());
                        return Value::new(v, Type::Int64);
                    }
                    _ => {}
                }
                
                if is_fn_param {
                    let fn_struct = ctx.get_current_function().get_param(&name).unwrap();
                    let fn_ptr = builder.build_struct_get(fn_struct, 0);
                    let extra_param = builder.build_struct_get(fn_struct, 1);
                    llvm_args.push(extra_param);
                    let v = builder.build_call_fn_ptr(
                        function_decl.as_llvm_type(),
                        fn_ptr,
                        llvm_args.as_mut_slice(),
                        "",
                    );
                    return Value::new(v, function_decl.get_function_return_type().unwrap());
                }
                
                let ty = func.return_type();
                let llvm_func = self.llvm_module.get_function(name).unwrap();
                let v = builder.build_call(llvm_func, llvm_args.as_mut_slice(), "");
                Value::new(v, ty.clone())
            }
            Expr::Int(i) => {
                let v = match ty0 {
                    Type::Int8 => Global::const_i8(i as i8),
                    Type::Int16 => Global::const_i16(i as i16),
                    Type::Int32 => Global::const_i32(i as i32),
                    Type::Int64 => Global::const_i64(i),
                    _ => panic!("Unknown type: {:?}", ty0),
                };
                Value::new(v, ty0)
            }
            Expr::Float(f) => Value::new(Global::const_float(f), ty0),
            Expr::String(s, ..) => {
                let ptr = builder.build_global_string("", s);
                Value::new(ptr, ty0)
            }
            Expr::Binary(op, l, r) => {
                let l = self.compile_expr(&l, ctx).get_value();
                let r = self.compile_expr(&r, ctx).get_value();
                let v = match op {
                    Op::Plus => builder.build_add(l, r),
                    Op::Minus => builder.build_sub(l, r),
                    Op::Mul => builder.build_mul(l, r),
                    Op::Equal => builder.build_eq(l, r),
                    Op::NotEqual => builder.build_neq(l, r),
                    Op::Less => builder.build_less(l, r),
                    Op::Greater => builder.build_greater(l, r),
                    _ => todo!("compile binary"),
                };
                Value::new(v, ty0)
            }
            Expr::Array(v) => {
                let mut llvm_args = vec![];
                let t = ty0.get_element_type().unwrap();
                let ty = self.get_type(ctx, t);
                for arg in v {
                    let mut v = self.compile_expr(&arg, ctx);
                    if t.is_any() {
                        let val = v.get_value();
                        let temp = builder.build_struct_insert(
                            Global::undef(ty.clone()),
                            0,
                            &Global::const_i32(v.get_type().id()),
                        );
                        // if v.get_type().is_i64(){
                        //     val = builder.build_i64_to_ptr(val);
                        // }
                        let r = builder.build_struct_insert(temp, 1, &val);
                        v = Value::new(r, t.clone());
                    }
                    llvm_args.push(v.get_value());
                }

                let v1 = builder.build_array(ty, llvm_args);
                Value::new(v1, ty0)
            }
            Expr::Address(target) => self.compile_expr_with_ptr(&target, ctx),
            Expr::EnumVariant(_, variant_name, value) => {
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
                        if name == &variant_name {
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
                    let val_value = self.compile_expr(&val, ctx);
                    
                    // 设置数据字段
                    let data_ptr = builder.build_struct_gep(enum_type.as_llvm_type(), enum_ptr, 1);
                    builder.build_store(data_ptr, val_value.value);
                }
                
                // 返回枚举实例
                Value::new(enum_ptr, enum_type)
            }
            Expr::None => Value::new(Global::const_unit(), ty0),
            t => todo!("Unknown expr: {:?}", t),
        }
    }
    pub(crate) fn compile_expr(&self, expr: &ExprNode, ctx: &Context) -> Value {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let ty0 = expr.get_type().unwrap();
        match expr.get_expr() {
            Expr::FnCall(fc) => {
                let mut name = fc.name;
                let args = &fc.args;
                let mut llvm_args = vec![];
                let function_decl;
                let is_fn_param;

                let symbol_type = ctx.get_symbol(&name);
                if symbol_type.is_some() {
                    let symbol_type = symbol_type.unwrap().get_type();
                    if symbol_type.is_closure() {
                        name = symbol_type.get_closure_name().unwrap();
                    }
                }
                if let Some(function) = self.module.get_function(&name) {
                    function_decl = function.get_type();

                    is_fn_param = false;
                } else {
                    let current_function = ctx.get_current_function();
                    dbg!(&name);
                    let function_index = current_function.get_param_index(&name).unwrap();
                    function_decl = ctx
                        .get_current_function_type()
                        .get_function_arg_type(function_index)
                        .unwrap();

                    is_fn_param = true;
                }
                
                // 获取函数定义
                let func = self.module.get_function(name.clone()).unwrap();
                let func_args = func.args();
                
                // 创建参数名到索引的映射
                let mut param_name_to_index = std::collections::HashMap::new();
                for (i, param) in func_args.iter().enumerate() {
                    param_name_to_index.insert(param.name(), i);
                }
                
                // 创建参数值数组，初始化为None
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
                                let mut val = v.get_value();
                                if !(v.ty.is_pointer() || v.ty.is_string()) {
                                    let ty = self.compile_type(&v.ty);
                                    val = builder.build_alloca("", &ty);
                                    builder.build_store(val, v.value);
                                }
                                let a = builder.build_alloca("any", &self.compile_type(&t));
                                let any_struct = Global::undef(Global::struct_type(vec![Global::i32_type(), Global::pointer_type(Global::i8_type())]));
                                let any_struct = builder.build_struct_insert(any_struct, 0, &Global::const_i32(v.get_type().id()));
                                let any_struct = builder.build_struct_insert(any_struct, 1, &val);
                                builder.build_store(a, any_struct);
                                v = Value::new(a, t);
                            }
                            
                            arg_values[idx] = Some(v.get_value());
                        } else {
                            eprintln!("函数 '{}' 没有名为 '{}' 的参数", name, arg_name);
                            panic!("未知的参数名");
                        }
                    }
                }
                
                // 处理位置参数
                let mut pos_idx = 0;
                for arg in args.iter() {
                    if arg.has_name() {
                        continue;  // 跳过命名参数
                    }
                    
                    // 找到下一个未赋值的位置
                    while pos_idx < arg_count && arg_values[pos_idx].is_some() {
                        pos_idx += 1;
                    }
                    
                    if pos_idx < arg_count {
                        let t = function_decl.get_function_arg_type(pos_idx).unwrap();
                        let mut v = self.compile_expr(&arg.value, ctx);
                        
                        // 处理Any类型
                        if t.is_any() {
                            let mut val = v.get_value();
                            if !(v.ty.is_pointer() || v.ty.is_string()) {
                                dbg!(&v.ty);
                                let ty = self.compile_type(&v.ty);
                                
                                val = builder.build_alloca("", &ty);
                                builder.build_store(val, v.value);
                            }
                            let a = builder.build_alloca("any", &self.compile_type(&t));
                            let any_struct = Global::undef(Global::struct_type(vec![Global::i32_type(), Global::pointer_type(Global::i8_type())]));
                            let any_struct = builder.build_struct_insert(any_struct, 0, &Global::const_i32(v.get_type().id()));
                            let any_struct = builder.build_struct_insert(any_struct, 1, &val);
                            builder.build_store(a, any_struct);
                            v = Value::new(a, t);
                        }
                        
                        arg_values[pos_idx] = Some(v.get_value());
                        pos_idx += 1;
                    }
                }
                
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
                            eprintln!("函数 '{}' 的参数 '{}' 没有提供值且没有默认值", name, param.name());
                            panic!("缺少必需参数");
                        }
                    }
                }
                
                // 构建LLVM参数列表
                for arg_val in arg_values {
                    if let Some(val) = arg_val {
                        llvm_args.push(val);
                    } else {
                        eprintln!("函数 '{}' 的某个参数没有值", name);
                        panic!("参数值缺失");
                    }
                }
                
                // 特殊函数转换为指令
                match name.as_str() {
                    "int32" => {
                        let val = *llvm_args.first().unwrap();
                        let v = builder.build_zext(val, Global::i32_type());
                        return Value::new(v, Type::Int32);
                    }
                    "int64" => {
                        let val = *llvm_args.first().unwrap();
                        let v = builder.build_zext(val, Global::i64_type());
                        return Value::new(v, Type::Int64);
                    }
                    "sizeof" => {
                        if fc.generics.len() != 1 {
                            panic!("sizeof must have one generic");
                        }
                        let generic = &fc.generics[0];
                        let v = Global::sizeof(self.compile_type(&generic));
                        return Value::new(v, Type::Int64);
                    }
                    _ => {}
                }
                
                if is_fn_param {
                    let fn_struct = ctx.get_current_function().get_param(&name).unwrap();
                    let fn_ptr = builder.build_struct_get(fn_struct, 0);
                    let extra_param = builder.build_struct_get(fn_struct, 1);
                    llvm_args.push(extra_param);
                    let v = builder.build_call_fn_ptr(
                        self.compile_type(&function_decl),
                        fn_ptr,
                        llvm_args.as_mut_slice(),
                        "",
                    );
                    return Value::new(v, function_decl.get_function_return_type().unwrap());
                }
                
                let ty = func.return_type();
                let llvm_func = self.llvm_module.get_function(name).unwrap();
                let v = builder.build_call(llvm_func, llvm_args.as_mut_slice(), "");
                Value::new(v, ty.clone())
            }
            Expr::Int(i) => {
                let v = match ty0 {
                    Type::Int8 => Global::const_i8(i as i8),
                    Type::Int16 => Global::const_i16(i as i16),
                    Type::Int32 => Global::const_i32(i as i32),
                    Type::Int64 => Global::const_i64(i),
                    _ => panic!("Unknown type: {:?}", ty0),
                };
                Value::new(v, ty0)
            }
            Expr::Float(f) => Value::new(Global::const_float(f), ty0),
            Expr::String(s, ..) => {
                let ptr = builder.build_global_string("", s);
                Value::new(ptr, ty0)
            }
            Expr::Binary(op, l, r) => {
                let l = self.compile_expr(&l, ctx).get_value();
                let r = self.compile_expr(&r, ctx).get_value();
                let v = match op {
                    Op::Plus =>self.compile_add(l,r,ctx),
                    Op::Minus => builder.build_sub(l, r),
                    Op::Mul => builder.build_mul(l, r),
                    Op::Equal => builder.build_eq(l, r),
                    Op::NotEqual => builder.build_neq(l, r),
                    Op::Less => builder.build_less(l, r),
                    Op::Greater => builder.build_greater(l, r),
                    _ => todo!("compile binary"),
                };
                Value::new(v, ty0)
            }
            Expr::Variable(name) => {
                let scope = ctx.get_scope();
                if scope.has(&name) {
                    let ptr = scope.get(name).unwrap();
                    if ptr.get_type() == ty0 {
                        return ptr;
                    }
                    let ty = self.get_type(ctx, &ty0);
                    return Value::new(builder.build_load(ty, ptr.get_value()), ty0);
                }
                let param = ctx.get_current_function().get_param(name.as_ref());
                if let Some(v) = param {
                    return Value::new(v, ty0);
                }
                let ptr = ctx.get_symbol(&name).unwrap_or_else(|| {
                    let f = self.llvm_module.get_function(name).unwrap().as_ref();
                    Value::new(LLVMValue::Pointer(f), ty0.clone())
                });
                dbg!(&ptr);
                dbg!(&ty0);
                let ptr_type = ptr.get_type();
                if ty0.is_ref() {
                    let element_type = ty0.get_element_type().unwrap();
                    dbg!(&element_type);
                    let v0 = builder.build_load(self.compile_type(&element_type), ptr.value);
                    return Value::new(v0, element_type.clone());
                }else if ptr_type.is_ref() {
                    let element_type = ptr_type.get_element_type().unwrap();
                    dbg!(&element_type);
                    let v0 = builder.build_load(self.compile_type(&element_type), ptr.value);
                    return Value::new(v0, element_type.clone());
                }
                // let ptr_type = ptr.get_type();
                // if ptr_type.is_ref() {
                //     let element_type = ptr_type.get_element_type().unwrap();
                //     dbg!(&element_type);
                //     let v0 = builder.build_load(self.compile_type(&element_type), ptr.value);
                //     return Value::new(v0, element_type.clone());
                // }
                if ptr.get_type() == ty0 {
                    return ptr;
                }
                if !ty0.is_pointer() {
                    dbg!(&ptr.ty);
                    dbg!(&ty0);
                    let v0 = builder.build_load(self.compile_type(&ptr.ty), ptr.value);
                    return Value::new(v0, ty0);
                }
                
                ptr
            }
            Expr::Array(v) => {
                let mut llvm_args = vec![];
                let t = ty0.get_element_type().unwrap();
                let ty = self.get_type(ctx, t);
                for arg in v {
                    let mut v = self.compile_expr(&arg, ctx);
                    if t.is_any() {
                        let val = v.get_value();
                        let temp = builder.build_struct_insert(
                            Global::undef(ty.clone()),
                            0,
                            &Global::const_i32(v.get_type().id()),
                        );
                        // if v.get_type().is_i64(){
                        //     val = builder.build_i64_to_ptr(val);
                        // }
                        let r = builder.build_struct_insert(temp, 1, &val);
                        v = Value::new(r, t.clone());
                    }
                    llvm_args.push(v.get_value());
                }

                let v1 = builder.build_array(ty, llvm_args);
                Value::new(v1, ty0)
            }
            Expr::Index(target, index) => {
                let v = self.compile_expr(&target, ctx);
                let index = self.compile_expr(&index, ctx);
                let v = builder.build_array_get_in_bounds(
                    self.compile_type(&ty0),
                    v.get_value(),
                    index.get_value(),
                );
                Value::new(v, ty0)
            }
            Expr::Struct(s) => {
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
                let mut val = Global::undef(self.compile_type(&ty0));
                for (idx, v) in props.iter().enumerate() {
                    val = builder.build_struct_insert(val, idx, v);
                }

                Value::new(val, ty0)
            }
            Expr::Member(target, field_name) => {
                let v = self.compile_expr(&target, ctx);
                let ty = v.get_type();
                let mut val = v.get_value();

                if ty.is_pointer() {
                    let ty = ty.get_element_type().unwrap();
                    let llvm_type = self.compile_type(ty);
                    val = builder.build_load(llvm_type, v.get_value());
                }

                let (idx, _) = ty.get_struct_field(field_name).unwrap();
                let v = builder.build_struct_get(val, idx);
                Value::new(v, ty0)
            }
            Expr::Address(target) => self.compile_expr_with_ptr(&target, ctx),
            Expr::EnumVariant(_, variant_name, value) => {
                // 获取枚举类型
                let enum_type = ty0.clone();
                dbg!(&enum_type);
                // 创建枚举实例
                let builder = ctx.get_builder();
                
                // 分配内存
                // let enum_ptr = builder.build_alloca("enum_instance", &self.compile_type(&enum_type));
                
                // 获取变体索引
                let mut variant_index = 0;
                if let Type::Enum(_, variants) = &enum_type {
                    for (i, (name, _)) in variants.iter().enumerate() {
                        if name == &variant_name {
                            variant_index = i;
                            break;
                        }
                    }
                }
                
                // 设置标签字段（第一个字段）
                let enum_val = Global::undef(self.compile_type(&enum_type));
                let mut enum_val = builder.build_struct_insert(enum_val, 0, &Global::const_i32(variant_index as i32));
                // let tag_ptr = builder.build_struct_gep(self.compile_type(&enum_type), enum_ptr, 0);
                // builder.build_store(tag_ptr, Global::const_i32(variant_index as i32));
                
                // 如果有关联值，设置数据字段（第二个字段）
                if let Some(val) = value {
                    // 编译关联值
                    let val_value = self.compile_expr(&val, ctx);
                    
                    // 设置数据字段
                    enum_val = builder.build_struct_insert(enum_val, 1, &val_value.value);
                    // let data_ptr = builder.build_struct_gep(self.compile_type(&enum_type), enum_ptr, 1);
                    // builder.build_store(data_ptr, val_value.value);
                }
                dbg!(&enum_type);
                // 返回枚举实例
                Value::new(enum_val, enum_type)
            }
            Expr::None => Value::new(Global::const_unit(), ty0),
            t => todo!("Unknown expr: {:?}", t),
        }
    }
    fn compile_add(&self,l:LLVMValue,r:LLVMValue,ctx:&Context) -> LLVMValue {
        let builder = ctx.get_builder();
        let ty = l.get_type();
        if ty.is_float(){
            return builder.build_fadd(l, r);
        }
        builder.build_add(l, r)
    }
}
