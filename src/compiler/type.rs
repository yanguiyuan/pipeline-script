use crate::compiler::Compiler;
use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::ast::r#type::Type;
impl Compiler {
    pub(crate) fn compile_type(&self, ty: &Type) -> LLVMType {
        match ty {
            Type::Int8 => Global::i8_type(),
            Type::Int16 => Global::i16_type(),
            Type::Int32 => Global::i32_type(),
            Type::Int64 => Global::i64_type(),
            Type::Unit => Global::unit_type(),
            Type::Pointer(i) => Global::pointer_type(self.compile_type(i)),
            Type::Any => {
                let v = vec![Global::i32_type(), Global::pointer_type(Global::i8_type())];
                self.ctx.create_named_struct_type("Any", v)
            }
            Type::Array(t) => Global::pointer_type(t.as_llvm_type()),
            Type::String => Global::pointer_type(Global::i8_type()),
            Type::Enum(name, variants) => {
                // 枚举类型编译为包含标签和数据的结构体
                // 标签是一个整数，表示枚举变体的索引
                // 数据是一个联合体，包含所有变体的数据
                if let Some(name) = name {
                    let t = self.llvm_module.get_struct(name);
                    if let Some((_, t)) = t {
                        return t.clone();
                    }
                }
                // 创建枚举结构体类型
                let mut fields = vec![
                    // 标签字段，用于区分不同的变体
                    Global::i32_type(),
                ];

                // 查找所有变体中最大的数据类型
                let mut max_data_type: Option<LLVMType> = None;
                for (_, variant_type) in variants.iter() {
                    if let Some(t) = variant_type {
                        let llvm_type = self.compile_type(t);
                        if let Some(max_type) = &max_data_type {
                            // 简单比较，选择大小更大的类型
                            if llvm_type.size() > max_type.size() {
                                max_data_type = Some(llvm_type);
                            }
                        } else {
                            max_data_type = Some(llvm_type);
                        }
                    }
                }

                // 添加数据字段
                if let Some(data_type) = max_data_type {
                    fields.push(data_type);
                } else {
                    // 如果没有变体有数据，添加一个空字段
                    fields.push(Global::unit_type());
                }

                // 创建命名结构体类型
                if let Some(name) = name {
                    self.ctx.create_named_struct_type(name, fields)
                } else {
                    Global::struct_type(fields)
                }
            }
            Type::Struct(name, s) => match name {
                None => {
                    let mut v = vec![];
                    for (_, t) in s.iter() {
                        v.push(self.compile_type(t));
                    }
                    Global::struct_type(v)
                }
                Some(name) => {
                    let ty = self.llvm_module.get_struct(name);
                    match ty {
                        Some((_, t)) => t.clone(),
                        None => {
                            let mut v = vec![];
                            for (_, t) in s.iter() {
                                v.push(self.compile_type(t));
                            }
                            self.ctx.create_named_struct_type(name, v)
                        }
                    }
                }
            },
            Type::Function(ret, args) => {
                let mut v = vec![];
                for t in args.iter() {
                    v.push(self.compile_type(t));
                }
                let t = ret.as_llvm_type();
                Global::struct_type(vec![
                    Global::pointer_type(Global::function_type(t, v)),
                    Global::pointer_type(Global::unit_type()),
                ])
            }
            Type::ArrayVarArg(t) => Global::struct_type(vec![
                Global::i64_type(),
                Global::pointer_type(self.compile_type(t)),
            ]),
            Type::Closure { name: _, ptr, env } => {
                let return_ty = self.compile_type(&ptr.0);
                let mut params_ty = vec![];
                for t in &ptr.1 {
                    params_ty.push(self.compile_type(t))
                }
                let mut env_ty = vec![];
                for a in env {
                    env_ty.push(self.compile_type(&a.1));
                }
                Global::pointer_type(Global::struct_type(vec![
                    Global::pointer_type(Global::function_type(return_ty, params_ty)),
                    Global::pointer_type(Global::struct_type(env_ty)),
                ]))
            }
            Type::Float => Global::float_type(),
            Type::Ref(t) => Global::pointer_type(t.as_llvm_type()),
            Type::Alias(_) => Global::i8_type(),
            Type::Bool => Global::i1_type(),
            _ => panic!("Unknown type: {:?}", ty),
        }
    }
    pub(crate) fn get_type(&self, ctx: &Context, ty: &Type) -> LLVMType {
        let r = ctx.get_type(ty);
        match r {
            None => {
                let t = self.compile_type(ty);
                ctx.register_type(ty, &t);
                t
            }
            Some(t) => t,
        }
    }
}
