use crate::ast::r#type::Type;
use crate::compiler::Compiler;
use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
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
                let v = vec![
                    ("id".into(), Global::i32_type()),
                    ("data".into(), Global::pointer_type(Global::i8_type())),
                ];
                self.ctx.create_named_struct_type("Any", v)
            }
            Type::Array(t) => Global::pointer_type(self.compile_type(t)),
            Type::String => Global::string_type(),
            Type::Enum(name, variants) => {
                // 枚举类型编译为包含标签和数据的结构体
                // 标签是一个整数，表示枚举变体的索引
                // 数据是一个联合体，包含所有变体的数据
                if let Some(name) = name {
                    let llvm_module = self.llvm_module.read().unwrap();
                    let t = llvm_module.get_struct(name);
                    if let Some((_, t)) = t {
                        return t.clone();
                    }
                }
                // 创建枚举结构体类型
                let mut fields = vec![
                    // 标签字段，用于区分不同的变体
                    ("tag".into(), Global::i32_type()),
                ];

                // 查找所有变体中最大的数据类型
                let mut max_data_type: Option<LLVMType> = None;
                for (_, variant_type) in variants.iter() {
                    if let Some(t) = variant_type {
                        if t.is_alias() {
                            continue;
                        }
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
                    fields.push(("data".into(), data_type));
                }

                // 创建命名结构体类型
                if let Some(name) = name {
                    self.ctx.create_named_struct_type(name, fields)
                } else {
                    panic!("Enum type without name");
                }
            }
            Type::Struct(name, s) => match name {
                None => {
                    // panic!("Struct type without name");
                    let fields = s
                        .iter()
                        .map(|(name, t)| (name.clone(), self.compile_type(t)))
                        .collect();
                    Global::struct_type("Struct".into(), fields)
                }
                Some(name) => {
                    let llvm_module = self.llvm_module.read().unwrap();
                    let ty = llvm_module.get_struct(name);
                    match ty {
                        Some((_, t)) => t.clone(),
                        None => {
                            let mut v = vec![];
                            for (name, t) in s.iter() {
                                let mut t = self.compile_type(t);
                                if t.is_function() {
                                    t = Global::pointer_type(t)
                                }
                                v.push((name.clone(), t));
                            }
                            self.ctx.create_named_struct_type(name, v)
                        }
                    }
                }
            },
            Type::Function(ret, args) => {
                let mut v = vec![];
                for (name, t) in args.iter() {
                    v.push((name.clone(), self.compile_type(t)));
                }
                let t = ret.as_llvm_type();
                Global::function_type(t, v)
            }
            Type::ArrayVarArg(t) => Global::struct_type(
                "ArrayVarArg".into(),
                vec![
                    ("size".into(), Global::i64_type()),
                    ("data".into(), Global::pointer_type(self.compile_type(t))),
                ],
            ),
            Type::Closure { name: _, ptr, env } => {
                let return_ty = self.compile_type(&ptr.0);
                let mut params_ty = vec![];
                for (name, t) in &ptr.1 {
                    params_ty.push((name.clone(), self.compile_type(t)))
                }
                let func_ty = Global::function_type(return_ty.clone(), params_ty.clone());
                let mut env_ty = vec![];
                for a in env {
                    env_ty.push((a.0.clone(), self.compile_type(&a.1)));
                }
                Global::struct_type(
                    "Closure".into(),
                    vec![
                        ("ptr".into(), Global::pointer_type(func_ty)),
                        (
                            "env".into(),
                            Global::pointer_type(Global::struct_type("env_ty".into(), env_ty)),
                        ),
                    ],
                )
            }
            Type::Float => Global::float_type(),
            Type::Ref(t) => Global::ref_type(self.compile_type(t)),
            // Type::Alias(_) => Global::i8_type(),
            Type::Bool => Global::i1_type(),
            Type::GenericInstance { instance, .. } => self.compile_type(instance),
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
