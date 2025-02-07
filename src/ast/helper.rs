use std::sync::{Arc, RwLock};
use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::module::LLVMModule;
use crate::llvm::types::LLVMType;
use crate::parser::r#type::Type;

pub fn build_function_declaration(llvm_module:Arc<RwLock<LLVMModule>>, is_extern:bool, return_type:&Type, name:&str,params_types:Vec<LLVMType>,params_names:Vec<String>){
    let mut mut_llvm_module = llvm_module.write().unwrap();
    if is_extern{
        mut_llvm_module.register_extern_function(name,Global::function_type(return_type.as_llvm_type(),params_types));
        return
    }
    mut_llvm_module.register_function(name,Global::function_type(return_type.as_llvm_type(),params_types),params_names);
}

pub fn build_type(ty: &Type,ctx:&Context) -> LLVMType {
    let llvm_ctx = ctx.get_llvm_context();
    let llvm_ctx = llvm_ctx.read().unwrap();
    match ty {
        Type::Int8 => Global::i8_type(),
        Type::Int16 => Global::i16_type(),
        Type::Int32 => Global::i32_type(),
        Type::Int64 => Global::i64_type(),
        Type::Unit => Global::unit_type(),
        Type::Pointer(i) => Global::pointer_type(i.as_llvm_type()),
        Type::Any => {
            let v = vec![Global::i32_type(), Global::pointer_type(Global::i8_type())];
            llvm_ctx.create_named_struct_type("Any", v)
        }
        Type::Array(t) => Global::struct_type(vec![
            Global::i64_type(),
            Global::pointer_type(t.as_llvm_type()),
        ]),
        Type::String => Global::pointer_type(Global::i8_type()),
        Type::Struct(name, s) => match name {
            None => {
                let mut v = vec![];
                for (_, t) in s.iter() {
                    v.push(t.as_llvm_type());
                }
                Global::struct_type(v)
            }
            Some(name) => {
                let mut v = vec![];
                for (_, t) in s.iter() {
                    v.push(t.as_llvm_type());
                }
                llvm_ctx.create_named_struct_type(name, v)
            }
        },
        Type::Function(ret, args) => {
            let mut v = vec![];
            for t in args.iter() {
                v.push(t.as_llvm_type());
            }
            let t = ret.as_llvm_type();
            Global::struct_type(vec![
                Global::pointer_type(Global::function_type(t, v)),
                Global::pointer_type(Global::unit_type()),
            ])
        }
        Type::ArrayVarArg(t) => Global::struct_type(vec![
            Global::i64_type(),
            Global::pointer_type(build_type(t,ctx)),
        ]),
        Type::Closure { ptr, env } => {
            let return_ty =build_type(&ptr.0,ctx);
            let mut params_ty = vec![];
            for t in &ptr.1 {
                params_ty.push(build_type(t,ctx))
            }
            let mut env_ty = vec![];
            for a in env {
                env_ty.push(build_type(&a.1,ctx));
            }
            Global::pointer_type(Global::struct_type(vec![
                Global::pointer_type(Global::function_type(return_ty, params_ty)),
                Global::pointer_type(Global::struct_type(env_ty)),
            ]))
        }
        _ => panic!("Unknown type: {:?}", ty),
    }
}