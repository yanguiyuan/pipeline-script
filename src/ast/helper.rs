use std::sync::{Arc, RwLock};
use crate::llvm::global::Global;
use crate::llvm::module::LLVMModule;
use crate::parser::r#type::Type;

pub fn build_function(llvm_module:Arc<RwLock<LLVMModule>>, is_extern:bool, return_type:&Type, name:&str){
    let mut mut_llvm_module = llvm_module.write().unwrap();
    if is_extern{
        mut_llvm_module.register_extern_function(name,Global::function_type(return_type.as_llvm_type(),vec![]));
        return
    }
    mut_llvm_module.register_function(name,Global::function_type(return_type.as_llvm_type(),vec![]),vec![]);
}