use crate::context::Context;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::LLVMAppendBasicBlock;
use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use std::ffi::c_char;

#[derive(Clone, Debug)]
pub struct FunctionValue {
    reference: LLVMValueRef,
    name: String,
    return_type: Box<LLVMValue>,
    args: Vec<(String, LLVMValue)>,
}

impl FunctionValue {
    pub fn new(
        reference: LLVMValueRef,
        name: String,
        return_type: Box<LLVMValue>,
        args: Vec<(String, LLVMValue)>,
    ) -> Self {
        Self {
            reference,
            name,
            return_type,
            args,
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn append_basic_block(&self, name: impl Into<String>) -> LLVMBasicBlockRef {
        unsafe { LLVMAppendBasicBlock(self.reference, name.into().as_mut_ptr() as *mut c_char) }
    }
    pub fn get_param(&self, name: impl AsRef<str>) -> Option<LLVMValue> {
        let name_ref = name.as_ref();
        for (arg_name, arg_value) in &self.args {
            if arg_name == name_ref {
                return Some(arg_value.clone());
            }
        }
        None
    }
    pub fn get_param_index(&self, name: impl AsRef<str>) -> Option<usize> {
        let name_ref = name.as_ref();
        for (i, (arg_name, _)) in self.args.iter().enumerate() {
            if arg_name == name_ref {
                return Some(i);
            }
        }
        None
    }
    pub fn call(&self, ctx: &Context, args: Vec<Option<LLVMValue>>) -> LLVMValue {
        println!("call function: {}", self.name);
        println!("{:?}", args);
        todo!()
    }
}

impl From<FunctionValue> for LLVMValue {
    fn from(value: FunctionValue) -> Self {
        LLVMValue::Function(value)
    }
}
