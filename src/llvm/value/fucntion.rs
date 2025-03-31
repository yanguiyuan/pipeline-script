use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::{LLVMAppendBasicBlock, LLVMGetParam};
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
        for (index, (arg_name, _)) in self.args.iter().enumerate() {
            if arg_name == name_ref {
                return unsafe { Some(LLVMGetParam(self.reference, index as u32).into()) };
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
    pub fn get_llvm_type(&self) -> LLVMType {
        let mut args_type = vec![];
        for (_, v) in &self.args {
            args_type.push(v.get_llvm_type())
        }
        let return_type = self.return_type.get_llvm_type();
        Global::function_type(return_type, args_type)
    }
    pub fn call(&self, ctx: &Context, args: Vec<Option<LLVMValue>>) -> LLVMValue {
        let builder = ctx.get_builder();
        let mut function_call_args = vec![];
        for i in args.into_iter().flatten() {
            function_call_args.push(i)
        }
        builder.build_call(self, &mut function_call_args, "")
    }
}

impl From<FunctionValue> for LLVMValue {
    fn from(value: FunctionValue) -> Self {
        LLVMValue::Function(value)
    }
}
