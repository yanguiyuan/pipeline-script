use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::{LLVMAppendBasicBlock, LLVMGetParam, LLVMSetGC};
use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use std::ffi::CString;

#[derive(Clone, Debug)]
pub struct Function {
    function_ref: LLVMValueRef,
    declaration: LLVMType,
    param_names: Vec<String>,
}

impl Function {
    pub(crate) fn new(
        declaration: LLVMType,
        function_ref: LLVMValueRef,
        param_names: Vec<String>,
    ) -> Self {
        Self {
            function_ref,
            declaration,
            param_names,
        }
    }
    pub fn set_gc(&self, name: impl AsRef<str>) {
        let cstr = CString::new(name.as_ref()).unwrap();
        unsafe {
            LLVMSetGC(self.function_ref, cstr.as_ptr());
        }
    }
    pub fn has_param_name(&self, name: &str) -> bool {
        self.param_names.contains(&name.to_string())
    }

    pub fn get_param_index(&self, name: &str) -> Option<usize> {
        for (index, param_name) in self.param_names.iter().enumerate() {
            if param_name == name {
                return Some(index);
            }
        }
        None
    }
    pub fn append_basic_block(&self, name: impl AsRef<str>) -> LLVMBasicBlockRef {
        let name = name.as_ref();
        let name = CString::new(name).unwrap();
        unsafe { LLVMAppendBasicBlock(self.function_ref, name.as_ptr()) }
    }
    pub fn get_param(&self, name: &str) -> Option<LLVMValue> {
        let index = self.get_param_index(name);
        match index {
            None => None,
            Some(index) => Some(unsafe { LLVMGetParam(self.function_ref, index as u32) }.into()),
        }
    }
    pub fn get_function_ref(&self) -> LLVMValueRef {
        self.function_ref
    }
    pub fn get_declaration(&self) -> LLVMType {
        self.declaration.clone()
    }
    pub fn as_ref(&self) -> LLVMValueRef {
        self.function_ref
    }
}
