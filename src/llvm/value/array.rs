use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::LLVMIsUndef;
use llvm_sys::prelude::LLVMValueRef;

#[derive(Clone, Debug)]
pub struct ArrayValue {
    reference: LLVMValueRef,
    element: LLVMType,
    #[allow(unused)]
    length: usize,
}

impl ArrayValue {
    pub fn new(reference: LLVMValueRef, element: LLVMType, length: usize) -> Self {
        Self {
            reference,
            element,
            length,
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn get_element_type(&self) -> LLVMType {
        self.element.clone()
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}
impl From<ArrayValue> for LLVMValue {
    fn from(value: ArrayValue) -> Self {
        LLVMValue::Array(value)
    }
}
