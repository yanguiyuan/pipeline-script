use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::prelude::LLVMValueRef;
#[derive(Debug)]
pub struct ReferenceValue {
    reference: LLVMValueRef,
    element: LLVMValue,
}

impl ReferenceValue {
    pub fn new(reference: LLVMValueRef, element: LLVMValue) -> Self {
        Self { reference, element }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn get_element(&self) -> LLVMValue {
        self.element
    }
    pub fn get_element_type(&self) -> LLVMType {
        self.element.get_type()
    }
    pub fn get_struct_field_ptr(&self, name: &str) -> Option<LLVMValue> {
        todo!()
    }
}
