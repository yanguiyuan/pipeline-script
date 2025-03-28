use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::prelude::LLVMValueRef;
#[derive(Debug, Clone)]
pub struct ReferenceValue {
    reference: LLVMValueRef,
    element: Box<LLVMValue>,
}

impl ReferenceValue {
    pub fn new(reference: LLVMValueRef, element: LLVMValue) -> Self {
        Self {
            reference,
            element: Box::new(element),
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn get_value(&self) -> LLVMValue {
        *self.element.clone()
    }
    pub fn get_element_type(&self) -> LLVMType {
        self.element.get_type()
    }
    pub fn get_struct_field_ptr(&self, name: &str) -> Option<LLVMValue> {
        todo!()
    }
    pub fn get_enum_variant_data_ptr(&self) -> Option<LLVMValue> {
        todo!()
    }
}
