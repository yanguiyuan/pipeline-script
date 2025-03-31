use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::prelude::LLVMValueRef;

#[derive(Debug, Clone)]
pub struct PointerValue {
    reference: LLVMValueRef,
    element: Box<LLVMValue>, // 元素值，如果不确定则初始为undef
}
impl PointerValue {
    pub fn new(reference: LLVMValueRef, element: LLVMValue) -> Self {
        Self {
            reference,
            element: Box::new(element),
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn get_element(&self) -> LLVMValue {
        *self.element.clone()
    }
    pub fn get_element_type(&self) -> LLVMType {
        self.element.get_llvm_type()
    }
    pub fn get_llvm_type(&self) -> LLVMType {
        let element_type = self.element.get_llvm_type();
        Global::pointer_type(element_type)
    }
    pub fn get_struct_field_ptr(&self, name: &str) -> Option<LLVMValue> {
        todo!()
    }
}

impl From<PointerValue> for LLVMValue {
    fn from(value: PointerValue) -> Self {
        LLVMValue::Pointer(value)
    }
}
