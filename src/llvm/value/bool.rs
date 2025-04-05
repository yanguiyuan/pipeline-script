use crate::llvm::value::LLVMValue;
use llvm_sys::core::LLVMIsUndef;
use llvm_sys::prelude::LLVMValueRef;

#[derive(Clone, Debug)]
pub struct BoolValue {
    reference: LLVMValueRef,
}
impl BoolValue {
    pub fn new(reference: LLVMValueRef) -> Self {
        Self { reference }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}
impl From<BoolValue> for LLVMValue {
    fn from(value: BoolValue) -> Self {
        LLVMValue::Bool(value)
    }
}
