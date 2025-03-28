use crate::llvm::value::LLVMValue;
use llvm_sys::prelude::LLVMValueRef;
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub struct StructValue {
    reference: LLVMValueRef,
    name: String,
    field_index: HashMap<String, usize>,
    // 缓存字段值
    field_values: Vec<LLVMValue>,
}

impl StructValue {
    pub fn new(
        reference: LLVMValueRef,
        name: String,
        field_index: HashMap<String, usize>,
        field_values: Vec<LLVMValue>,
    ) -> Self {
        Self {
            reference,
            name,
            field_index,
            field_values,
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn get_field(&self, name: &str) -> LLVMValue {
        self.field_values[self.field_index[name]].clone()
    }
}
