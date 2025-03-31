use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
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
    pub fn get_llvm_type(&self) -> LLVMType {
        let mut field_type = Vec::new();
        for (name, index) in &self.field_index {
            field_type.push((name.clone(), self.field_values[*index].get_llvm_type()));
        }
        Global::struct_type(self.name.clone(), field_type)
    }
    pub fn with_field(&self, reference: LLVMValueRef, index: usize, value: LLVMValue) -> Self {
        let mut clone = self.clone();
        clone.reference = reference;
        clone.field_values[index] = value;
        clone
    }
}
