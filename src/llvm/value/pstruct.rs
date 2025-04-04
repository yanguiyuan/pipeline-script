use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::prelude::LLVMValueRef;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct StructValue {
    reference: LLVMValueRef,
    name: String,
    pub(crate) field_index: HashMap<String, usize>,
    // 缓存字段值
    pub(crate) field_values: Vec<LLVMValue>,
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
    pub fn get_field(&self, ctx: &Context, name: &str) -> LLVMValue {
        let index = self.field_index.get(name).unwrap();
        let builder = ctx.get_builder();
        builder.build_struct_get(self, *index)
        // self.field_values[self.field_index[name]].clone()
    }
    pub fn get_llvm_type(&self, ctx: &Context) -> LLVMType {
        let llvm_module = ctx.get_llvm_module();
        let llvm_module = llvm_module.read().unwrap();
        let ty = llvm_module.get_struct(self.name.clone());
        match ty {
            Some((_, t)) => t.clone(),
            None => {
                let mut field_type = Vec::new();
                for (name, index) in &self.field_index {
                    field_type.push((name.clone(), self.field_values[*index].get_llvm_type(ctx)));
                }
                Global::struct_type(self.name.clone(), field_type)
            }
        }
    }
    pub fn with_field(&self, reference: LLVMValueRef, index: usize, value: LLVMValue) -> Self {
        let mut clone = self.clone();
        clone.reference = reference;
        clone.field_values[index] = value;
        clone
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
    pub fn get_field_index(&self, name: &str) -> Option<usize> {
        self.field_index.get(name).cloned()
    }
    pub fn get_field_by_index(&self, index: usize) -> Option<LLVMValue> {
        self.field_values.get(index).cloned()
    }
    pub fn set_reference(&mut self, reference: LLVMValueRef) {
        self.reference = reference;
    }
}

impl From<StructValue> for LLVMValue {
    fn from(value: StructValue) -> Self {
        Self::Struct(value)
    }
}
