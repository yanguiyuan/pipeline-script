use crate::llvm::value::LLVMValue;
use llvm_sys::prelude::LLVMValueRef;
use std::collections::HashMap;

pub struct StructValue {
    reference: LLVMValueRef,
    name: String,
    field_index: HashMap<String, usize>,
    // 缓存字段值
    field_values: HashMap<usize, LLVMValue>,
}
