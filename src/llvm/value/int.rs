use crate::context::Context;
use crate::llvm::value::bool::BoolValue;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::LLVMIsUndef;
use llvm_sys::prelude::LLVMValueRef;

#[derive(Debug, Clone)]
pub struct Int32Value {
    reference: LLVMValueRef,
}

impl Int32Value {
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn new(reference: LLVMValueRef) -> Self {
        Self { reference }
    }
    pub fn eq(&self, ctx: &Context, other: &Int32Value) -> BoolValue {
        let builder = ctx.get_builder();
        builder.build_eq(self.reference, other.reference)
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}

impl From<Int32Value> for LLVMValue {
    fn from(value: Int32Value) -> Self {
        LLVMValue::Int32(value)
    }
}

//--------------

#[derive(Clone, Debug)]
pub struct Int8Value {
    reference: LLVMValueRef,
}
impl Int8Value {
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn new(reference: LLVMValueRef) -> Self {
        Self { reference }
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}

impl From<Int8Value> for LLVMValue {
    fn from(value: Int8Value) -> Self {
        LLVMValue::Int8(value)
    }
}

/**
 *  Int16Value  
 */
#[derive(Clone, Debug)]
pub struct Int16Value {
    reference: LLVMValueRef,
}

impl Int16Value {
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn new(reference: LLVMValueRef) -> Self {
        Self { reference }
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}

impl From<Int16Value> for LLVMValue {
    fn from(value: Int16Value) -> Self {
        LLVMValue::Int16(value)
    }
}

/**
 *  Int32Value  
 */
#[derive(Clone, Debug)]
pub struct Int64Value {
    reference: LLVMValueRef,
}

impl Int64Value {
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn new(reference: LLVMValueRef) -> Self {
        Self { reference }
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}

impl From<Int64Value> for LLVMValue {
    fn from(value: Int64Value) -> Self {
        LLVMValue::Int64(value)
    }
}
