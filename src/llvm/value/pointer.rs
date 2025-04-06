use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::LLVMIsUndef;
use llvm_sys::prelude::LLVMValueRef;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct PointerValue {
    reference: LLVMValueRef,
    element: RefCell<Box<LLVMValue>>, // 元素值，如果不确定则初始为undef
}
impl PointerValue {
    pub fn new(reference: LLVMValueRef, element: LLVMValue) -> Self {
        Self {
            reference,
            element: RefCell::new(Box::new(element)),
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn get_element(&self, ctx: &Context) -> LLVMValue {
        if self.element.borrow().is_undef() {
            let builder = ctx.get_builder();
            let element =
                builder.build_load(self.element.borrow().get_llvm_type(ctx), self.reference);
            self.element.replace(Box::new(element));
        }
        *self.element.borrow().clone()
    }
    pub fn get_element_type(&self, ctx: &Context) -> LLVMType {
        self.element.borrow().get_llvm_type(ctx)
    }
    pub fn get_llvm_type(&self, ctx: &Context) -> LLVMType {
        let element_type = self.element.borrow().get_llvm_type(ctx);
        Global::pointer_type(element_type)
    }
    pub fn set_reference(&mut self, reference: LLVMValueRef) {
        self.reference = reference;
    }
    pub fn get_struct_field_ptr(&self, ctx: &Context, name: &str) -> Option<LLVMValue> {
        let element = self.element.borrow();
        if let LLVMValue::Struct(struct_value) = &**element {
            if let Some(index) = struct_value.get_field_index(name) {
                let builder = ctx.get_builder();
                let field_ptr = builder.build_struct_gep(
                    &struct_value.get_llvm_type(ctx),
                    LLVMValue::Pointer(self.clone()),
                    index,
                );
                return Some(field_ptr);
            }
        }
        None
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}

impl From<PointerValue> for LLVMValue {
    fn from(value: PointerValue) -> Self {
        LLVMValue::Pointer(value)
    }
}
