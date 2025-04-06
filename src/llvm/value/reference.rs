use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::LLVMIsUndef;
use llvm_sys::prelude::LLVMValueRef;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ReferenceValue {
    reference: LLVMValueRef,
    element: Rc<RefCell<Box<LLVMValue>>>,
}

impl ReferenceValue {
    pub fn new(reference: LLVMValueRef, element: LLVMValue) -> Self {
        Self {
            reference,
            element: Rc::new(RefCell::new(Box::new(element))),
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn store(&self, ctx: &Context, value: LLVMValue) {
        let builder = ctx.get_builder();
        self.element.replace(Box::new(value.clone()));
        builder.build_store(self.reference, value);
    }
    pub fn get_value(&self, ctx: &Context) -> LLVMValue {
        if self.element.borrow().is_undef() {
            let builder = ctx.get_builder();
            let load = builder.build_load(self.element.borrow().get_llvm_type(ctx), self.reference);
            self.element.replace(Box::new(load));
        }
        *self.element.borrow().clone()
        // let builder = ctx.get_builder();
        // builder.build_load(self.element.borrow().get_llvm_type(ctx), self.reference)
    }
    pub fn get_element_type(&self, ctx: &Context) -> LLVMType {
        self.element.borrow().get_llvm_type(ctx)
    }
    pub fn get_llvm_type(&self, ctx: &Context) -> LLVMType {
        let element_type = self.element.borrow().get_llvm_type(ctx);
        Global::ref_type(element_type)
    }
    pub fn set_reference(&mut self, reference: LLVMValueRef) {
        self.reference = reference;
    }
    pub fn get_struct_field_ptr(&self, name: &str) -> Option<LLVMValue> {
        todo!()
    }
    pub fn get_enum_variant_data_ptr(&self) -> Option<LLVMValue> {
        todo!()
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}
