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
        let builder = ctx.get_builder();
        let load = builder.build_load(self.element.borrow().get_llvm_type(ctx), self.reference);
        load
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
    pub fn get_struct_field_ptr(&self, ctx: &Context, name: &str) -> Option<LLVMValue> {
        let element = self.get_value(ctx);
        if let LLVMValue::Struct(struct_value) = element {
            if let Some(index) = struct_value.get_field_index(name) {
                let builder = ctx.get_builder();
                let field_ptr = builder.build_struct_gep(
                    &struct_value.get_llvm_type(ctx),
                    LLVMValue::Reference(self.clone()),
                    index,
                );
                return Some(field_ptr);
            }
        }
        None
    }
    // pub fn get_enum_variant_data_ptr(&self, ctx: &Context) -> Option<LLVMValue> {
    //     let element = self.element.borrow();
    //     if let LLVMValue::EnumVariant(enum_variant) = &**element {
    //         // 获取枚举值的引用
    //         let builder = ctx.get_builder();
    //         // 获取数据字段的指针（第二个字段，索引为1）
    //         let element_type =enum_variant.get_llvm_type(ctx);
    //         dbg!(&element_type);
    //         let data_ptr = builder.build_struct_gep(
    //             &element_type,
    //             LLVMValue::Reference(self.clone()),
    //             1,
    //         );
    //
    //         dbg!(&data_ptr);
    //         // todo!();
    //         return Some(data_ptr);
    //     }
    //     None
    // }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
}
