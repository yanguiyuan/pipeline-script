use llvm_sys::core::LLVMIsUndef;
use llvm_sys::prelude::LLVMValueRef;

use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::int::Int32Value;
use crate::llvm::value::LLVMValue;

#[derive(Debug, Clone)]
pub struct EnumVariantValue {
    #[allow(unused)]
    reference: LLVMValueRef,
    #[allow(unused)]
    enum_name: String,
    #[allow(unused)]
    variant_name: String,
    tag: Int32Value,
    value: Option<Box<LLVMValue>>,
}

impl EnumVariantValue {
    pub fn new(
        reference: LLVMValueRef,
        enum_name: String,
        variant_name: String,
        tag: Int32Value,
        value: Option<Box<LLVMValue>>,
    ) -> Self {
        Self {
            reference,
            enum_name,
            variant_name,
            tag,
            value,
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }
    pub fn get_tag(&self) -> Int32Value {
        self.tag.clone()
    }
    pub fn get_value(&self) -> Option<LLVMValue> {
        self.value.as_ref().map(|v| *v.clone())
    }
    pub fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.reference) == 1 }
    }
    pub fn get_llvm_type(&self, ctx: &Context) -> LLVMType {
        // 枚举类型在LLVM中被编译为包含标签和数据的结构体
        let fields = vec![
            // 标签字段，用于区分不同的变体
            ("tag".into(), Global::i32_type()),
            // 数据字段，如果有值则使用值的类型，否则使用i64作为默认类型
            (
                "data".into(),
                self.value
                    .as_ref()
                    .map_or(Global::i64_type(), |v| v.get_llvm_type(ctx)),
            ),
        ];
        Global::struct_type(self.enum_name.clone(), fields)
    }
}
