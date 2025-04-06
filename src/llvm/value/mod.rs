pub mod array;
pub mod bool;
pub mod double;
pub mod float;
pub mod fucntion;
pub mod int;
pub mod penum;
pub mod pointer;
pub mod pstruct;
pub mod reference;

use crate::context::Context;
use crate::llvm::types::LLVMType;
use crate::llvm::value::array::ArrayValue;
use crate::llvm::value::bool::BoolValue;
use crate::llvm::value::double::DoubleValue;
use crate::llvm::value::float::FloatValue;
use crate::llvm::value::fucntion::FunctionValue;
use crate::llvm::value::int::{Int16Value, Int32Value, Int64Value, Int8Value};
use crate::llvm::value::penum::EnumVariantValue;
use crate::llvm::value::pointer::PointerValue;
use crate::llvm::value::pstruct::StructValue;
use crate::llvm::value::reference::ReferenceValue;
use llvm_sys::core::{
    LLVMConstString, LLVMGetIntTypeWidth, LLVMGetTypeKind, LLVMGetUndef, LLVMTypeOf, LLVMVoidType,
};
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::LLVMTypeKind;
use std::ffi::{c_uint, CString};

#[derive(Clone, Debug)]
pub enum LLVMValue {
    String(LLVMValueRef),
    Bool(BoolValue),
    Int8(Int8Value),
    Int16(Int16Value),
    Int32(Int32Value),
    Int64(Int64Value),
    Float(FloatValue),
    Double(DoubleValue),
    Pointer(PointerValue),
    Array(ArrayValue),
    Struct(StructValue),
    // Undef(LLVMValueRef),
    Function(FunctionValue),
    Reference(ReferenceValue),
    EnumVariant(EnumVariantValue),
    Unit,
}

impl From<&str> for LLVMValue {
    fn from(value: &str) -> Self {
        let str = CString::new(value).unwrap();
        let c = unsafe { LLVMConstString(str.as_ptr(), value.len() as c_uint, 0) };
        LLVMValue::String(c)
    }
}

impl From<LLVMValueRef> for LLVMValue {
    fn from(value: LLVMValueRef) -> Self {
        let ty = unsafe { LLVMTypeOf(value) };
        let type_kind = unsafe { LLVMGetTypeKind(ty) };
        match type_kind {
            LLVMTypeKind::LLVMIntegerTypeKind => {
                let width = unsafe { LLVMGetIntTypeWidth(ty) };
                let width = width as i8;

                match width {
                    1 => LLVMValue::Bool(BoolValue::new(value)),
                    8 => LLVMValue::Int8(Int8Value::new(value)),
                    16 => LLVMValue::Int16(Int16Value::new(value)),
                    32 => LLVMValue::Int32(Int32Value::new(value)),
                    64 => LLVMValue::Int64(Int64Value::new(value)),
                    _ => {
                        todo!()
                    }
                }
            }
            // LLVMTypeKind::LLVMPointerTypeKind => {
            //     LLVMValue::Pointer(PointerValue::new(value, LLVMGetUndef(ty)))
            // }
            // LLVMTypeKind::LLVMArrayTypeKind => LLVMValue::Array(value),
            LLVMTypeKind::LLVMDoubleTypeKind => LLVMValue::Double(DoubleValue::new(value)),
            LLVMTypeKind::LLVMVoidTypeKind => LLVMValue::Unit,
            // LLVMTypeKind::LLVMStructTypeKind => LLVMValue::Struct(StructValue::new(value, UNNAMED.into(), HashMap::new(), ty)),
            LLVMTypeKind::LLVMFloatTypeKind => LLVMValue::Float(FloatValue::new(value)),
            t => {
                println!("{t:?}");
                todo!()
            }
        }
    }
}

impl From<ReferenceValue> for LLVMValue {
    fn from(value: ReferenceValue) -> Self {
        LLVMValue::Reference(value)
    }
}

impl LLVMValue {
    pub fn id(&self) -> i32 {
        match self {
            LLVMValue::Unit => 0,
            LLVMValue::Bool(_) => 1,
            LLVMValue::Int8(_) => 3,
            LLVMValue::Int16(_) => 5,
            LLVMValue::Int32(_) => 7,
            LLVMValue::Int64(_) => 9,
            LLVMValue::Float(_) => 11,
            LLVMValue::Double(_) => 13,
            LLVMValue::String(_) => 15,
            t => {
                panic!("Unknown type: {:?}", t)
            }
        }
    }
    pub fn as_llvm_value_ref(&self) -> LLVMValueRef {
        unsafe {
            match self {
                LLVMValue::String(i) => *i,
                LLVMValue::Float(i) => i.get_reference(),
                LLVMValue::Double(i) => i.get_reference(),
                LLVMValue::Int64(i) => i.get_reference(),
                LLVMValue::Int32(i) => i.get_reference(),
                LLVMValue::Int16(i) => i.get_reference(),
                LLVMValue::Int8(i) => i.get_reference(),
                LLVMValue::Bool(i) => i.get_reference(),
                LLVMValue::Pointer(i) => i.get_reference(),
                LLVMValue::Array(i) => i.get_reference(),
                LLVMValue::Struct(i) => i.get_reference(),
                LLVMValue::Reference(i) => i.get_reference(),
                LLVMValue::Unit => LLVMGetUndef(LLVMVoidType()),
                LLVMValue::Function(i) => i.get_reference(),
                LLVMValue::EnumVariant(i) => i.get_reference(),
            }
        }
    }
    // pub fn as_enum_variant(&self) -> Option<&EnumVariantValue> {
    //     match self {
    //         LLVMValue::EnumVariant(v) => Some(v),
    //         _ => None,
    //     }
    // }
    pub fn as_int32(&self) -> Option<&Int32Value> {
        match self {
            LLVMValue::Int32(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_bool(&self) -> Option<&BoolValue> {
        match self {
            LLVMValue::Bool(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_reference(&self) -> Option<&ReferenceValue> {
        match self {
            LLVMValue::Reference(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_array(&self) -> Option<&ArrayValue> {
        match self {
            LLVMValue::Array(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_pointer(&self) -> Option<&PointerValue> {
        match self {
            LLVMValue::Pointer(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_struct(&self) -> Option<&StructValue> {
        match self {
            LLVMValue::Struct(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_function(&self) -> Option<FunctionValue> {
        match self {
            LLVMValue::Function(v) => Some(v.clone()),
            _ => None,
        }
    }
    pub fn is_reference(&self) -> bool {
        matches!(self, LLVMValue::Reference(_))
    }
    pub fn is_struct(&self) -> bool {
        matches!(self, LLVMValue::Struct(_))
    }
    pub fn is_pointer(&self) -> bool {
        matches!(self, LLVMValue::Pointer(_))
    }
    pub fn is_function(&self) -> bool {
        matches!(self, LLVMValue::Function(_))
    }
    pub fn is_array(&self) -> bool {
        matches!(self, LLVMValue::Array(_))
    }
    pub fn get_llvm_type(&self, ctx: &Context) -> LLVMType {
        let ty = unsafe { LLVMTypeOf(self.as_llvm_value_ref()) };
        match self {
            LLVMValue::Bool(_) => LLVMType::Int1(ty),
            LLVMValue::Int8(_) => LLVMType::Int8(ty),
            LLVMValue::Int16(_) => LLVMType::Int16(ty),
            LLVMValue::Int32(_) => LLVMType::Int32(ty),
            LLVMValue::Int64(_) => LLVMType::Int64(ty),
            LLVMValue::Float(_) => LLVMType::Float(ty),
            LLVMValue::Double(_) => LLVMType::Double(ty),
            LLVMValue::Pointer(pointer_value) => pointer_value.get_llvm_type(ctx),
            LLVMValue::Array(array) => LLVMType::Array(Box::new(array.get_element_type()), ty),
            LLVMValue::Struct(struct_value) => struct_value.get_llvm_type(ctx),
            LLVMValue::Unit => LLVMType::Unit(ty),
            LLVMValue::String(_) => LLVMType::String(ty),
            LLVMValue::Function(function_value) => function_value.get_llvm_type(ctx),
            LLVMValue::Reference(reference_value) => reference_value.get_llvm_type(ctx),
            LLVMValue::EnumVariant(enum_variant_value) => enum_variant_value.get_llvm_type(ctx),
        }
    }
    pub fn is_unit(&self) -> bool {
        matches!(self, LLVMValue::Unit)
    }
    pub fn is_undef(&self) -> bool {
        match self {
            LLVMValue::Bool(v) => v.is_undef(),
            LLVMValue::Int8(v) => v.is_undef(),
            LLVMValue::Int16(v) => v.is_undef(),
            LLVMValue::Int32(v) => v.is_undef(),
            LLVMValue::Int64(v) => v.is_undef(),
            LLVMValue::Float(v) => v.is_undef(),
            LLVMValue::Double(v) => v.is_undef(),
            LLVMValue::Pointer(v) => v.is_undef(),
            LLVMValue::Reference(v) => v.is_undef(),
            LLVMValue::Struct(v) => v.is_undef(),
            LLVMValue::Array(v) => v.is_undef(),
            LLVMValue::EnumVariant(v) => v.is_undef(),
            LLVMValue::Function(_) => false,
            LLVMValue::Unit => false,
            LLVMValue::String(_) => false,
        }
    }
}
