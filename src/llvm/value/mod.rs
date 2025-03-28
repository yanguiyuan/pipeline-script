pub mod bool;
pub mod fucntion;
pub mod int;
pub mod penum;
pub mod pointer;
pub mod pstruct;
pub mod reference;

use crate::llvm::types::LLVMType;
use crate::llvm::value::bool::BoolValue;
use crate::llvm::value::fucntion::FunctionValue;
use crate::llvm::value::int::{Int16Value, Int32Value, Int64Value, Int8Value};
use crate::llvm::value::penum::EnumVariantValue;
use crate::llvm::value::pointer::PointerValue;
use crate::llvm::value::pstruct::StructValue;
use crate::llvm::value::reference::ReferenceValue;
use llvm_sys::core::{
    LLVMConstString, LLVMGetElementType, LLVMGetIntTypeWidth, LLVMGetTypeKind, LLVMTypeOf,
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
    Float(LLVMValueRef),
    Double(LLVMValueRef),
    Pointer(PointerValue),
    Array(LLVMValueRef),
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
        unsafe {
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
                //     LLVMValue::Pointer(PointerValue::new(value, LLVMValue::Undef(value)))
                // }
                LLVMTypeKind::LLVMArrayTypeKind => LLVMValue::Array(value),
                LLVMTypeKind::LLVMDoubleTypeKind => LLVMValue::Double(value),
                LLVMTypeKind::LLVMVoidTypeKind => LLVMValue::Unit,
                // LLVMTypeKind::LLVMStructTypeKind => LLVMValue::Struct(StructValue::new(value, UNNAMED.into(), HashMap::new(), ty)),
                LLVMTypeKind::LLVMFloatTypeKind => LLVMValue::Float(value),
                t => {
                    println!("{t:?}");
                    todo!()
                }
            }
        }
    }
}
impl LLVMValue {
    pub fn id(&self) -> i32 {
        match self {
            LLVMValue::String(_) => 0,
            LLVMValue::Bool(_) => 1,
            LLVMValue::Int8(_) => 3,
            LLVMValue::Int32(_) => 5,
            LLVMValue::Int64(_) => 7,
            LLVMValue::Float(_) => 9,
            LLVMValue::Double(_) => 11,
            LLVMValue::Unit => 0,
            t => {
                panic!("Unknown type: {:?}", t)
            }
        }
    }
    pub fn as_llvm_value_ref(&self) -> LLVMValueRef {
        match self {
            LLVMValue::String(i) => *i,
            LLVMValue::Float(i) => *i,
            LLVMValue::Double(i) => *i,
            LLVMValue::Int64(i) => i.get_reference(),
            LLVMValue::Int32(i) => i.get_reference(),
            LLVMValue::Int16(i) => i.get_reference(),
            LLVMValue::Int8(i) => i.get_reference(),
            LLVMValue::Bool(i) => i.get_reference(),
            LLVMValue::Pointer(i) => i.get_reference(),
            LLVMValue::Array(i) => *i,
            LLVMValue::Struct(i) => i.get_reference(),
            // LLVMValue::Undef(i) => *i,
            // LLVMValue::Unit => Global::undef(Global::unit_type()).as_llvm_value_ref(),
            t => {
                panic!("Unknown type: {:?}", t)
            }
        }
    }
    pub fn as_enum_variant(&self) -> Option<&EnumVariantValue> {
        match self {
            LLVMValue::EnumVariant(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_reference(&self) -> Option<&ReferenceValue> {
        match self {
            LLVMValue::Reference(v) => Some(v),
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
    pub fn get_type(&self) -> LLVMType {
        let ty = unsafe { LLVMTypeOf(self.as_llvm_value_ref()) };
        match self {
            LLVMValue::Bool(_) => LLVMType::Int1(ty),
            LLVMValue::Int8(_) => LLVMType::Int8(ty),
            LLVMValue::Int32(_) => LLVMType::Int32(ty),
            LLVMValue::Int64(_) => LLVMType::Int64(ty),
            LLVMValue::Float(_) => LLVMType::Float(ty),
            LLVMValue::Double(_) => LLVMType::Double(ty),
            // LLVMValue::Pointer(_) => LLVMType::Pointer(Box::new(ty)),
            LLVMValue::Array(_) => {
                let element_type = unsafe { LLVMGetElementType(ty) };
                LLVMType::Array(Box::new(LLVMType::from(element_type)), ty)
            }
            // LLVMValue::Struct(_)=>LLVMType::Struct(ty),
            t => panic!("{t:?}"),
        }
    }
    pub fn is_unit(&self) -> bool {
        matches!(self, LLVMValue::Unit)
    }
}
