use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use llvm_sys::core::{
    LLVMConstString, LLVMGetElementType, LLVMGetIntTypeWidth, LLVMGetTypeKind, LLVMTypeOf,
};
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::LLVMTypeKind;
use std::ffi::{c_uint, CString};
#[derive(Clone, Debug, Copy)]
pub enum LLVMValue {
    String(LLVMValueRef),
    Int1(LLVMValueRef),
    Int8(LLVMValueRef),
    Int16(LLVMValueRef),
    Int32(LLVMValueRef),
    Int64(LLVMValueRef),
    Float(LLVMValueRef),
    Double(LLVMValueRef),
    Pointer(LLVMValueRef),
    Array(LLVMValueRef),
    Struct(LLVMValueRef),
    Undef(LLVMValueRef),
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
                    1 => LLVMValue::Int1(value),
                    8 => LLVMValue::Int8(value),
                    32 => LLVMValue::Int32(value),
                    64 => LLVMValue::Int64(value),
                    _ => {
                        todo!()
                    }
                }
            }
            LLVMTypeKind::LLVMPointerTypeKind => LLVMValue::Pointer(value),
            LLVMTypeKind::LLVMArrayTypeKind => LLVMValue::Array(value),
            LLVMTypeKind::LLVMDoubleTypeKind => LLVMValue::Double(value),
            LLVMTypeKind::LLVMVoidTypeKind => LLVMValue::Unit,
            LLVMTypeKind::LLVMStructTypeKind => LLVMValue::Struct(value),
            LLVMTypeKind::LLVMFloatTypeKind => LLVMValue::Float(value),
            t => {
                println!("{t:?}");
                todo!()
            }
        }
    }
}
impl LLVMValue {
    pub fn as_llvm_value_ref(&self) -> LLVMValueRef {
        match self {
            LLVMValue::String(i) => *i,
            LLVMValue::Float(i) => *i,
            LLVMValue::Double(i) => *i,
            LLVMValue::Int64(i) => *i,
            LLVMValue::Int32(i) => *i,
            LLVMValue::Int8(i) => *i,
            LLVMValue::Int1(i) => *i,
            LLVMValue::Pointer(i) => *i,
            LLVMValue::Array(i) => *i,
            LLVMValue::Struct(i) => *i,
            LLVMValue::Undef(i) => *i,
            LLVMValue::Unit => Global::undef(Global::unit_type()).as_llvm_value_ref(),
            t => {
                panic!("Unknown type: {:?}", t)
            }
        }
    }
    pub fn is_pointer(&self) -> bool {
        matches!(self, LLVMValue::Pointer(_))
    }
    pub fn get_type(&self) -> LLVMType {
        let ty = unsafe { LLVMTypeOf(self.as_llvm_value_ref()) };
        match self {
            LLVMValue::Int1(_) => LLVMType::Int1(ty),
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
