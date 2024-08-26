use std::collections::HashMap;
use std::ffi::{c_uint, CStr};
use std::fmt::{Display, Formatter};
use llvm_sys::core::{LLVMGetElementType, LLVMGetIntTypeWidth, LLVMGetStructElementTypes, LLVMGetTypeKind, LLVMInt32Type, LLVMIsFunctionVarArg, LLVMPrintTypeToString, LLVMStructGetTypeAtIndex, LLVMTypeOf};
use llvm_sys::LLVMTypeKind;
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use crate::global::Global;

#[derive(Clone, Debug, PartialOrd, PartialEq)]
pub enum LLVMType{
    Int1(LLVMTypeRef),
    Int8(LLVMTypeRef),
    Int16(LLVMTypeRef),
    Int32(LLVMTypeRef),
    Int64(LLVMTypeRef),
    Float(LLVMTypeRef),
    Double(LLVMTypeRef),
    Struct(Vec<LLVMType>,LLVMTypeRef),
    Array(Box<LLVMType>,LLVMTypeRef),
    Function(Box<LLVMType>,Vec<LLVMType>,LLVMTypeRef),
    Pointer(Box<LLVMType>,LLVMTypeRef),
    Unit(LLVMTypeRef)
}


impl From<LLVMValueRef> for LLVMType{
    fn from(value: LLVMValueRef) -> Self {
        let ty = unsafe{ LLVMTypeOf(value)};
        let type_kind = unsafe {
            LLVMGetTypeKind(ty)
        };
        match type_kind {
            LLVMTypeKind::LLVMIntegerTypeKind => {
                let width = unsafe{LLVMGetIntTypeWidth(ty)};
                let width = width as i8;
                match width {
                    8=>LLVMType::Int8(ty),
                    32=>LLVMType::Int32(ty),
                    _=>{todo!()}
                }
            }
           _=>{todo!()}
        }

    }
}

impl From<LLVMTypeRef> for LLVMType {
    fn from(ty: LLVMTypeRef) -> Self {
        let type_kind = unsafe {
            LLVMGetTypeKind(ty)
        };
        match type_kind {
            LLVMTypeKind::LLVMIntegerTypeKind => {
                let width = unsafe{LLVMGetIntTypeWidth(ty)};
                let width = width as i8;
                match width {
                    8=>LLVMType::Int8(ty),
                    32=>LLVMType::Int32(ty),
                    64=>LLVMType::Int64(ty),
                    _=>{todo!()}
                }
            }
            LLVMTypeKind::LLVMArrayTypeKind=> {
                let element_ty = LLVMType::from(unsafe{LLVMGetElementType(ty)});
                LLVMType::Array(Box::new(element_ty),ty)
            },
            LLVMTypeKind::LLVMDoubleTypeKind=> LLVMType::Double(ty),
            t=>{
                println!("{t:?}");
                todo!()}
        }
    }
}


impl LLVMType{
    pub fn as_llvm_type_ref(&self)->LLVMTypeRef{
        match self {
            LLVMType::Int1(i) => *i,
            LLVMType::Int8(i) => *i,
            LLVMType::Int16(i) => *i,
            LLVMType::Int32(i) => *i,
            LLVMType::Int64(i) => *i,
            LLVMType::Float(i) => *i,
            LLVMType::Double(i) => *i,
            LLVMType::Array(_,i)=>*i,
            LLVMType::Function(_,_,i)=> *i,
            LLVMType::Pointer(_,i)=>*i,
            LLVMType::Unit(i)=>*i,
            LLVMType::Struct(_,i)=>*i,
        }
    }
    pub fn get_element_type(&self)->LLVMType{
        match self {
            LLVMType::Array(e_t,_)=> *e_t.clone(),
            LLVMType::Pointer(e,i)=> {
               *e.clone()
            },
            t=> {
                println!("{t:?}");
                todo!()
            }
        }
    }
    pub fn get_function_param_type(&self,index:usize)->LLVMType{
        match self {
            LLVMType::Function(_,v,f)=>{
                let r = unsafe { LLVMIsFunctionVarArg(*f) };
                if r==1{
                    return LLVMType::Unit(Global::unit_type().as_llvm_type_ref())
                }
                println!("{}",index);
                v.get(index).cloned().unwrap()
            },
            _=>panic!("Not a function")
        }
    }
    pub fn get_struct_field_type(&self,index:usize)->LLVMType{
        match self {
            LLVMType::Struct(v,s)=>{
                v.get(index).cloned().unwrap()
            },
            _=>panic!("Not a struct")
        }
    }
    pub fn is_array(&self)->bool{
        match self {
            LLVMType::Array(_,_) => true,
            _ => false
        }
    }
    pub fn is_struct(&self)->bool{
        match self {
            LLVMType::Struct(_,_) => true,
            _ => false
        }
    }
    pub fn i32()->Self{
        let t = unsafe{LLVMInt32Type()};
        LLVMType::Int32(t)
    }
    pub fn is_pointer(&self)->bool{
        match self {
            LLVMType::Pointer(_,_) => true,
            _ => false
        }
    }
    pub fn is_i32(&self)->bool{
        match self { LLVMType::Int32(_) => true,
            _ => false
        }
    }
}

impl Display for LLVMType{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str =unsafe{
            let c =LLVMPrintTypeToString(self.as_llvm_type_ref());
            let c_str = CStr::from_ptr(c);
            let str_slice = c_str.to_str().expect("Failed to convert C string to Rust string slice");
            str_slice
        };
        write!(f,"{str}")
    }
}