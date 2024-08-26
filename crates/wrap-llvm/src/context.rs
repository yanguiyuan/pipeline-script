use std::ffi::{c_uint, CString};
use llvm_sys::core::{LLVMArrayType2, LLVMConstString, LLVMFunctionType, LLVMInt32Type, LLVMInt8Type, LLVMPointerType, LLVMVoidType};
use llvm_sys::execution_engine::LLVMLinkInMCJIT;
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use llvm_sys::target::{LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget};
use crate::module::LLVMModule;
use crate::types::LLVMType;

#[derive(Clone, Debug)]
pub struct LLVMJITContext{
}

impl LLVMJITContext{
    pub fn new() ->Self{
        unsafe {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();
            LLVMLinkInMCJIT();
        }
        LLVMJITContext{}
    }
    pub fn create_module(&self,name:impl AsRef<str>)->LLVMModule{
        LLVMModule::new(name)
    }
    pub fn i32_type(&self)->LLVMType{
        let t = unsafe { LLVMInt32Type() };
        LLVMType::Int32(t)
    }
    pub fn array_type(&self,el_ty:LLVMType,n:u64)->LLVMType{
        let t = unsafe{LLVMArrayType2(el_ty.as_llvm_type_ref(),n)};
        LLVMType::Array(Box::new(el_ty),t)
    }
    pub fn i8_type(&self)->LLVMType{
        let t = unsafe { LLVMInt8Type() };
        LLVMType::Int8(t)
    }
    pub fn function_type(&self,return_type:LLVMType,param_types:Vec<LLVMType>)->LLVMType{
        let mut param_types0 = param_types.iter().map(|t|t.as_llvm_type_ref()).collect::<Vec<LLVMTypeRef>>();
        let t = unsafe{LLVMFunctionType(return_type.as_llvm_type_ref(), param_types0.as_mut_ptr(), param_types.len() as c_uint, 0) };
        LLVMType::Function(Box::new(return_type),param_types,t)
    }
    pub fn function_type_with_var_arg(&self,return_type:LLVMType,param_types:Vec<LLVMType>)->LLVMType{
        let mut param_types0 = param_types.iter().map(|t|t.as_llvm_type_ref()).collect::<Vec<LLVMTypeRef>>();
        let t = unsafe{LLVMFunctionType(return_type.as_llvm_type_ref(), param_types0.as_mut_ptr(), param_types.len() as c_uint, 1) };
        LLVMType::Function(Box::new(return_type),param_types,t)
    }
    pub fn unit_type(&self)->LLVMType{
        let t = unsafe{
            LLVMVoidType()
        };
        LLVMType::Unit(t)
    }
    pub fn ptr_type(&self,t:LLVMType)->LLVMType{
        let t0 =unsafe { LLVMPointerType(t.as_llvm_type_ref(), 0) };
        LLVMType::Pointer(Box::new(t),t0)
    }
    pub fn const_string(&self,str:impl AsRef<str>)->LLVMValueRef{
        let str0 = str.as_ref();
        let str = CString::new(str0).unwrap();
        let c =unsafe { LLVMConstString(str.as_ptr(), str0.len() as c_uint,0) };
        c
    }
}