use std::ffi::{c_uint, CString, c_char, CStr};
use std::fmt::Result;
use std::ptr;
use llvm_sys::core::{LLVMArrayType2, LLVMConstString, LLVMFunctionType, LLVMInt32Type, LLVMInt8Type, LLVMPointerType, LLVMVoidType, LLVMContextCreate, LLVMCreateMemoryBufferWithMemoryRangeCopy, LLVMContextDispose};
use llvm_sys::execution_engine::LLVMLinkInMCJIT;
use llvm_sys::ir_reader::LLVMParseIRInContext;
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef, LLVMContextRef};
use llvm_sys::target::{LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget};
use crate::module::{LLVMModule, self};
use crate::types::LLVMType;

#[derive(Clone, Debug)]
pub struct LLVMContext{
     llvm_ref:LLVMContextRef
}

impl LLVMContext{
    pub fn new() ->Self{
        unsafe {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();
            LLVMLinkInMCJIT();
        }
        let ctx = unsafe {
            LLVMContextCreate()
        };
        LLVMContext{llvm_ref:ctx}
    }
    pub fn with_jit() ->Self{
        unsafe {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();
            LLVMLinkInMCJIT();
        }
        let ctx = unsafe {
            LLVMContextCreate()
        };
        LLVMContext{llvm_ref:ctx}
    }
    pub fn as_ref(&self)->LLVMContextRef{
        self.llvm_ref
    }
    pub fn parse_ir(&self,llvm_ir:impl AsRef<str>)->core::result::Result<LLVMModule,String>{
        let llvm_ir = llvm_ir.as_ref();
        let input_data = llvm_ir.as_bytes();
        let input_data_length = input_data.len();
        // 创建 MemoryBuffer
        let buffer_name = CString::new("llvm_ir_buffer").expect("CString::new failed");
        let memory_buffer = unsafe {
            LLVMCreateMemoryBufferWithMemoryRangeCopy(
                input_data.as_ptr() as *const c_char,
                input_data_length,
                buffer_name.as_ptr(),
            )
        };
        // 解析 IR
        let mut module = ptr::null_mut();
        let error_msg = &mut ptr::null_mut();

        let result = unsafe {
            LLVMParseIRInContext(
                self.llvm_ref,
                memory_buffer,
                &mut module as *mut _,
                error_msg,
            )
        };
        if result ==0{
            return Ok(LLVMModule::from_raw(module))
        }
        let s;
        if !(*error_msg).is_null() {
            // 将 C 字符串指针转换为 Rust 字符串
            let error_string = unsafe { CStr::from_ptr(*error_msg) };

            // 将 CStr 转换为 Rust String
            s = error_string.to_string_lossy().into_owned();
        }else{
            s = String::new();
        }
        Err(s)
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

impl Drop for LLVMContext {
    fn drop(&mut self) {
        unsafe { LLVMContextDispose(self.llvm_ref) };
    }
}