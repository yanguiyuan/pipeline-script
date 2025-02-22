use crate::llvm::module::LLVMModule;
use crate::llvm::types::LLVMType;
use llvm_sys::core::{
    LLVMArrayType2, LLVMConstStringInContext, LLVMContextCreate, LLVMContextDispose,
    LLVMCreateMemoryBufferWithMemoryRangeCopy, LLVMFunctionType, LLVMInt32TypeInContext,
    LLVMInt8TypeInContext, LLVMPointerTypeInContext, LLVMStructCreateNamed, LLVMStructSetBody,
    LLVMVoidTypeInContext,
};
use llvm_sys::execution_engine::LLVMLinkInMCJIT;
use llvm_sys::ir_reader::LLVMParseIRInContext;
use llvm_sys::prelude::{LLVMContextRef, LLVMTypeRef, LLVMValueRef};
use llvm_sys::target::{
    LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget,
};
use std::ffi::{c_char, c_uint, CStr, CString};
use std::ptr;

#[derive(Clone, Debug)]
pub struct LLVMContext {
    llvm_ref: LLVMContextRef,
}
impl Default for LLVMContext {
    fn default() -> Self {
        Self::new()
    }
}

impl LLVMContext {
    pub fn new() -> Self {
        unsafe {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();
            LLVMLinkInMCJIT();
        }
        let ctx = unsafe { LLVMContextCreate() };
        LLVMContext { llvm_ref: ctx }
    }
    pub fn with_jit() -> Self {
        unsafe {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();
            LLVMLinkInMCJIT();
        }
        let ctx = unsafe { LLVMContextCreate() };
        LLVMContext { llvm_ref: ctx }
    }
    pub fn as_ref(&self) -> LLVMContextRef {
        self.llvm_ref
    }
    pub fn parse_ir(&self, llvm_ir: impl AsRef<str>) -> core::result::Result<LLVMModule, String> {
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
        if result == 0 {
            return Ok(LLVMModule::from_raw(module));
        }
        let s = if !(*error_msg).is_null() {
            // 将 C 字符串指针转换为 Rust 字符串
            let error_string = unsafe { CStr::from_ptr(*error_msg) };

            // 将 CStr 转换为 Rust String
            error_string.to_string_lossy().into_owned()
        } else {
            String::new()
        };
        Err(s)
    }
    pub fn create_module(&self, name: impl AsRef<str>) -> LLVMModule {
        LLVMModule::new(name)
    }
    pub fn i32_type(&self) -> LLVMType {
        let t = unsafe { LLVMInt32TypeInContext(self.llvm_ref) };
        LLVMType::Int32(t)
    }
    pub fn array_type(&self, el_ty: LLVMType, n: u64) -> LLVMType {
        let t = unsafe { LLVMArrayType2(el_ty.as_llvm_type_ref(), n) };
        LLVMType::Array(Box::new(el_ty), t)
    }
    pub fn i8_type(&self) -> LLVMType {
        let t = unsafe { LLVMInt8TypeInContext(self.llvm_ref) };
        LLVMType::Int8(t)
    }
    pub fn function_type(&self, return_type: LLVMType, param_types: Vec<LLVMType>) -> LLVMType {
        let mut param_types0 = param_types
            .iter()
            .map(|t| t.as_llvm_type_ref())
            .collect::<Vec<LLVMTypeRef>>();
        let t = unsafe {
            LLVMFunctionType(
                return_type.as_llvm_type_ref(),
                param_types0.as_mut_ptr(),
                param_types.len() as c_uint,
                0,
            )
        };
        LLVMType::Function(Box::new(return_type), param_types, t)
    }
    pub fn function_type_with_var_arg(
        &self,
        return_type: LLVMType,
        param_types: Vec<LLVMType>,
    ) -> LLVMType {
        let mut param_types0 = param_types
            .iter()
            .map(|t| t.as_llvm_type_ref())
            .collect::<Vec<LLVMTypeRef>>();
        let t = unsafe {
            LLVMFunctionType(
                return_type.as_llvm_type_ref(),
                param_types0.as_mut_ptr(),
                param_types.len() as c_uint,
                1,
            )
        };
        LLVMType::Function(Box::new(return_type), param_types, t)
    }
    pub fn unit_type(&self) -> LLVMType {
        let t = unsafe { LLVMVoidTypeInContext(self.llvm_ref) };
        LLVMType::Unit(t)
    }
    pub fn ptr_type(&self, t: LLVMType) -> LLVMType {
        let t0 = unsafe { LLVMPointerTypeInContext(self.llvm_ref, 0) };
        LLVMType::Pointer(Box::new(t), t0)
    }
    pub fn const_string(&self, str: impl AsRef<str>) -> LLVMValueRef {
        let str0 = str.as_ref();
        let str = CString::new(str0).unwrap();
        unsafe { LLVMConstStringInContext(self.llvm_ref, str.as_ptr(), str0.len() as c_uint, 0) }
    }
    pub fn create_named_struct_type(
        &self,
        name: impl AsRef<str>,
        element_type: Vec<LLVMType>,
    ) -> LLVMType {
        let name = name.as_ref();
        let name = CString::new(name).unwrap();
        let mut et = element_type
            .iter()
            .map(|t| t.as_llvm_type_ref())
            .collect::<Vec<LLVMTypeRef>>();
        let t = unsafe { LLVMStructCreateNamed(self.llvm_ref, name.as_ptr()) };
        unsafe {
            LLVMStructSetBody(t, et.as_mut_ptr(), element_type.len() as c_uint, 0);
        }
        LLVMType::Struct(element_type, t)
    }
}

impl Drop for LLVMContext {
    fn drop(&mut self) {
        unsafe { LLVMContextDispose(self.llvm_ref) };
    }
}
