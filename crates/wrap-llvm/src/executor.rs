use std::collections::HashMap;
use std::ffi::{c_uint, c_void};
use llvm_sys::execution_engine::{LLVMAddGlobalMapping, LLVMExecutionEngineRef, LLVMGenericValueRef, LLVMRunFunction};
use llvm_sys::prelude::LLVMValueRef;
use crate::function::Function;
use crate::value::LLVMValue;

pub struct JITExecutor{
    inner:LLVMExecutionEngineRef,
    function_map:HashMap<String,Function>
}
impl JITExecutor {
    pub(crate) fn new(executor:LLVMExecutionEngineRef,func_map:HashMap<String,Function>)->Self{
        Self{inner:executor,function_map:func_map}
    }
    pub fn add_global_mapping(&self, value:LLVMValueRef,address:*mut c_void){
        unsafe { LLVMAddGlobalMapping(self.inner, value, address); }
    }
    pub fn run_function(&self, name:impl AsRef<str>, args: &mut [LLVMGenericValueRef])->LLVMGenericValueRef{
        let f = self.function_map.get(name.as_ref()).unwrap();
        let result =  unsafe { LLVMRunFunction(self.inner, f.get_function_ref(), args.len() as c_uint, args.as_mut_ptr()) };
        result
    }
}