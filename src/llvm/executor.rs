use crate::llvm::function::Function;
use llvm_sys::core::LLVMGetNamedFunction;
use llvm_sys::execution_engine::{
    LLVMAddGlobalMapping, LLVMExecutionEngineRef, LLVMGenericValueRef, LLVMRunFunction,
};
use llvm_sys::prelude::{LLVMModuleRef, LLVMValueRef};
use std::collections::HashMap;
use std::ffi::{c_uint, c_void, CString};
use std::sync::Arc;

pub struct JITExecutor {
    inner: LLVMExecutionEngineRef,
    module: Arc<LLVMModuleRef>,
    function_map: HashMap<String, Function>,
}
impl JITExecutor {
    pub(crate) fn new(
        module: Arc<LLVMModuleRef>,
        executor: LLVMExecutionEngineRef,
        func_map: HashMap<String, Function>,
    ) -> Self {
        Self {
            module,
            inner: executor,
            function_map: func_map,
        }
    }
    pub fn add_global_mapping(&self, value: LLVMValueRef, address: *mut c_void) {
        unsafe {
            LLVMAddGlobalMapping(self.inner, value, address);
        }
    }
    pub fn run_function(
        &self,
        name: impl AsRef<str>,
        args: &mut [LLVMGenericValueRef],
    ) -> LLVMGenericValueRef {
        let f0 = self.function_map.get(name.as_ref());
        let f;
        if f0.is_none() {
            let name = CString::new(name.as_ref()).unwrap();
            f = unsafe { LLVMGetNamedFunction(*self.module.as_ref(), name.as_ptr()) };
        } else {
            f = f0.unwrap().get_function_ref();
        }
        let result =
            unsafe { LLVMRunFunction(self.inner, f, args.len() as c_uint, args.as_mut_ptr()) };
        result
    }
}
