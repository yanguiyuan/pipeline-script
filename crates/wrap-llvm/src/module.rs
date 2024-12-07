use crate::executor::JITExecutor;
use crate::function::Function;
use crate::global::Global;
use crate::types::LLVMType;
use crate::value::LLVMValue;
use llvm_sys::core::{
    LLVMAddFunction, LLVMAddGlobal, LLVMDisposeModule, LLVMDumpModule, LLVMGetNamedFunction,
    LLVMModuleCreateWithName, LLVMSetInitializer, LLVMTypeOf,
};
use llvm_sys::execution_engine::{LLVMCreateJITCompilerForModule, LLVMExecutionEngineRef};
use llvm_sys::prelude::{LLVMModuleRef, LLVMValueRef};
use std::collections::HashMap;
use std::ffi::CString;
use std::ptr;
use std::sync::Arc;

pub struct LLVMModule {
    module_ref: Arc<LLVMModuleRef>,
    function_map: HashMap<String, Function>,
    struct_map: HashMap<String, (HashMap<String, usize>, LLVMType)>,
    global_map: HashMap<String, (LLVMType, LLVMValue)>,
}

impl LLVMModule {
    pub(crate) fn new(name: impl AsRef<str>) -> Self {
        let name = name.as_ref();
        let name = CString::new(name).unwrap();
        let module_ref = unsafe { LLVMModuleCreateWithName(name.as_ptr()) };
        LLVMModule {
            module_ref: Arc::new(module_ref),
            function_map: HashMap::new(),
            global_map: HashMap::new(),
            struct_map: HashMap::new(),
        }
    }
    pub fn from_raw(module_ref: LLVMModuleRef) -> Self {
        LLVMModule {
            module_ref: Arc::new(module_ref),
            function_map: HashMap::new(),
            global_map: HashMap::new(),
            struct_map: HashMap::new(),
        }
    }
    pub fn register_struct(
        &mut self,
        name: impl AsRef<str>,
        field_map: HashMap<String, usize>,
        t: LLVMType,
    ) {
        self.struct_map.insert(name.as_ref().into(), (field_map, t));
    }
    pub fn register_extern_function(
        &mut self,
        name: impl AsRef<str>,
        function_type: LLVMType,
    ) -> Function {
        let module = self.module_ref.as_ref();
        let name0 = CString::new(name.as_ref()).unwrap();
        let f =
            unsafe { LLVMAddFunction(*module, name0.as_ptr(), function_type.as_llvm_type_ref()) };
        let f = Function::new(function_type, f, vec![]);
        self.function_map.insert(name.as_ref().into(), f.clone());
        f
    }
    pub fn register_function(
        &mut self,
        name: impl AsRef<str>,
        function_type: LLVMType,
        param_names: Vec<String>,
    ) -> Function {
        let module = self.module_ref.as_ref();
        let name0 = CString::new(name.as_ref()).unwrap();
        let f =
            unsafe { LLVMAddFunction(*module, name0.as_ptr(), function_type.as_llvm_type_ref()) };
        let f = Function::new(function_type.clone(), f, param_names.clone());
        self.function_map.insert(name.as_ref().into(), f.clone());
        f
    }
    pub fn add_global(&mut self, name: impl AsRef<str>, v: LLVMValue) {
        let module = self.module_ref.as_ref();
        let name0 = name.as_ref();
        let name1 = CString::new(name0).unwrap();
        let v = v.as_llvm_value_ref();
        let ty = unsafe { LLVMTypeOf(v) };
        let global_val = unsafe {
            let global_val = LLVMAddGlobal(*module, ty, name1.as_ptr());
            LLVMSetInitializer(global_val, v);
            global_val
        };
        self.global_map
            .insert(name.as_ref().into(), (ty.into(), global_val.into()));
    }

    pub fn create_executor(&self) -> Result<JITExecutor, String> {
        let mut engine: LLVMExecutionEngineRef = ptr::null_mut();
        let mut error: *mut i8 = ptr::null_mut();
        let module = self.module_ref.as_ref();
        unsafe {
            if LLVMCreateJITCompilerForModule(&mut engine, *module, 0, &mut error) != 0 {
                let error_msg = CString::from_raw(error).into_string().unwrap();
                let e = format!("Error creating JIT: {}", error_msg);
                return Err(e);
            }
        }

        Ok(JITExecutor::new(
            self.module_ref.clone(),
            engine,
            self.function_map.clone(),
        ))
    }
    pub fn get_function(&self, name: impl AsRef<str>) -> Option<&Function> {
        self.function_map.get(name.as_ref())
    }
    pub fn get_function_ref(&self, name: impl AsRef<str>) -> LLVMValueRef {
        let name = CString::new(name.as_ref()).unwrap();
        let f = unsafe { LLVMGetNamedFunction(*self.module_ref.as_ref(), name.as_ptr()) };
        f
    }
    pub fn get_struct(&self, name: impl AsRef<str>) -> Option<&(HashMap<String, usize>, LLVMType)> {
        self.struct_map.get(name.as_ref())
    }
    pub fn get_struct_by_type(
        &self,
        t: &LLVMType,
    ) -> Option<(&String, &HashMap<String, usize>, &LLVMType)> {
        for (name, (map, ty)) in self.struct_map.iter() {
            // if &Global::pointer_type(ty.clone()) == t{
            //     return Some((name,map,ty));
            // }
            if ty == t {
                return Some((name, map, ty));
            }
        }
        None
    }
    pub fn get_struct_by_pointer_type(
        &self,
        t: &LLVMType,
    ) -> Option<(&String, &HashMap<String, usize>, &LLVMType)> {
        dbg!(&self.struct_map);
        for (name, (map, ty)) in self.struct_map.iter() {
            dbg!(name, map, ty, Global::pointer_type(ty.clone()));
            if &Global::pointer_type(ty.clone()) == t {
                return Some((name, map, ty));
            }
        }
        None
    }
    pub fn dump(&self) {
        let module = self.module_ref.as_ref();
        if module.is_null() {
            return;
        }
        unsafe { LLVMDumpModule(*module) }
    }
}

impl Drop for LLVMModule {
    fn drop(&mut self) {
        if Arc::strong_count(&self.module_ref) == 1 {
            // 只有一个引用，安全释放 LLVMModuleRef
            unsafe {
                // 这里需要调用相应的 LLVM API 函数释放 LLVMModuleRef
                // 例如：LLVMDisposeModule
                // LLVMDisposeModule(self.module);
                // 释放 Arc 的引用
                // 注意：Arc 会自动处理引用计数和内存释放
                let module_ptr = Arc::get_mut(&mut self.module_ref).unwrap();
                if !module_ptr.is_null() {
                    // 释放模块
                    LLVMDisposeModule(*module_ptr);
                }
            }
        }
    }
}
