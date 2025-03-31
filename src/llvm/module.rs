use crate::llvm::executor::JITExecutor;
use crate::llvm::function::Function;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
use llvm_sys::core::{
    LLVMAddFunction, LLVMAddGlobal, LLVMDisposeMemoryBuffer, LLVMDisposeMessage, LLVMDisposeModule,
    LLVMDumpModule, LLVMGetBufferSize, LLVMGetBufferStart, LLVMGetNamedFunction,
    LLVMModuleCreateWithName, LLVMSetInitializer, LLVMSetTarget, LLVMTypeOf,
};
use llvm_sys::execution_engine::{LLVMCreateJITCompilerForModule, LLVMExecutionEngineRef};
use llvm_sys::prelude::{LLVMModuleRef, LLVMValueRef};
use llvm_sys::target_machine::{
    LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine,
    LLVMDisposeTargetMachine, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTarget,
    LLVMTargetMachineEmitToMemoryBuffer,
};
use llvm_sys::LLVMMemoryBuffer;
use std::collections::HashMap;
use std::ffi::CString;
use std::ptr;
use std::rc::Rc;

#[derive(Debug)]
pub struct LLVMModule {
    module_ref: Rc<LLVMModuleRef>,
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
            module_ref: Rc::new(module_ref),
            function_map: HashMap::new(),
            global_map: HashMap::new(),
            struct_map: HashMap::new(),
        }
    }
    pub fn from_raw(module_ref: LLVMModuleRef) -> Self {
        LLVMModule {
            module_ref: Rc::new(module_ref),
            function_map: HashMap::new(),
            global_map: HashMap::new(),
            struct_map: HashMap::new(),
        }
    }
    pub fn to_assembly(&self, target_triple: &str) -> Result<String, String> {
        fn compile_to_assembly(
            module: *mut llvm_sys::LLVMModule,
            target_triple: &str,
        ) -> Result<String, String> {
            unsafe {
                // 2. 创建目标机器
                let mut target: *mut LLVMTarget = std::ptr::null_mut();
                let mut error_msg = std::ptr::null_mut();
                let target_triple_cstr = std::ffi::CString::new(target_triple).unwrap();

                if LLVMGetTargetFromTriple(target_triple_cstr.as_ptr(), &mut target, &mut error_msg)
                    != 0
                {
                    return Err(std::ffi::CStr::from_ptr(error_msg)
                        .to_string_lossy()
                        .into_owned());
                }
                if target.is_null() {
                    return Err("Failed to get target".to_string());
                }
                let cpu = CString::new("generic").unwrap();
                let features = CString::new("").unwrap();
                let opt_level = LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;

                let target_machine = LLVMCreateTargetMachine(
                    target,
                    target_triple_cstr.as_ptr(),
                    cpu.as_ptr(),
                    features.as_ptr(),
                    opt_level,
                    LLVMRelocMode::LLVMRelocDefault,
                    LLVMCodeModel::LLVMCodeModelDefault,
                );

                if target_machine.is_null() {
                    return Err("Failed to create target machine".to_string());
                }
                // 3. 配置模块目标
                LLVMSetTarget(module, target_triple_cstr.as_ptr());
                // 4. 创建内存缓冲输出
                let mut output_buffer: *mut LLVMMemoryBuffer = std::ptr::null_mut();
                let output_type = LLVMCodeGenFileType::LLVMAssemblyFile;

                let result = LLVMTargetMachineEmitToMemoryBuffer(
                    target_machine,
                    module,
                    output_type,
                    &mut error_msg,
                    &mut output_buffer,
                );

                if result != 0 {
                    return Err(std::ffi::CStr::from_ptr(error_msg)
                        .to_string_lossy()
                        .into_owned());
                }

                // 5. 提取汇编代码
                let data_ptr = LLVMGetBufferStart(output_buffer);
                let data_size = LLVMGetBufferSize(output_buffer);
                let assembly = std::slice::from_raw_parts(data_ptr as *const u8, data_size);
                let assembly_str = std::str::from_utf8(assembly).unwrap().to_owned();

                // 清理资源
                LLVMDisposeMemoryBuffer(output_buffer);
                LLVMDisposeTargetMachine(target_machine);

                Ok(assembly_str)
            }
        }
        compile_to_assembly(*self.module_ref, target_triple)
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
    pub fn verify_with_debug_info(&self) {
        unsafe {
            // 即时打印错误到控制台
            LLVMVerifyModule(
                *self.module_ref,
                LLVMVerifierFailureAction::LLVMPrintMessageAction,
                std::ptr::null_mut(),
            );

            // 同时获取错误信息副本
            let mut error_copy = std::ptr::null_mut();
            let status = LLVMVerifyModule(
                *self.module_ref,
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                &mut error_copy,
            );

            if status != 0 {
                // let error_str = CStr::from_ptr(error_copy).to_string_lossy();
                // let debug_str =error_str.to_string();
                // println!("Additional error details: {}", debug_str);
                LLVMDisposeMessage(error_copy);
            }
        }
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
        if Rc::strong_count(&self.module_ref) == 1 {
            // 只有一个引用，安全释放 LLVMModuleRef
            unsafe {
                // 这里需要调用相应的 LLVM API 函数释放 LLVMModuleRef
                // 例如：LLVMDisposeModule
                // LLVMDisposeModule(self.module);
                // 释放 Arc 的引用
                // 注意：Arc 会自动处理引用计数和内存释放
                let module_ptr = Rc::get_mut(&mut self.module_ref).unwrap();
                if !module_ptr.is_null() {
                    // 释放模块
                    LLVMDisposeModule(*module_ptr);
                }
            }
        }
    }
}
