use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::pointer::PointerValue;
use crate::llvm::value::pstruct::StructValue;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::{LLVMAppendBasicBlock, LLVMGetParam};
use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use std::collections::HashMap;
use std::ffi::c_char;

#[derive(Clone, Debug)]
pub struct FunctionValue {
    reference: LLVMValueRef,
    name: String,
    return_type: Box<LLVMValue>,
    args: Vec<(String, LLVMValue)>,
    is_closure: bool,
}

impl FunctionValue {
    pub fn new(
        reference: LLVMValueRef,
        name: String,
        return_type: Box<LLVMValue>,
        args: Vec<(String, LLVMValue)>,
    ) -> Self {
        Self {
            reference,
            name,
            return_type,
            args,
            is_closure: false,
        }
    }
    pub fn get_reference(&self) -> LLVMValueRef {
        self.reference
    }

    pub fn append_basic_block(&self, name: impl Into<String>) -> LLVMBasicBlockRef {
        unsafe { LLVMAppendBasicBlock(self.reference, name.into().as_mut_ptr() as *mut c_char) }
    }
    pub fn get_param(&self, name: impl AsRef<str>) -> Option<LLVMValue> {
        let name_ref = name.as_ref();
        for (index, (arg_name, v)) in self.args.iter().enumerate() {
            if arg_name == name_ref {
                let param = unsafe { LLVMGetParam(self.reference, index as u32) };
                return match v {
                    LLVMValue::Struct(v) => {
                        return Some(LLVMValue::Struct(StructValue::new(
                            param,
                            v.get_name(),
                            v.field_index.clone(),
                            v.field_values.borrow().clone(),
                        )))
                    }
                    LLVMValue::String(_) => Some(LLVMValue::String(param)),
                    LLVMValue::Function(function_value) => {
                        let mut function_value = function_value.clone();
                        function_value.set_reference(param);
                        Some(LLVMValue::Function(function_value))
                    }
                    LLVMValue::Pointer(p) => {
                        let mut p = p.clone();
                        p.set_reference(param);
                        Some(LLVMValue::Pointer(p))
                    }
                    LLVMValue::Reference(r) => {
                        let mut r = r.clone();
                        r.set_reference(param);
                        Some(LLVMValue::Reference(r))
                    }
                    _ => Some(param.into()),
                };
            }
        }
        None
    }
    pub fn set_reference(&mut self, reference: LLVMValueRef) {
        self.reference = reference;
    }
    pub fn get_param_index(&self, name: impl AsRef<str>) -> Option<usize> {
        let name_ref = name.as_ref();
        for (i, (arg_name, _)) in self.args.iter().enumerate() {
            if arg_name == name_ref {
                return Some(i);
            }
        }
        None
    }
    pub fn get_llvm_type(&self, ctx: &Context) -> LLVMType {
        let mut args_type = vec![];
        for (name, v) in &self.args {
            args_type.push((name.clone(), v.get_llvm_type(ctx)))
        }
        let return_type = self.return_type.get_llvm_type(ctx);
        Global::function_type(return_type, args_type)
    }
    pub fn get_param_index_map(&self) -> HashMap<String, usize> {
        let mut map = HashMap::new();
        for (i, (name, _)) in self.args.iter().enumerate() {
            map.insert(name.clone(), i);
        }
        map
    }
    pub fn call(&self, ctx: &Context, args: Vec<Option<LLVMValue>>) -> LLVMValue {
        let builder = ctx.get_builder();
        let mut function_call_args = vec![];

        let r = if self.is_closure {
            // 对于闭包，self.reference 就是结构体值
            let mut field_index = HashMap::new();
            field_index.insert("ptr".to_string(), 0);
            field_index.insert("env".to_string(), 1);

            let closure_struct = StructValue::new(
                self.reference,
                "Closure".into(),
                field_index,
                vec![
                    LLVMValue::Function(self.clone()),
                    LLVMValue::Pointer(PointerValue::new(self.reference, LLVMValue::Unit)),
                ],
            );

            let function_ptr = builder.build_extract_value(ctx, &closure_struct, 0);
            let env_ptr = builder.build_extract_value(ctx, &closure_struct, 1);

            // 处理其他参数
            for i in args.into_iter().flatten() {
                function_call_args.push(i)
            }
            // 将环境指针作为第最后一个参数
            function_call_args.push(env_ptr);
            // 调用闭包函数
            let function_value = function_ptr.as_function().unwrap();
            builder.build_call(ctx, &function_value, &mut function_call_args, "")
        } else {
            // 处理普通函数调用
            for i in args.into_iter().flatten() {
                function_call_args.push(i)
            }
            builder.build_call(ctx, self, &mut function_call_args, "")
        };
        if self.name == "panic" || self.name == "exit" {
            builder.build_unreachable();
        }
        r
    }
    pub fn get_return_value(&self) -> LLVMValue {
        *self.return_type.clone()
    }
    pub fn get_args_count(&self) -> usize {
        self.args.len()
    }
    pub fn get_arg_name(&self, index: usize) -> Option<String> {
        self.args.get(index).map(|(name, _)| name.clone())
    }
    pub fn get_param_by_index(&self, index: usize) -> Option<LLVMValue> {
        self.args.get(index).map(|(_, ty)| ty.clone())
    }
    pub fn is_arg_undef(&self, index: usize) -> bool {
        if let Some((_, arg_value)) = self.args.get(index) {
            arg_value.is_undef()
        } else {
            false
        }
    }
    pub fn set_closure(&mut self) {
        self.is_closure = true;
    }
    pub fn is_closure(&self) -> bool {
        self.is_closure
    }
}

impl From<FunctionValue> for LLVMValue {
    fn from(value: FunctionValue) -> Self {
        LLVMValue::Function(value)
    }
}
