use crate::llvm::function::Function;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;
use std::cell::RefCell;
use llvm_sys::core::{
    LLVMArrayType2, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr,
    LLVMBuildExtractValue, LLVMBuildFAdd, LLVMBuildGEP2, LLVMBuildGlobalString,
    LLVMBuildGlobalStringPtr, LLVMBuildICmp, LLVMBuildInsertValue, LLVMBuildLoad2, LLVMBuildMul,
    LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildStore, LLVMBuildStructGEP2,
    LLVMBuildSub, LLVMBuildZExt, LLVMDisposeBuilder, LLVMPositionBuilderAtEnd,
};
use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMBuilderRef, LLVMValueRef};
use llvm_sys::LLVMIntPredicate::{LLVMIntEQ, LLVMIntNE, LLVMIntSGT, LLVMIntSLT};
use std::collections::HashMap;
use std::ffi::{c_uint, CString};

#[derive(Clone, Debug)]
pub struct Builder {
    inner: LLVMBuilderRef,
    strings: RefCell<HashMap<String, LLVMValue>>,
    // function_map:HashMap<String,Function>,
    // symbol_table:RefCell<HashMap<String,(LLVMType,LLVMValue)>>,
    // current_function:Function
}
unsafe impl Send for Builder {}
unsafe impl Sync for Builder {}
impl Builder {
    pub(crate) fn new(builder: LLVMBuilderRef) -> Self {
        Self { inner: builder ,strings:HashMap::new().into()}
    }
    pub fn build_return_void(&self) {
        unsafe {
            LLVMBuildRetVoid(self.inner);
        }
    }
    pub fn build_return(&self, value: LLVMValue) {
        if value.is_unit() {
            self.build_return_void();
            return;
        }
        unsafe {
            LLVMBuildRet(self.inner, value.as_llvm_value_ref());
        }
    }

    pub fn build_global_string_ptr(
        &self,
        name: impl AsRef<str>,
        value: impl AsRef<str>,
    ) -> LLVMValue {
        let name = name.as_ref();
        let name = CString::new(name).unwrap();
        let str = value.as_ref();
        let str = CString::new(str).unwrap();
        let r = unsafe { LLVMBuildGlobalStringPtr(self.inner, str.as_ptr(), name.as_ptr()) };
        r.into()
    }
    pub fn build_global_string(&self, name: impl AsRef<str>, value: impl AsRef<str>) -> LLVMValue {
        let name0 = name.as_ref();
        if self.strings.borrow().contains_key(name0){
            return self.strings.borrow().get(name0).unwrap().clone();
        }
        let name = CString::new(name0).unwrap();
        let str = value.as_ref();
        let str = CString::new(str).unwrap();
        let r = unsafe { LLVMBuildGlobalString(self.inner, str.as_ptr(), name.as_ptr()) };
        self.strings.borrow_mut().insert(name0.to_string(), r.into());
        r.into()
    }
    pub fn build_alloca(&self, name: impl AsRef<str>, ty: &LLVMType) -> LLVMValue {
        let name0 = name.as_ref();
        let name = CString::new(name0).unwrap();
        let local = unsafe { LLVMBuildAlloca(self.inner, ty.as_llvm_type_ref(), name.as_ptr()) };
        local.into()
    }

    pub fn build_array(&self, el_ty: LLVMType, arr: Vec<LLVMValue>) -> LLVMValue {
        let arr_ty = unsafe { LLVMArrayType2(el_ty.as_llvm_type_ref(), arr.len() as u64) };
        let name = CString::new("").unwrap();
        let arr0 = unsafe { LLVMBuildAlloca(self.inner, arr_ty, name.as_ptr()) };
        for i in 0..arr.len() {
            let index = Global::const_i32(i as i32);
            let value = arr[i].clone();
            let gep = unsafe {
                LLVMBuildGEP2(
                    self.inner,
                    el_ty.as_llvm_type_ref(),
                    arr0,
                    &mut index.as_llvm_value_ref() as *mut _,
                    1,
                    name.as_ptr(),
                )
            };
            unsafe { LLVMBuildStore(self.inner, value.as_llvm_value_ref(), gep) };
        }
        arr0.into()
    }
    pub fn build_struct_get(&self, val: LLVMValue, idx: usize) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            let v = LLVMBuildExtractValue(
                self.inner,
                val.as_llvm_value_ref(),
                idx as c_uint,
                name.as_ptr(),
            );
            v
        }
        .into()
    }
    pub fn build_struct_insert(&self, val: LLVMValue, idx: usize, value: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildInsertValue(
                self.inner,
                val.as_llvm_value_ref(),
                value.as_llvm_value_ref(),
                idx as c_uint,
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_struct_gep(&self, ty: LLVMType, val: LLVMValue, idx: usize) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildStructGEP2(
                self.inner,
                ty.as_llvm_type_ref(),
                val.as_llvm_value_ref(),
                idx as c_uint,
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_store(&self, ptr: LLVMValue, val: LLVMValue) {
        unsafe { LLVMBuildStore(self.inner, val.as_llvm_value_ref(), ptr.as_llvm_value_ref()) };
    }
    pub fn build_load(&self, ty: LLVMType, ptr: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildLoad2(
                self.inner,
                ty.as_llvm_type_ref(),
                ptr.as_llvm_value_ref(),
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_array_get(&self, ty: LLVMType, ptr: LLVMValue, index: LLVMValue) -> LLVMValue {
        // 获取ptr所指数组的类型
        let name = CString::new("").unwrap();
        let mut indices = [index.as_llvm_value_ref()];
        let r = unsafe {
            let ptr0 = LLVMBuildGEP2(
                self.inner,
                ty.as_llvm_type_ref(),
                ptr.as_llvm_value_ref(),
                indices.as_mut_ptr(),
                1,
                name.as_ptr(),
            );
            LLVMBuildLoad2(self.inner, ty.as_llvm_type_ref(), ptr0, name.as_ptr())
        };
        r.into()
    }
    pub fn build_array_gep(&self, ty: LLVMType, ptr: LLVMValue, index: LLVMValue) -> LLVMValue {
        // 获取ptr所指数组的类型
        let name = CString::new("").unwrap();
        let mut indices = [index.as_llvm_value_ref()];
        let r = unsafe {
            LLVMBuildGEP2(
                self.inner,
                ty.as_llvm_type_ref(),
                ptr.as_llvm_value_ref(),
                indices.as_mut_ptr(),
                1,
                name.as_ptr(),
            )
        };
        r.into()
    }
    pub fn position_at_end(&self, block: LLVMBasicBlockRef) {
        unsafe { LLVMPositionBuilderAtEnd(self.inner, block) };
    }
    pub fn build_br(&self, block: LLVMBasicBlockRef) {
        unsafe { LLVMBuildBr(self.inner, block) };
    }
    pub fn build_cond_br(
        &self,
        condition: LLVMValue,
        then_block: LLVMBasicBlockRef,
        else_block: LLVMBasicBlockRef,
    ) {
        unsafe {
            LLVMBuildCondBr(
                self.inner,
                condition.as_llvm_value_ref(),
                then_block,
                else_block,
            );
        }
    }

    pub fn build_add(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        let r = unsafe {
            LLVMBuildAdd(
                self.inner,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        };
        r.into()
    }
    pub fn build_fadd(&self, l: LLVMValue, r: LLVMValue) -> LLVMValueRef {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildFAdd(
                self.inner,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        }
    }
    pub fn build_mul(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        let r = unsafe {
            LLVMBuildMul(
                self.inner,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        };
        r.into()
    }
    pub fn build_sub(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        let r = unsafe {
            LLVMBuildSub(
                self.inner,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        };
        r.into()
    }
    pub fn build_div(&self, l: LLVMValueRef, r: LLVMValueRef) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe { LLVMBuildSDiv(self.inner, l, r, name.as_ptr()) }.into()
    }
    pub fn build_eq(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildICmp(
                self.inner,
                LLVMIntEQ,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_neq(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildICmp(
                self.inner,
                LLVMIntNE,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_greater(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildICmp(
                self.inner,
                LLVMIntSGT,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_less(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildICmp(
                self.inner,
                LLVMIntSLT,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_call(
        &self,
        function: &Function,
        params: &mut [LLVMValue],
        var_name: impl AsRef<str>,
    ) -> LLVMValue {
        let dec = function.get_declaration().as_llvm_type_ref();
        let var_name = CString::new(var_name.as_ref()).unwrap();
        let mut params = params
            .iter()
            .map(|e| e.as_llvm_value_ref())
            .collect::<Vec<LLVMValueRef>>();
        let call_res = unsafe {
            LLVMBuildCall2(
                self.inner,
                dec,
                function.get_function_ref(),
                params.as_mut_ptr(),
                params.len() as c_uint,
                var_name.as_ptr(),
            )
        };
        call_res.into()
    }
    pub fn build_call_fn_ptr(
        &self,
        fn_type: LLVMType,
        ptr: LLVMValue,
        params: &mut [LLVMValue],
        var_name: impl AsRef<str>,
    ) -> LLVMValue {
        let name = CString::new(var_name.as_ref()).unwrap();
        let mut params = params
            .iter()
            .map(|e| e.as_llvm_value_ref())
            .collect::<Vec<LLVMValueRef>>();
        unsafe {
            LLVMBuildCall2(
                self.inner,
                fn_type.as_llvm_type_ref(),
                ptr.as_llvm_value_ref(),
                params.as_mut_ptr(),
                params.len() as c_uint,
                name.as_ptr(),
            )
        }
        .into()
    }
    pub fn build_zext(&self, value: LLVMValue, ty: LLVMType) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildZExt(
                self.inner,
                value.as_llvm_value_ref(),
                ty.as_llvm_type_ref(),
                name.as_ptr(),
            )
        }
        .into()
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe { LLVMDisposeBuilder(self.inner) };
    }
}
