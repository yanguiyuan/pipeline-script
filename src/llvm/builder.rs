use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::llvm::value::array::ArrayValue;
use crate::llvm::value::bool::BoolValue;
use crate::llvm::value::fucntion::FunctionValue;
use crate::llvm::value::pointer::PointerValue;
use crate::llvm::value::pstruct::StructValue;
use crate::llvm::value::reference::ReferenceValue;
use crate::llvm::value::LLVMValue;
use llvm_sys::core::{
    LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildArrayAlloca, LLVMBuildBr, LLVMBuildCall2,
    LLVMBuildCondBr, LLVMBuildExtractValue, LLVMBuildFAdd, LLVMBuildGEP2, LLVMBuildGlobalString,
    LLVMBuildGlobalStringPtr, LLVMBuildICmp, LLVMBuildInBoundsGEP2, LLVMBuildInsertValue,
    LLVMBuildLoad2, LLVMBuildMul, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildSDiv, LLVMBuildStore,
    LLVMBuildStructGEP2, LLVMBuildSub, LLVMBuildUnreachable, LLVMBuildZExt, LLVMConstArray2,
    LLVMConstIntToPtr, LLVMDisposeBuilder, LLVMGetArrayLength2, LLVMInt8Type, LLVMPointerType,
    LLVMPositionBuilderAtEnd,
};
use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMBuilderRef, LLVMValueRef};
use llvm_sys::LLVMIntPredicate::{LLVMIntEQ, LLVMIntNE, LLVMIntSGT, LLVMIntSLT};
use std::cell::RefCell;
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
        Self {
            inner: builder,
            strings: HashMap::new().into(),
        }
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
        let name = CString::new(name0).unwrap();
        let str0 = value.as_ref();
        if self.strings.borrow().contains_key(str0) {
            return self.strings.borrow().get(str0).unwrap().clone();
        }
        let str = CString::new(str0).unwrap();
        let r = unsafe { LLVMBuildGlobalString(self.inner, str.as_ptr(), name.as_ptr()) };
        let s = LLVMValue::String(r);
        self.strings
            .borrow_mut()
            .insert(str0.to_string(), s.clone());
        s
    }
    pub fn build_alloca(&self, name: impl AsRef<str>, ty: &LLVMType) -> LLVMValue {
        let name0 = name.as_ref();
        let name = CString::new(name0).unwrap();
        let local = unsafe { LLVMBuildAlloca(self.inner, ty.as_llvm_type_ref(), name.as_ptr()) };
        LLVMValue::Reference(ReferenceValue::new(local, ty.get_undef()))
    }

    pub fn build_array(&self, el_ty: LLVMType, arr: Vec<LLVMValue>) -> LLVMValue {
        let name = CString::new("").unwrap();
        let array_address = unsafe {
            LLVMBuildArrayAlloca(
                self.inner,
                el_ty.as_llvm_type_ref(),
                Global::const_i64(arr.len() as i64).get_reference(),
                name.as_ptr(),
            )
        };
        let mut constant_vals = arr
            .iter()
            .map(|v| v.as_llvm_value_ref())
            .collect::<Vec<_>>();
        let constant_array = unsafe {
            LLVMConstArray2(
                el_ty.as_llvm_type_ref(),
                constant_vals.as_mut_ptr(),
                arr.len() as u64,
            )
        };
        unsafe {
            LLVMBuildStore(self.inner, constant_array, array_address);
        }
        LLVMValue::Array(ArrayValue::new(array_address, el_ty, arr.len()))
    }
    pub fn build_struct_get(&self, val: &StructValue, idx: usize) -> LLVMValue {
        let name = CString::new("").unwrap();
        let r = unsafe {
            LLVMBuildExtractValue(
                self.inner,
                val.get_reference(),
                idx as c_uint,
                name.as_ptr(),
            )
        };
        let field_type = val.simply_get_field_by_index(idx).unwrap();
        match field_type {
            LLVMValue::String(_) => LLVMValue::String(r),
            LLVMValue::Struct(mut v) => {
                v.set_reference(r);
                v.into()
            }
            LLVMValue::Pointer(pointer_value) => {
                let mut pointer_value = pointer_value.clone();
                pointer_value.set_reference(r);
                pointer_value.into()
            }
            LLVMValue::Function(f) => {
                let mut function_value = f.clone();
                function_value.set_reference(r);
                function_value.into()
            }
            LLVMValue::Reference(reference_value) => {
                let mut reference_value = reference_value.clone();
                reference_value.set_reference(r);
                reference_value.into()
            }
            _ => r.into(),
        }
    }

    pub fn build_struct_insert(&self, val: LLVMValue, idx: usize, value: &LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        let new_struct = unsafe {
            LLVMBuildInsertValue(
                self.inner,
                val.as_llvm_value_ref(),
                value.as_llvm_value_ref(),
                idx as c_uint,
                name.as_ptr(),
            )
        };
        let struct_value = val.as_struct().unwrap();
        LLVMValue::Struct(struct_value.with_field(new_struct, idx, value.clone()))
    }
    pub fn build_i64_to_ptr(&self, val: LLVMValue) -> LLVMValue {
        let target = unsafe { LLVMPointerType(LLVMInt8Type(), 0) };
        unsafe { LLVMConstIntToPtr(val.as_llvm_value_ref(), target) }.into()
    }
    pub fn build_struct_gep(&self, ty: &LLVMType, val: LLVMValue, idx: usize) -> LLVMValue {
        let name = CString::new("").unwrap();
        let r = unsafe {
            LLVMBuildStructGEP2(
                self.inner,
                ty.as_llvm_type_ref(),
                val.as_llvm_value_ref(),
                idx as c_uint,
                name.as_ptr(),
            )
        };
        let ty = ty.get_struct_field_type(idx);
        match &ty {
            LLVMType::String(_) => ReferenceValue::new(r, ty.get_undef()).into(),
            LLVMType::Struct(_, _, _) => ReferenceValue::new(r, ty.get_undef()).into(),
            LLVMType::Pointer(element_type, _) => {
                ReferenceValue::new(r, element_type.get_undef()).into()
            }
            LLVMType::Function(_, _, _) => ReferenceValue::new(r, ty.get_undef()).into(),
            LLVMType::Ref(element_type, _) => {
                ReferenceValue::new(r, element_type.get_undef()).into()
            }
            _ => ReferenceValue::new(r, ty.get_undef()).into(),
        }
    }
    pub fn build_unreachable(&self) {
        unsafe {
            LLVMBuildUnreachable(self.inner);
        }
    }
    pub fn build_store(&self, ptr: LLVMValueRef, val: LLVMValue) {
        unsafe { LLVMBuildStore(self.inner, val.as_llvm_value_ref(), ptr) };
    }
    pub fn build_load(&self, ty: LLVMType, ptr: LLVMValueRef) -> LLVMValue {
        let name = CString::new("").unwrap();
        let val = unsafe {
            LLVMBuildLoad2(
                self.inner,
                ty.as_llvm_type_ref(), // 指针元素类型，如果指针是*i8,这这里的类型应该是i8
                ptr,
                name.as_ptr(),
            )
        };
        unsafe {
            match ty {
                LLVMType::Pointer(_, _) => PointerValue::new(val, ty.get_undef()).into(),
                LLVMType::String(_) => LLVMValue::String(val),
                LLVMType::Array(element_type, array_type) => {
                    let length = LLVMGetArrayLength2(array_type);
                    ArrayValue::new(val, *element_type, length as usize).into()
                }
                LLVMType::Struct(name, fields, _) => {
                    let mut field_index = HashMap::new();
                    for (i, (name, _)) in fields.iter().enumerate() {
                        field_index.insert(name.clone(), i);
                    }
                    StructValue::new(
                        val,
                        name,
                        field_index,
                        fields.iter().map(|(_, ty)| ty.get_undef()).collect(),
                    )
                    .into()
                }
                _ => val.into(),
            }
        }
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
    pub fn build_array_get_in_bounds(
        &self,
        ty: LLVMType,
        ptr: LLVMValue,
        index: LLVMValue,
    ) -> LLVMValue {
        // 获取ptr所指数组的类型
        let name = CString::new("").unwrap();
        let mut indices = [index.as_llvm_value_ref()];
        let r = unsafe {
            let ptr0 = LLVMBuildInBoundsGEP2(
                self.inner,
                ty.as_llvm_type_ref(),
                ptr.as_llvm_value_ref(),
                indices.as_mut_ptr(),
                1,
                name.as_ptr(),
            );
            LLVMBuildLoad2(self.inner, ty.as_llvm_type_ref(), ptr0, name.as_ptr())
        };
        match ty {
            LLVMType::Pointer(_, _) => PointerValue::new(r, ty.get_undef()).into(),
            LLVMType::String(_) => LLVMValue::String(r),
            _ => r.into(),
        }
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
        ReferenceValue::new(r, ty.get_undef()).into()
    }
    pub fn position_at_end(&self, block: LLVMBasicBlockRef) {
        unsafe { LLVMPositionBuilderAtEnd(self.inner, block) };
    }
    pub fn build_br(&self, block: LLVMBasicBlockRef) {
        unsafe { LLVMBuildBr(self.inner, block) };
    }
    pub fn build_cond_br(
        &self,
        condition: &BoolValue,
        then_block: LLVMBasicBlockRef,
        else_block: LLVMBasicBlockRef,
    ) {
        unsafe {
            LLVMBuildCondBr(
                self.inner,
                condition.get_reference(),
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
        let r = r.into();
        r
    }
    pub fn build_fadd(&self, l: LLVMValue, r: LLVMValue) -> LLVMValue {
        let name = CString::new("").unwrap();
        unsafe {
            LLVMBuildFAdd(
                self.inner,
                l.as_llvm_value_ref(),
                r.as_llvm_value_ref(),
                name.as_ptr(),
            )
        }
        .into()
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
    pub fn build_eq(&self, l: LLVMValueRef, r: LLVMValueRef) -> BoolValue {
        let name = CString::new("").unwrap();
        let r = unsafe { LLVMBuildICmp(self.inner, LLVMIntEQ, l, r, name.as_ptr()) };
        BoolValue::new(r)
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
        ctx: &Context,
        function: &FunctionValue,
        params: &mut [LLVMValue],
        var_name: impl AsRef<str>,
    ) -> LLVMValue {
        let dec = function.get_llvm_type(ctx).as_llvm_type_ref();
        let var_name = CString::new(var_name.as_ref()).unwrap();
        let mut params = params
            .iter()
            .map(|e| e.as_llvm_value_ref())
            .collect::<Vec<LLVMValueRef>>();
        let call_res = unsafe {
            LLVMBuildCall2(
                self.inner,
                dec,
                function.get_reference(),
                params.as_mut_ptr(),
                params.len() as c_uint,
                var_name.as_ptr(),
            )
        };
        let return_value = function.get_return_value();
        if return_value.is_struct() {
            let mut struct_value = return_value.as_struct().unwrap().clone();
            struct_value.set_reference(call_res);
            return struct_value.clone().into();
        }
        match return_value {
            LLVMValue::String(_) => LLVMValue::String(call_res),
            LLVMValue::Pointer(pointer_value) => {
                let mut pointer_value = pointer_value.clone();
                pointer_value.set_reference(call_res);
                pointer_value.into()
            }
            LLVMValue::Struct(mut v) => {
                v.set_reference(call_res);
                v.into()
            }
            _ => call_res.into(),
        }
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
    pub fn build_extract_value(&self, ctx: &Context, val: &StructValue, idx: usize) -> LLVMValue {
        let name = CString::new("").unwrap();
        let r = unsafe {
            LLVMBuildExtractValue(
                self.inner,
                val.get_reference(),
                idx as c_uint,
                name.as_ptr(),
            )
        };

        let field_type = val.get_field_by_index(ctx, idx).unwrap();
        match field_type {
            LLVMValue::String(_) => LLVMValue::String(r),
            LLVMValue::Struct(mut v) => {
                v.set_reference(r);
                v.into()
            }
            LLVMValue::Pointer(pointer_value) => {
                let mut pointer_value = pointer_value.clone();
                pointer_value.set_reference(r);
                pointer_value.into()
            }
            LLVMValue::Function(f) => {
                let mut function_value = f.clone();
                function_value.set_reference(r);
                function_value.into()
            }
            _ => r.into(),
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe { LLVMDisposeBuilder(self.inner) };
    }
}
