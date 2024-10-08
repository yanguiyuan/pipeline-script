use std::ffi::{c_uint, CString};
use llvm_sys::core::{LLVMArrayType2, LLVMConstArray2, LLVMConstInt, LLVMConstReal, LLVMConstString, LLVMConstStruct, LLVMCreateBuilder, LLVMDoubleType, LLVMFloatType, LLVMFunctionType, LLVMInt16Type, LLVMInt32Type, LLVMInt64Type, LLVMInt8Type, LLVMPointerType, LLVMStructType, LLVMVoidType};
use llvm_sys::prelude::{LLVMBool, LLVMTypeRef, LLVMValueRef};
use crate::builder::Builder;
use crate::types::LLVMType;
use crate::value::LLVMValue;

pub struct Global;

impl Global{
    pub  fn const_array(array:&[LLVMValue])->LLVMValue{
        // 获取类型
        let ty = array[0].get_type();
        // 转换成LLVMValueRef
        let mut llvm_values: Vec<LLVMValueRef> = array.iter().map(|v| v.as_llvm_value_ref()).collect();
        let a = unsafe { LLVMConstArray2(ty.as_llvm_type_ref(), llvm_values.as_mut_ptr(), llvm_values.len()  as u64) };
        LLVMValue::Array(a)
    }
    pub fn const_unit()->LLVMValue{
        LLVMValue::Unit
    }
    pub fn const_double(value:f64)->LLVMValue{
        let v = unsafe { LLVMConstReal(LLVMDoubleType(), value) };
        LLVMValue::Double(v)
    }
    pub fn const_i8(value:i8)->LLVMValue{
        let v = unsafe { LLVMConstInt(LLVMInt8Type(), value as u64,0) };
        LLVMValue::Int8(v)
    }
    pub fn const_i16(value:i16)->LLVMValue{
        let v = unsafe { LLVMConstInt(LLVMInt16Type(), value as u64,0) };
        LLVMValue::Int16(v)
    }
    pub fn const_i32(value:i32)->LLVMValue{
        let v = unsafe { LLVMConstInt(LLVMInt32Type(), value as u64,0) };
        LLVMValue::Int32(v)
    }
    pub fn const_i64(value:i64)->LLVMValue{
        let v = unsafe { LLVMConstInt(LLVMInt64Type(), value as u64,0) };
        LLVMValue::Int64(v)
    }
    pub fn i8_type()->LLVMType{
        let t = unsafe { LLVMInt8Type() };
        LLVMType::Int8(t)
    }
    pub fn i16_type()->LLVMType{
        let t = unsafe { LLVMInt16Type() };
        LLVMType::Int16(t)
    }
    pub fn i32_type()->LLVMType{
        let t = unsafe { LLVMInt32Type() };
        LLVMType::Int32(t)
    }
    pub fn i64_type()->LLVMType{
        let t = unsafe { LLVMInt64Type() };
        LLVMType::Int64(t)
    }
    pub fn struct_type(element_type:Vec<LLVMType>)->LLVMType{
        let mut t = element_type.iter().map(|t| t.as_llvm_type_ref()).collect::<Vec<LLVMTypeRef>>();
        let t = unsafe { LLVMStructType(t.as_mut_ptr(),element_type.len() as c_uint,0) };
        LLVMType::Struct(element_type,t)
    }
    pub fn const_struct(element_type:Vec<LLVMValue>)->LLVMValue{
        let mut t = element_type.iter().map(|t| t.as_llvm_value_ref()).collect::<Vec<LLVMValueRef>>();
        let t = unsafe { LLVMConstStruct(t.as_mut_ptr(),element_type.len() as c_uint,0) };
        LLVMValue::Struct(t)
    }
    pub fn array_type(element_type:LLVMType)->LLVMType{
        let t = unsafe { LLVMArrayType2(element_type.as_llvm_type_ref(),0) };
        LLVMType::Array(Box::new(element_type),t)
    }
    pub fn unit_type()->LLVMType{
        let t = unsafe { LLVMVoidType() };
        LLVMType::Unit(t)
    }
    pub fn float_type()->LLVMType{
        let t = unsafe { LLVMFloatType() };
        LLVMType::Float(t)
    }
    pub fn double_type()->LLVMType{
        let t = unsafe { LLVMDoubleType() };
        LLVMType::Double(t)
    }
    pub fn function_type(return_type:LLVMType,param_types:Vec<LLVMType>)->LLVMType{
        let mut t = param_types.iter().map(|t| t.as_llvm_type_ref()).collect::<Vec<LLVMTypeRef>>();
        let t = unsafe { LLVMFunctionType(return_type.as_llvm_type_ref(),t.as_mut_ptr(),t.len() as c_uint,0) };
        LLVMType::Function(Box::new(return_type),param_types,t)
    }
    pub fn function_type_with_var_arg(return_type:LLVMType,param_types:Vec<LLVMType>)->LLVMType{
        let mut t = param_types.iter().map(|t| t.as_llvm_type_ref()).collect::<Vec<LLVMTypeRef>>();
        let t = unsafe { LLVMFunctionType(return_type.as_llvm_type_ref(),t.as_mut_ptr(),t.len() as c_uint,1) };
        LLVMType::Function(Box::new(return_type),param_types,t)
    }
    pub fn pointer_type(element_type:LLVMType)->LLVMType{
        let t = unsafe { LLVMPointerType(element_type.as_llvm_type_ref(),0) };
        LLVMType::Pointer(Box::new(element_type),t)
    }
    pub fn const_string(str:impl AsRef<str>)->LLVMValue{
        let str0 = str.as_ref();
        let str = CString::new(str0).unwrap();
        let c =unsafe { LLVMConstString(str.as_ptr(), str0.len() as c_uint,0) };
        c.into()
    }
    pub fn create_builder()->Builder{
        let builder = unsafe{LLVMCreateBuilder()};
        Builder::new(builder)
    }
    pub fn type_of_string(s:&str)->LLVMType{
        match s {
            "Int32" => Global::i32_type(),
            "Float" => Global::float_type(),
            "Double" => Global::double_type(),
            // "Any"=> Global::struct_type(),
            _ => panic!("unknown type"),
        }
    }
}