use std::collections::HashMap;

use wrap_llvm::global::Global;
use wrap_llvm::types::LLVMType;
use crate::parser::r#struct::Struct;
#[derive(Clone, Debug,PartialEq)]
pub enum Type {
    Int8,
    Int16,
    Int32,
    Int64,
    Float,
    Double,
    String,
    Bool,
    Pointer(Box<Type>),
    Alias(String),
    Struct(HashMap<String,(usize,Type)>),
    Function(Box<Type>, Vec<Type>),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Any,
    Unit,
}
impl From<&str> for Type {
    fn from(s: &str) -> Self {
        match s {
            "Unit"=>Type::Unit,
            "Int8" => Type::Int8,
            "Int16" =>Type::Int16,
            "Int32" => Type::Int32,
            "Int64" => Type::Int64,
            "Float" => Type::Float,
            "Double" => Type::Double,
            "String" => Type::String,
            "Bool" => Type::Bool,
//            t=>Type::Struct(t.into()),
            _ => panic!("Unknown type: {}", s),
        }
    }
}

impl From<LLVMType> for Type {
    fn from(value: LLVMType) -> Self {
        match value {
            LLVMType::Int1(_) => Type::Bool,
            LLVMType::Int8(_) => Type::Int8,
            LLVMType::Int16(_) => Type::Int16,
            LLVMType::Int32(_) => Type::Int32,
            LLVMType::Int64(_) => Type::Int64,
            LLVMType::Float(_) => Type::Float,
            LLVMType::Double(_) => Type::Double,
            LLVMType::Pointer(e,_)=>Type::Pointer(Box::new((*e).into())),
            LLVMType::Unit(_)=>Type::Any,
            t=>panic!("{t:?}")
        }
    }
}
impl From<String> for Type {
    fn from(s: String) -> Self {
        match s.as_str() {
            "Unit"=>Type::Unit,
            "Int16" => Type::Int16,
            "Int8" => Type::Int8,
            "Int32" => Type::Int32,
            "Int64" => Type::Int64,
            "Float" => Type::Float,
            "Double" => Type::Double,
            "String" => Type::String,
            "Bool" => Type::Bool,
            "Any" => Type::Any,
            s if s.starts_with("*")=>{
                let s = s.strip_prefix("*").unwrap();
                let t = s.into();
                Type::Pointer(Box::new(t))
            },
            s if s.starts_with("[]")=>{
                let s = s.strip_prefix("[]").unwrap();
                let t = s.into();
                Type::Array(Box::new(t))
            }
            t => Type::Alias(t.into()),
        }
    }
}
impl Type {
    pub fn id(&self) -> i32 {
        match self {
            Type::Int8 => 1,
            Type::Int16 => 2,
            Type::Int32 => 3,
            Type::Int64 => 4,
            Type::Float => 5,
            Type::Double => 6,
            Type::String => 7,
            Type::Bool => 8,
            // Type::Any => 9,
            t=>panic!("{t:?}")
        }
    }
    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Int32 | Type::Int64 | Type::Float | Type::Double | Type::String | Type::Bool => {
                true
            }
            _ => false,
        }
    }
    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => false,
        }
    }
    pub fn get_struct_field(&self,name:impl AsRef<str>)->Option<(usize,Type)>{
        match self {
            Type::Struct(m)=>{
                let e = m.get(name.as_ref()).unwrap();
                Some(e.clone())
            }
            _=>None,
        }
    }
    pub fn is_i8(&self) -> bool {
        match self {
            Type::Int8 => true,
            _ => false,
        }
    }
    pub fn is_any(&self)->bool{
        match self {
            Type::Any => true,
            _ => false,
        }
    }
    pub fn is_string(&self) -> bool {
        match self {
            Type::String => true,
            _ => false,
        }
    }
    pub fn is_i16(&self) -> bool {
        match self {
            Type::Int16 => true,
            _ => false,
        }
    }
    pub fn is_i32(&self) -> bool {
        match self {
            Type::Int32 => true,
            _ => false,
        }
    }
    pub fn is_i64(&self) -> bool {
        match self {
            Type::Int64 => true,
            _ => false,
        }
    }
    pub fn is_struct(&self) -> bool {
        match self {
            Type::Struct(_) => true,
            _ => false,
        }
    }
    pub fn is_array(&self) -> bool {
        match self {
            Type::Array(_) => true,
            _ => false,
        }
    }

    pub fn as_llvm_type(&self) -> LLVMType{
        match self {
            Type::Int8 => Global::i8_type(),
            Type::Int16 => Global::i16_type(),
            Type::Int32 => Global::i32_type(),
            Type::Int64 => Global::i64_type(),
            Type::Unit => Global::unit_type(),
            Type::Pointer(i) => Global::pointer_type(i.as_llvm_type()),
            Type::Any=>Global::struct_type(vec![Global::i32_type(),Global::pointer_type(Global::i8_type())]),
            Type::Array(t)=>Global::array_type(t.as_llvm_type()),
            Type::String=>Global::pointer_type(Global::i8_type()),
            // Type::Struct(s)
            _=> panic!("Unknown type: {:?}", self),
        }
    }
    pub fn get_alias_name(&self) -> Option<String> {
        match self {
            Type::Alias(s) => Some(s.clone()),
            _ => None,
        }
    }
    pub fn get_function_return_type(&self) -> Option<Type> {
        match self {
            Type::Function(r, _) => Some(*r.clone()),
            _ => None,
        }
    }
    pub fn get_element_type(&self) -> Option<&Type> {
        match self {
            Type::Array(t) => Some(t),
            Type::Pointer(t) => Some(t),
            _ => None,
        }
    }
}
