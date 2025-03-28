use crate::ast::r#type::Type;
use crate::llvm::value::LLVMValue;

#[derive(Debug, Clone)]
pub struct Value {
    pub value: LLVMValue,
    pub ty: Type,
}
impl Value {
    pub fn new(value: LLVMValue, ty: Type) -> Value {
        Value { value, ty }
    }
    pub fn get_value(&self) -> LLVMValue {
        self.value.clone()
    }
    pub fn get_type(&self) -> Type {
        self.ty.clone()
    }
}
