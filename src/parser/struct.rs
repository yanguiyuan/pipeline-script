use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use crate::parser::r#type::Type;
#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

impl Struct {
    pub fn new(name: String, fields: Vec<StructField>) -> Self {
        Self { name, fields }
    }
    pub fn get_llvm_type(&self) -> LLVMType {
        Global::struct_type(
            self.fields
                .iter()
                .map(|f| f.field_type.as_llvm_type())
                .collect(),
        )
    }
    pub fn get_type(&self) -> Type {
        let mut m = vec![];
        for field in self.fields.clone() {
            m.push((field.name, field.field_type))
        }
        Type::Struct(Some(self.name.clone()), m)
    }
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_fields(&self) -> &Vec<StructField> {
        &self.fields
    }
}
#[derive(Clone, Debug)]

pub struct StructField {
    pub name: String,
    pub field_type: Type,
}

impl StructField {
    pub fn new(name: String, field_type: Type) -> Self {
        Self { name, field_type }
    }
}
