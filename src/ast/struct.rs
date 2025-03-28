use crate::ast::r#type::Type;
use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
    pub generics: Vec<Type>,
}

impl Struct {
    pub fn new(name: String, fields: Vec<StructField>, generics: Vec<Type>) -> Self {
        Self {
            name,
            fields,
            generics,
        }
    }
    pub fn get_llvm_type(&self) -> LLVMType {
        Global::struct_type(
            self.name.clone(),
            self.fields
                .iter()
                .map(|f| (f.name.clone(), f.field_type.as_llvm_type()))
                .collect(),
        )
    }
    pub fn get_field_type(&self, field_name: &str) -> Option<Type> {
        for field in self.fields.clone() {
            if field.name == field_name {
                return Some(field.field_type);
            }
        }
        None
    }
    pub fn has_generic(&self) -> bool {
        !self.generics.is_empty()
    }
    pub fn get_generics(&self) -> &Vec<Type> {
        &self.generics
    }
    pub fn get_type(&self) -> Type {
        let mut m = vec![];
        for field in self.fields.clone() {
            m.push((field.name, field.field_type))
        }
        Type::Struct(Some(self.name.clone()), m)
    }
    pub fn get_props(&self) -> Vec<(String, Type)> {
        self.fields
            .iter()
            .map(|f| (f.name.clone(), f.field_type.clone()))
            .collect()
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
