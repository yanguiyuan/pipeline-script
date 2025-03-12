use super::r#type::Type;

#[derive(Clone, Debug)]
pub struct TypeAlias {
    generic_list: Vec<String>,
    ty: Type,
}

impl TypeAlias {
    pub fn new(ty: Type, generic_list: Vec<String>) -> Self {
        TypeAlias { generic_list, ty }
    }
    pub fn get_type(&self) -> &Type {
        &self.ty
    }
    pub fn get_generic_list(&self) -> &Vec<String> {
        &self.generic_list
    }
}

impl From<Type> for TypeAlias {
    fn from(value: Type) -> Self {
        TypeAlias {
            generic_list: vec![],
            ty: value,
        }
    }
}
