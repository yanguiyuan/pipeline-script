use crate::ast::data::Data;

use super::expr::ExprNode;
use crate::parser::r#type::Type;
#[derive(Clone, Debug)]
pub struct VariableDeclaration {
    pub name: String,
    pub default: Option<ExprNode>,
    pub declaration_type: Option<Type>,
    pub is_var_arg: bool,
    pub is_closure: bool,
}

impl VariableDeclaration {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            default: None,
            declaration_type: None,
            is_var_arg: false,
            is_closure: false,
        }
    }
    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }

    pub fn with_type(mut self, dec: Type) -> Self {
        self.declaration_type = Some(dec);
        self
    }
    pub fn with_closure(mut self, is_closure: bool) -> Self {
        self.is_closure = is_closure;
        self
    }
    pub fn with_var_arg(mut self, is_var_arg: bool) -> Self {
        self.is_var_arg = is_var_arg;
        self
    }
    pub fn is_var_arg(&self) -> bool {
        self.is_var_arg
    }
    pub fn r#type(&self) -> Option<Type> {
        self.declaration_type.clone()
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn has_default(&self) -> bool {
        self.default.is_some()
    }
    pub fn get_default(&self) -> Option<&ExprNode> {
        match &self.default {
            None => None,
            Some(s) => Some(s),
        }
    }
    pub fn get_mut_default(&mut self) -> Option<&mut ExprNode> {
        match &mut self.default {
            None => None,
            Some(s) => Some(s),
        }
    }
    pub fn get_data(&self, key: &str) -> Option<Data> {
        match key {
            "name" => Some(Data::String(self.name.clone())),
            "type" => self
                .declaration_type
                .as_ref()
                .map(|s| Data::Type(s.clone())),
            "is_var_arg" => Some(Data::Boolean(self.is_var_arg)),
            _ => None,
        }
    }
    pub fn set_type(&mut self, ty: Type) {
        self.declaration_type = Some(ty)
    }
    pub fn set_default(&mut self, expr: ExprNode) {
        self.default = Some(expr)
    }
    pub fn with_default(mut self, default: ExprNode) -> Self {
        self.default = Some(default);
        self
    }
}
