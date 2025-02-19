use std::any::Any;
use std::collections::HashMap;
use crate::ast::data::Data;
use crate::ast::node::Node;
use crate::parser::r#type::Type;
use super::expr::ExprNode;
#[derive(Clone, Debug)]
pub struct VariableDeclaration {
    pub name: String,
    pub default: Option<ExprNode>,
    pub declaration_type: Option<Type>,
    pub is_var_arg: bool,
}
impl VariableDeclaration {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            default: None,
            declaration_type: None,
            is_var_arg: false,
        }
    }
    pub fn to_ast(&self) -> Node {
        let mut data = HashMap::new();
        data.insert("name".into(),Data::String(self.name.clone()));
        data.insert("is_var_arg".into(),Data::Boolean(self.is_var_arg));
        if self.declaration_type.is_some() {
            data.insert("type".into(),Data::Type(self.declaration_type.as_ref().unwrap().clone()));
        }
        let mut children = vec![];
        if self.default.is_some() {
            let default = self.default.as_ref().unwrap();
            children.push(default.to_ast())
        }
        Node::new("Declaration:Variable").with_data(data).with_children(children)
    }

    pub fn with_type(mut self, dec: Type) -> Self {
        self.declaration_type = Some(dec);
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
