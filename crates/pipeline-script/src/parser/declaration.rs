use crate::parser::expr::Expr;
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
    pub fn new(name: String) -> Self {
        Self {
            name,
            default: None,
            declaration_type: None,
            is_var_arg: false,
        }
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
