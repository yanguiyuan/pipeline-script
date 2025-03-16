use crate::ast::expr::Argument;
use crate::ast::r#type::Type;

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub generics: Vec<Type>,
    pub is_method: bool,
    pub args: Vec<Argument>,
    pub type_generics: Vec<Type>, // 静态方法调用时有效,例如 Vec<Int>.create()
}
