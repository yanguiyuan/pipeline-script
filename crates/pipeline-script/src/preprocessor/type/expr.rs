use crate::parser::expr::Expr;
use crate::parser::r#type::Type;

pub struct TypeExpr{
    expr:Expr,
    ty:Type
}