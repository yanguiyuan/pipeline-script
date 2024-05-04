use crate::module::FnDef;
use crate::position::Position;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expr {
    StringConstant(String, Position),
    IntConstant(i64, Position),
    FloatConstant(f64, Position),
    FnClosure(FnClosureExpr, Position),
    FnCall(FnCallExpr, Position),
    Variable(String, Position),
    Binary(Op, Box<Expr>, Box<Expr>, Position),
    Array(Vec<Expr>, Position),
    Map(Vec<(Expr, Expr)>, Position),
    Index(Box<Expr>, Box<Expr>, Position),
    Struct(StructExpr, Position),
    /// a=Person::new()
    /// a.name  -> MemberAccess
    MemberAccess(Box<Expr>, String, Position),
    None(Position),
}
#[derive(Debug, Clone)]
pub struct StructExpr {
    name: String,
    props: HashMap<String, Expr>,
}

impl StructExpr {
    pub fn new(name: String, props: HashMap<String, Expr>) -> Self {
        Self { name, props }
    }
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_props(&self) -> &HashMap<String, Expr> {
        &self.props
    }
}
#[derive(Debug, Clone)]
pub enum Op {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Greater,
    Less,
    Equal,
    NotEqual,
}
#[derive(Debug, Clone)]
pub struct FnCallExpr {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct FnClosureExpr {
    pub(crate) def: FnDef,
}
impl Expr {
    pub fn is_fn_call(&self) -> bool {
        matches!(self, Expr::FnCall(_, _))
    }
    pub fn try_get_variable_name(&self) -> Option<String> {
        match self {
            Expr::Variable(s, _) => Some(s.clone()),
            Expr::MemberAccess(_, name, _) => Some(name.clone()),
            _ => None,
        }
    }
    pub fn try_get_member_access(&self) -> Option<(Box<Expr>, String)> {
        match self {
            Expr::MemberAccess(b, name, _) => Some((b.clone(), name.clone())),
            _ => None,
        }
    }
    pub fn position(&self) -> Position {
        match self {
            Expr::StringConstant(_, pos) => pos.clone(),
            Expr::IntConstant(_, pos) => pos.clone(),
            Expr::FloatConstant(_, pos) => pos.clone(),
            Expr::Variable(_, pos) => pos.clone(),
            Expr::FnClosure(_, pos) => pos.clone(),
            Expr::FnCall(_, pos) => pos.clone(),
            Expr::Binary(_, _, _, pos) => pos.clone(),
            Expr::Array(_, pos) => pos.clone(),
            Expr::Index(_, _, pos) => pos.clone(),
            Expr::Map(_, pos) => pos.clone(),
            Expr::None(pos) => pos.clone(),
            Expr::Struct(_, pos) => pos.clone(),
            Expr::MemberAccess(_, _, pos) => pos.clone(),
        }
    }
}
