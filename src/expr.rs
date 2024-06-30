use crate::module::FnDef;
use crate::position::Position;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Expr {
    StringConstant(String, Position),
    IntConstant(i64, Position),
    FloatConstant(f64, Position),
    FnClosure(FnClosureExpr, Position),
    FnCall(FnCallExpr, Position),
    Variable(String, Position),
    Binary(Op, Box<Expr>, Box<Expr>, Position),
    Unary(Op, Box<Expr>, Position),
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

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::StringConstant(s, _) => write!(f, "{s:?}"),
            Expr::IntConstant(i, _) => write!(f, "{i}"),
            Expr::Variable(v, _) => write!(f, "{v}"),
            Expr::FnCall(fc, _) => {
                write!(f, "{}(", fc.name)?;
                for (i, arg) in fc.args.iter().enumerate() {
                    write!(f, "{}", arg.value)?;
                    if i < fc.args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::FnClosure(fc, _) => {
                write!(f, "(")?;
                for arg in fc.def.args.iter() {
                    write!(f, "{}", arg.name)?;
                }
                write!(f, ")")?;
                write!(f, "->{{\n")?;
                for v in fc.def.body.iter() {
                    write!(f, "    {}\n", v)?;
                }
                write!(f, "}}")
            }
            Expr::Array(v, _) => {
                write!(f, "[")?;
                for (i, e) in v.iter().enumerate() {
                    write!(f, "{e}")?;
                    if i < v.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Expr::Binary(op, a, b, _) => match op {
                Op::Plus => write!(f, "{a} + {b}"),
                Op::Mul => write!(f, "{a} * {b}"),
                Op::Or => write!(f, "{a} || {b}"),
                _ => write!(f, "<Op>"),
            },
            _ => write!(f, "<Expr>"),
        }
    }
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
    Negate,
    And,
    Or,
}
#[derive(Debug, Clone)]
pub struct FnCallExpr {
    pub name: String,
    pub args: Vec<Argument>,
}
#[derive(Debug, Clone)]
pub struct Argument {
    name: Option<String>,
    pub value: Expr,
}
impl Argument {
    pub fn new(expr: Expr) -> Self {
        Self {
            name: None,
            value: expr,
        }
    }
    pub fn with_name(name: impl Into<String>, expr: Expr) -> Self {
        Self {
            name: Some(name.into()),
            value: expr,
        }
    }
    pub fn has_name(&self) -> bool {
        self.name.is_some()
    }
    pub fn get_name(&self) -> Option<&str> {
        match &self.name {
            None => None,
            Some(s) => Some(s),
        }
    }
}
#[derive(Debug, Clone)]
pub struct FnClosureExpr {
    pub(crate) def: FnDef,
    pub(crate) is_outside: bool,
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
            Expr::Unary(_, _, pos) => pos.clone(),
        }
    }
}
