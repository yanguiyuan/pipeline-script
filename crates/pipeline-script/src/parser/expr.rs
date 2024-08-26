
use std::collections::HashMap;
use scanner_rust::generic_array::typenum::N10;

use crate::lexer::position::Position;
use crate::parser::r#type::Type;
#[derive(Debug, Clone)]
pub struct ExprNode{
    expr:Expr,
    pos:Position,
    ty:Option<Type>
}

#[derive(Debug, Clone)]
pub enum Expr {
    String(String),
    Int(i64),
    Float(f64),
    Boolean(bool),
    BraceExpr(Box<ExprNode>),
    FnCall(FnCallExpr),
    Variable(String),
    Binary(Op, Box<ExprNode>, Box<ExprNode>),
    Unary(Op, Box<ExprNode>),
    Array(Vec<ExprNode>),
    Map(Vec<(ExprNode, ExprNode)>),
    Index(Box<ExprNode>, Box<ExprNode>),
    Address(Box<ExprNode>),
    Struct(StructExpr),
    Member(Box<ExprNode>, String),
    None,
}
#[derive(Debug, Clone)]
pub struct StructExpr {
    name: String,
    props: HashMap<String, ExprNode>,
}


impl StructExpr {
    pub fn new(name: String, props: HashMap<String, ExprNode>) -> Self {
        Self { name, props }
    }
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_props(&self) -> &HashMap<String, ExprNode> {
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
    pub value: ExprNode,
}
impl Argument {
    pub fn new(expr: ExprNode) -> Self {
        Self {
            name: None,
            value: expr,
        }
    }
    pub fn with_name(name: impl Into<String>, expr: ExprNode) -> Self {
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
impl ExprNode {
    pub fn new(expr:Expr)->Self{
        Self{
            expr,
            pos:Position::none(),
            ty:None
        }
    }
    pub fn with_position(mut self,pos:Position)->Self{
        self.pos = pos;
        self
    }
    pub fn with_type(mut self,ty:Type)->Self{
        self.ty = Some(ty);
        self
    }
    pub fn position(&self)->Position{
        self.pos.clone()
    }
    pub fn get_expr(&self)->Expr{
        self.expr.clone()
    }
    pub fn get_type(&self)->Option<Type>{
        self.ty.clone()
    }
}

impl From<Expr> for ExprNode {
    fn from(value: Expr) -> Self {
        ExprNode{
            expr:value,
            pos:Position::none(),
            ty:None,
        }
    }
}