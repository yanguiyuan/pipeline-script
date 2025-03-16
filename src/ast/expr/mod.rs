mod function_call;

use crate::ast::data::Data;
use crate::ast::declaration::VariableDeclaration;
pub use crate::ast::expr::function_call::FunctionCall;
use crate::ast::r#type::Type;
use crate::ast::stmt::StmtNode;
use crate::ast::NodeTrait;
use crate::lexer::position::Position;
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct ExprNode {
    expr: Expr,
    pos: Position,
    ty: Option<Type>,
}

impl NodeTrait for ExprNode {
    fn get_id(&self) -> &str {
        match self.expr {
            Expr::String(_) => "Expr:String",
            Expr::Int(_) => "Expr:Int",
            Expr::Float(_) => "Expr:Float",
            Expr::Boolean(_) => "Expr:Boolean",
            Expr::Brace(_) => "Expr:Brace",
            Expr::FnCall(_) => "Expr:FnCall",
            Expr::Variable(_) => "Expr:Variable",
            Expr::Binary(_, _, _) => "Expr:Binary",
            Expr::Unary(_, _) => "Expr:Unary",
            Expr::Array(_) => "Expr:Array",
            Expr::Map(_) => "Expr:Map",
            Expr::Index(_, _) => "Expr:Index",
            Expr::Address(_) => "Expr:Address",
            Expr::Closure(_, _, _) => "Expr:Closure",
            Expr::Struct(_) => "Expr:Struct",
            Expr::Member(_, _) => "Expr:Member",
            Expr::EnumVariant(_, _, _) => "Expr:EnumVariant",
            Expr::None => "Expr:None",
        }
    }

    fn get_data(&self, key: &str) -> Option<Data> {
        match &self.expr {
            Expr::Variable(name) => {
                if key == "name" {
                    return Some(Data::String(name.clone()));
                }
            }
            Expr::FnCall(call) => {
                if key == "name" {
                    return Some(Data::String(call.name.clone()));
                }
            }
            _ => todo!(),
        }
        None
    }

    fn set_data(&mut self, key: &str, value: Data) {
        match &mut self.expr {
            Expr::Variable(name) => {
                if key == "name" {
                    *name = value.as_str().unwrap().to_string()
                }
            }
            Expr::FnCall(call) => {
                if key == "name" {
                    call.name = value.as_str().unwrap().to_string()
                }
            }
            _ => todo!(),
        }
    }

    fn get_children(&self) -> Vec<&dyn NodeTrait> {
        todo!()
    }

    fn get_mut_children(&mut self) -> Vec<&mut dyn NodeTrait> {
        match &mut self.expr {
            Expr::String(_) => vec![],
            Expr::Int(_) => vec![],
            Expr::Float(_) => vec![],
            Expr::Boolean(_) => vec![],
            Expr::Brace(expr) => vec![&mut **expr],
            Expr::FnCall(call) => {
                let mut children: Vec<&mut dyn NodeTrait> = vec![];
                for arg in call.args.iter_mut() {
                    children.push(&mut arg.value)
                }
                children
            }
            Expr::Variable(_) => vec![],
            Expr::Binary(_, left, right) => vec![&mut **left, &mut **right],
            Expr::Unary(_, expr) => vec![&mut **expr],
            Expr::Array(exprs) => {
                let mut children: Vec<&mut dyn NodeTrait> = vec![];
                for expr in exprs.iter_mut() {
                    children.push(expr)
                }
                children
            }
            Expr::Map(exprs) => {
                let mut children = vec![];
                for (k, v) in exprs.iter_mut() {
                    children.push(k as &mut dyn NodeTrait);
                    children.push(v)
                }
                children
            }
            Expr::Index(left, right) => vec![&mut **left, &mut **right],
            Expr::Address(expr) => vec![&mut **expr as &mut dyn NodeTrait],
            Expr::Closure(vd, body, _) => {
                let mut children: Vec<&mut dyn NodeTrait> = vec![];
                for arg in vd.iter_mut() {
                    if let Some(expr) = &mut arg.default {
                        children.push(expr as &mut dyn NodeTrait);
                    }
                }

                for stmt in body.iter_mut() {
                    children.push(stmt as &mut dyn NodeTrait)
                }
                children
            }
            Expr::Struct(_) => vec![],
            Expr::Member(_, _) => vec![],
            Expr::EnumVariant(_, _, value) => {
                if let Some(expr) = value {
                    vec![&mut **expr]
                } else {
                    vec![]
                }
            }
            Expr::None => vec![],
        }
    }

    fn get_extra(&self) -> &HashMap<String, Box<dyn Any>> {
        todo!()
    }
}
impl ExprNode {
    #[allow(unused)]
    pub(crate) fn get_closure_body(&self) -> Vec<StmtNode> {
        match &self.expr {
            Expr::Closure(_, body, _) => body.clone(),
            _ => panic!("not closure expr"),
        }
    }
    pub fn is_fn_call(&self) -> bool {
        matches!(&self.expr, Expr::FnCall(_))
    }
    pub fn get_closure_params(&self) -> Vec<VariableDeclaration> {
        match &self.expr {
            Expr::Closure(params, _, _) => params.clone(),
            _ => panic!("not closure expr"),
        }
    }
    pub fn get_variable_name(&self) -> Option<String> {
        match &self.expr {
            Expr::Variable(name) => Some(name.clone()),
            _ => None,
        }
    }
    pub fn get_closure_captures(&self) -> Vec<(String, Type)> {
        match &self.expr {
            Expr::Closure(_, _, captures) => captures.clone(),
            _ => panic!("not closure expr"),
        }
    }
    pub fn get_expr_mut(&mut self) -> &mut Expr {
        &mut self.expr
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    String(String),
    Int(i64),
    Float(f32),
    Boolean(bool),
    Brace(Box<ExprNode>),
    FnCall(FunctionCall),
    Variable(String),
    Binary(Op, Box<ExprNode>, Box<ExprNode>),
    Unary(Op, Box<ExprNode>),
    Array(Vec<ExprNode>),
    Map(Vec<(ExprNode, ExprNode)>),
    Index(Box<ExprNode>, Box<ExprNode>),
    Address(Box<ExprNode>),
    Closure(Vec<VariableDeclaration>, Vec<StmtNode>, Vec<(String, Type)>),
    Struct(StructExpr),
    Member(Box<ExprNode>, String),
    EnumVariant(String, String, Option<Box<ExprNode>>),
    None,
}
#[derive(Debug, Clone)]
pub struct StructExpr {
    pub(crate) name: String,
    pub(crate) props: HashMap<String, ExprNode>,
    generics: Vec<Type>,
}

impl StructExpr {
    pub fn new(name: String, props: HashMap<String, ExprNode>) -> Self {
        Self {
            name,
            props,
            generics: vec![],
        }
    }
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_props(&self) -> &HashMap<String, ExprNode> {
        &self.props
    }
    pub fn has_generic(&self) -> bool {
        !self.generics.is_empty()
    }
    pub fn get_generics(&self) -> &Vec<Type> {
        &self.generics
    }
    pub fn with_generics(mut self, generics: Vec<Type>) -> Self {
        self.generics = generics;
        self
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
impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Mod => write!(f, "%"),
            Op::Greater => write!(f, ">"),
            Op::Less => write!(f, "<"),
            Op::Equal => write!(f, "=="),
            Op::NotEqual => write!(f, "!="),
            Op::Negate => write!(f, "!"),
            Op::And => write!(f, "&&"),
            Op::Or => write!(f, "||"),
        }
    }
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
    pub fn new(expr: Expr) -> Self {
        Self {
            expr,
            pos: Position::none(),
            ty: None,
        }
    }
    pub fn is_closure(&self) -> bool {
        matches!(&self.expr, Expr::Closure(..))
    }
    pub fn with_position(mut self, pos: Position) -> Self {
        self.pos = pos;
        self
    }
    pub fn with_type(mut self, ty: Type) -> Self {
        self.ty = Some(ty);
        self
    }
    pub fn position(&self) -> Position {
        self.pos.clone()
    }
    pub fn get_expr(&self) -> &Expr {
        &self.expr
    }
    pub fn get_type(&self) -> Option<Type> {
        if let Some(t) = &self.ty {
            if t.is_ref() {
                let element_type = t.get_element_type().unwrap();
                return Some(element_type.clone());
            }
        }
        self.ty.clone()
    }
    pub fn get_member_name(&self) -> String {
        match &self.expr {
            Expr::Member(_, name) => name.clone(),
            _ => panic!("not member expr"),
        }
    }
    pub fn get_member_root(&self) -> ExprNode {
        match &self.expr {
            Expr::Member(root, _) => *root.clone(),
            _ => panic!("not member expr"),
        }
    }
}

impl From<Expr> for ExprNode {
    fn from(value: Expr) -> Self {
        ExprNode {
            expr: value,
            pos: Position::none(),
            ty: None,
        }
    }
}
