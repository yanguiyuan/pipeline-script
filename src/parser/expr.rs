use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::ast::data::Data;
use crate::ast::node::Node;
use crate::lexer::position::Position;
use crate::parser::declaration::VariableDeclaration;
use crate::parser::r#type::Type;
use crate::parser::stmt::StmtNode;

#[derive(Debug, Clone)]
pub struct ExprNode {
    expr: Expr,
    pos: Position,
    ty: Option<Type>,
}

impl ExprNode {
    pub fn to_ast(&self) -> Node {
        let mut data = HashMap::new();
        data.insert("type".into(),Data::Type(self.ty.as_ref().unwrap().clone()));
        match &self.expr {
            Expr::String(s) => {
                data.insert("value".to_string(), Data::String(s.clone()));
                Node::new("Expr:String").with_data(data)
            },
            Expr::Int(i) => {
                data.insert("value".to_string(), Data::Int64(*i));
                Node::new("Expr:Int").with_data(data)
            },

            Expr::Float(f) => {
                data.insert("value".to_string(), Data::Float64(*f));
                Node::new("Expr:Float").with_data(data)
            },
            Expr::Boolean(b) => {
                data.insert("value".to_string(), Data::Boolean(*b));
                Node::new("Expr:Boolean").with_data(data)
            },
            Expr::BraceExpr(expr) => expr.to_ast(),
            Expr::FnCall(call) => {
                let mut children = vec![];
                for arg in call.args.iter() {
                    children.push(arg.value.to_ast())
                }
                data.insert("name".into(),Data::String(call.name.clone()));
                data.insert("is_method".into(),Data::Boolean(call.is_method));
                Node::new("Expr:FnCall").with_data(data).with_children(children)
            }
            Expr::Variable(v) => {
                data.insert("name".into(),Data::String(v.clone()));
                Node::new("Expr:Variable").with_data(data)
            },
            Expr::Binary(op, left, right) => {
                data.insert("op".into(),op.to_string().into());
                let l = left.to_ast();
                let r = right.to_ast();
                Node::new("Expr:Binary").with_data(data).with_children(vec![l,r])
            }
            Expr::Unary(op, expr) => {
                data.insert("op".into(),op.to_string().into());
                let node = expr.to_ast();
                Node::new("Expr:Unary").with_data(data).with_children(vec![node])
            }
            Expr::Array(exprs) => {
                let mut children = vec![];
                data.insert("type".into(),Data::Type(self.ty.as_ref().unwrap().clone()));
                for expr in exprs.iter() {
                    children.push(expr.to_ast())
                }
                Node::new("Expr:Array").with_children(children).with_data(data)
            }
            Expr::Map(exprs) => {
                let mut children = vec![];
                for (k, v) in exprs.iter() {
                    children.push(k.to_ast());
                    children.push(v.to_ast());
                }
                Node::new("Expr:Map").with_children(children)
            }
            Expr::Index(left, right) => {
                let left = left.to_ast();
                let right = right.to_ast();
                Node::new("Expr:Index").with_children(vec![left, right])
            }
            Expr::Address(expr) => {
                let node = expr.to_ast();
                Node::new("Expr:Address").with_children(vec![node])
            }
            Expr::Closure(params, body, captures) => {
                let mut children = vec![];
                for stmt in body.iter() {
                    children.push(stmt.to_ast())
                }
                Node::new("Expr:Closure").with_children(children)
            }
            Expr::Struct(s) => {
                let mut children = vec![];
                for (k, v) in s.props.iter() {
                    children.push(v.to_ast());
                }
                Node::new("Expr:Struct").with_children(children)
            }
            Expr::Member(root, name) => {
                let node = root.to_ast();
                Node::new("Expr:Member").with_children(vec![node])
            }
            Expr::None => Node::new("None"),
        }
    }

    #[allow(unused)]
    pub(crate) fn get_closure_body(&self) -> Vec<StmtNode> {
        match &self.expr {
            Expr::Closure(_, body, _) => body.clone(),
            _ => panic!("not closure expr"),
        }
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
    Closure(Vec<VariableDeclaration>, Vec<StmtNode>, Vec<(String, Type)>),
    Struct(StructExpr),
    Member(Box<ExprNode>, String),
    None,
}
#[derive(Debug, Clone)]
pub struct StructExpr {
    pub(crate) name: String,
    pub(crate) props: HashMap<String, ExprNode>,
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
pub struct FnCallExpr {
    pub name: String,
    pub generics: Vec<Type>,
    pub is_method: bool,
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
    pub fn new(expr: Expr) -> Self {
        Self {
            expr,
            pos: Position::none(),
            ty: None,
        }
    }
    pub fn is_closure(&self) -> bool {
        match &self.expr {
            Expr::Closure(..) => true,
            _ => false,
        }
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
    pub fn get_expr(&self) -> Expr {
        self.expr.clone()
    }
    pub fn get_type(&self) -> Option<Type> {
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
