use crate::expr::Expr;
use crate::position::Position;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Stmt {
    EvalExpr(Box<Expr>, Position),
    Val(Box<(String, Expr)>, Position),
    Var(Box<(String, Expr)>, Position),
    Assign(Box<(Expr, Expr)>, Position),
    Return(Box<Expr>, Position),
    If(Box<IfStmt>, Position),
    While(Box<Expr>, Vec<Stmt>, Position),
    ForIn(String, Option<String>, Box<Expr>, Vec<Stmt>, Position),
    /// 第一个Expr表示的是获取Array或者Map的表达式
    /// 第二个Expr表示的是获取索引的表达式
    /// 第三个Expr表示的是对索引处的赋值
    IndexAssign(Box<Expr>, Box<Expr>, Box<Expr>, Position),
    Break(Position),
    Continue(Position),
    Import(String, Position),
    Noop,
}
#[derive(Debug, Clone)]
pub struct IfStmt {
    branches: Vec<IfBranchStmt>,
    else_body: Option<Vec<Stmt>>,
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Import(name, _) => write!(f, "import {name};"),
            Stmt::Val(v, _) => {
                write!(f, "val {} = {};", v.0, v.1)
            }
            Stmt::EvalExpr(e, _) => write!(f, "{e};"),
            Stmt::Return(r, _) => write!(f, "return {};", r),
            Stmt::Var(v, _) => {
                write!(f, "var {} = {};", v.0, v.1)
            }
            Stmt::If(i, _) => {
                for i in &i.branches {
                    write!(f, "if {} {{\n", i.condition)?;
                    for b in &i.body {
                        write!(f, "    {b}\n")?;
                    }
                    write!(f, "}}")?;
                }
                if i.else_body.is_some() {
                    write!(f, "else {{\n")?;
                    for b in i.else_body.clone().unwrap().iter() {
                        write!(f, "    {b}\n")?;
                    }
                    write!(f, "}}\n")?;
                }
                Ok(())
            }
            _ => write!(f, "<Stmt>"),
        }
    }
}

#[test]
fn test_display() {
    let s = Stmt::Val(
        Box::new((
            "a".to_string(),
            Expr::StringConstant("Hello,world!".to_string(), Position::none()),
        )),
        Position::none(),
    );
    println!("{s}")
}
impl IfStmt {
    pub fn new(branches: Vec<IfBranchStmt>, else_body: Option<Vec<Stmt>>) -> Self {
        Self {
            branches,
            else_body,
        }
    }
    pub fn get_branches(&self) -> &Vec<IfBranchStmt> {
        &self.branches
    }
    pub fn get_else_body(&self) -> Option<Vec<Stmt>> {
        self.else_body.clone()
    }
}
#[derive(Debug, Clone)]
pub struct IfBranchStmt {
    condition: Expr,
    body: Vec<Stmt>,
}

impl IfBranchStmt {
    pub fn new(condition: Expr, body: Vec<Stmt>) -> Self {
        Self { condition, body }
    }
    pub fn get_condition(&self) -> &Expr {
        &self.condition
    }
    pub fn get_body(&self) -> &Vec<Stmt> {
        &self.body
    }
}
impl Stmt {
    pub fn is_noop(&self) -> bool {
        matches!(self, Stmt::Noop)
    }
    pub fn position(&self) -> Position {
        match self {
            Stmt::Continue(pos) => pos.clone(),
            Stmt::EvalExpr(_, pos) => pos.clone(),
            Stmt::ForIn(_, _, _, _, pos) => pos.clone(),
            Stmt::Val(_, pos) => pos.clone(),
            Stmt::Var(_, pos) => pos.clone(),
            Stmt::Assign(_, pos) => pos.clone(),
            Stmt::Return(_, pos) => pos.clone(),
            Stmt::If(_, pos) => pos.clone(),
            Stmt::While(_, _, pos) => pos.clone(),
            Stmt::IndexAssign(_, _, _, pos) => pos.clone(),
            Stmt::Import(_, pos) => pos.clone(),
            Stmt::Noop => Position::none(),
            Stmt::Break(pos) => pos.clone(),
        }
    }
}
