use crate::ast::data::Data;
use crate::ast::declaration::VariableDeclaration;
use crate::ast::expr::{Expr, ExprNode};
use crate::ast::NodeTrait;
use crate::lexer::position::Position;
use std::any::Any;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct StmtNode {
    stmt: Stmt,
    pos: Position,
}
#[derive(Debug, Clone)]
pub enum Stmt {
    EvalExpr(Box<ExprNode>),
    ValDecl(VariableDeclaration),
    VarDecl(VariableDeclaration),
    Assign(Box<ExprNode>, Box<ExprNode>),
    Return(Box<ExprNode>),
    If(Box<IfStmt>),
    While(Box<ExprNode>, Vec<StmtNode>),
    ForIn(String, Box<ExprNode>, Vec<StmtNode>),
    /// 第一个Expr表示的是获取Array或者Map的表达式
    /// 第二个Expr表示的是获取索引的表达式
    /// 第三个Expr表示的是对索引处的赋值
    IndexAssign(Box<ExprNode>, Box<ExprNode>, Box<ExprNode>),
    Break,
    Continue,
    Import(String),
    // Match语句，包含匹配的表达式和匹配分支
    Match(Box<ExprNode>, Vec<MatchBranch>),
    // If let语句，用于模式匹配
    IfLet(
        Box<ExprNode>,
        Box<ExprNode>,
        Vec<StmtNode>,
        Option<Vec<StmtNode>>,
    ),
    IfConst(
        Box<ExprNode>,
        Box<ExprNode>,
        Vec<StmtNode>,
        Option<Vec<StmtNode>>,
    ),
    Noop,
}
#[derive(Debug, Clone)]
pub struct IfStmt {
    branches: Vec<IfBranchStmt>,
    else_body: Option<Vec<StmtNode>>,
}

impl IfStmt {
    pub fn new(branches: Vec<IfBranchStmt>, else_body: Option<Vec<StmtNode>>) -> Self {
        Self {
            branches,
            else_body,
        }
    }
    pub fn set_else_body(&mut self, else_body: Option<Vec<StmtNode>>) {
        self.else_body = else_body;
    }
    pub fn set_branches(&mut self, branches: Vec<IfBranchStmt>) {
        self.branches = branches;
    }
    pub fn get_branches(&self) -> &Vec<IfBranchStmt> {
        &self.branches
    }
    pub fn get_else_body(&self) -> Option<Vec<StmtNode>> {
        self.else_body.clone()
    }
}
#[derive(Debug, Clone)]
pub struct IfBranchStmt {
    condition: ExprNode,
    body: Vec<StmtNode>,
}

impl IfBranchStmt {
    pub fn new(condition: ExprNode, body: Vec<StmtNode>) -> Self {
        Self { condition, body }
    }

    pub fn get_condition(&self) -> &ExprNode {
        &self.condition
    }
    pub fn get_body(&self) -> &Vec<StmtNode> {
        &self.body
    }
}

// 匹配分支，包含模式和对应的语句
#[derive(Debug, Clone)]
pub struct MatchBranch {
    pattern: ExprNode,
    body: Vec<StmtNode>,
}

impl MatchBranch {
    pub fn new(pattern: ExprNode, body: Vec<StmtNode>) -> Self {
        Self { pattern, body }
    }

    pub fn get_pattern(&self) -> &ExprNode {
        &self.pattern
    }

    pub fn get_body(&self) -> &Vec<StmtNode> {
        &self.body
    }
}

impl Stmt {
    pub fn is_noop(&self) -> bool {
        matches!(self, Stmt::Noop)
    }
    pub fn is_fn_call(&self) -> bool {
        match self {
            Stmt::EvalExpr(expr) => expr.is_fn_call(),
            _ => false,
        }
    }
    pub fn is_closure_decl(&self) -> bool {
        match &self {
            Stmt::ValDecl(v) => v.is_closure,
            _ => false,
        }
    }
}
impl NodeTrait for StmtNode {
    fn get_id(&self) -> &str {
        match &self.stmt {
            Stmt::EvalExpr(e) => e.get_id(),
            Stmt::ValDecl(_) => "ValDecl",
            Stmt::VarDecl(_) => "VarDecl",
            Stmt::Assign(_, _) => "Assign",
            Stmt::Return(_) => "Return",
            Stmt::If(_) => "If",
            Stmt::While(_, _) => "While",
            Stmt::ForIn(_, _, _) => "ForIn",
            Stmt::IndexAssign(_, _, _) => "IndexAssign",
            Stmt::Break => "Break",
            Stmt::Continue => "Continue",
            Stmt::Import(_) => "Import",
            Stmt::Noop => "Noop",
            Stmt::Match(_, _) => "Match",
            Stmt::IfLet(_, _, _, _) => "IfLet",
            Stmt::IfConst(_, _, _, _) => "IfConst",
        }
    }

    fn get_data(&self, key: &str) -> Option<Data> {
        match &self.stmt {
            Stmt::ValDecl(v) => v.get_data(key),
            Stmt::EvalExpr(e) => e.get_data(key),
            Stmt::VarDecl(v) => v.get_data(key),
            _ => {
                todo!()
            }
        }
    }

    fn set_data(&mut self, key: &str, value: Data) {
        match &mut self.stmt {
            Stmt::ValDecl(v) => {
                if key == "name" {
                    v.set_name(value.as_str().unwrap());
                }
            }
            Stmt::EvalExpr(e) => e.set_data(key, value),
            Stmt::VarDecl(v) => {
                if key == "name" {
                    v.set_name(value.as_str().unwrap());
                }
            }
            _ => {
                todo!()
            }
        }
    }

    fn get_children(&self) -> Vec<&dyn NodeTrait> {
        todo!()
    }

    fn get_mut_children(&mut self) -> Vec<&mut dyn NodeTrait> {
        match &mut self.stmt {
            Stmt::ValDecl(v) => {
                let mut children = vec![];
                let default = v.get_mut_default();
                match default {
                    Some(default) => {
                        children.push(default as &mut dyn NodeTrait);
                        children
                    }
                    None => vec![],
                }
            }
            Stmt::EvalExpr(e) => e.get_mut_children(),
            Stmt::ForIn(_, target, block) => {
                let mut children = vec![];
                children.push(&mut **target as &mut dyn NodeTrait);
                for stmt in block.iter_mut() {
                    children.push(stmt as &mut dyn NodeTrait)
                }
                children
            }
            Stmt::Assign(target, value) => {
                vec![&mut **target, &mut **value]
            }
            Stmt::VarDecl(v) => {
                let mut children = vec![];
                let default = v.get_mut_default();
                match default {
                    Some(default) => {
                        children.push(default as &mut dyn NodeTrait);
                        children
                    }
                    None => vec![],
                }
            }
            t => {
                dbg!(t);
                todo!()
            }
        }
    }

    fn get_extra(&self) -> &HashMap<String, Box<dyn Any>> {
        todo!()
    }
}
impl StmtNode {
    pub fn new(stmt: Stmt, pos: Position) -> Self {
        Self { stmt, pos }
    }

    pub fn get_stmt(&self) -> &Stmt {
        &self.stmt
    }

    pub fn position(&self) -> Position {
        self.pos.clone()
    }

    pub fn is_noop(&self) -> bool {
        self.stmt.is_noop()
    }
    pub fn is_fn_call(&self) -> bool {
        self.stmt.is_fn_call()
    }
    pub fn get_fn_call_name(&self) -> Option<String> {
        match &self.stmt {
            Stmt::EvalExpr(expr) => match expr.get_expr() {
                Expr::FnCall(fn_call) => Some(fn_call.name.clone()),
                _ => None,
            },
            _ => None,
        }
    }
    pub fn get_mut_stmt(&mut self) -> &mut Stmt {
        &mut self.stmt
    }
    pub fn set_fn_call_name(&mut self, name: String) {
        if let Stmt::EvalExpr(expr) = &mut self.stmt {
            if let Expr::FnCall(ref mut fn_call) = expr.get_expr_mut() {
                fn_call.name = name;
            }
        }
    }
    pub fn is_val_decl(&self) -> bool {
        matches!(&self.stmt, Stmt::ValDecl(_))
    }
}
