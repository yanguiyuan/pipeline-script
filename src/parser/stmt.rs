use std::collections::HashMap;
use crate::ast::data::Data;
use crate::ast::node::Node;
use super::expr::{Expr, ExprNode};
use crate::lexer::position::Position;
use crate::parser::declaration::VariableDeclaration;

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
    ForIn(String, Option<String>, Box<ExprNode>, Vec<StmtNode>),
    /// 第一个Expr表示的是获取Array或者Map的表达式
    /// 第二个Expr表示的是获取索引的表达式
    /// 第三个Expr表示的是对索引处的赋值
    IndexAssign(Box<ExprNode>, Box<ExprNode>, Box<ExprNode>),
    Break,
    Continue,
    Import(String),
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
}

impl StmtNode {
    pub fn new(stmt: Stmt, pos: Position) -> Self {
        Self { stmt, pos }
    }

    pub fn get_stmt(&self) -> Stmt {
        self.stmt.clone()
    }
    pub fn is_noop(&self) -> bool {
        self.stmt.is_noop()
    }
    pub fn is_fn_call(&self) -> bool {
        self.stmt.is_fn_call()
    }
    pub fn get_fn_call_name(&self)->Option<String>{
        match &self.stmt {
            Stmt::EvalExpr(expr) => {
                match expr.get_expr() {
                    Expr::FnCall(fn_call) => Some(fn_call.name.clone()),
                    _ => None,
                }
            },
            _ => None,
        }
    }
    pub fn get_mut_stmt(&mut self) -> &mut Stmt {
        &mut self.stmt
    }
    pub fn set_fn_call_name(&mut self,name:String){
        match &mut self.stmt {
            Stmt::EvalExpr(expr) => {
                match expr.get_expr_mut() {
                    Expr::FnCall(ref mut fn_call) => {
                        fn_call.name = name;
                    },
                    _ => {}
                }
            },
            _ => {}
        }
    }
    pub fn is_val_decl(&self) -> bool {
        match &self.stmt {
            Stmt::ValDecl(_) => true,
            _ => false,
        }
    }
    pub fn to_ast(&self) -> Node {
        match &self.stmt {
            Stmt::EvalExpr(expr) => expr.to_ast(),
            Stmt::ValDecl(decl) => decl.to_ast(),
            Stmt::VarDecl(decl) => decl.to_ast(),
            Stmt::Assign(lhs, rhs) => {
                let mut children = vec![];
                children.push(lhs.to_ast());
                children.push(rhs.to_ast());
                Node::new(
                    "Assign"
                ).with_children(children)
            },
            Stmt::Return(expr) => {
                let mut children = vec![];
                children.push(expr.to_ast());
                Node::new(
                    "Return"
                ).with_children(children)
            },
            // If=>[IfBranch([Expr,Block]),*Block]
            Stmt::If(if_stmt) => {
                let mut children = vec![];
                for i in if_stmt.get_branches() {
                    let mut branch_children = vec![];
                    branch_children.push(i.get_condition().to_ast());
                    let mut body_children = vec![];
                    for j in i.get_body() {
                        body_children.push(j.to_ast());
                    }
                    let body_node = Node::new(
                        "Block"
                    ).with_children(body_children);
                    branch_children.push(body_node);
                    let branch_node = Node::new(
                        "IfBranch",
                    ).with_children(branch_children);
                    children.push(branch_node);
                }
                if let Some(else_body) = if_stmt.get_else_body() {
                    let mut else_children = vec![];
                    for i in else_body {
                        else_children.push(i.to_ast());
                    }
                    let else_node = Node::new(
                        "Block",
                    ).with_children(else_children);
                    children.push(else_node);
                }
                Node::new(
                    "If",
                ).with_children(children)
            },
            Stmt::While(condition, body) => {
                let mut children = vec![];
                children.push(condition.to_ast());
                let mut body_children = vec![];
                for i in body {
                    body_children.push(i.to_ast());
                }
                let body_node = Node::new(
                    "Block",
                ).with_children(body_children);
                children.push(body_node);
                Node::new(
                    "While",
                ).with_children(children)
            },
            // Stmt::ForIn(ident, range, expr, body) => {
            //     let mut children = vec![];
            //     let mut ident_children = vec![];
            //     ident_children.push(Node::new(
            //         node_manager.register_id("Ident"),
            //         vec![],
            //         vec![Node::new(
            //             node_manager.register_id("String"),
            //             vec![Data::String(ident.clone())],
            //             vec![],
            //         )],
            //     ));
            //     let ident_node = Node::new(
            //         node_manager.register_id("Pattern"),
            //         vec![],
            //         ident_children,
            //     );
            //     children.push(ident_node);
            //     if let Some(range) = range {
            //         let mut range_children = vec![];
            //         range_children.push(Node::new(
            //             node_manager.register_id("Ident"),
            //             vec![],
            //             vec![Node::new(
            //                 node_manager.register_id("String"),
            //                 vec![Data::String(range.clone())],
            //                 vec![],
            //             )],
            //         ));
            //         let range_node = Node::new(
            //             node_manager.register_id("Pattern"),
            //             vec![],
            //             range_children,
            //         );
            //         children.push(range_node);
            //     }
            //     children.push(expr.to_ast(node_manager));
            //     let mut body_children = vec![];
            //     for i in body {
            //         body_children.push(i.to_ast(node_manager));
            //     }
            //     let body_node = Node::new(
            //         node_manager.register_id("Block"),
            //         vec![],
            //         body_children,
            //     );
            //     children.push(body_node);
            //     Node::new(
            //         node_manager.register_id("ForIn"),
            //         vec![],
            //         children,
            //     )
            // },
            Stmt::IndexAssign(lhs, index, rhs) => {
                let mut children = vec![];
                children.push(lhs.to_ast());
                children.push(index.to_ast());
                children.push(rhs.to_ast());
                Node::new(
                    "IndexAssign",
                ).with_children(children)
            },
            Stmt::Break => {
                Node::new(
                    "Break",
                )
            },
            Stmt::Continue => {
                Node::new(
                    "Continue",
                )
            },
            Stmt::Import(path) => {
                let mut data = HashMap::new();
                data.insert("path".to_string(), Data::String(path.clone()));
                Node::new(
                    "Import",
                ).with_data(data)
            },
            Stmt::Noop => {
                Node::new("Noop")
            },
            _=>panic!("Unknown stmt: {:?}", self),
        }
    }
    pub fn position(&self) -> Position {
        self.pos.clone()
    }
}
