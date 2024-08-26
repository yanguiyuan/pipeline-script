
use crate::lexer::position::Position;
use crate::parser::declaration::VariableDeclaration;
use crate::parser::expr::Expr;
use crate::parser::r#type::Type;
use super::expr::ExprNode;

#[derive(Debug, Clone)]
pub struct StmtNode{
    stmt:Stmt,
    pos:Position
}
#[derive(Debug, Clone)]
pub enum Stmt {
    EvalExpr(Box<ExprNode>),
    ValDecl(VariableDeclaration),
    VarDecl(VariableDeclaration),
    Assign(Box<ExprNode>,Box<ExprNode>),
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
    pub fn new(branches: Vec<IfBranchStmt>,else_body: Option<Vec<StmtNode>>)->Self{
        Self{
            branches,else_body
        }
    }
    pub fn set_else_body(&mut self,else_body: Option<Vec<StmtNode>>){
        self.else_body = else_body;
    }
    pub fn set_branches(&mut self,branches: Vec<IfBranchStmt>){
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
    pub fn new( condition: ExprNode,body: Vec<StmtNode>)->Self{
        Self{
            condition,body
        }
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
}

impl StmtNode {
    pub fn new(stmt:Stmt,pos:Position)->Self{
        Self{
            stmt,pos
        }
    }

    pub fn get_stmt(&self)->Stmt{
        self.stmt.clone()
    }
    pub fn is_noop(&self)->bool{
        self.stmt.is_noop()
    }
    pub fn position(&self)->Position{
        self.pos.clone()
    }
}