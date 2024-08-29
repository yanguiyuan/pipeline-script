use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use crate::context::Context;
use crate::context::key::ContextKey;
use crate::context::value::ContextValue;
use crate::parser::expr::{Argument, Expr, ExprNode};
use crate::parser::module::Module;
use crate::parser::r#type::Type;
use crate::parser::stmt::{IfBranchStmt, Stmt, StmtNode};


pub struct  TypePreprocessor{
}
impl TypePreprocessor {
    pub fn new()->Self{
        Self{}
    }
    pub fn process_module(&mut self,module:&Module)->Module{
        let mut new_module = Module::new(module.get_name());
        let ctx = Context::background();
        let mut symbols = HashMap::new();
        for (name ,f) in module.get_functions(){
            let mut new_f;
            if f.is_extern{
                new_f = f.clone();
            }else{
                let mut symbol_types = HashMap::new();
                for i in f.args(){
                    let ty = i.r#type().unwrap();
                    symbol_types.insert(i.name(),ty);
                }
                let ctx =  Context::with_value(&ctx, ContextKey::SymbolType, ContextValue::SymbolType(Arc::new(RwLock::new(symbol_types))));
                let mut v = vec![];
                for i in f.body(){
                    let stmt = self.process_stmt(i,&ctx);
                    v.push(stmt)
                }
                new_f = f.clone();
                new_f.set_body(v);
            }
            new_module.register_function(&name,new_f);
            symbols.insert(name.clone(),f.get_type());
        }
        let ctx = Context::with_value(&ctx, ContextKey::SymbolType, ContextValue::SymbolType(Arc::new(RwLock::new(symbols))));
        for stmt in module.get_global_block().clone(){
            let stmt =self.process_stmt(&stmt,&ctx);
            new_module.add_stmt(stmt);
        }
        new_module
    }
    fn process_expr(&self,expr:&ExprNode,ctx:&Context)->ExprNode{
        match expr.get_expr(){
            Expr::FnCall(fc)=>{
                let fc_name = fc.name.clone();
                let mut new_fc = fc.clone();
                let mut args = vec![];
                for arg in fc.args.iter(){
                    let mut new_arg = arg.clone();
                    let arg = self.process_expr(&arg.value,ctx);
                    new_arg.value = arg;
                    args.push(new_arg);
                }
                new_fc.args = args;
                let fc_return_type = ctx.get_symbol_type(&fc_name).unwrap().get_function_return_type().unwrap();
                ExprNode::new(Expr::FnCall(new_fc)).with_type(fc_return_type)
            },
            Expr::Binary(op,l,r)=>{
                let l = self.process_expr(&l,ctx);
                let r = self.process_expr(&r,ctx);
                let ty = l.get_type().unwrap();
                if ty == r.get_type().unwrap(){
                    ExprNode::new(Expr::Binary(op,Box::new(l),Box::new(r))).with_type(ty)
                }else{
                    panic!("type mismatch")
                }
            },
            Expr::String(s)=>{
                ExprNode::new(Expr::String(s)).with_type(Type::String)
            }
            Expr::Int(i)=>{
                ExprNode::new(Expr::Int(i)).with_type(Type::Int32)
            },
            Expr::Variable(name)=>{
                let ty = ctx.get_symbol_type(&name).unwrap();
                ExprNode::new(Expr::Variable(name)).with_type(ty.clone())
            },
            Expr::Array(v0)=>{
                let mut v = vec![];
                for i in v0.iter(){
                    let i = self.process_expr(i,ctx);
                    v.push(i)
                }
                ExprNode::new(Expr::Array(v)).with_type(Type::Array(Box::new(Type::Int32)))
            }
            Expr::Index(target,index)=>{
                let target = self.process_expr(&target,ctx);
                let index = self.process_expr(&index,ctx);
                let ty = target.get_type().unwrap();
                let ty = ty.get_element_type().unwrap();
                ExprNode::new(Expr::Index(Box::new(target),Box::new(index))).with_type(ty.clone())

            }
            _ =>expr.clone()
        }
    }
    fn process_stmt(&mut self,stmt:&StmtNode,ctx:&Context)->StmtNode{
        match stmt.get_stmt(){
            Stmt::ValDecl(decl)=>{
                let expect = decl.declaration_type.clone();
                let actual = decl.get_default().unwrap();
                let actual = self.process_expr(actual,ctx);
                let actual_type = actual.get_type().unwrap();
                if expect == None{
                    ctx.set_symbol_type(decl.name.clone(),actual_type.clone());
                    let mut new_decl = decl.clone();
                    new_decl.set_default(actual);
                    new_decl.set_type(actual_type);
                    return StmtNode::new(Stmt::ValDecl(new_decl),stmt.position())
                }
                if expect == Some(actual_type){
                    let mut new_decl = decl.clone();
                    ctx.set_symbol_type(decl.name.clone(),expect.unwrap());
                    new_decl.set_default(actual);
                    StmtNode::new(Stmt::ValDecl(new_decl),stmt.position())
                }else{
                    panic!("type mismatch")
                }
            }
            Stmt::Return(expr)=>{
                let expr = self.process_expr(&expr,ctx);
                StmtNode::new(Stmt::Return(Box::new(expr)),stmt.position())
            }
            Stmt::EvalExpr(expr)=>{
                let expr = self.process_expr(&expr,ctx);
                StmtNode::new(Stmt::EvalExpr(Box::new(expr)),stmt.position())
            }
            Stmt::If(if_stmt)=>{
                let mut branches = vec![];
                for i in if_stmt.get_branches(){
                    let condition = self.process_expr(i.get_condition(),ctx);
                    let body = i.get_body().iter().map(|x|self.process_stmt(x,ctx)).collect::<Vec<_>>();
                    let new_branch = IfBranchStmt::new(condition,body);
                    branches.push(new_branch)
                }
                let else_body = if let Some(else_body) = if_stmt.get_else_body(){
                    Some(else_body.iter().map(|x|self.process_stmt(x,ctx)).collect::<Vec<_>>())
                }else{
                    None
                };
                let mut new_if_stmt = if_stmt.clone();
                new_if_stmt.set_branches(branches);
                new_if_stmt.set_else_body(else_body);
                StmtNode::new(Stmt::If(new_if_stmt),stmt.position())

            }
            _=>stmt.clone()

        }
    }

}