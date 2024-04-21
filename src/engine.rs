use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use crate::context::{Context, Scope};
use crate::error::PipelineResult;
use crate::expr::Expr;
use crate::lexer::{Lexer, TokenStream};

use crate::module::{Class, Function, Module};
use crate::parser::PipelineParser;
use crate::plugin::builtin::BuiltinPlugin;
use crate::plugin::Plugin;
use crate::stmt::Stmt;
use crate::types::{Dynamic, Value};

pub enum AST{
    Stmt(Stmt),
    Expr(Expr),
    Block(Vec<Stmt>),
    Function(Function),
    Class(Class),
    Module(Module)
}
#[derive(Debug)]
pub struct Engine{
    ctx:Context,
    has_run_main_block:bool
}
fn parse_expr(token_stream: TokenStream)->PipelineResult<Expr>{
    todo!()
}
impl Engine{
    pub fn register_module(&mut self,module:Module){

    }
    pub fn register_into_main_module(&mut self,module: Module){
        let binding = self.ctx.get_module();
        let mut main_module=binding.write().unwrap();
        for (function_name,function) in module.get_functions(){
            main_module.register_function(function_name,function);
        }
    }
    pub fn get_context(&self)->Context{
        self.ctx.clone()
    }
    pub fn compile(&mut self,script:impl AsRef<str>){
        let lexer=Lexer::from_script("main",script);
        let m=self.ctx.get_module();
        let mut parser=PipelineParser::new(lexer,m.clone());
        let block=parser.parse_stmt_blocks().unwrap();
        m.write().unwrap().push_block(block);
    }
    pub fn compile_expr(&mut self,script:impl AsRef<str>)->PipelineResult<Expr>{
        let lexer=Lexer::from_script("main",script);
        let m=self.ctx.get_module();
        let mut parser=PipelineParser::new(lexer,m.clone());
        parser.parse_expr()
    }pub fn compile_stmt(&mut self,script:impl AsRef<str>)->PipelineResult<Stmt>{
        let lexer=Lexer::from_script("main",script);
        let m=self.ctx.get_module();
        let mut parser=PipelineParser::new(lexer,m.clone());
        parser.parse_stmt()
    }

    pub fn eval(&mut self,script:impl AsRef<str>)->PipelineResult<Value>{
        let lexer=Lexer::from_script("main",script);
        let m=self.ctx.get_module();
        let mut parser=PipelineParser::new(lexer,m.clone());
        let block=parser.parse_stmt_blocks().unwrap();
        let m=self.ctx.get_module();
        if !self.has_run_main_block{
            m.read().unwrap().run_block(&mut self.ctx);
            self.has_run_main_block=true;
        }
        let mut r=Value::Immutable(Dynamic::Unit);
        for i in &block{
            r=self.ctx.eval_stmt(i)?;
        }
        return Ok(r)
    }
    pub fn run(&mut self,script:impl AsRef<str>){
        self.eval(script).unwrap();
    }
    pub fn compile_module(&mut self,module_name:impl Into<String>,script:impl Into<String>)->PipelineResult<Module>{ todo!() }
    pub fn eval_expr(&mut self)->PipelineResult<Value>{
        todo!()
    }
    pub fn eval_stmt(&mut self)->PipelineResult<Value>{todo!()}
    pub fn use_plugin<T:Plugin>(&mut self){
        T::apply(self)
    }

}
impl Default for Engine {
    fn default() -> Self {
        let mut e=Self{
            ctx:Context::background(),
            has_run_main_block:false
        };
        e.use_plugin::<BuiltinPlugin>();
        return e;

    }
}