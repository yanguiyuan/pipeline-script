use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use crate::context::{Context, ContextKey, ContextValue, Scope, SourceCode};
use crate::error::{PipelineError, PipelineResult};
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
    has_run_main_block:bool,
    enable_ast_debug:bool

}
fn parse_expr(token_stream: TokenStream)->PipelineResult<Expr>{
    todo!()
}
impl Engine{
    pub fn register_module(&mut self,module:Module){

    }
    pub fn enable_ast_debug(&mut self){
        self.enable_ast_debug = true;
    }
    pub fn register_context_value(&mut self,key:ContextKey,value:ContextValue){
        let ctx=Context::with_value(&self.ctx,key,value);
        self.ctx=ctx;
    }
    pub fn register_into_main_module(&mut self,module: Module){
        let binding = self.ctx.get_module();
        let mut main_module= binding.write().unwrap();
        for (function_name,function) in module.get_functions(){
            main_module.register_function(function_name,function);
        }
        for (_,class) in module.get_classes(){
            main_module.register_class(class.clone())
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
        if self.enable_ast_debug{
            dbg!(block.clone());
        }
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
        self.ctx = Context::with_value(
            &self.ctx,
            ContextKey::SourceCode("main".into()),
            ContextValue::Source(SourceCode::Source(script.as_ref().into()))
        );
        let lexer=Lexer::from_script("main",script);
        let m=self.ctx.get_module();
        let mut parser=PipelineParser::new(lexer,m.clone());
        let block=parser.parse_stmt_blocks().unwrap();
        let m=self.ctx.get_module();
        if !self.has_run_main_block{
            let block=m.read().unwrap().get_block().clone();
            drop(m);
            for i in &block{
                self.ctx.eval_stmt(i)?;
            }
            self.has_run_main_block=true;
        }
        let mut r=Value::Immutable(Dynamic::Unit);
        if self.enable_ast_debug{
            dbg!(block.clone());
        }
        for i in &block{
            r=self.ctx.eval_stmt(i)?;
        }
        return Ok(r)
    }
    pub fn run(&mut self,script:impl AsRef<str>){
        let result = self.eval(script);
        if let Err(e)=result{
            match e {
                PipelineError::FunctionUndefined(i, pos) => {
                    println!("\x1b[31m[错误] 函数'{i}'未定义");
                    let source = self.ctx.get(ContextKey::SourceCode(pos.module_name));
                    let source = source.unwrap().as_source().unwrap();
                    let line = source.get_line(pos.row);
                    println!("{:4}|{}",pos.row,line);
                    let mut p=String::from(' ');
                    let mut p=p.repeat(pos.col-1);
                    let arrow = String::from('↑');
                    let arrow = arrow.repeat(pos.span);
                    p.push_str(&arrow);
                    println!("    |{}\x1b[0m",p);
                }
                PipelineError::VariableUndefined(i, pos) => {
                    println!("\x1b[31m[错误] 变量'{i}'未定义");
                    let source = self.ctx.get(ContextKey::SourceCode(pos.module_name));
                    let source = source.unwrap().as_source().unwrap();
                    let line = source.get_line(pos.row);
                    println!("{:4}|{}",pos.row,line);
                    let mut p=String::from(' ');
                    let mut p=p.repeat(pos.col-1);
                    let arrow = String::from('↑');
                    let arrow = arrow.repeat(pos.span);
                    p.push_str(&arrow);
                    println!("    |{}\x1b[0m",p);
                }
                PipelineError::ExpectedType(_) => {}
                PipelineError::UnexpectedType(_) => {}
                PipelineError::UnexpectedToken(_, _) => {}
                PipelineError::UnusedKeyword(_) => {}
                PipelineError::UnknownModule(_) => {}
                PipelineError::UndefinedOperation(_) => {}
                PipelineError::MismatchedType(need,actual,pos)=>{
                    println!("\x1b[31m[错误] 不匹配的类型,期望'{need}',实际'{actual}'");
                    let source = self.ctx.get(ContextKey::SourceCode(pos.module_name));
                    let source = source.unwrap().as_source().unwrap();
                    let line = source.get_line(pos.row);
                    println!("{:4}|{}",pos.row,line);
                    let mut p=String::from(' ');
                    let mut p=p.repeat(pos.col-1);
                    let arrow = String::from('↑');
                    let arrow = arrow.repeat(pos.span);
                    p.push_str(&arrow);
                    println!("    |{}\x1b[0m",p);
                }
            }
        }
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
            has_run_main_block:false,
            enable_ast_debug:false
        };
        e.use_plugin::<BuiltinPlugin>();
        return e;

    }
}