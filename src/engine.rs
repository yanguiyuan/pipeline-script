use crate::context::{Context, ContextKey, ContextValue, SourceCode};
use crate::error::{PipelineError, PipelineResult};
use crate::expr::Expr;
use crate::lexer::Lexer;
use std::fs;
use std::path::Path;

use crate::module::Module;
use crate::parser::PipelineParser;
use crate::plugin::builtin::BuiltinPlugin;
use crate::plugin::Plugin;
use crate::position::Position;
use crate::stmt::Stmt;
use crate::types::{Dynamic, Value};

#[derive(Debug)]
pub struct Engine {
    ctx: Context,
    #[allow(unused)]
    has_run_main_block: bool,
    enable_ast_debug: bool,
}
impl Engine {
    #[allow(unused)]
    pub fn register_module(&mut self, module: Module) {}
    #[allow(unused)]
    pub fn enable_ast_debug(&mut self) {
        self.enable_ast_debug = true;
    }
    pub fn register_context_value(&mut self, key: ContextKey, value: ContextValue) {
        let ctx = Context::with_value(&self.ctx, key, value);
        self.ctx = ctx;
    }
    pub fn register_into_main_module(&mut self, module: Module) {
        let binding = self.ctx.get_module();
        let mut main_module = binding.write().unwrap();
        for (function_name, function) in module.get_functions() {
            main_module.register_function(function_name, function);
        }
        for class in module.get_classes().values() {
            main_module.register_class(class.clone())
        }
    }
    #[allow(unused)]
    pub fn get_context(&self) -> Context {
        self.ctx.clone()
    }
    #[allow(unused)]
    pub fn compile(&mut self, script: impl AsRef<str>) {
        let lexer = Lexer::from_script("main", script);
        let m = self.ctx.get_module();
        let mut parser = PipelineParser::new(lexer, m.clone());
        let block = parser.parse_stmt_blocks().unwrap();
        if self.enable_ast_debug {
            dbg!(block.clone());
        }
        m.write().unwrap().push_block(block);
    }
    #[allow(unused)]
    pub fn compile_expr(&mut self, script: impl AsRef<str>) -> PipelineResult<Expr> {
        let lexer = Lexer::from_script("main", script);
        let m = self.ctx.get_module();
        let mut parser = PipelineParser::new(lexer, m.clone());
        parser.parse_expr()
    }
    #[allow(unused)]
    pub fn compile_stmt(&mut self, script: impl AsRef<str>) -> PipelineResult<Stmt> {
        let lexer = Lexer::from_script("main", script);
        let m = self.ctx.get_module();
        let mut parser = PipelineParser::new(lexer, m.clone());
        parser.parse_stmt()
    }
    #[allow(unused)]
    pub fn eval(&mut self, script: impl AsRef<str>) -> PipelineResult<Value> {
        self.ctx = Context::with_value(
            &self.ctx,
            ContextKey::SourceCode("main".into()),
            ContextValue::Source(SourceCode::Source(script.as_ref().into())),
        );
        let lexer = Lexer::from_script("main", script);
        let m = self.ctx.get_module();
        let mut parser = PipelineParser::new(lexer, m.clone());
        let block = parser.parse_stmt_blocks().unwrap();
        let m = self.ctx.get_module();
        if !self.has_run_main_block {
            let block = m.read().unwrap().get_block().clone();
            drop(m);
            for i in &block {
                self.ctx.eval_stmt(i)?;
            }
            self.has_run_main_block = true;
        }
        let mut r = Value::with_immutable(Dynamic::Unit);
        if self.enable_ast_debug {
            dbg!(block.clone());
        }
        for i in &block {
            r = self.ctx.eval_stmt(i)?;
        }
        Ok(r)
    }
    fn display_source_line(&self, pos: &Position) {
        let source = self
            .ctx
            .get(ContextKey::SourceCode(pos.module_name.clone()));
        let source = source.unwrap().as_source().unwrap();
        let line = source.get_line(pos.row);
        println!("{:4}|{}", pos.row, line);
        let p = String::from(' ');
        let mut p = p.repeat(pos.col - 1);
        let arrow = String::from('↑');
        let arrow = arrow.repeat(pos.span);
        p.push_str(&arrow);
        println!("    |{}\x1b[0m", p);
    }
    fn handle_err(&self, e: PipelineError) {
        match e {
            PipelineError::FunctionUndefined(i, pos) => {
                println!("\x1b[31m[错误] 函数'{i}'未定义");
                self.display_source_line(&pos)
            }
            PipelineError::AssignToImmutableVariable(variable, pos) => {
                println!("\x1b[31m[错误] 无法对不可变变量'{variable}'赋值");
                self.display_source_line(&pos)
            }
            PipelineError::MapKeyNotExist(m, prop, pos) => {
                println!("\x1b[31m[错误] Map容器'{m}'不存在键'{prop}'");
                self.display_source_line(&pos)
            }
            PipelineError::VariableUndefined(i, pos) => {
                println!("\x1b[31m[错误] 变量'{i}'未定义");
                self.display_source_line(&pos)
            }
            PipelineError::ExpectedType(_) => {}
            PipelineError::UnexpectedType(_) => {}
            PipelineError::UnexpectedToken(_, _) => {}
            PipelineError::UnusedKeyword(_) => {}
            PipelineError::UnknownModule(_) => {}
            PipelineError::UndefinedOperation(_) => {}
            PipelineError::MismatchedType(need, actual, pos) => {
                println!("\x1b[31m[错误] 不匹配的类型,期望'{need}',实际'{actual}'");
                self.display_source_line(&pos)
            }
        }
    }
    #[allow(unused)]
    pub fn run(&mut self, script: impl AsRef<str>) {
        let result = self.eval(script);
        if let Err(e) = result {
            self.handle_err(e)
        }
    }
    #[allow(unused)]
    pub fn run_file(&mut self, path: impl AsRef<Path>) {
        let r = fs::read_to_string(path.as_ref()).unwrap();
        self.run(r);
    }
    #[allow(unused)]
    pub fn compile_module(
        &mut self,
        module_name: impl Into<String>,
        script: impl Into<String>,
    ) -> PipelineResult<Module> {
        todo!()
    }
    #[allow(unused)]
    pub fn eval_stmt(&mut self) -> PipelineResult<Value> {
        todo!()
    }
    pub fn use_plugin<T: Plugin>(&mut self) {
        T::apply(self)
    }
}
impl Default for Engine {
    fn default() -> Self {
        let mut e = Self {
            ctx: Context::background(),
            has_run_main_block: false,
            enable_ast_debug: false,
        };
        e.use_plugin::<BuiltinPlugin>();
        e
    }
}
