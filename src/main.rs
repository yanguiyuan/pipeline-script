mod token;
mod lexer;
mod position;
mod plugin;
mod engine;
mod module;
mod error;
mod stmt;
mod expr;
mod context;
mod types;
mod parser;
use crate::engine::Engine;
fn main() {
    let mut e=Engine::default();
    e.compile(r#"
        import hello
        "#);
    e.run(r#"
        hello()
        "#);
}