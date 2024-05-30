mod context;
mod engine;
mod error;
mod expr;
mod lexer;
mod module;
mod parser;
mod plugin;
mod position;
mod stmt;
mod token;
mod types;
use crate::engine::Engine;

fn main() {
    let mut e = Engine::default();
    e.compile("import buildin");
    e.run_file("main.ppl")
}
