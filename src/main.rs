use crate::core::app::App;

pub mod compiler;
mod context;
mod core;
pub mod lexer;
pub mod parser;
mod plugin;
mod preprocessor;
mod llvm;

fn main() {
    App::new().add_file("main.ppl").run();
}
