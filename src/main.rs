use crate::core::app::App;

pub mod compiler;
mod context;
mod core;
pub mod lexer;
mod llvm;
pub mod parser;
mod plugin;
mod preprocessor;

fn main() {
    App::new().add_file("main.ppl").run();
}
