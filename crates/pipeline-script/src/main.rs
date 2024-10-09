
use crate::core::app::App;

pub mod lexer;
pub mod parser;
pub mod core;
pub mod compiler;
mod context;
mod plugin;
mod preprocessor;

fn main() {
   App::new()
       .add_file("main.ppl")
       .run();

}

