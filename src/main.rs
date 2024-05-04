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
    // e.enable_ast_debug();
    let script = "
        fun main(){
            println(\"Hello,World\"+1+2)
        }
    ";
    e.run(script.to_owned() + "main()")
}
