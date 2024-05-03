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
use crate::lexer::Lexer;

fn main() {
    let mut e=Engine::default();
    // e.enable_ast_debug();
    let script="
        class Solution(){
            fun display(){
                for i in range(0,10,2){
                   println(i)
                }
            }
        }
        val p=Solution()
        p.display()
    ";
    e.run(script)
}