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
        fun Int.max(o:Int):Int{
            if this>o{return this.clone()
            }else{return o.clone()}
        }
        "#);
    e.run(r#"
        let a=3.max(2)
        print(a)
        "#);
}