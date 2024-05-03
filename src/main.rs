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
    let script=r#"
        class Person(name:String,age:Int){
            fun sayYes(){
                println(this.name+",Yes")
                this.name = "李四"
            }
            fun sayHello(){
                println("你好，我是"+this.name)
            }
            fun toString():String{
                return "Person("+this.name+")"
            }
        }
        val p = Person("张三",18)
        print(p)
    "#;
    e.run(script)
}