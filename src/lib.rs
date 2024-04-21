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


#[cfg(test)]
mod tests {
    use std::sync::{Arc, RwLock};
    use crate::engine::Engine;
    // use crate::engine::Engine;
    use crate::lexer::Lexer;
    use crate::module::Module;
    use crate::parser::PipelineParser;
    // use crate::module::Module;
    // use crate::parser::PipelineParser;

    #[test]
    fn test_lexer(){
        let mut lexer=Lexer::from_script("main",r#"
fun add(a:Int,b:Int){
    return a+b
}
        "#);
        for (token,pos) in lexer.clone(){
            println!("{:?}",pos);
            let line1=lexer.line(pos.row);
            println!("{}",line1);
            let mut p=String::from(' ');
            let mut p=p.repeat(pos.col-1);
            p.push('↑');
            println!("{}",p);
        }
    }
    #[test]
    fn test_parser(){
        let lexer=Lexer::from_script("main",r#"
        val a=123456;
        println(a);
        fun test():Unit{
            println("Hello,Word")
        }
        "#);
        let mut m=Module::new("main");
        let mut parser=PipelineParser::new(lexer,Arc::new(RwLock::new(m)));
        let s=parser.parse_stmt_blocks().unwrap();
        // let f=parser.get_module();
        println!("{:?}",s);
        // println!("{:?}",m);
    }
    #[test]
    fn test_engine(){
        let mut e=Engine::default();
        e.compile(r#"let a=Object{
            name:"张三",
            age:17,
            child:Object{
                name:"小孩",
                age:12
            }
        }"#);
        // e.run(r#"a.age=10"#);
        let r=e.eval("a").unwrap();
        println!("{r}");
        e.run("println(a.age+3)");
    }

    #[test]
    fn it_works() {

    }
}
