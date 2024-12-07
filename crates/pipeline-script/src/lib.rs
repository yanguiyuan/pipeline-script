mod compiler;
pub mod core;
pub mod lexer;
pub mod parser;
pub mod plugin;

mod context;
pub mod preprocessor;

#[cfg(test)]
mod tests {
    use crate::core::engine::Engine;
    // use crate::engine::Engine;
    use crate::lexer::Lexer;
    // use crate::module::Module;
    // use crate::parser::PipelineParser;

    #[test]
    fn test_lexer() {
        let lexer = Lexer::from_script(
            "main",
            r#"
fun add(a:Int,b:Int){
    return a+b
}
        "#,
        );
        for (_, pos) in lexer.clone() {
            println!("{:?}", pos);
            let line1 = lexer.line(pos.row);
            println!("{}", line1);
            let p = String::from(' ');
            let mut p = p.repeat(pos.col - 1);
            p.push('↑');
            println!("{}", p);
        }
    }
    #[test]
    fn test_parser() {
        // let lexer = Lexer::from_script(
        //     "main",
        //     r#"
        // val a=123456;
        // println(a);
        // fun test():Unit{
        //     println("Hello,Word")
        // }
        // "#,
        // );
        // let m = Module::new("main");
        // let mut parser = PipelineParser::new(lexer, Arc::new(RwLock::new(m)));
        // let s = parser.parse_stmt_blocks().unwrap();
        // let f=parser.get_module();
        // println!("{:?}", s);
        // println!("{:?}",m);
    }
    #[test]
    fn test_engine() {
        let mut e = Engine::default();
        e.run_script(
            r#"
        fun range(n:Int):Array{
            let i=0
            let a=[]
            while i<n{
                a.append(i.clone())
                i=i+1
            }
            return a
        }
        let a=readInt()
        print(a)
        "#,
        );
    }

    #[test]
    fn it_works() {}
}
