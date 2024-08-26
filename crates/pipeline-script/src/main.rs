use pipeline_script::core::engine::Engine;


pub mod lexer;
pub mod parser;
pub mod core;
pub mod compiler;
mod context;
mod plugin;
mod preprocessor;

fn main() {
    // let ptr = Global::pointer_type(Global::i8_type());
    // println!("{:?}", ptr);
    // let r = ptr.get_element_type();
    // println!("{:?}", r);
    let mut e = Engine::default();
    // e.enable_ast_debug();
    // e.use_plugin::<TaskPlugin>();
    // e.use_plugin::<TestPlugin>();
    // e.compile("import buildin");
    // e.compile("import test");
    e.run_file("main.ppl");

    // e.run_script(r#"
    //     // fun add(a:Int32,b:Int32):Int32=a+b
    //     // val a = [10,34]
    //     extern fun printf(format: *Int8,*args):Int32
    //     struct Any{
    //         typeID:Int32
    //     }
    //     printf("%s","Hello,world!")
    //     // val a = "Hello,world!"
    //     // val b = 129
    //     // printf("%s",a)
    //     // puts(a)
    //     // print(b,5)
    // "#)
}

