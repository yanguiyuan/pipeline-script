use pipeline_script::core::engine::Engine;

use wrap_llvm::context::LLVMContext;
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
//    let mut e = Engine::default();
    // e.enable_ast_debug();
    // e.use_plugin::<TaskPlugin>();
    // e.use_plugin::<TestPlugin>();
    // e.compile("import buildin");
    // e.compile("import test");
//    e.run_file("main.ppl");

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
    let ctx = LLVMContext::with_jit();
        let llvm_ir = r#"
        ; ModuleID = 'main'
        source_filename = "main"

        @0 = private unnamed_addr constant [13 x i8] c"Hello,World!\00", align 1

        ;declare void @println({ i32, ptr } %0)

        define void @"$main.__main__"() {
        entry:
            ;%0 = load ptr, ptr @0, align 8
            %any = alloca { i32, ptr }, align 8
            store { i32, ptr } { i32 7, ptr @0 }, ptr %any, align 8
            ;call void @println(ptr %any)
            ret void
        }
        "#;
        let module = ctx.parse_ir(llvm_ir).expect("解析IR失败");
//        let f1 = module.get_function("println").unwrap();
//        module.dump();
        let exec = module.create_executor().expect("创建执行器失败");

//        exec.add_global_mapping(f1.as_ref(),println as *mut c_void);
        exec.run_function("$main.__main__", &mut []);
}

