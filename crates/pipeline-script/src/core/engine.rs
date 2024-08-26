use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::fs;
use std::path::Path;
use wrap_llvm::global::Global;
use wrap_llvm::value::LLVMValue;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::preprocessor::r#type::TypePreprocessor;

pub struct Engine{

}
impl Default for Engine{
    fn default() -> Self {
        Self{}
    }
}
#[repr(C)]
struct Any{
    id:i32,
    ptr:*mut i8,
}
extern "C" fn println(obj:Any){
    match obj.id {
        7=>{
            let s = unsafe { CStr::from_ptr(obj.ptr as *const c_char) };
            println!("{}",s.to_str().unwrap());
        }
        4=>{
            let v =obj.ptr as i64;
            println!("{}",v);
        }
        3=>{
            let value = obj.ptr as *mut i32;
            unsafe { println!("{}", *value); }
        }
        t=>todo!("{t}")
    }

}
impl Engine{
    pub fn run_file(&mut self, path: impl AsRef<Path>) {
        let r = fs::read_to_string(path.as_ref()).unwrap();
        self.run_script(r);
    }
    pub fn run_script(&mut self, script: impl AsRef<str>) {
        // 分词
        let lexer = Lexer::from_script("main",script);
        // 解析
        let mut parser = Parser::new(lexer);
        let module = parser.parse().unwrap();
        dbg!(module);
        let mut type_preprocessor = TypePreprocessor::new();
        let module = type_preprocessor.process_module(&module);
        dbg!(&module);
        //编译
        let mut compiler = Compiler::new(module.clone());
        let mut llvm_module = compiler.compile();
        let f1 = llvm_module.get_function("println").unwrap();
        llvm_module.dump();
        let executor = llvm_module.create_executor().unwrap();
        executor.add_global_mapping(f1.as_ref(),println as *mut c_void);
        executor.run_function("$main.__main__",&mut []);
    }
    pub fn run_function(&mut self, name: impl Into<String>, args: Vec<LLVMValue>) {

    }

}