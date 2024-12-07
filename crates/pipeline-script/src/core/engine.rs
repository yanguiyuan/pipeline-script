use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::{ Parser};
use crate::preprocessor::r#type::TypePreprocessor;
use std::ffi::c_void;
use std::fs;
use std::path::Path;
use wrap_llvm::value::LLVMValue;

pub struct Engine {}
impl Default for Engine {
    fn default() -> Self {
        Self {}
    }
}

impl Engine {
    pub fn run_file(&mut self, path: impl AsRef<Path>) {
        let r = fs::read_to_string(path.as_ref()).unwrap();
        self.run_script(r);
    }
    pub fn run_script(&mut self, script: impl AsRef<str>) {
        // 分词
        let lexer = Lexer::from_script("main", script);
        // 解析
        let mut parser = Parser::new(lexer);
        let module = parser.parse().unwrap();
        let mut type_preprocessor = TypePreprocessor::new();
        let module = type_preprocessor.process_module(&module);
        dbg!(&module);
        //编译
        let mut compiler = Compiler::new(module.clone());
        let llvm_module = compiler.compile();
        let f1 = llvm_module.get_function("println").unwrap();
        let f2 = llvm_module.get_function("print").unwrap();
        let f3 = llvm_module.get_function("append").unwrap();
        let f4 = llvm_module.get_function("len").unwrap();
        llvm_module.dump();
        let executor = llvm_module.create_executor().unwrap();
        executor.add_global_mapping(f1.as_ref(), crate::core::buidin::println as *mut c_void);
        executor.add_global_mapping(f2.as_ref(), crate::core::buidin::print as *mut c_void);
        executor.add_global_mapping(f3.as_ref(), crate::core::buidin::append as *mut c_void);
        executor.add_global_mapping(f4.as_ref(), crate::core::buidin::len as *mut c_void);
        executor.run_function("$main.__main__", &mut []);
    }
    pub fn run_function(&mut self, _: impl Into<String>, _: Vec<LLVMValue>) {}
}
