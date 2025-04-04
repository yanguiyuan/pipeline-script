use crate::ast::r#type::Type;
use crate::compiler::Compiler;
use crate::context::Context;
use crate::core::builtin::println;
use crate::lexer::Lexer;
use crate::llvm::context::LLVMContext;
use crate::llvm::value::LLVMValue;
use crate::parser::Parser;
use crate::postprocessor::r#type::TypePostprocessor;
use crate::postprocessor::{run_visitor, DynVisitor, Stage, Visitor};
use crate::preprocessor::{ImportPreprocessor, Preprocessor};
use std::collections::HashMap;
use std::ffi::c_void;
use std::fs;
use std::path::{Path, PathBuf};

pub struct Engine {
    prelude_scripts: Vec<String>,
    preprocessors: Vec<Box<dyn Preprocessor>>,
    function_map: HashMap<String, *mut c_void>,
    visitors: Vec<Box<dyn DynVisitor>>,
    builtin_symbol_types: HashMap<String, Type>,
    builtin_symbol: HashMap<String, LLVMValue>,
    // builtin_llvm_functions: HashMap<String, LLVMValue>,
    pub test_llvm: bool,
    pub test_llvm_files: Vec<PathBuf>,
}
impl Default for Engine {
    fn default() -> Self {
        Self {
            prelude_scripts: vec![],
            preprocessors: vec![Box::new(ImportPreprocessor)],
            visitors: vec![],
            function_map: HashMap::new(),
            builtin_symbol_types: HashMap::new(),
            test_llvm: false,
            test_llvm_files: vec![],
            builtin_symbol: HashMap::new(),
        }
    }
}

impl Engine {
    pub fn run_llvm_file(&self, path: impl AsRef<Path>) {
        let r = fs::read_to_string(path.as_ref()).unwrap();
        let ctx = LLVMContext::with_jit();
        let module = ctx.parse_ir(&r).unwrap();
        let executor = module.create_executor().unwrap();
        let println_function = module.get_function_ref("println");
        executor.add_global_mapping(println_function, println as *mut c_void);
        executor.run_function("$Module.main", &mut []);
    }
    pub fn run_file(&mut self, path: impl AsRef<Path>) {
        let r = fs::read_to_string(path.as_ref()).unwrap();
        self.run_script(r);
    }
    pub fn set_test_llvm(&mut self, test: bool) {
        self.test_llvm = test;
    }
    pub fn add_test_llvm_file(&mut self, path: impl AsRef<Path>) {
        self.test_llvm_files.push(path.as_ref().to_path_buf());
    }
    pub fn register_external_function(&mut self, name: impl Into<String>, f: *mut c_void) {
        self.function_map.insert(name.into(), f);
    }
    pub fn register_builtin_symbol_type(&mut self, name: impl Into<String>, ty: Type) {
        self.builtin_symbol_types.insert(name.into(), ty);
    }
    pub fn register_builtin_symbol(&mut self, name: impl Into<String>, value: LLVMValue) {
        self.builtin_symbol.insert(name.into(), value);
    }
    pub fn register_visitor(&mut self, visitor: impl Visitor + 'static) {
        self.visitors.push(Box::new(visitor));
    }
    pub fn register_preprocessor(&mut self, preprocessor: impl Preprocessor + 'static) {
        self.preprocessors.push(Box::new(preprocessor));
    }
    pub fn register_precluded_scripts(&mut self, scripts: &[&str]) {
        for script in scripts {
            self.prelude_scripts.push(script.to_string());
        }
    }
    pub fn run_script(&mut self, script: impl AsRef<str>) {
        let mut s = script.as_ref().to_string();
        for i in self.prelude_scripts.iter() {
            s = format!("{}\n{}", i, s)
        }
        for preprocessor in &mut self.preprocessors {
            s = preprocessor.process(&s)
        }
        // 分词
        let lexer = Lexer::from_script("main", s);
        // 解析
        let ctx = Context::background();
        let ctx = Context::with_module_slot_map(&ctx, Default::default());
        let mut parser = Parser::new(lexer, &ctx);
        let module_key = parser.parse(&ctx).unwrap();
        ctx.apply_mut_module(module_key, |module| {
            for i in self
                .visitors
                .iter()
                .filter(|i| i.stage() == Stage::BeforeTypeInfer)
            {
                run_visitor(module, &**i)
            }
        });
        let mut type_preprocessor = TypePostprocessor::new();
        for (name, ty) in self.builtin_symbol_types.iter() {
            type_preprocessor.register_builtin_symbol_type(name, ty.clone());
        }
        let module = type_preprocessor.process(module_key, &ctx);
        dbg!(&module);
        let mut compiler = Compiler::new(module.clone());
        for (name, value) in self.builtin_symbol.iter() {
            compiler.register_builtin_symbol(name, value.clone());
        }
        ctx.register_module(module);
        ctx.apply_mut_module(module_key, |module| {
            for i in self
                .visitors
                .iter()
                .filter(|i| i.stage() == Stage::AfterTypeInfer)
            {
                run_visitor(module, &**i)
            }
        });

        //编译
        let llvm_module = compiler.compile(&ctx);
        let llvm_module = llvm_module.read().unwrap();
        llvm_module.verify_with_debug_info();
        llvm_module.dump();
        // match llvm_module.to_assembly("x86_64-pc-windows-msvc"){
        //     Ok(s)=>println!("=====assembly=====>\n{}",s),
        //     Err(e)=>println!("{}",e)
        // }
        let executor = llvm_module.create_executor().unwrap();
        for (name, f) in &self.function_map {
            let func = llvm_module.get_function(name).unwrap();
            executor.add_global_mapping(func.as_ref(), *f);
        }
        println!("\n>>>Output>>>:");
        executor.run_function("$Module.main", &mut []);
    }
}
