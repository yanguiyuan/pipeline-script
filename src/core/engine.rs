use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::postprocessor::r#type::TypePostprocessor;
use crate::preprocessor::{FormatStringPreprocessor, ImportPreprocessor, Preprocessor};
use std::collections::{HashMap, VecDeque};
use std::ffi::c_void;
use std::fs;
use std::path::Path;
use crate::ast::visit::{ VisitResult, Visitor};

pub struct Engine {
    prelude_scripts: Vec<String>,
    preprocessors: Vec<Box<dyn Preprocessor>>,
    function_map: HashMap<String, *mut c_void>,
    visitors: Vec<Box<dyn Visitor>>
}
impl Default for Engine {
    fn default() -> Self {
        Self {
            prelude_scripts: vec![],
            preprocessors: vec![
                Box::new(ImportPreprocessor::default()),
            ],
            visitors: vec![],
            function_map: HashMap::new(),
        }
    }
}

impl Engine {
    pub fn run_file(&mut self, path: impl AsRef<Path>) {
        let r = fs::read_to_string(path.as_ref()).unwrap();
        self.run_script(r);
    }
    pub fn register_external_function(&mut self, name: impl Into<String>, f: *mut c_void) {
        self.function_map.insert(name.into(), f);
    }
    pub fn register_visitor(&mut self, visitor: impl Visitor +'static) {
        self.visitors.push(Box::new(visitor));
    }
    pub fn register_preprocessor(&mut self, preprocessor: impl Preprocessor + 'static) {
        self.preprocessors.push(Box::new(preprocessor));
    }
    pub fn register_preluded_scripts(&mut self, scripts: &[&str]) {
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
        let mut parser = Parser::new(lexer);
        let module = parser.parse().unwrap();
        let mut type_preprocessor = TypePostprocessor::new();
        let module = type_preprocessor.process_module(&module);
        // dbg!(&module);
        let mut ast = module.to_ast();
        for i in self.visitors.iter_mut() {
            let mut queue = VecDeque::new();
            queue.push_back(&mut ast);
            while !queue.is_empty() {
                let mut skip = false;
                let node = queue.pop_front().unwrap();
                if i.match_id(node.get_id()){
                    let result =i.visit(node);
                    if let VisitResult::Break = result {
                        break;
                    }
                    if let VisitResult::Skip = result {
                        skip = true;
                    }
                }
                if skip {
                    continue;
                }
                for i in node.get_mut_children() {
                    queue.push_back(i);
                }
            }
        }
        dbg!(&ast);
        //编译
        // let mut compiler = Compiler::new(module.clone());
        // let llvm_module = compiler.compile();
        // llvm_module.dump();
        // let executor = llvm_module.create_executor().unwrap();
        // for (name, f) in &self.function_map {
        //     let func = llvm_module.get_function(name).unwrap();
        //     executor.add_global_mapping(func.as_ref(), *f);
        // }
        // executor.run_function("$main.__main__", &mut []);
    }
}

