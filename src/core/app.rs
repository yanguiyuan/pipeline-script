use crate::core::engine::Engine;
use crate::plugin::Plugin;
use crate::postprocessor::Visitor;
use std::ffi::c_void;
use std::path::{Path, PathBuf};

pub struct App {
    engine: Engine,
    path: PathBuf,
}
impl App {
    pub fn new() -> Self {
        Self {
            engine: Engine::default(),
            path: PathBuf::default(),
        }
    }
    #[allow(unused)]
    pub fn register_external_function(mut self, name: &str, func: *mut c_void) -> Self {
        self.engine.register_external_function(name, func);
        self
    }
    pub fn set_test_llvm(mut self, test: bool) -> Self {
        self.engine.set_test_llvm(test);
        self
    }
    pub fn add_test_llvm_file(mut self, path: impl AsRef<Path>) -> Self {
        self.engine.add_test_llvm_file(path);
        self
    }
    #[allow(unused)]
    pub fn register_visitor(mut self, visitor: impl Visitor + 'static) -> Self {
        self.engine.register_visitor(visitor);
        self
    }
    pub fn add_plugin(mut self, plugin: impl Plugin) -> Self {
        plugin.apply(&mut self.engine);
        self
    }
    pub fn run(&mut self) {
        if self.engine.test_llvm {
            for file in self.engine.test_llvm_files.iter() {            
                self.engine.run_llvm_file(file);
            }
        } else {
            self.engine.run_file(self.path.clone())
        }
    }
    pub fn set_entry_file(mut self, path: impl AsRef<Path>) -> Self {
        self.path = path.as_ref().to_path_buf();
        self
    }
}
