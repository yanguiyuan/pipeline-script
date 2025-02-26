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
        self.engine.run_file(self.path.clone())
    }
    pub fn set_entry_file(mut self, path: impl AsRef<Path>) -> Self {
        self.path = path.as_ref().to_path_buf();
        self
    }
}
