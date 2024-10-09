use std::path::{Path, PathBuf};
use crate::core::engine::Engine;

pub struct App{
    engine:Engine,
    path:Vec<PathBuf>
}

impl App{
    pub fn new() -> Self {
        Self{
            engine:Engine::default(),
            path:vec![]
        }
    }
    pub fn run(&mut self) {
        self.engine.run_file(self.path.first().unwrap().clone())
    }
    pub fn add_file(mut self, path: impl AsRef<Path>) ->Self {
        self.path.push(path.as_ref().to_path_buf());
        self
    }
}