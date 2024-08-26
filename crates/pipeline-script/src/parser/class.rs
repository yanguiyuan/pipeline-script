use std::collections::HashMap;
use crate::parser::declaration::VariableDeclaration;
use crate::parser::function::Function;

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    attributions: Vec<VariableDeclaration>,
    pub(crate) methods: HashMap<String, Function>,
    static_methods: HashMap<String, Function>,
}

impl Class {
    pub fn new(name: &str, attributions: Vec<VariableDeclaration>) -> Self {
        Self {
            name: name.to_string(),
            attributions,
            methods: HashMap::new(),
            static_methods: HashMap::new(),
        }
    }
    pub fn get_attributions(&self) -> &Vec<VariableDeclaration> {
        &self.attributions
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
    pub fn get_static_function(&self, name: &str) -> Option<Function> {
        self.static_methods.get(name).cloned()
    }
    pub fn register_method(&mut self, name: String, method: Function) {
        self.methods.insert(name, method);
    }
    pub fn register_static_method(&mut self, name: String, method: Function) {
        self.static_methods.insert(name, method);
    }
}