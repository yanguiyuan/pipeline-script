
use std::collections::HashMap;
use crate::parser::class::Class;
use crate::parser::function::Function;
use crate::parser::r#struct;
use crate::parser::stmt::StmtNode;


#[derive(Clone, Debug)]
pub struct Module {
    name: String,
    functions: HashMap<String, Function>,
    classes: HashMap<String, Class>,
    structs: HashMap<String, r#struct::Struct>,
    global_block: Vec<StmtNode>,
    submodules: HashMap<String, Box<Module>>,
}

impl Module {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            structs: Default::default(),
            submodules: HashMap::new(),
            global_block: vec![],
        }
    }
    pub fn register_struct(&mut self, name: &str, s: r#struct::Struct) {
        self.structs.insert(name.into(), s);
    }
    pub fn get_structs(&self)-> &HashMap<String, r#struct::Struct> {
        &self.structs
    }
    pub fn get_struct(&self, name: &str) -> Option<&r#struct::Struct> {
        self.structs.get(name)
    }
    pub fn add_stmt(&mut self, stmt: StmtNode) {
        self.global_block.push(stmt)
    }

    pub fn get_class(&self, class_name: &str) -> Option<&Class> {
        return self.classes.get(class_name);
    }
    pub fn get_functions_ref(&self) -> &HashMap<String, Function> {
        &self.functions
    }
    pub fn get_functions(&self) ->HashMap<String, Function> {
        self.functions.clone()
    }
    pub fn register_function(&mut self, name: &str, f: Function) {
        self.functions.insert(name.into(), f);
    }
    pub fn push_block(&mut self, block: Vec<StmtNode>) {
        block.into_iter().for_each(|e| self.global_block.push(e))
    }

    pub fn get_global_block(&self) -> &Vec<StmtNode> {
        &self.global_block
    }
    pub fn get_mut_global_block(&mut self) -> &mut Vec<StmtNode> {
        &mut self.global_block
    }
    pub fn get_classes(&self) -> &HashMap<String, Class> {
        &self.classes
    }
    pub fn get_class_function(&self, class_name: &str, function_name: &str) -> Option<Function> {
        let class_result = self.classes.get(class_name);
        match class_result {
            None => None,
            Some(class) => {
                let function_result = class.methods.get(function_name);
                function_result.cloned()
            }
        }
    }
    pub fn register_class_method(
        &mut self,
        class_name: impl AsRef<str>,
        method_name: impl Into<String>,
        method:Function,
    ) {
        let class_result = self.classes.get_mut(class_name.as_ref()).unwrap();
        class_result.register_method(method_name.into(), method)
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
    pub fn merge(&mut self, module: &Module) {

        let new_functions: HashMap<_, _> = module
            .functions
            .iter()
            .filter_map(|(k, v)| {
                if !self.functions.contains_key(k) {
                    Some((k.clone(), v.clone()))
                } else {
                    None
                }
            })
            .collect();

        self.functions.extend(new_functions);
    }
    pub fn get_submodule(&self, name: &str) -> &Module {
        let m = self.submodules.get(name).unwrap();
        m
    }
    pub fn merge_into_main(&mut self, name: &str) -> bool {
        let m = self.submodules.get(name);
        match m {
            Some(m) => {
                let m = m.clone();
                self.merge(&m);
                true
            }
            None => false,
        }
    }

    pub fn register_submodule(&mut self, name: impl Into<String>, module: Module) {
        self.submodules.insert(name.into(), Box::new(module));
    }

    pub fn get_function(&self, name: impl Into<String>) -> Option<Function> {
        let r = self.functions.get(name.into().as_str());
        r.cloned()
    }
    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }
}