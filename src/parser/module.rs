use std::any::Any;

use crate::parser::class::Class;
use crate::parser::function::Function;
use crate::parser::r#struct;
use crate::parser::stmt::StmtNode;
use std::collections::HashMap;

use crate::ast::data::Data;
use crate::ast::NodeTrait;
use crate::context::Context;
use crate::postprocessor::module_merger::ModuleMerger;
use crate::postprocessor::run_visitor;
use slotmap::DefaultKey;

#[derive(Clone, Debug)]
pub struct Module {
    name: String,
    functions: HashMap<String, Function>,
    classes: HashMap<String, Class>,
    structs: HashMap<String, r#struct::Struct>,
    global_block: Vec<StmtNode>,
    submodules: HashMap<String, DefaultKey>,
    type_aliases: HashMap<String, crate::parser::r#type::Type>,
}
impl NodeTrait for Module {
    fn get_id(&self) -> &str {
        "Module"
    }

    fn get_data(&self, key: &str) -> Option<Data> {
        match key {
            "global_variables" => {
                let mut global_variables = vec![];
                for stmt in self.global_block.iter().filter(|e| e.is_val_decl()) {
                    let name = stmt.get_data("name").unwrap();
                    global_variables.push(name)
                }
                Some(Data::Array(global_variables))
            }
            "functions" => {
                let mut functions = vec![];
                for (name, _) in self.functions.iter() {
                    functions.push(Data::String(name.clone()))
                }
                for i in self.global_block.iter().filter(|e| {
                    if !e.is_val_decl() {
                        return false;
                    }
                    e.is_val_decl()
                }) {
                    let name = i.get_data("name").unwrap();
                    let name = name.as_str().unwrap();
                    functions.push(Data::String(name.to_string()))
                }
                Some(Data::Array(functions))
            }
            "name" => Some(Data::String(self.name.clone())),
            _ => None,
        }
    }

    fn set_data(&mut self, _: &str, _: Data) {
        todo!()
    }

    fn get_children(&self) -> Vec<&dyn NodeTrait> {
        todo!()
    }

    fn get_mut_children(&mut self) -> Vec<&mut dyn NodeTrait> {
        let mut r = vec![];
        for f in self.functions.values_mut() {
            r.push(f as &mut dyn NodeTrait)
        }
        for stmt in self.global_block.iter_mut() {
            r.push(stmt as &mut dyn NodeTrait)
        }
        r
    }

    fn get_extra(&self) -> &HashMap<String, Box<dyn Any>> {
        todo!()
    }
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
            type_aliases: HashMap::new(),
        }
    }
    pub fn register_struct(&mut self, name: &str, s: r#struct::Struct) {
        self.structs.insert(name.into(), s);
    }
    pub fn get_structs(&self) -> &HashMap<String, r#struct::Struct> {
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
    // pub fn get_functions_ref(&self) -> &HashMap<String, Function> {
    //     &self.functions
    // }
    pub fn get_functions(&self) -> HashMap<String, Function> {
        self.functions.clone()
    }
    pub fn get_submodules(&self) -> &HashMap<String, DefaultKey> {
        &self.submodules
    }
    pub fn get_mut_functions(&mut self) -> &mut HashMap<String, Function> {
        &mut self.functions
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
    // pub fn get_mut_global_block(&mut self) -> &mut Vec<StmtNode> {
    //     &mut self.global_block
    // }
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
        method: Function,
    ) {
        let class_result = self.classes.get_mut(class_name.as_ref()).unwrap();
        class_result.register_method(method_name.into(), method)
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
    pub fn merge(&mut self, module: &Module) {
        let merger = ModuleMerger::new();
        let mut new_module = module.clone();
        run_visitor(&mut new_module, &merger);
        // Merge functions with namespace
        let new_functions: HashMap<_, _> = module
            .functions
            .iter()
            .filter_map(|(k, v)| {
                let name = format!("{}:{}", module.name, k);
                if !self.functions.contains_key(name.as_str()) {
                    let mut new_function = v.clone();
                    for stmt in new_function.mut_body() {
                        if stmt.is_fn_call() {
                            let name = stmt.get_fn_call_name().unwrap();
                            if module.functions.contains_key(name.as_str()) {
                                stmt.set_fn_call_name(format!("{}.{}", module.name, name));
                            }
                        }
                    }
                    new_function.set_name(name.clone());
                    Some((name, new_function))
                } else {
                    None
                }
            })
            .collect();

        // Merge global block with namespace
        let mut new_block = new_module.global_block.clone();
        new_block.extend(self.global_block.clone());
        self.global_block = new_block;
        self.functions.extend(new_functions);
    }
    pub fn sort_global_block(&mut self) {
        self.global_block
            .sort_by(|a, b| a.position().pos.cmp(&b.position().pos));
    }
    pub fn get_submodule(&self, name: &str) -> &DefaultKey {
        let m = self.submodules.get(name).unwrap();
        m
    }
    pub fn merge_into_main(&mut self, ctx: &Context, name: &str) -> bool {
        let m = self.submodules.get(name);
        match m {
            Some(m) => {
                let module_slot_map = ctx.get_module_slot_map();
                let module_slot_map = module_slot_map.read().unwrap();
                let module = module_slot_map.get(*m).unwrap();
                self.merge(module);
                true
            }
            None => false,
        }
    }

    pub fn register_submodule(&mut self, name: &str, module: DefaultKey) {
        self.submodules.insert(name.into(), module);
    }

    pub fn get_function(&self, name: impl Into<String>) -> Option<Function> {
        let r = self.functions.get(name.into().as_str());
        r.cloned()
    }
    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }
    pub fn register_type_alias(&mut self, name: &str, ty: crate::parser::r#type::Type) {
        self.type_aliases.insert(name.to_string(), ty);
    }
    
    pub fn get_type_alias(&self, name: &str) -> Option<&crate::parser::r#type::Type> {
        self.type_aliases.get(name)
    }
    
    pub fn get_type_aliases(&self) -> &HashMap<String, crate::parser::r#type::Type> {
        &self.type_aliases
    }
}
