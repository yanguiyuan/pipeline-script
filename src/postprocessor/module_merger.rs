use crate::ast::data::Data;
use crate::ast::NodeTrait;
use crate::postprocessor::{Stage, VisitResult, Visitor};
use std::cell::RefCell;
use std::rc::Rc;

pub struct ModuleMerger {
    global_variables: Rc<RefCell<Vec<String>>>,
    local_variables: Rc<RefCell<Vec<String>>>,
    module_name: Rc<RefCell<Option<String>>>,
    module_functions: Rc<RefCell<Vec<String>>>,
}

impl ModuleMerger {
    pub fn new() -> Self {
        Self {
            global_variables: Rc::new(RefCell::new(vec![])),
            local_variables: Rc::new(RefCell::new(vec![])),
            module_name: Rc::new(RefCell::new(None)),
            module_functions: Rc::new(RefCell::new(vec![])),
        }
    }
    pub fn is_module_function(&self, name: &str) -> bool {
        self.module_functions.borrow().contains(&name.into())
    }
}

impl Visitor for ModuleMerger {
    fn stage(&self) -> Stage {
        todo!()
    }

    fn match_id(&self, id: &str) -> bool {
        [
            "Module",
            "Function",
            "ValDecl",
            "Expr:Variable",
            "Expr:FnCall",
        ]
        .contains(&id)
    }

    fn visit(&self, node: &mut (impl NodeTrait + ?Sized)) -> VisitResult
    where
        Self: Sized,
    {
        let id = node.get_id();
        match id {
            "Module" => {
                let data = node.get_data("global_variables").unwrap();
                let global_variables = data.as_array().unwrap();
                for i in global_variables {
                    self.global_variables
                        .borrow_mut()
                        .push(i.as_str().unwrap().to_string());
                }
                let data = node.get_data("functions").unwrap();
                let functions = data.as_array().unwrap();
                for i in functions {
                    self.module_functions
                        .borrow_mut()
                        .push(i.as_str().unwrap().to_string());
                }
                let name = node.get_data("name").unwrap();
                self.module_name
                    .borrow_mut()
                    .replace(name.as_str().unwrap().to_string());
            }
            "ValDecl" => {
                let name = node.get_data("name").unwrap();
                let name = name.as_str().unwrap();
                let module_name = self.module_name.borrow();
                let module_name = module_name.as_ref().unwrap();
                node.set_data("name", Data::String(format!("{}:{}", module_name, name)));
            }
            "Expr:Variable" => {
                let name = node.get_data("name").unwrap();
                let name = name.as_str().unwrap().to_string();
                // 判断是否是全局block
                if self.local_variables.borrow().contains(&name) {
                    return VisitResult::Continue;
                }
                if !self.global_variables.borrow().contains(&name) {
                    return VisitResult::Continue;
                }
                let module_name = self.module_name.borrow();
                let module_name = module_name.as_ref().unwrap();
                node.set_data("name", Data::String(format!("{}:{}", module_name, name)));
            }
            "Expr:FnCall" => {
                let name = node.get_data("name").unwrap();
                let name = name.as_str().unwrap();
                if !self.is_module_function(name) {
                    return VisitResult::Continue;
                }
                let module_name = self.module_name.borrow();
                let module_name = module_name.as_ref().unwrap();
                node.set_data("name", Data::String(format!("{}:{}", module_name, name)));
            }
            t => {
                dbg!(t);
            }
        }
        VisitResult::Continue
    }
}
