use crate::ast::data::Data;
use crate::ast::NodeTrait;
use crate::ast::declaration::VariableDeclaration;
use crate::ast::r#type::Type;
use std::any::Any;
use std::collections::HashMap;
use std::vec;

use crate::ast::stmt::StmtNode;
#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    return_type: Type,
    // 泛型模版，用来区分实例和模版，模版不用于生成llvm ir
    pub is_template: bool,
    pub generic_list: Vec<Type>,
    args: Vec<VariableDeclaration>,
    body: Vec<StmtNode>,
    #[allow(unused)]
    is_generic: bool,
    binding_type: Option<String>,
    #[allow(unused)]
    pub(crate) is_extern: bool,
    type_generics: Vec<Type>,
}
impl NodeTrait for Function {
    fn get_id(&self) -> &str {
        "Function"
    }

    fn get_data(&self, key: &str) -> Option<Data> {
        match key {
            "name" => Some(Data::String(self.name.clone())),
            "binding_struct" => self.binding_type.as_ref().map(|s| Data::String(s.clone())),
            _ => None,
        }
    }

    fn set_data(&mut self, key: &str, value: Data) {
        dbg!(key, &value);
        if key == "name" {
            self.name = value.as_str().unwrap().into();
        }
    }

    fn get_children(&self) -> Vec<&dyn NodeTrait> {
        todo!()
    }

    fn get_mut_children(&mut self) -> Vec<&mut dyn NodeTrait> {
        vec![]
    }

    fn get_extra(&self) -> &HashMap<String, Box<dyn Any>> {
        todo!()
    }
}
impl Function {
    pub fn new(
        name: String,
        return_type: Type,
        args: Vec<VariableDeclaration>,
        body: Vec<StmtNode>,
        is_extern: bool,
    ) -> Self {
        Self {
            name,
            return_type,
            args,
            body,
            is_template: false,
            generic_list: vec![],
            binding_type: None,
            is_generic: false,
            is_extern,
            type_generics: vec![],
        }
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn body(&self) -> &Vec<StmtNode> {
        &self.body
    }
    pub fn mut_body(&mut self) -> &mut Vec<StmtNode> {
        &mut self.body
    }
    pub fn args(&self) -> &Vec<VariableDeclaration> {
        &self.args
    }
    pub fn args_mut(&mut self) -> &mut Vec<VariableDeclaration> {
        &mut self.args
    }
    pub fn return_type(&self) -> &Type {
        &self.return_type
    }
    pub fn with_generic_list(mut self, list: Vec<Type>) -> Self {
        self.generic_list = list;
        self
    }
    pub fn with_extern(mut self, is_extern: bool) -> Self {
        self.is_extern = is_extern;
        self
    }
    pub fn with_template(mut self, is_template: bool) -> Self {
        self.is_template = is_template;
        self
    }
    pub fn add_generic(&mut self, g: Type) {
        self.generic_list.push(g);
    }
    pub fn args_count(&self) -> usize {
        self.args.len()
    }
    pub fn has_binding(&self) -> bool {
        self.binding_type.is_some()
    }
    pub fn get_binding(&self) -> String {
        self.binding_type.clone().unwrap()
    }
    pub fn insert_arg(&mut self, index: usize, vd: VariableDeclaration) {
        self.args.insert(index, vd)
    }
    pub fn set_binding_type(&mut self, binding_struct: impl Into<String>) {
        self.binding_type = Some(binding_struct.into());
    }
    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }
    pub fn set_body(&mut self, body: Vec<StmtNode>) {
        self.body = body;
    }
    pub fn with_return_type(mut self, return_type: Type) -> Self {
        self.return_type = return_type;
        self
    }
    pub fn set_return_type(&mut self, return_type: Type) {
        self.return_type = return_type;
    }
    pub fn with_name(mut self, name: String) -> Self {
        self.name = name;
        self
    }
    pub fn with_args(mut self, args: Vec<VariableDeclaration>) -> Self {
        self.args = args;
        self
    }
    pub fn get_param_type(&self, index: usize) -> Option<Type> {
        if index < self.args.len() {
            self.args[index].r#type()
        } else {
            None
        }
    }
    pub fn get_type(&self) -> Type {
        let mut args = vec![];
        for i in &self.args {
            args.push(i.r#type().unwrap());
        }
        Type::Function(Box::new(self.return_type.clone()), args)
    }
    pub fn set_type_generics(&mut self, type_generics: Vec<Type>) {
        self.type_generics = type_generics;
    }
}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            return_type: Type::Unit,
            is_generic: false,
            args: vec![],
            generic_list: vec![],
            binding_type: None,
            body: vec![],
            is_extern: false,
            is_template: false,
            type_generics: vec![],
        }
    }
}
