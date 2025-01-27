use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use crate::ast::data::Data;
use crate::context::Context;

#[derive(Debug)]
pub struct Node{
    id: String,
    data: HashMap<String,Data>,
    children: Vec<Node>,
    extra: HashMap<String,Box<dyn Any>>,
}


impl Node {
    pub fn new(id:impl Into<String>)->Self {
        Self {
            id:id.into(),
            data:HashMap::new(),
            children: vec![],
            extra: HashMap::new()
        }
    }
    pub fn build_llvm(&mut self,ctx:&Context) {
        for child in self.children.iter_mut() {
            child.build_llvm(ctx);
        }
    }
    pub fn with_extra(mut self, extra: HashMap<String,Box<dyn Any>>) ->Self{
        self.extra = extra;
        self
    }
    pub fn with_data(mut self, data: HashMap<String,Data>) ->Self{
        self.data = data;
        self
    }
    pub fn with_children(mut self, children: Vec<Node>) ->Self{
        self.children = children;
        self
    }
    pub fn get_children(&self) -> &Vec<Node> {
        &self.children
    }
    pub fn get_mut_children(&mut self) -> &mut Vec<Node> {
        &mut self.children
    }
    pub fn get_id(&self) -> &str {
        &self.id
    }
    pub fn set_id(&mut self,id:impl Into<String>){
        self.id = id.into();
    }
    pub fn get_data(&self,key:&str)->&Data{
        self.data.get(key).unwrap()
    }
    pub fn set_data(&mut self,key:impl Into<String>,value:Data){
        self.data.insert(key.into(), value);
    }
}