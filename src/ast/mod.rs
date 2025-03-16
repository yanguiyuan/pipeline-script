use crate::ast::data::Data;
use std::any::Any;
use std::collections::HashMap;
pub mod class;
pub mod data;
pub mod declaration;
pub mod expr;
pub mod function;
pub mod module;
pub mod stmt;
pub mod r#struct;
pub mod r#type;
pub mod type_alias;

pub trait NodeTrait {
    fn get_id(&self) -> &str;
    fn get_data(&self, key: &str) -> Option<Data>;
    fn set_data(&mut self, key: &str, value: Data);
    fn get_children(&self) -> Vec<&dyn NodeTrait>;
    fn get_mut_children(&mut self) -> Vec<&mut dyn NodeTrait>;
    #[allow(unused)]
    fn get_extra(&self) -> &HashMap<String, Box<dyn Any>>;
}
