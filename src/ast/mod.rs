use std::any::Any;
use std::collections::HashMap;
use crate::ast::data::Data;


pub mod data;
pub mod node;
pub mod helper;
mod compile;

pub trait NodeTrait{
    fn get_id(&self)->&str;
    fn get_data(&self,key:&str)->Option<Data>;
    fn set_data(&mut self,key:&str,value:Data);
    fn get_children(&self)->Vec<&dyn NodeTrait>;
    fn get_mut_children(&mut self) -> Vec<&mut dyn NodeTrait>;
    #[allow(unused)]
    fn get_extra(&self)->&HashMap<String,Box<dyn Any>>;
}