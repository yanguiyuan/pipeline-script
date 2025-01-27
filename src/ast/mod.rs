use crate::ast::data::Data;

pub mod data;
pub mod node;

trait NodeTrait{
    fn get_id(&self)->i32;
    fn get_data(&self)->&Vec<Data>;
    fn get_children(&self)->&Vec<node::Node>;
}