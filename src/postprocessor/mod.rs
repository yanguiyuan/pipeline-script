use crate::ast::node::Node;

mod id;
pub mod r#type;
mod method;
mod manage;
pub mod function_printer;
pub mod printer;

pub enum VisitOrder{
    PreOrder,
    PostOrder,
}
pub enum VisitResult{
    Continue,
    // 停止遍历
    Break,
    // 跳过该Node的Children
    Skip
}
pub trait Visitor{
    fn match_id(&self, id:&str) -> bool;
    fn visit(&mut self, node:&mut Node)-> crate::postprocessor::VisitResult;
}
