use crate::ast::node::Node;
use crate::ast::NodeTrait;

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
    // fn visit(&mut self, node:&mut Node)-> crate::postprocessor::VisitResult;
    fn visit(&mut self, node:&mut (impl NodeTrait + ?Sized))-> crate::postprocessor::VisitResult
    where
        Self: Sized;
}

pub trait DynVisitor: Visitor{
    fn dyn_visit(&mut self, node:&mut dyn NodeTrait)-> crate::postprocessor::VisitResult;
}

impl<T: Visitor> DynVisitor for T {
    fn dyn_visit(&mut self, node: &mut dyn NodeTrait)  -> crate::postprocessor::VisitResult  {
        self.visit(node)
    }
}