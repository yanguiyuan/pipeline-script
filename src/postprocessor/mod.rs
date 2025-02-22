use crate::ast::NodeTrait;
use std::collections::VecDeque;

pub mod function_printer;
mod id;
pub mod module_merger;
pub mod printer;
pub mod r#type;

pub enum VisitResult {
    Continue,
    // 停止遍历
    Break,
    // 跳过该Node的Children
    #[allow(unused)]
    Skip,
}
#[derive(PartialEq)]
pub enum Stage {
    BeforeTypeInfer,
    AfterTypeInfer,
}
pub trait Visitor {
    fn stage(&self) -> Stage;
    fn match_id(&self, id: &str) -> bool;
    // fn visit(&mut self, node:&mut Node)-> crate::postprocessor::VisitResult;
    fn visit(&self, node: &mut (impl NodeTrait + ?Sized)) -> crate::postprocessor::VisitResult
    where
        Self: Sized;
}

pub trait DynVisitor: Visitor {
    fn dyn_visit(&self, node: &mut dyn NodeTrait) -> crate::postprocessor::VisitResult;
}

impl<T: Visitor> DynVisitor for T {
    fn dyn_visit(&self, node: &mut dyn NodeTrait) -> crate::postprocessor::VisitResult {
        self.visit(node)
    }
}

pub fn run_visitor(node: &mut dyn NodeTrait, visitor: &dyn DynVisitor) {
    let mut queue: VecDeque<&mut dyn NodeTrait> = VecDeque::new();
    queue.push_back(node);
    while !queue.is_empty() {
        let mut skip = false;
        let node = queue.pop_front().unwrap();
        if visitor.match_id(node.get_id()) {
            let result = visitor.dyn_visit(node);
            if let VisitResult::Break = result {
                break;
            }
            if let VisitResult::Skip = result {
                skip = true;
            }
        }
        if skip {
            continue;
        }
        for i in node.get_mut_children() {
            queue.push_back(i);
        }
    }
}
