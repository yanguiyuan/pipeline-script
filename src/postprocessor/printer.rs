use crate::ast::node::Node;
use crate::ast::NodeTrait;
use crate::postprocessor::{VisitResult, Visitor};

pub struct Printer {}

impl Visitor for Printer {
    fn match_id(&self, id: &str) -> bool {
        true
    }

    fn visit(&mut self, node:&mut (impl NodeTrait + ?Sized)) -> VisitResult {
        dbg!(node.get_id());
        VisitResult::Break
    }
}