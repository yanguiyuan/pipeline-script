use crate::ast::NodeTrait;
use crate::postprocessor::{Stage, VisitResult, Visitor};

pub struct Printer {}

impl Visitor for Printer {
    fn stage(&self) -> Stage {
        Stage::AfterTypeInfer
    }
    fn match_id(&self, _: &str) -> bool {
        true
    }

    fn visit(&self, node: &mut (impl NodeTrait + ?Sized)) -> VisitResult {
        dbg!(node.get_id());
        VisitResult::Break
    }
}
