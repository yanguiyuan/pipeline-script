use crate::ast::node::Node;

pub enum VisitOrder{
    PreOrder,
    PostOrder,
}
pub enum VisitMode{
    One,
    Each,
}
pub enum VisitResult{
    Continue,
    // 停止遍历
    Break,
    // 跳过该Node的Children
    Skip
}
pub trait Visitor{
    fn mode(&self) -> VisitMode;
    fn match_id(&self, id:&str) -> bool;
    fn visit(&mut self, node:&mut Node)->VisitResult;
}

pub struct Printer;
impl Visitor for Printer{
    fn mode(&self) -> VisitMode {
        VisitMode::Each
    }

    fn match_id(&self, id: &str) -> bool {
        if id.starts_with("Function") {
            true
        }else {
            false
        }
    }

    fn visit(&mut self, node: &mut Node) -> VisitResult {
        println!("{}-> {:?}",node.get_id(),node.get_data("name"));
        let children = node.get_mut_children();
        for i in children {
            let id = i.get_id();
            if id.starts_with("Declaration:Variable") {
                i.set_id("Declaration:Parameter");
            }
        }
        VisitResult::Continue
    }
}