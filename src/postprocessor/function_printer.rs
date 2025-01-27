use crate::ast::node::Node;
use crate::postprocessor::{VisitResult, Visitor};

pub struct FunctionPrinter{
    name:String,
}
impl FunctionPrinter{
    pub fn new(name:impl Into<String>)->Self{
        Self {
            name:name.into(),
        }
    }
}
impl Visitor for FunctionPrinter{

    fn match_id(&self, id: &str) -> bool {
        if id.starts_with("Function") {
            true
        }else {
            false
        }
    }

    fn visit(&mut self, node: &mut Node) -> VisitResult {
        let name = node.get_data("name").as_str().unwrap();
        if name.starts_with(self.name.as_str()) {
            println!("{:#?}",node);
            return VisitResult::Break;
        }
        VisitResult::Continue
    }
}