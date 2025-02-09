use crate::ast::NodeTrait;
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

    fn visit(&self, node:&mut (impl NodeTrait + ?Sized)) -> VisitResult {
        let data = node.get_data("name").unwrap();
        let name = data.as_str().unwrap();
        if name.starts_with(self.name.as_str()) {
            println!("{:#?}",node.get_id());
            return VisitResult::Break;
        }
        VisitResult::Continue
    }
}