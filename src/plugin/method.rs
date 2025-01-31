use crate::core::engine::Engine;
use crate::plugin::Plugin;
use crate::postprocessor::Visitor;

pub struct MethodPlugin;
struct MethodVisitor;
impl Visitor for MethodVisitor {
    fn match_id(&self, id: &str) -> bool {
        id == "Expr:FnCall" || id == "Function"
    }
    fn visit(&mut self, node: &mut crate::ast::node::Node) -> crate::postprocessor::VisitResult {
        if node.get_id().starts_with("Function") {
            let binding = node.get_data("binding_struct").as_str();
            return match binding {
                None => crate::postprocessor::VisitResult::Continue,
                Some(binding) => {
                    if binding== ""{
                        return crate::postprocessor::VisitResult::Continue;
                    }
                    node.set_data("name", format!("{binding}.{}", node.get_data("name").as_str().unwrap()).into());
                    crate::postprocessor::VisitResult::Continue
                }
            }

        }
        let is_method = node.get_data("is_method").as_bool();
        match is_method {
            None => crate::postprocessor::VisitResult::Continue,
            Some(b) => {
                if b {
                    let name = node.get_data("name").as_str().unwrap();
                    let first_child = node.get_children().first().unwrap();
                    let ty = first_child.get_data("type").as_type().unwrap();
                    let struct_name = ty.get_struct_name().unwrap();
                    node.set_data("name", format!("{struct_name}.{name}").into());
                }
                crate::postprocessor::VisitResult::Continue
            }
        }
    }
}
impl Plugin for MethodPlugin {
    fn apply(self: Self, e: &mut Engine) {
        e.register_visitor(MethodVisitor)
    }
}