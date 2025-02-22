use crate::ast::NodeTrait;
use crate::core::engine::Engine;
use crate::plugin::Plugin;
use crate::postprocessor::{Stage, Visitor};

pub struct MethodPlugin;
struct MethodVisitor;
impl Visitor for MethodVisitor {
    fn stage(&self) -> Stage {
        Stage::AfterTypeInfer
    }
    fn match_id(&self, id: &str) -> bool {
        id == "Expr:FnCall" || id == "Function"
    }
    fn visit(&self, node: &mut (impl NodeTrait + ?Sized)) -> crate::postprocessor::VisitResult {
        if node.get_id().starts_with("Function") {
            let data = node.get_data("binding_struct");
            if data.is_none() {
                return crate::postprocessor::VisitResult::Continue;
            }
            let data = data.unwrap();
            let binding = data.as_str();
            return match binding {
                None => crate::postprocessor::VisitResult::Continue,
                Some(binding) => {
                    if binding.is_empty() {
                        return crate::postprocessor::VisitResult::Continue;
                    }
                    node.set_data(
                        "name",
                        format!(
                            "{binding}.{}",
                            node.get_data("name").unwrap().as_str().unwrap()
                        )
                        .into(),
                    );
                    crate::postprocessor::VisitResult::Continue
                }
            };
        }
        let is_method = node.get_data("is_method").unwrap().as_bool();
        match is_method {
            None => crate::postprocessor::VisitResult::Continue,
            Some(b) => {
                if b {
                    let data = node.get_data("binding_struct").unwrap();
                    let name = data.as_str().unwrap();
                    let children = node.get_children();
                    let first_child = children.first().unwrap();
                    let data = first_child.get_data("type").unwrap();
                    let ty = data.as_type().unwrap();
                    let struct_name = ty.get_struct_name().unwrap();
                    node.set_data("name", format!("{struct_name}.{name}").into());
                }
                crate::postprocessor::VisitResult::Continue
            }
        }
    }
}
impl Plugin for MethodPlugin {
    fn apply(self, e: &mut Engine) {
        e.register_visitor(MethodVisitor)
    }
}
