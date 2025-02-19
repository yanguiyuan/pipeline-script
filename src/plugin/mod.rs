use crate::core::engine::Engine;
pub mod format_string;
pub mod builtin;
pub mod task;
pub mod test;
pub mod method;

#[allow(unused)]
pub trait Plugin {
    fn apply(self:Self,e: &mut Engine);
}
