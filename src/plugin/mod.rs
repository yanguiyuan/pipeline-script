use crate::core::engine::Engine;
pub mod builtin;
pub mod format_string;
pub mod method;
pub mod task;
pub mod test;

#[allow(unused)]
pub trait Plugin {
    fn apply(self, e: &mut Engine);
}
