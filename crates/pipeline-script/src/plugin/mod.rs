use crate::core::engine::Engine;

pub mod builtin;
pub mod task;
pub mod test;

#[allow(unused)]
pub trait Plugin {
    fn apply(e: &mut Engine);
}
