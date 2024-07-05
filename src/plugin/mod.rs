pub mod builtin;
pub mod task;
pub mod test;

use crate::engine::Engine;

pub trait Plugin {
    fn apply(e: &mut Engine);
}
