pub mod builtin;
pub mod task;

use crate::engine::Engine;

pub trait Plugin {
    fn apply(e: &mut Engine);
}
