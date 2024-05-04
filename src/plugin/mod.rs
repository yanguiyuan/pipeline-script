pub mod builtin;

use crate::engine::Engine;

pub trait Plugin {
    fn apply(e: &mut Engine);
}
