use crate::core::builtin::{cmd, println,panic};
use crate::core::engine::Engine;
use crate::plugin::Plugin;
use std::ffi::c_void;

pub struct BuiltinPlugin;
impl Plugin for BuiltinPlugin {
    fn apply(self, e: &mut Engine) {
        e.register_external_function("println", println as *mut c_void);
        e.register_external_function("cmd", cmd as *mut c_void);
        e.register_external_function("panic", panic as *mut c_void);
    }
}
