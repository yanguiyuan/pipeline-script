use crate::core::engine::Engine;
use crate::core::buidin::append;
use crate::preprocessor::FormatStringPreprocessor;
use crate::plugin::Plugin;
use std::ffi::c_void;

pub struct FormatStringPlugin;

impl Plugin for FormatStringPlugin{
    fn apply(self, e: &mut Engine) {
        e.register_preluded_scripts(&[
            "extern fn FormatAppend(obj:..Any)->String"
        ]);
        e.register_external_function("FormatAppend", append as *mut c_void);
        e.register_preprocessor(FormatStringPreprocessor)
    }
}