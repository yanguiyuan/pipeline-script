use plugin::format_string::FormatStringPlugin;

use crate::core::app::App;
use crate::core::buidin::{ cmd, println ,call};
use std::ffi::c_void;
pub mod compiler;
mod context;
mod core;
pub mod lexer;
mod llvm;
pub mod parser;
mod plugin;
mod postprocessor;
mod preprocessor;

fn main() {
    App::new()
        .set_entry_file("main.ppl")
        .add_plugin(FormatStringPlugin)
        .register_external_function("call", call as *mut c_void)
        .register_external_function("println", println as *mut c_void)
        .register_external_function("cmd", cmd as *mut c_void)
        .run();
}
