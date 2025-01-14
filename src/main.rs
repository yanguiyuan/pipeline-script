use plugin::format_string::FormatStringPlugin;

use crate::core::app::App;
use crate::core::buidin::{ cmd, println ,call};
use std::ffi::c_void;
use crate::ast::visit::Printer;
use crate::plugin::builtin::BuiltinPlugin;

pub mod compiler;
mod context;
mod core;
pub mod lexer;
mod llvm;
pub mod parser;
mod plugin;
mod postprocessor;
mod preprocessor;
mod ast;

fn main() {
    App::new()
        .set_entry_file("main.ppl")
        .register_visitor(Printer)
        .add_plugin(BuiltinPlugin)
        .add_plugin(FormatStringPlugin)
        .run();
}
