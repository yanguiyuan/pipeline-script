use plugin::format_string::FormatStringPlugin;

use crate::core::app::App;
use crate::core::buidin::{ cmd, println ,call};
use std::ffi::c_void;
use crate::plugin::builtin::BuiltinPlugin;
use crate::plugin::method::MethodPlugin;
use crate::postprocessor::function_printer::FunctionPrinter;
use crate::postprocessor::printer::Printer;

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
        .add_plugin(BuiltinPlugin)
        .add_plugin(FormatStringPlugin)
        // .add_plugin(MethodPlugin)
        // .register_visitor(Printer{})
        .register_visitor(FunctionPrinter::new("Object.test"))
        .run();
}
