use plugin::format_string::FormatStringPlugin;

use crate::core::app::App;
use crate::plugin::builtin::BuiltinPlugin;

mod ast;
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
        .add_plugin(BuiltinPlugin)
        .add_plugin(FormatStringPlugin)
        .run();

}
