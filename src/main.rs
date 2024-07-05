mod context;
mod engine;
mod error;
mod expr;
mod lexer;
mod module;
mod parser;
mod plugin;
mod position;
mod stmt;
mod token;
mod types;

use crate::engine::Engine;
use crate::plugin::task::TaskPlugin;
use crate::plugin::test::TestPlugin;

fn main() {
    let mut e = Engine::default();
    // e.enable_ast_debug();
    e.use_plugin::<TaskPlugin>();
    e.use_plugin::<TestPlugin>();
    e.compile("import buildin");
    e.compile("import test");
    e.run_file("main.ppl")
}

