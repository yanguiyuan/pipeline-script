use crate::context::Context;
use crate::engine::Engine;
use crate::module::Module;
use crate::plugin::Plugin;
use crate::types::Value;

pub struct BuiltinPlugin;
impl Plugin for BuiltinPlugin{
    fn apply(e: &mut Engine) {
        let mut m=Module::new("std");
        m.register_pipe_function("println",|ctx:&mut Context,args:Vec<Value>|{
            for v in args{
                let v=v.as_dynamic();
                print!("{}",v);
            }
            println!();
            return Ok(().into())
        });
        e.register_into_main_module(m);
    }
}