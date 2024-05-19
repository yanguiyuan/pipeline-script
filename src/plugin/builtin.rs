use crate::context::{Context, ContextKey, ContextValue, Scope};
use crate::engine::Engine;
use crate::error::PipelineError;
use crate::module::{Class, Module};
use crate::plugin::Plugin;
use crate::types::{Dynamic, Value};

use scanner_rust::Scanner;

use std::io;
use std::io::{stdin, Stdin, Write};
use std::sync::{Arc, RwLock};

pub struct BuiltinPlugin;
impl Plugin for BuiltinPlugin {
    fn apply(e: &mut Engine) {
        e.register_context_value(
            ContextKey::NativeObject("$Scanner".into()),
            ContextValue::Native(Arc::new(RwLock::new(Scanner::new(stdin())))),
        );
        let mut m = Module::new("std");
        m.register_class(Class::new("Int", vec![]));
        m.register_class(Class::new("Array", vec![]));
        m.register_pipe_function("println", |ctx: &mut Context, args: Vec<Value>| {
            for v in args {
                let vd = v.as_dynamic();
                let module = ctx.get_module();
                let module = module.read().unwrap();
                let method = module.get_class_function(vd.type_name().as_str(), "toString");
                if let Some(f) = method {
                    let r = f.call(ctx, vec![v.into()])?;
                    print!("{}", r.as_dynamic());
                    continue;
                }
                print!("{}", vd);
            }
            println!();
            Ok(().into())
        });
        m.register_pipe_function("print", |ctx: &mut Context, args: Vec<Value>| {
            for v in args {
                let vd = v.as_dynamic();
                let module = ctx.get_module();
                let module = module.read().unwrap();
                let method = module.get_class_function(vd.type_name().as_str(), "toString");
                if let Some(f) = method {
                    let r = f.call(ctx, vec![v.into()])?;
                    print!("{}", r.as_dynamic());
                    continue;
                }
                print!("{}", vd);
            }
            Ok(().into())
        });
        m.register_pipe_function("append", |_: &mut Context, args: Vec<Value>| {
            let raw_array = args.first().unwrap().get_mut_arc();
            let mut array = raw_array.write().unwrap();
            let array = array.as_mut_array().unwrap();
            for v in args.iter().skip(1) {
                array.push(v.clone());
            }
            Ok(Value::Mutable(raw_array.clone()))
        });
        m.register_pipe_function("call", |ctx: &mut Context, args: Vec<Value>| {
            let function_ptr = args.first().unwrap();
            let mut ptr = function_ptr.as_dynamic().as_fn_ptr().unwrap();
            let args_dec = ptr.fn_def.clone().unwrap().args;
            let mut scope = Scope::new();
            for (i, v) in args_dec.iter().enumerate() {
                scope.set(v.name.as_str(), args[i + 1].clone());
            }
            if args_dec.is_empty() && args.len() == 2 {
                scope.set("it", args[1].clone());
            } else if args_dec.is_empty() && args.len() > 2 {
                scope.set(
                    "it",
                    Value::Mutable(Arc::new(RwLock::new(Dynamic::Array(
                        args[1..args.len()].to_vec(),
                    )))),
                )
            }
            let parent_scope = ctx.get_scope();
            scope.set_parent(parent_scope);
            let mut ctx = Context::with_value(
                ctx,
                ContextKey::Scope,
                ContextValue::Scope(Arc::new(RwLock::new(scope))),
            );
            ptr.call(&mut ctx)
        });
        m.register_pipe_function("remove", |_, args| {
            let target = args.first().unwrap().as_dynamic();
            match target {
                Dynamic::Array(_) => {
                    let a = args.first().unwrap().as_arc();
                    let mut a = a.write().unwrap();
                    let a = a.as_mut_array().unwrap();
                    let index = args.get(1).unwrap().as_dynamic();
                    let index = index.as_integer().unwrap();
                    a.remove(index as usize);
                }
                Dynamic::Map(_) => {
                    let key = args.get(1).unwrap().as_dynamic();
                    let m = args.first().unwrap().as_arc();
                    let mut m = m.write().unwrap();
                    let m = m.as_mut_map().unwrap();
                    m.remove(&key);
                }
                t => {
                    panic!("{} not support remove", t.type_name())
                }
            }
            Ok(().into())
        });
        m.register_pipe_function("range", |_, args| {
            let n = args.first().unwrap();
            let o = args.get(1);
            let mut v = vec![];
            match o {
                None => {
                    let n = n.as_dynamic();
                    let n = n.as_integer().unwrap();
                    for i in 0..n {
                        v.push(i.into())
                    }
                }
                Some(end) => {
                    let n = n.as_dynamic();
                    let n = n.as_integer().unwrap();
                    let end = end.as_dynamic().as_integer().unwrap();
                    let step = args.get(2);
                    match step {
                        None => {
                            for i in n..end {
                                v.push(i.into())
                            }
                        }
                        Some(step) => {
                            let mut i = n;
                            let step = step.as_integer().unwrap();
                            while i < end {
                                v.push(i.into());
                                i += step;
                            }
                        }
                    }
                }
            }
            Ok(Dynamic::Array(v).into())
        });
        m.register_pipe_function("clone", |_, args| {
            let c = args.first().unwrap();
            Ok(match c {
                Value::Immutable(i) => {
                    let a = i.read().unwrap().clone();
                    Value::Immutable(Arc::new(RwLock::new(a)))
                }
                Value::Mutable(m) => {
                    let a = m.read().unwrap().clone();
                    Value::Mutable(Arc::new(RwLock::new(a)))
                }
                Value::Refer(r) => {
                    let a = r.upgrade().unwrap().read().unwrap().clone();
                    Value::Mutable(Arc::new(RwLock::new(a)))
                }
                _ => panic!("signal can not be cloned"),
            })
        });
        m.register_pipe_function("len", |_, args| {
            let c = args.first().unwrap().as_dynamic();
            match c {
                Dynamic::String(s) => Ok((s.len() as i64).into()),
                Dynamic::Array(a) => Ok((a.len() as i64).into()),
                Dynamic::Map(m) => Ok((m.len() as i64).into()),
                t => Err(PipelineError::UnexpectedType(t.type_name())),
            }
        });
        m.register_pipe_function("type", |_, args| {
            let c = args.first().unwrap();
            Ok(c.as_dynamic().type_name().into())
        });
        m.register_pipe_function("readInt", |ctx, args| {
            if !args.is_empty() {
                let c = args.first().unwrap().as_dynamic().as_string().unwrap();
                print!("{c}");
                io::stdout().flush().unwrap();
            }
            let sc = ctx
                .get(ContextKey::NativeObject("$Scanner".into()))
                .unwrap();
            let sc = sc.as_native().unwrap();
            let mut sc = sc.write().unwrap();
            let sc = sc.downcast_mut::<Scanner<Stdin>>().unwrap();
            let i = sc.next_i64().unwrap().unwrap();
            Ok(i.into())
        });
        e.register_into_main_module(m);
    }
}
