use std::collections::HashMap;
use crate::context::{Context, ContextKey, ContextValue, Scope};
use crate::engine::Engine;
use crate::error::PipelineError;
use crate::module::{Class, Module};
use crate::plugin::Plugin;
use crate::types::{Dynamic, SignalType, Value};

use scanner_rust::Scanner;

use std::io;
use std::io::{Read, stdin, Stdin, Write};
use std::path::Path;
use std::process::{Command, exit, Stdio};
use std::sync::{Arc, RwLock};

pub struct BuiltinPlugin;
impl Plugin for BuiltinPlugin {
    fn apply(e: &mut Engine) {
        e.register_context_value(
            ContextKey::NativeObject("$Scanner".into()),
            ContextValue::Native(Arc::new(RwLock::new(Scanner::new(stdin())))),
        );
        e.register_context_value(
            ContextKey::NativeObject("workspace".into()),
            ContextValue::Native(Arc::new(RwLock::new(String::from("."))))
        );
        e.register_context_value(
            ContextKey::NativeObject("envs".into()),
            ContextValue::Native(Arc::new(RwLock::new(HashMap::<String, String>::new())))
        );
        let mut m = Module::new("buildin");
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
        m.register_pipe_function("max", |_: &mut Context, args: Vec<Value>| {
            let mut max_value = args.first().unwrap().as_dynamic().convert_float().unwrap();
            for v in args {
                let vd = v.as_dynamic();
                let n = vd.convert_float().unwrap();
                if n > max_value {
                    max_value = n;
                }
            }
            Ok(max_value.into())
        });
        m.register_pipe_function("min", |_: &mut Context, args: Vec<Value>| {
            let mut min_value = args.first().unwrap().as_dynamic().convert_float().unwrap();
            for v in args {
                let vd = v.as_dynamic();
                let n = vd.convert_float().unwrap();
                if n < min_value {
                    min_value = n;
                }
            }
            Ok(min_value.into())
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
            let r = ptr.call(&mut ctx)?;
            if let Value::Signal(SignalType::Return(v)) = r {
                return Ok(*v);
            }
            Ok(r)
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
        m.register_pipe_function("readFloat", |ctx, args| {
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
            let i = sc.next_f64().unwrap().unwrap();
            Ok(i.into())
        });
        m.register_pipe_function("readString", |ctx, args| {
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
            let i = sc.next().unwrap().unwrap();
            Ok(i.into())
        });
        m.register_pipe_function("readLine", |ctx, args| {
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
            let i = sc.next_line().unwrap().unwrap();
            Ok(i.into())
        });
        m.register_pipe_function("nu", |ctx, args| {
            let cmd = "nu";
            let command = args.first().unwrap().as_string().unwrap();

            let mut cmd = Command::new(cmd);
            let workspace = ctx.get(ContextKey::NativeObject("workspace".to_string()));
            if let Some(workspace) = workspace {
                let workspace = workspace.as_native().unwrap();
                let workspace = workspace.read().unwrap();
                let workspace = workspace.downcast_ref::<String>().unwrap();
                cmd.current_dir(workspace);
            }
            let mode = args.get(1);
            let mut is_stdout =false;
            if let Some(mode)=mode{
                let mode =mode.as_string().unwrap();
                if mode.as_str()=="stdout"{
                    cmd.stdout(Stdio::piped());
                    is_stdout=true;
                }

            }
            let envs = ctx.get(ContextKey::NativeObject("envs".to_string()));
            let envs =envs.unwrap().as_native().unwrap();
            let envs = envs.read().unwrap();
            let envs =envs.downcast_ref::<HashMap<String,String>>().unwrap();
            let mut child = cmd.envs(envs.iter()).args(&["-c", &command]).spawn().expect("执行命令失败");
            let mut buffer = String::new();
            let _ = child.wait().expect("Failed to wait for command execution");
            if is_stdout{
                child.stdout.unwrap().read_to_string(&mut buffer).expect("读取控制台输出错误");
                return Ok(buffer.into());
            }
            return Ok(().into())

        });
        m.register_pipe_function("workspace", |ctx, args| {
            let path = args.first().unwrap().as_string().unwrap();
            if !Path::new(&path).exists(){
                println!("\x1b[31m[错误]:路径\"{path}\"不存在\x1b[0m");
                exit(0);
            }
            let workspace = ctx.get(ContextKey::NativeObject("workspace".to_string()));
            let workspace = workspace.unwrap().as_native().unwrap();
            let mut workspace = workspace.write().unwrap();
            let workspace =workspace.downcast_mut::<String>().unwrap();
            *workspace=path;
            return Ok(().into());
        });
        m.register_pipe_function("env", |ctx, args| {

            let envs = ctx.get(ContextKey::NativeObject("envs".to_string()));
            let envs =envs.unwrap().as_native().unwrap();
            let mut envs = envs.write().unwrap();
            let envs =envs.downcast_mut::<HashMap<String,String>>().unwrap();
            let key = args.first().unwrap().as_string().unwrap();
            let value =args[1].as_string().unwrap();
            envs.insert(key,value);
            return Ok(().into());
        });
        e.register_into_main_module(m);
    }
}
