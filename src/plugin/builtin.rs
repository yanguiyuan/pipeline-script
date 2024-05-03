use std::io;
use std::io::{Stdin, stdin, Write};
use std::sync::{Arc, RwLock};
use scanner_rust::Scanner;
use crate::context::{Context, ContextKey, ContextValue};
use crate::engine::Engine;
use crate::error::PipelineError;
use crate::module::{Class, Module};
use crate::plugin::Plugin;
use crate::types::{Dynamic, Value};

pub struct BuiltinPlugin;
impl Plugin for BuiltinPlugin{
    fn apply(e: &mut Engine) {
        e.register_context_value(ContextKey::NativeObject("$Scanner".into()),ContextValue::Native(Arc::new(RwLock::new(Scanner::new(stdin())))));
        let mut m=Module::new("std");
        m.register_class(Class::new("Int",vec![]));
        m.register_pipe_function("println",|ctx:&mut Context,args:Vec<Value>|{
            for v in args{
                let vd=v.as_dynamic();
                let module=ctx.get_module();
                let module=module.read().unwrap();
                let method=module.get_class_function(vd.type_name().as_str(),"toString");
                if let Some(f)=method{
                    let r=f.call(ctx,vec![v])?;
                    print!("{}",r.as_dynamic());
                    continue
                }
                print!("{}",vd);
            }
            println!();
            return Ok(().into())
        });
        m.register_pipe_function("print",|ctx:&mut Context,args:Vec<Value>|{
            for v in args{
                let vd=v.as_dynamic();
                let module=ctx.get_module();
                let module=module.read().unwrap();
                let method=module.get_class_function(vd.type_name().as_str(),"toString");
                if let Some(f)=method{
                    let r=f.call(ctx,vec![v])?;
                    print!("{}",r.as_dynamic());
                    continue
                }
                print!("{}",vd);
            }
            return Ok(().into())
        });
        m.register_pipe_function("append",|ctx:&mut Context,args:Vec<Value>|{
            let raw_array=args.get(0).unwrap().get_mut_arc();
            let mut array=raw_array.write().unwrap();
            let array=array.as_mut_array().unwrap();
            for v in args.iter().skip(1){
               array.push(v.clone());
            }
            return Ok(Value::Mutable(raw_array.clone()))
        });
        m.register_pipe_function("remove",|ctx,args|{
            let target=args.get(0).unwrap().as_dynamic();
            match target {
                Dynamic::Array(a)=>{
                    let a=args.get(0).unwrap().as_arc();
                    let mut a=a.write().unwrap();
                    let a=a.as_mut_array().unwrap();
                    let index=args.get(1).unwrap().as_dynamic();
                    let index=index.as_integer().unwrap();
                    a.remove(index as usize);
                }
                Dynamic::Map(_)=>{
                    let key=args.get(1).unwrap().as_dynamic();
                    let m=args.get(0).unwrap().as_arc();
                    let mut m=m.write().unwrap();
                    let m=m.as_mut_map().unwrap();
                    m.remove(&key);
                }
                t=>{
                    panic!("{} not support remove",t.type_name())
                }
            }
            Ok(().into())
        });
        m.register_pipe_function("range",|_,args|{
            let n=args.get(0).unwrap();
            let o=args.get(1);
            let mut v=vec![];
            match o {
                None => {
                    let n=n.as_dynamic();
                    let n=n.as_integer().unwrap();
                    for i in 0..n{
                        v.push(i.into())
                    }
                }
                Some(end) => {
                    let n=n.as_dynamic();
                    let n=n.as_integer().unwrap();
                    let end=end.as_dynamic().as_integer().unwrap();
                    let step=args.get(2);
                    match step {
                        None => {
                            for i in n..end{
                                v.push(i.into())
                            }
                        }
                        Some(step) => {
                            let mut i=n;
                            let step=step.as_integer().unwrap();
                            while i<end{
                                v.push(i.into());
                                i+=step;
                            }
                        }
                    }
                }
            }
            return Ok(Dynamic::Array(v).into())
        });
        m.register_pipe_function("clone",|_,args|{
            let c=args.get(0).unwrap();
            Ok(match c {
                Value::Immutable(i) => {
                    i.clone().into()
                }
                Value::Mutable(m) => {
                    let a=m.read().unwrap().clone();
                    Value::Mutable(Arc::new(RwLock::new(a)))
                }
                Value::Refer(r) => {
                    let a=r.upgrade().unwrap().read().unwrap().clone();
                    Value::Mutable(Arc::new(RwLock::new(a)))
                }
                _=>panic!("signal can not be cloned")
            })
        });
        m.register_pipe_function("len",|_,args|{
            let c=args.get(0).unwrap().as_dynamic();
            match c {
                Dynamic::String(s) => {
                    Ok((s.len() as i64).into())
                }
                Dynamic::Array(a) => {
                    Ok((a.len() as i64).into())
                }
                Dynamic::Map(m) => {
                    Ok((m.len() as i64).into())
                }
                t=>return Err(PipelineError::UnexpectedType(t.type_name()))
            }

        });
        m.register_pipe_function("type",|_,args|{
            let c=args.get(0).unwrap();
            Ok(c.as_dynamic().type_name().into())
        });
        m.register_pipe_function("readInt",|ctx,args|{
            if args.len()>0{
                let c=args.get(0).unwrap().as_dynamic().as_string().unwrap();
                print!("{c}");
                io::stdout().flush().unwrap();
            }
            let sc=ctx.get(ContextKey::NativeObject("$Scanner".into())).unwrap();
            let sc=sc.as_native().unwrap();
            let mut sc=sc.write().unwrap();
            let mut sc=sc.downcast_mut::<Scanner<Stdin>>().unwrap();
            let i =sc.next_i64().unwrap().unwrap();
            Ok(i.into())
        });
        e.register_into_main_module(m);
    }
}