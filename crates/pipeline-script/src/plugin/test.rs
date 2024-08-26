// use std::collections::HashMap;
// use std::sync::{Arc, RwLock};
// use crate::types::{Dynamic, Struct};
// use crate::engine::Engine;
// use crate::module::{Class, Function, Module, VariableDeclaration};
// use crate::plugin::Plugin;
// use crate::types::Value;
//
// pub struct TestPlugin;
//
// impl Plugin for TestPlugin{
//     fn apply(e: &mut Engine) {
//         let  mut test = Module::new("test");
//         let mut expect_class = Class::new("Expect",vec![VariableDeclaration::new("value".into(),"Any".into())]);
//         expect_class.register_method("toBe".into(),Function::Native(Arc::new(|ctx,args|{
//             return Ok(().into())
//         })));
//         test.register_class(expect_class);
//         test.register_pipe_function("test",|ctx,args|{
//             let test_name =args.get(0).unwrap().as_string().unwrap();
//             let mut closure = args.get(1).unwrap().as_dynamic().as_fn_ptr().unwrap();
//             closure.call(ctx).unwrap();
//
//             return Ok(().into())
//         });
//         test.register_pipe_function("expect",|_,args|{
//             let value = args.into_iter().next().unwrap();
//             let mut props = HashMap::new();
//             props.insert("value".into(),value);
//             let s = Struct::new("Expect".into(),props);
//             return Ok(Value::Mutable(Arc::new(RwLock::new(Dynamic::Struct(Box::new(s))))))
//         });
//         e.register_module(test);
//     }
// }