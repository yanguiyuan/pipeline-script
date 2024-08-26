// use crate::context::{Context, ContextKey, ContextValue};
// use crate::engine::Engine;
// use crate::error::PipelineResult;
// use crate::module::Module;
// use crate::plugin::Plugin;
// use crate::types::Value;
// use std::sync::{Arc, Mutex, RwLock};
// use std::thread;
// use std::thread::JoinHandle;
//
//
// pub struct TaskPlugin;
// impl Plugin for TaskPlugin {
//     fn apply(e: &mut Engine) {
//         let mut m = Module::new("pipeline");
//         m.register_pipe_function("pipeline", |ctx, args| {
//             let pipeline_name = args.get(0).unwrap().as_string().unwrap();
//             let blocks = args
//                 .get(1)
//                 .unwrap()
//                 .as_dynamic()
//                 .as_fn_ptr()
//                 .unwrap()
//                 .fn_def
//                 .unwrap()
//                 .body;
//             let path: String = ctx.get_value("PIPELINE_NAME").unwrap().into();
//             let mut ctx = Context::with_value(
//                 ctx,
//                 ContextKey::NativeObject("join_set".into()),
//                 ContextValue::Native(Arc::new(
//                     RwLock::<Vec<JoinHandle<PipelineResult<Value>>>>::new(vec![]),
//                 )),
//             );
//             if path == pipeline_name || path == "*" {
//                 for i in &blocks {
//                     ctx.eval_stmt(i)?;
//                 }
//             }
//             let join_set = ctx
//                 .get(ContextKey::NativeObject("join_set".into()))
//                 .unwrap()
//                 .as_native()
//                 .unwrap();
//
//             let mut binding = join_set.write().unwrap();
//             let join_set = binding
//                 .downcast_mut::<Vec<JoinHandle<PipelineResult<Value>>>>()
//                 .unwrap();
//             while !join_set.is_empty() {
//                 let e = join_set.pop().unwrap();
//                 e.join().unwrap().unwrap();
//             }
//             Ok(().into())
//         });
//         m.register_pipe_function("step", |ctx, args| {
//             let task_name = args.get(0).unwrap().as_string().unwrap();
//             let mut ptr = args.get(1).unwrap().as_dynamic().as_fn_ptr().unwrap();
//             let active_task: String = ctx.get_value("ACTIVE_TASK").unwrap().into();
//             if active_task == task_name || active_task.as_str() == "*" {
//                 return ptr.call(ctx);
//             }
//             Ok(().into())
//         });
//         m.register_pipe_function("parallel", |ctx, args| {
//             let task_name = args.get(0).unwrap().as_string().unwrap();
//             let mut ptr = args.get(1).unwrap().as_dynamic().as_fn_ptr().unwrap();
//             let active_task: String = ctx.get_value("ACTIVE_TASK").unwrap().into();
//             if active_task == task_name || active_task.as_str() == "*" {
//                 let join_set = ctx
//                     .get(ContextKey::NativeObject("join_set".into()))
//                     .unwrap()
//                     .as_native()
//                     .unwrap();
//                 let mut binding = join_set.write().unwrap();
//                 let join_set = binding
//                     .downcast_mut::<Vec<JoinHandle<PipelineResult<Value>>>>()
//                     .unwrap();
//                 // 创建一个 Arc<Mutex<Context>> 来共享 ctx
//                 let shared_ctx = Arc::new(Mutex::new(ctx.clone()));
//                 let join_handle = thread::spawn(move || {
//                     let mut ctx = shared_ctx.lock().unwrap();
//                     return ptr.call(&mut ctx);
//                 });
//                 join_set.push(join_handle);
//             }
//             Ok(().into())
//         });
//         e.register_module(m)
//     }
// }
