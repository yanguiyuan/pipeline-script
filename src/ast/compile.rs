use std::sync::{Arc, RwLock};
use crate::ast::helper::{build_function_declaration, build_type};
use crate::ast::node::Node;
use crate::context::Context;
use crate::core::value::Value;
use crate::llvm::builder::Builder;
use crate::llvm::global::Global;
use crate::llvm::module::LLVMModule;
use crate::llvm::types::LLVMType;
use crate::parser::r#type::Type;

impl Node{
    pub fn build_llvm(&self,ctx:&Context) ->Option<Value> {
        let llvm_module = ctx.get_llvm_module();
        let builder = ctx.get_builder();

        match self.id.as_str() {
            "File"=>{
                // 获取子节点中的函数，提前注册函数声明
                let function_nodes = self.children.iter().filter(|i| {
                    i.id.starts_with("Function")
                }).collect::<Vec<&Node>>();
                for i in function_nodes.iter() {
                    let name = i.data.get("name").unwrap().as_str().unwrap();
                    let return_type = i.data.get("type").unwrap().as_type().unwrap();
                    let is_extern = i.data.get("is_extern").unwrap().as_bool().unwrap();
                    let param_count = i.data.get("params_count").unwrap().as_i64().unwrap();
                    let param_nodes = i.children.iter().take(param_count as usize).collect::<Vec<&Node>>();
                    let param_types = param_nodes.iter().map(|i| {
                        let ty = i.data.get("type").unwrap().as_type().unwrap() ;
                        build_type(&ty,ctx)
                    }).collect::<Vec<LLVMType>>();
                    let param_names = param_nodes.iter().map(|i| {
                        i.data.get("name").unwrap().as_str().unwrap().to_string()
                    }).collect::<Vec<String>>();
                    build_function_declaration(llvm_module.clone(),is_extern,return_type,name,param_types,param_names);
                }
            }
            "Function"=>{
                let is_extern = self.data.get("is_extern").unwrap().as_bool().unwrap();
                if is_extern {
                    return None
                }
                let name = self.data.get("name").unwrap().as_str().unwrap();
                let llvm_module = llvm_module.read().unwrap();
                let function = llvm_module.get_function(name).unwrap();
                let entry = function.append_basic_block("entry");

                builder.position_at_end(entry);
                let ctx = Context::with_flag(&ctx, "return", false);
                for child in self.children.iter() {
                    child.build_llvm(&ctx);
                }
                let flag = ctx.get_flag("return");
                if !flag {
                    builder.build_return_void();
                }
                return None
            }
            "Return"=> {
                ctx.set_flag("return", true);
                let value = self.children.first().unwrap().build_llvm(ctx).unwrap();
                builder.build_return(value.get_value());
                return None
            }
            "Expr:Int"=>{
                let value = self.data.get("value").unwrap().as_i64().unwrap();
                let value = Global::const_i64(value);
                return Some(Value::new(value,Type::Int64))
            }
            "Expr:String"=>{
                let ty0 = self.data.get("type").unwrap().as_type().unwrap();
                let value = self.data.get("value").unwrap().as_str().unwrap();
                let ptr = builder.build_global_string("", value);
                return Some(Value::new(ptr, ty0.clone()))
            }
            "Expr:Array"=>{
                let ty0 = self.data.get("type").unwrap().as_type().unwrap();
                let mut llvm_args = vec![];
                let t = ty0.get_element_type().unwrap();
                let element_ty = build_type(t, ctx);
                for child in self.children.iter() {
                    let mut v = child.build_llvm(ctx).unwrap();
                    if t.is_any() {
                        let val = v.get_value();
                        let temp = builder.build_struct_insert(
                            Global::undef(element_ty.clone()),
                            0,
                            Global::const_i32(v.get_type().id()),
                        );
                        let r = builder.build_struct_insert(temp, 1, val);
                        v = Value::new(r, t.clone());
                    }
                    llvm_args.push(v.get_value());
                }
                let len = llvm_args.len();
                let array_llvm_type = build_type(ty0,ctx);
                let v1 = builder.build_array(array_llvm_type, llvm_args);
                let ty = build_type(ty0,ctx);
                let tmp =
                    builder.build_struct_insert(Global::undef(ty), 0, Global::const_i64(len as i64));
                let v = builder.build_struct_insert(tmp, 1, v1);
                return Some(Value::new(v, ty0.clone()))
            }
            "Expr:FnCall"=>{
                dbg!(&self);

            }
            _=>{
                println!("unsupported {}",self.id);
            }
        }
        for child in self.children.iter() {
            child.build_llvm(ctx);
        }
        return None
    }
    fn build_fn_call(&self,ctx:&Context,builder:Arc<Builder>,llvm_module:Arc<RwLock<LLVMModule>>) {
        let name = self.data.get("name").unwrap().as_str().unwrap();
    }
}