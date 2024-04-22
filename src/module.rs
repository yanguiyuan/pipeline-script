
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::{fs, io, ptr, thread};
use std::fs::File;
use std::io::{Stdin, Write};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::process::exit;
use std::sync::{Arc, RwLock};
use rand::{random, Rng};
use regex::Regex;
use scanner_rust::Scanner;
use ssh::LocalSession;
use crate::context::{Context, ContextKey, ContextValue, Scope};
use crate::error::{PipelineError, PipelineResult};
use crate::stmt::Stmt;
use crate::types::{SignalType, Value};

trait NativeFunction<Marker>{
    fn into_pipe_function(self) ->Arc<PipeFn>;
}
trait NativeType{}
impl NativeType for String{}
impl NativeType for i64{}
impl NativeType for f64{}

type PipeFn= dyn Send+Sync+ Fn(&mut Context, Vec<Value>) -> PipelineResult<Value>;
#[derive(Clone)]
pub enum Function{
    Native(Arc<PipeFn>),
    Script(Box<FnDef>),
    Method(Box<FnDef>),
}
#[derive(Debug,Clone)]
pub struct  VariableDeclaration{
    pub name:String,
    pub declaration_type:String
}

impl VariableDeclaration {
    pub fn new(name:String,dec:String)->Self{
        Self{name,declaration_type:dec}
    }
}
#[derive(Debug,Clone)]
pub struct FnDef{
    pub name:String,
    pub return_type:String,
    pub args:Vec<VariableDeclaration>,
    pub body:Vec<Stmt>
}

impl FnDef {
    pub fn new(name:String,args:Vec<VariableDeclaration>,body:Vec<Stmt>,return_type:String)->Self{
        Self{name,args,body,return_type}
    }
}
impl Debug for Function{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(_) => {
                write!(f,"Native Function")
            }
            Function::Script(s) => {
                write!(f,"{s:?}")
            }
            Function::Method(m)=>{
                write!(f,"{m:?}")
            }
        }

    }
}
#[derive(Clone,Debug)]
pub struct Module{
    name:String,
    functions:HashMap<String,Function>,
    classes:HashMap<String,Class>,
    block:Vec<Stmt>,
    submodules:HashMap<String,Box<Module>>
}
#[derive(Clone,Debug)]
pub struct Class{
    name:String,
    attributions:Vec<VariableDeclaration>,
    methods:HashMap<String,Function>
}

impl Class {
    pub fn new(name:&str,attributions:Vec<VariableDeclaration>)->Self{
        Self{
            name:name.to_string(),attributions,methods:HashMap::new()
        }
    }
    pub fn get_name(&self)->String{
        self.name.clone()
    }
    pub fn register_method(&mut self,name:String,method:Function){
        self.methods.insert(name,method);
    }
}
impl Function {
    pub fn call(&self, ctx:&mut Context,  args:Vec<Value>) ->PipelineResult<Value>{
        let mut scope=Scope::new();
        let old_scope=ctx.get_scope();
        scope.set_parent(old_scope);
        let mut ctx=Context::with_value(ctx,ContextKey::Scope,ContextValue::Scope(Arc::new(RwLock::new(scope))));
        match self {
            Function::Native(n) => {
                return (*n)(&mut ctx,args);
            }
            Function::Script(s) => {
                let scope=ctx.get_scope();
                let mut scope=scope.write().unwrap();
                let mut index =0;
                for i in &s.args{
                    scope.set(i.name.as_str(),args[index].clone());
                    index+=1;
                }
                drop(scope);
                for i in &s.body{
                    let r=ctx.eval_stmt(i)?;
                    if  let Value::Signal(SignalType::Return(return_value))=r{
                        return Ok(*return_value)
                    }
                }
                return Ok(().into())
            }
            Function::Method(s) => {
                let scope=ctx.get_scope();
                let mut scope=scope.write().unwrap();
                let mut index =0;
                scope.set("this",args[0].clone());
                for i in &s.args{
                    scope.set(i.name.as_str(),args[index+1].clone());
                    index+=1;
                }
                drop(scope);
                for i in &s.body{
                    let r=ctx.eval_stmt(i)?;
                    if  let Value::Signal(SignalType::Return(return_value))=r{
                        return Ok(*return_value)
                    }
                }
                return Ok(().into())
            }
        }
    }
}
impl Module{
    pub fn new(name:impl Into<String>)->Self{
        Self{
            name:name.into(),
            functions:HashMap::new(),
            classes:HashMap::new(),
            block:vec![],
            submodules:HashMap::new()
        }
    }
    pub fn get_functions(&self)->&HashMap<String,Function>{
        return &self.functions
    }
    pub fn register_function(&mut self,name:&str,f:&Function){
        self.functions.insert(name.into(),f.clone());
    }
    pub fn push_block(&mut self,block:Vec<Stmt>){
        block.into_iter().for_each(|e|{
            self.block.push(e)
        })
    }
    pub fn run_block(&self,ctx:&mut Context){
        for i in &self.block{
            ctx.eval_stmt(i);
        }
    }
    pub fn get_block(&self)->&Vec<Stmt>{
       &self.block
    }
    pub fn get_classes(&self)->&HashMap<String,Class>{
        &self.classes
    }
    pub fn get_class_function(&self,class_name:&str,function_name:&str)->Option<Function>{
        let class_result=self.classes.get(class_name);
        return match class_result {
            None => None,
            Some(class) => {
                let function_result = class.methods.get(function_name);
                match function_result {
                    None => None,
                    Some(f) => {
                        Some(f.clone())
                    }
                }
            }
        }
    }
    pub fn register_class_method(&mut self,class_name:impl AsRef<str>,method_name:impl Into<String>,method:Function){
        let class_result=self.classes.get_mut(class_name.as_ref()).unwrap();
        class_result.register_method(method_name.into(),method)
    }
    pub fn register_class(&mut self,class:Class){
        self.classes.insert(class.name.clone(),class);
    }
    pub fn get_name(&self)->String{
        return self.name.clone()
    }
    pub fn merge(&mut self,module: &Module){
        for (k,v) in &module.functions{
            if !self.functions.contains_key(k){
                self.functions.insert(k.clone(),v.clone());
            }
        }
        for (name,class) in &module.classes{
            self.classes.insert(name.clone(),class.clone());
        }
    }
    pub fn register_native_function<A:'static,F>(&mut self, name:impl Into<String>, f:F )
        where F:NativeFunction<A>
    {
        self.functions.insert(name.into(),Function::Native(f.into_pipe_function()));
    }
    pub fn register_pipe_function(&mut self,name:impl Into<String>,f:impl Send+Sync+Fn(&mut Context,Vec<Value>)->PipelineResult<Value> + 'static){
        let a: Arc<PipeFn> = Arc::new(f);
        self.functions.insert(name.into(),Function::Native(a));
    }
    pub fn register_script_function(&mut self,name:impl Into<String>,f:FnDef){
        self.functions.insert(name.into(),Function::Script(Box::new(f)));
    }
    pub fn register_submodule(&mut self,name:impl Into<String>,module:Module){
        self.submodules.insert(name.into(),Box::new(module));
    }
    pub fn call(&self,ctx:&mut Context,function_name:impl Into<String>,args: Vec<Value>)->PipelineResult<Value>{
        let name=function_name.into();
        let f=self.functions.get(name.clone().as_str());
        match f {
            None => {
                let type_name=args[0].as_dynamic().type_name();
                let fun=self.get_class_function(type_name.as_str(),name.as_str()).unwrap();
                return fun.call(ctx,args);
            }
            Some(f) => {
                let r=f.call(ctx,args);
                return r
            }
        }
    }
    pub fn get_function(&self,name:impl Into<String>)->Option<Function>{
        let r=self.functions.get(name.into().as_str());
        match r {
            None => {None}
            Some(s) => {Some(s.clone())}
        }
    }
}
impl<
    T:Fn(A,B)->Ret + 'static + Send + Sync,A:NativeType + From<Value>,
    B:NativeType + From<Value>,
    Ret:NativeType + From<Value>>
NativeFunction<(A,B)> for T
    where Value: From<Ret>
{
    fn into_pipe_function(self) -> Arc<PipeFn> {
        Arc::new(move |_:&mut Context, args:Vec< Value >|->PipelineResult<Value>{
            let mut it=args.iter();
            let a=it.next().unwrap().clone().into();
            let b=it.next().unwrap().clone().into();
            let r=self(a,b);
            return Ok(r.into())
        })
    }
}
impl <T:Fn(A) + 'static + Send + Sync,A:NativeType + From<Value>>NativeFunction<(A)> for T {
    fn into_pipe_function(self) -> Arc<PipeFn> {
        Arc::new(move|_:&mut Context, args:Vec< Value >|->PipelineResult<Value>{
            self(args[0].clone().into());
            Ok(().into())
        })
    }
}