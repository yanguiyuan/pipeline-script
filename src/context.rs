use std::any::Any;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::thread::JoinHandle;
use crate::error::{PipelineError, PipelineResult};
use crate::expr::{Expr, Op};
use crate::module::Module;
use crate::position::Position;
use crate::stmt::Stmt;
use crate::types::{Dynamic, FnPtr, SignalType, Struct, Value};
use crate::types::Value::Mutable;

#[derive(Clone,Debug)]
pub struct Context{
    parent:Option<Box<Context>>,
    key:ContextKey,
    value:ContextValue
}
#[derive(Clone, Debug)]
pub enum  ContextKey{
    MainModule,
    SourceCode(String),
    Scope,
    NativeObject(String)
}

impl PartialEq for ContextKey {
    fn eq(&self, other: &Self) -> bool {
        return match self {
            ContextKey::MainModule => {
                if let ContextKey::MainModule = other {
                    return true
                }
                false
            }
            ContextKey::SourceCode(s) => {
                if let ContextKey::SourceCode(other) = other {
                    if s == other {
                        return true
                    }
                }
                false
            }
            ContextKey::Scope=>{
                if let ContextKey::Scope= other {
                    return true
                }
                false
            }
            ContextKey::NativeObject(s)=>{
                if let ContextKey::NativeObject(other) = other {
                    if s == other {
                        return true
                    }
                }
                false
            }
        }
    }
}

impl Context{
    pub fn background()->Self{
        let ctx=Self{
            parent:None,
            key:ContextKey::MainModule,
            value:ContextValue::Module(Arc::new(RwLock::new(Module::new("main"))))
        };

        Self{
            parent:Some(Box::new(ctx)),
            key:ContextKey::Scope,
            value:ContextValue::Scope(Arc::new(RwLock::new(Scope::new())))
        }
    }
    pub fn with_value(parent:&Context,key:ContextKey,value:ContextValue)->Self{
        Self{
            parent:Some(Box::new(parent.clone())),key,value
        }
    }
    pub fn get_module(&self)->Arc<RwLock<Module>>{
       let r=self.get(ContextKey::MainModule).unwrap();
        return r.as_module().unwrap()
    }
    pub fn get_scope(&self)->Arc<RwLock<Scope>>{
        let r=self.get(ContextKey::Scope).unwrap();
        return r.as_scope().unwrap()
    }
    pub fn get_value(&self,key:impl AsRef<str>)->Value{
        let s=self.get_scope();
        let s=s.read().unwrap();
        return s.get(key.as_ref()).unwrap()
    }

    pub fn get(&self,key:ContextKey)->Option<ContextValue>{
        if self.key==key{
            return Some(self.value.clone())
        }
        return match &self.parent {
            None => None,
            Some(parent) => {
                parent.get(key)
            }
        }
    }
    pub fn eval_stmt(&mut self, stmt:&Stmt)->PipelineResult<Value>{
        match stmt {
            // Stmt::FnCall(fc, pos) => {
            //     todo!()
            //     // self.eval_fn_call_expr_with_context(*fc)?;
            // }
            Stmt::EvalExpr(e,_)=>{
                return self.eval_expr(e);
            }
            Stmt::Import(s,_)=>{
                // self.merge_into_main_module(s)?;
                todo!()
            }
            Stmt::Let(l,_)=>{
                let mut d =self.eval_expr( &l.1)?;
                if !d.is_mutable(){
                    d=Value::Mutable(d.as_arc());
                }
                let scope=self.get_scope();
                let mut scope=scope.write().unwrap();
                scope.set(l.0.as_str(),d);
                return Ok(().into())
            }
            Stmt::Break(_)=>{
                return Ok(Value::Signal(SignalType::Break))
            }
            Stmt::Continue(_)=>{
                return Ok(Value::Signal(SignalType::Continue))
            }
            Stmt::Assign(e,_)=>{
                let target=self.eval_expr(&e.0)?;
                let value=self.eval_expr(&e.1)?;
                if !target.can_mutable(){
                    panic!("it must be mutable")
                }
                let target=target.get_mut_arc();
                let mut target=target.write().unwrap();
                *target=value.as_dynamic();
            }
            Stmt::Return(e,_)=>{
                let r=self.eval_expr( e)?;
                return Ok(Value::Signal(SignalType::Return(Box::new(r))))
            }
            Stmt::If(b,_)=>{
                for if_branch in b.get_branches(){
                    let d=self.eval_expr(if_branch.get_condition())?;
                    let d=d.as_dynamic().as_bool();
                    match d {
                        None => {
                            return Err(PipelineError::ExpectedType("bool".into()))
                        }
                        Some(d) => {
                            let mut l=None;
                            if d {
                                for i in if_branch.get_body() {
                                    let t=self.eval_stmt( i)?;
                                    l=Some(t)
                                }
                                if let None=l{
                                    return Ok(().into())
                                }
                                return Ok(l.unwrap())
                            }
                        }
                    }
                }
                if let Some(else_body)=b.get_else_body(){
                    let mut l=None;
                    for i in &else_body {
                        l=Some(self.eval_stmt( i)?);
                    }
                    return Ok(l.unwrap())
                }

            }
            Stmt::IndexAssign(target,i,v,_)=>{
                let i=self.eval_expr(i)?;
                let v=self.eval_expr(v)?;
                let target=self.eval_expr(target)?;
                let target=target.get_mut_arc();
                let mut target=target.write().unwrap();
                if target.is_array(){
                    let a=target.as_mut_array().unwrap();
                    let index=i.as_dynamic().as_integer().unwrap();
                    a[index as usize]=v;
                }else if target.is_map(){
                    let m=target.as_mut_map().unwrap();
                    let key=i.as_dynamic();
                    m.insert(key,v);
                }else{
                    panic!("{} cannot support index assign",target.type_name())
                }
            }
            Stmt::While(b,blocks,_)=>{
                let d=self.eval_expr(b)?;
                let d=d.as_dynamic().as_bool();
                return match d {
                    None => {
                        Err(PipelineError::ExpectedType("bool".into()))
                    }
                    Some(d) => {

                        let mut condition=d;
                        'outer:while condition {
                            'inner:for i in blocks.iter() {
                                let r=self.eval_stmt( i)?;
                                if let Value::Signal(s)=r{
                                    match s {
                                        SignalType::Break => {
                                            break 'outer
                                        }
                                        SignalType::Continue => {
                                            break 'inner
                                        }
                                        SignalType::Return(v) => {
                                            return Ok(Value::Signal(SignalType::Return(v)))
                                        }
                                    }
                                }
                            }
                            let d0=self.eval_expr(b)?;
                            condition=d0.as_dynamic().as_bool().unwrap();
                        }
                        Ok(().into())
                    }
                }
            }
            Stmt::ForIn(one,other ,target, blocks, ..)=> {
                let target = self.eval_expr( target)?;
                let target = target.as_dynamic().as_array().unwrap();
                let mut count=0;
                'outer:for i in target{
                    let mut scope=Scope::new();
                    match other.clone() {
                        None => {
                            scope.set(one.as_str(),i);
                        }
                        Some(s) => {
                            scope.set(one.as_str(),count.into());
                            count+=1;
                            scope.set(s.as_str(),i);
                        }
                    }
                    let parent_scope=self.get_scope();
                    scope.set_parent(parent_scope);
                    let mut scope=Arc::new(RwLock::new(scope));
                    let mut ctx =Context::with_value(&self, ContextKey::Scope, ContextValue::Scope(scope));
                    'inner: for i in blocks.iter() {
                        let r = ctx.eval_stmt(i)?;
                        if let Value::Signal(s) = r {
                            match s {
                                SignalType::Break => {
                                    break 'outer
                                }
                                SignalType::Continue => {
                                    break 'inner
                                }
                                SignalType::Return(v) => {
                                    return Ok(Value::Signal(SignalType::Return(v)))
                                }
                            }
                        }
                    }
                }
            }
            Stmt::Noop => {}
        }
        Ok(().into())
    }
    pub fn eval_expr(&mut self,expr:&Expr)->PipelineResult<Value>{
        match expr {
            Expr::FnCall(fn_call, _)=>{
                let mut v=vec![];
                for i in &fn_call.args{
                    let r=self.eval_expr(&i)?;
                    v.push(r);
                }
                let module=self.get_module();
                let module=module.read().unwrap();
                module.call(self,&fn_call.name,v)
            }
            Expr::Variable(i,_)=>{
                let d=self.get_value(&i);
                Ok(d)
            }
            Expr::Array(v,_)=>{
                let mut dv=vec![];
                for e in v{
                    let d=self.eval_expr(e)?;
                    // if d.is_mutable(){
                    //     panic!("不能持有所有权")
                    // }
                    dv.push(d)
                }
                Ok(Value::Mutable(Arc::new(RwLock::new(Dynamic::Array(dv)))))
            }
            Expr::Map(v,_)=>{
                let mut dv=HashMap::new();
                for e in v{
                    let key=self.eval_expr( &e.0)?;
                    let mut value =self.eval_expr( &e.1)?;
                    if value.is_immutable(){
                        value=Mutable(Arc::new(RwLock::new(value.as_dynamic())));
                    }
                    dv.insert(key.as_dynamic().clone(),value);
                }
                Ok(Value::Mutable(Arc::new(RwLock::new(Dynamic::Map(dv)))))
            }
            Expr::Index(s,e,_)=>{
                let d=self.eval_expr(s)?;
                let d=d.as_dynamic();
                match d {
                    Dynamic::Array(a) => {
                        let index=self.eval_expr(e)?;
                        let index=index.as_dynamic().as_integer().unwrap();
                        Ok(a[index as usize].clone())
                    }
                    Dynamic::Map(m) => {
                        let index=self.eval_expr(e)?;
                        let index=index.as_dynamic();
                        Ok(m[&index].clone())
                    }
                    Dynamic::String(s)=>{
                        let index=self.eval_expr(e)?;
                        let index=index.as_dynamic().as_integer().unwrap();
                        let r=String::from(s.chars().nth(index as usize).unwrap());
                        Ok(r.into())
                    }
                    t=>{
                        return Err(PipelineError::UndefinedOperation(format!("index [] to {}",t.type_name())))
                    }
                }

            }


            Expr::BinaryExpr(op,l,r,_)=>{
                match op {
                    Op::Plus => {
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r+r_r).into())
                    }
                    Op::Minus => {
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r-r_r).into())
                    }
                    Op::Mul=>{
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r*r_r).into())
                    }
                    Op::Greater=>{
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r>r_r).into())
                    }
                    Op::Less=>{
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r<r_r).into())
                    }
                    Op::Equal=>{
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r==r_r).into())
                    }
                    Op::NotEqual=>{
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r!=r_r).into())
                    }
                    Op::Div=>{
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r/r_r).into())
                    }
                    Op::Mod=>{
                        let l_r=self.eval_expr(l)?;
                        let l_r=l_r.as_dynamic();
                        let r_r=self.eval_expr(r)?;
                        let r_r=r_r.as_dynamic();
                        return Ok((l_r%r_r).into())
                    }
                }
            }
            Expr::Struct(e,_)=>{
                let mut props=HashMap::new();
                for (k,i) in e.get_props(){
                    let mut v=self.eval_expr(i)?;
                    if v.is_immutable()||v.is_mutable(){
                        let scope=self.get_scope();
                        let mut scope=scope.write().unwrap();
                        let  v0=Value::Mutable(v.as_arc());
                        v=Value::Refer(v0.as_weak());
                        let id=scope.data.len()+1;
                        scope.set(format!("{}.{}.{}",e.get_name(),k,id).as_str(),v0)
                    }
                    props.insert(k.clone(),v);
                }
                Ok(
                    Value::Mutable(
                        Arc::new(
                            RwLock::new(
                                Dynamic::Struct(
                                    Box::new(
                                        Struct::new(e.get_name().into(),props)
                                    )
                                )
                            )
                        )
                    )
                )
            }
            Expr::MemberAccess(father,prop,_)=>{
                let obj=self.eval_expr(father)?;
                let obj=obj.as_dynamic();
                match obj {
                    Dynamic::Struct(s)=>{
                        let r=s.get_prop(&prop).unwrap();
                        return Ok(r)
                    }
                    Dynamic::Map(m)=>{
                        let r=m.get(&Dynamic::String(prop.into())).unwrap();
                        return Ok(r.clone())
                    }
                    _=>panic!("can not support access member")
                }

            }
            Expr::StringConstant(s,_)=>Ok(s.to_string().into()),
            Expr::IntConstant(i, _) => Ok((*i).into()),
            Expr::FloatConstant(f, _) =>Ok((*f).into()),
            Expr::FnClosure(f, _) => {
                let mut ptr=FnPtr::new("");
                ptr.set_fn_def(&(f.def));
                Ok(Value::Immutable(Dynamic::FnPtr(Box::new(ptr))))
            }
            Expr::None(_) => {Ok(().into())}
        }
    }
}
#[derive(Clone, Debug)]
pub enum SourceCode{
    Path(String),
    Source(String)
}
#[derive(Debug,Clone)]
pub enum ContextValue{
    // GlobalState(Arc<RwLock<AppContext<String>>>),
    JoinSet(Arc<RwLock<Vec<JoinHandle<PipelineResult<()>>>>>),
    Scope(Arc<RwLock<Scope>>),
    Env(Arc<RwLock<HashMap<String,String>>>),
    Position(Position),
    Local(String),
    Module(Arc<RwLock<Module>>),
    Source(SourceCode),
    Native(Arc<RwLock<dyn Any+Send+Sync>>)
}

impl ContextValue {
    pub fn as_module(&self)->Option<Arc<RwLock<Module>>>{
        match self {
            ContextValue::Module(m)=>Some(m.clone()),
            _=>None
        }
    }
    pub fn as_native(&self)->Option<Arc<RwLock<dyn Any+Send+Sync>>>{
        match self {
            ContextValue::Native(m)=>Some(m.clone()),
            _=>None
        }
    }
    pub fn as_scope(&self)->Option<Arc<RwLock<Scope>>>{
        match self {
            ContextValue::Scope(m)=>Some(m.clone()),
            _=>None
        }
    }
}
#[derive(Debug,Clone)]
pub struct Scope{
    parent:Option<Arc<RwLock<Scope>>>,
    data:HashMap<String,Value>
}

impl Scope {
    pub fn new()->Self{
        Self{data:HashMap::new(),parent:None}
    }
    pub fn set_parent(&mut self,p:Arc<RwLock<Scope>>){self.parent=Some(p)}
    pub fn get(&self, key:&str) ->Option<Value>{
        let r=self.data.get(key);
        match r {
            None => {
                if self.parent.is_some(){
                   let rr=self.parent.clone().unwrap();
                    let rr=rr.read().unwrap();
                    return rr.get(key)
                }
                return None
            }
            Some(s) => {Some(s.clone())}
        }
    }
    pub fn set(&mut self,key:&str,value:Value){
        self.data.insert(key.into(),value);
    }
}