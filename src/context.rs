use crate::error::{PipelineError, PipelineResult};
use crate::expr::{Expr, Op};
use crate::lexer::Lexer;
use crate::module::Module;
use crate::position::Position;
use crate::stmt::Stmt;
use crate::types::Value::Mutable;
use crate::types::{Dynamic, FnPtr, SignalType, Struct, Value, WrapValue};
use std::any::Any;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};


#[derive(Clone, Debug)]
pub struct Context {
    parent: Option<Box<Context>>,
    key: ContextKey,
    value: ContextValue,
}
#[derive(Clone, Debug)]
pub enum ContextKey {
    MainModule,
    SourceCode(String),
    Scope,
    Position,
    NativeObject(String),
}

impl PartialEq for ContextKey {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ContextKey::MainModule => {
                if let ContextKey::MainModule = other {
                    return true;
                }
                false
            }
            ContextKey::SourceCode(s) => {
                if let ContextKey::SourceCode(other) = other {
                    if s == other {
                        return true;
                    }
                }
                false
            }
            ContextKey::Scope => {
                if let ContextKey::Scope = other {
                    return true;
                }
                false
            }
            ContextKey::NativeObject(s) => {
                if let ContextKey::NativeObject(other) = other {
                    if s == other {
                        return true;
                    }
                }
                false
            }
            ContextKey::Position => {
                if let ContextKey::Position = other {
                    return true;
                }
                false
            }
        }
    }
}

impl Context {
    pub fn background() -> Self {
        let ctx = Self {
            parent: None,
            key: ContextKey::MainModule,
            value: ContextValue::Module(Arc::new(RwLock::new(Module::new("main")))),
        };

        Self {
            parent: Some(Box::new(ctx)),
            key: ContextKey::Scope,
            value: ContextValue::Scope(Arc::new(RwLock::new(Scope::new()))),
        }
    }
    pub fn with_value(parent: &Context, key: ContextKey, value: ContextValue) -> Self {
        Self {
            parent: Some(Box::new(parent.clone())),
            key,
            value,
        }
    }
    pub fn get_module(&self) -> Arc<RwLock<Module>> {
        let r = self.get(ContextKey::MainModule).unwrap();
        r.as_module().unwrap()
    }
    pub fn get_scope(&self) -> Arc<RwLock<Scope>> {
        let r = self.get(ContextKey::Scope).unwrap();
        r.as_scope().unwrap()
    }
    pub fn get_value(&self, key: impl AsRef<str>) -> Option<Value> {
        let s = self.get_scope();
        let s = s.read().unwrap();
        let v = s.get(key.as_ref());
        match v {
            None => {
                let module = self.get_module();
                let module = module.read().unwrap();
                let f = module.get_function(key.as_ref());
                if let Some(f) = f {
                    let ptr = f.get_ptr();
                    let d = Dynamic::FnPtr(Box::new(ptr));
                    return Some(d.into());
                }
                None
            }
            Some(v) => Some(v),
        }
    }
    pub fn set_value(&self,key:impl AsRef<str>,value:Value){
        let scope =self.get_scope();
        let mut scope = scope.write().unwrap();
        scope.set(key.as_ref(), value);
    }

    pub fn get(&self, key: ContextKey) -> Option<ContextValue> {
        if self.key == key {
            return Some(self.value.clone());
        }
        match &self.parent {
            None => None,
            Some(parent) => parent.get(key),
        }
    }
    pub fn eval_stmt(&mut self, stmt: &Stmt) -> PipelineResult<Value> {
        match stmt {
            Stmt::EvalExpr(e, _) => {
                return self.eval_expr(e);
            }
            Stmt::Import(s, pos) => {
                let m = self.get_module();
                let mut m = m.write().unwrap();
                let b = m.merge_into_main(s);
                if !b {
                    return Err(PipelineError::UnknownModule(s.into(), pos.clone()));
                }
            }
            Stmt::Var(l, _) => {
                let mut d = self.eval_expr(&l.1)?;
                if !d.is_mutable() {
                    d = Value::Mutable(d.as_arc());
                }
                let scope = self.get_scope();
                let mut scope = scope.write().unwrap();
                scope.set(l.0.as_str(), d);
                return Ok(().into());
            }
            Stmt::Val(l, _) => {
                let mut d = self.eval_expr(&l.1)?;
                if d.is_mutable() {
                    d = Value::with_immutable(d.as_dynamic());
                }
                let scope = self.get_scope();
                let mut scope = scope.write().unwrap();
                scope.set(l.0.as_str(), d);
                return Ok(().into());
            }
            Stmt::Break(_) => return Ok(Value::Signal(SignalType::Break)),
            Stmt::Continue(_) => return Ok(Value::Signal(SignalType::Continue)),
            Stmt::Assign(e, pos) => {
                let target = self.eval_expr(&e.0)?;
                let value = self.eval_expr(&e.1)?;
                if !target.can_mutable() {
                    return Err(PipelineError::AssignToImmutableVariable(
                        e.0.try_get_variable_name().unwrap(),
                        pos.clone(),
                    ));
                }
                let target = target.get_mut_arc();
                let mut target = target.write().unwrap();
                *target = value.as_dynamic();
            }
            Stmt::Return(e, _) => {
                let r = self.eval_expr(e)?;
                return Ok(Value::Signal(SignalType::Return(Box::new(r))));
            }
            Stmt::If(b, _) => {
                for if_branch in b.get_branches() {
                    let d = self.eval_expr(if_branch.get_condition())?;
                    let d = d.as_dynamic().as_bool();
                    match d {
                        None => return Err(PipelineError::ExpectedType("bool".into())),
                        Some(d) => {
                            let mut l = None;
                            if d {
                                for i in if_branch.get_body() {
                                    let t = self.eval_stmt(i)?;
                                    l = Some(t)
                                }
                                if l.is_none() {
                                    return Ok(().into());
                                }
                                return Ok(l.unwrap());
                            }
                        }
                    }
                }
                if let Some(else_body) = b.get_else_body() {
                    let mut l = None;
                    for i in &else_body {
                        l = Some(self.eval_stmt(i)?);
                    }
                    return Ok(l.unwrap());
                }
            }
            Stmt::IndexAssign(target, i, v, _) => {
                let i = self.eval_expr(i)?;
                let v = self.eval_expr(v)?;
                let target = self.eval_expr(target)?;
                let target = target.as_arc();
                let mut target = target.write().unwrap();
                if target.is_array() {
                    let a = target.as_mut_array().unwrap();
                    let index = i.as_dynamic().as_integer().unwrap();
                    let e = a[index as usize].get_mut_arc();
                    let mut e = e.write().unwrap();
                    *e = v.as_dynamic();
                } else if target.is_map() {
                    let m = target.as_mut_map().unwrap();
                    let key = i.as_dynamic();
                    m.insert(key, v);
                } else {
                    panic!("{} cannot support index assign", target.type_name())
                }
            }
            Stmt::While(b, blocks, _) => {
                let d = self.eval_expr(b)?;
                let d = d.as_dynamic().as_bool();
                return match d {
                    None => Err(PipelineError::ExpectedType("bool".into())),
                    Some(d) => {
                        let mut condition = d;
                        'outer: while condition {
                            'inner: for i in blocks.iter() {
                                let r = self.eval_stmt(i)?;
                                if let Value::Signal(s) = r {
                                    match s {
                                        SignalType::Break => break 'outer,
                                        SignalType::Continue => break 'inner,
                                        SignalType::Return(v) => {
                                            return Ok(Value::Signal(SignalType::Return(v)))
                                        }
                                    }
                                }
                            }
                            let d0 = self.eval_expr(b)?;
                            condition = d0.as_dynamic().as_bool().unwrap();
                        }
                        Ok(().into())
                    }
                };
            }
            Stmt::ForIn(one, other, target, blocks, ..) => {
                let target = self.eval_expr(target)?;
                let target = target.as_dynamic().as_array().unwrap();
                let mut count = 0;
                'outer: for i in target {
                    let mut scope = Scope::new();
                    match other.clone() {
                        None => {
                            scope.set(one.as_str(), i);
                        }
                        Some(s) => {
                            scope.set(one.as_str(), count.into());
                            count += 1;
                            scope.set(s.as_str(), i);
                        }
                    }
                    let parent_scope = self.get_scope();
                    scope.set_parent(parent_scope);
                    let scope = Arc::new(RwLock::new(scope));
                    let mut ctx =
                        Context::with_value(self, ContextKey::Scope, ContextValue::Scope(scope));
                    'inner: for i in blocks.iter() {
                        let r = ctx.eval_stmt(i)?;
                        if let Value::Signal(s) = r {
                            match s {
                                SignalType::Break => break 'outer,
                                SignalType::Continue => break 'inner,
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
    pub fn eval_expr(&mut self, expr: &Expr) -> PipelineResult<Value> {
        match expr {
            Expr::FnCall(fn_call, pos) => {
                let mut v = vec![];
                for i in &fn_call.args {
                    let r = self.eval_expr(&i.value)?;
                    if i.has_name() {
                        v.push(WrapValue::with_name(i.get_name().unwrap(), r));
                    } else {
                        v.push(WrapValue::new(r));
                    }
                }
                // 判断是否是构造函数调用
                let module = self.get_module();
                let module = module.read().unwrap();
                let class = module.get_class(fn_call.name.as_str());
                // 找得到说明是构造函数
                if let Some(c) = class {
                    let mut props = HashMap::new();
                    // 给类赋默认值
                    for vd in c.get_attributions().iter() {
                        if vd.has_default() {
                            let default = self.eval_expr(vd.get_default().unwrap())?;
                            if default.is_immutable() {
                                let d = default.as_dynamic();
                                if vd.declaration_type != d.type_name() {
                                    return Err(PipelineError::MismatchedType(
                                        vd.declaration_type.clone(),
                                        d.type_name(),
                                        pos.clone(),
                                    ));
                                }
                                props.insert(vd.name.clone(), Value::with_mutable(d));
                                continue;
                            }
                            props.insert(vd.name.clone(), default);
                        }
                    }
                    for (i, vd) in c.get_attributions().iter().enumerate() {
                        if i >= v.len() {
                            break;
                        }
                        if v[i].is_immutable() {
                            let d = v[i].as_dynamic();
                            if vd.declaration_type != d.type_name() {
                                return Err(PipelineError::MismatchedType(
                                    vd.declaration_type.clone(),
                                    d.type_name(),
                                    pos.clone(),
                                ));
                            }
                            if v[i].has_name() {
                                props.insert(
                                    v[i].get_name().unwrap().into(),
                                    Value::with_mutable(d),
                                );
                            } else {
                                props.insert(vd.name.clone(), Value::with_mutable(d));
                                continue;
                            }
                        }
                        if v[i].has_name() {
                            props.insert(v[i].get_name().unwrap().into(), v[i].clone_value());
                        } else {
                            props.insert(vd.name.clone(), v[i].clone_value());
                        }
                    }
                    let obj = Struct::new(fn_call.name.clone(), props);
                    return Ok(Value::Mutable(Arc::new(RwLock::new(Dynamic::Struct(
                        Box::new(obj),
                    )))));
                }
                // 正常函数调用
                let mut ctx = Context::with_value(
                    self,
                    ContextKey::Position,
                    ContextValue::Position(pos.clone()),
                );
                module.call(&mut ctx, &fn_call.name, v)
            }
            Expr::Variable(i, pos) => {
                let d = self.get_value(i);
                match d {
                    None => Err(PipelineError::VariableUndefined(i.clone(), pos.clone())),
                    Some(v) => Ok(v),
                }
            }
            Expr::Array(v, _) => {
                let mut dv = vec![];
                for e in v {
                    let mut d = self.eval_expr(e)?;
                    if d.is_immutable() {
                        d = Value::Mutable(Arc::new(RwLock::new(d.as_dynamic())))
                    }
                    dv.push(d)
                }
                Ok(Value::Mutable(Arc::new(RwLock::new(Dynamic::Array(dv)))))
            }
            Expr::Map(v, _) => {
                let mut dv = HashMap::new();
                for e in v {
                    let key = self.eval_expr(&e.0)?;
                    let mut value = self.eval_expr(&e.1)?;
                    if value.is_immutable() {
                        value = Mutable(Arc::new(RwLock::new(value.as_dynamic())));
                    }
                    dv.insert(key.as_dynamic().clone(), value);
                }
                Ok(Value::Mutable(Arc::new(RwLock::new(Dynamic::Map(dv)))))
            }
            Expr::Index(s, e, _) => {
                let d = self.eval_expr(s)?;
                let d = d.as_dynamic();
                match d {
                    Dynamic::Array(a) => {
                        let index = self.eval_expr(e)?;
                        let index = index.as_dynamic().as_integer().unwrap();
                        Ok(a[index as usize].clone())
                    }
                    Dynamic::Map(m) => {
                        let index = self.eval_expr(e)?;
                        let index = index.as_dynamic();
                        Ok(m[&index].clone())
                    }
                    Dynamic::String(s) => {
                        let index = self.eval_expr(e)?;
                        let index = index.as_dynamic().as_integer().unwrap();
                        let r = String::from(s.chars().nth(index as usize).unwrap());
                        Ok(r.into())
                    }
                    t => Err(PipelineError::UndefinedOperation(format!(
                        "index [] to {}",
                        t.type_name()
                    ))),
                }
            }
            Expr::Unary(op, expr, _) => match op {
                Op::Negate => {
                    let e = self.eval_expr(expr)?;
                    let r = e.as_bool().unwrap();
                    return Ok((!r).into());
                }
                _ => {
                    panic!("不支持一元操作")
                }
            },
            Expr::Binary(op, l, r, _) => match op {
                Op::Plus => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r + r_r).into())
                }
                Op::Minus => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r - r_r).into())
                }
                Op::Mul => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r * r_r).into())
                }
                Op::Greater => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r > r_r).into())
                }
                Op::Less => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r < r_r).into())
                }
                Op::Equal => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r == r_r).into())
                }
                Op::NotEqual => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r != r_r).into())
                }
                Op::Div => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r / r_r).into())
                }
                Op::Mod => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic();
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic();
                    Ok((l_r % r_r).into())
                }
                Op::And => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic().as_bool().unwrap();
                    if !l_r {
                        return Ok(l_r.into());
                    }
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic().as_bool().unwrap();
                    Ok((l_r && r_r).into())
                }
                Op::Or => {
                    let l_r = self.eval_expr(l)?;
                    let l_r = l_r.as_dynamic().as_bool().unwrap();
                    if l_r {
                        return Ok(l_r.into());
                    }
                    let r_r = self.eval_expr(r)?;
                    let r_r = r_r.as_dynamic().as_bool().unwrap();
                    Ok((l_r || r_r).into())
                }
                _ => panic!("不支持二元操作"),
            },
            Expr::Struct(e, _) => {
                let mut props = HashMap::new();
                for (k, i) in e.get_props() {
                    let mut v = self.eval_expr(i)?;
                    if v.is_immutable() || v.is_mutable() {
                        let scope = self.get_scope();
                        let mut scope = scope.write().unwrap();
                        let v0 = Value::Mutable(v.as_arc());
                        v = Value::Refer(v0.as_weak());
                        let id = scope.data.len() + 1;
                        scope.set(format!("{}.{}.{}", e.get_name(), k, id).as_str(), v0)
                    }
                    props.insert(k.clone(), v);
                }
                Ok(Value::Mutable(Arc::new(RwLock::new(Dynamic::Struct(
                    Box::new(Struct::new(e.get_name().into(), props)),
                )))))
            }
            Expr::MemberAccess(father, prop, pos) => {
                let obj = self.eval_expr(father)?;
                let obj = obj.as_dynamic();
                match obj {
                    Dynamic::Struct(s) => {
                        let r = s.get_prop(prop).unwrap();
                        Ok(r)
                    }
                    Dynamic::Map(m) => {
                        let r = m.get(&Dynamic::String(prop.into()));
                        match r {
                            None => {
                                let name = father.try_get_variable_name().unwrap();
                                Err(PipelineError::MapKeyNotExist(
                                    name,
                                    prop.clone(),
                                    pos.clone(),
                                ))
                            }
                            Some(r) => Ok(r.clone()),
                        }
                    }
                    _ => panic!("can not support access member"),
                }
            }
            Expr::StringConstant(s, _) => Ok(s.to_string().into()),
            Expr::IntConstant(i, _) => Ok((*i).into()),
            Expr::FloatConstant(f, _) => Ok((*f).into()),
            Expr::FnClosure(f, _) => {
                let mut ptr = FnPtr::new("");
                ptr.set_fn_def(&(f.def));
                Ok(Value::with_immutable(Dynamic::FnPtr(Box::new(ptr))))
            }
            Expr::None(_) => Ok(().into()),
        }
    }
}
#[derive(Clone, Debug)]
pub enum SourceCode {
    Path(String),
    Source(String),
}

impl SourceCode {
    pub fn get_line(&self, line: usize) -> String {
        match self {
            SourceCode::Path(_) => todo!(),
            SourceCode::Source(s) => {
                let lexer = Lexer::from_script("", s);
                lexer.line(line)
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ContextValue {
    // GlobalState(Arc<RwLock<AppContext<String>>>),
    // JoinSet(Arc<RwLock<Vec<JoinHandle<PipelineResult<()>>>>>),
    Scope(Arc<RwLock<Scope>>),
    Env(Arc<RwLock<HashMap<String, String>>>),
    Position(Position),
    Local(String),
    Module(Arc<RwLock<Module>>),
    Source(SourceCode),
    Native(Arc<RwLock<dyn Any + Send + Sync>>),
}

impl ContextValue {
    pub fn as_module(&self) -> Option<Arc<RwLock<Module>>> {
        match self {
            ContextValue::Module(m) => Some(m.clone()),
            _ => None,
        }
    }
    pub fn as_native(&self) -> Option<Arc<RwLock<dyn Any + Send + Sync>>> {
        match self {
            ContextValue::Native(m) => Some(m.clone()),
            _ => None,
        }
    }
    // pub fn as_join_set(&self) -> Option<Arc<RwLock<dyn Any + Send + Sync>>> {
    //     match self {
    //         ContextValue::JoinSet(m) => Some(m.clone()),
    //         _ => None,
    //     }
    // }
    pub fn as_position(&self) -> Option<Position> {
        match self {
            ContextValue::Position(m) => Some(m.clone()),
            _ => None,
        }
    }
    pub fn as_source(&self) -> Option<SourceCode> {
        match self {
            ContextValue::Source(m) => Some(m.clone()),
            _ => None,
        }
    }
    pub fn as_scope(&self) -> Option<Arc<RwLock<Scope>>> {
        match self {
            ContextValue::Scope(m) => Some(m.clone()),
            _ => None,
        }
    }
}
#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<Arc<RwLock<Scope>>>,
    data: HashMap<String, Value>,
}
impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            parent: None,
        }
    }

    pub fn set_parent(&mut self, p: Arc<RwLock<Scope>>) {
        self.parent = Some(p)
    }
    pub fn get(&self, key: &str) -> Option<Value> {
        let r = self.data.get(key);
        match r {
            None => {
                if self.parent.is_some() {
                    let rr = self.parent.clone().unwrap();
                    let rr = rr.read().unwrap();
                    return rr.get(key);
                }
                None
            }
            Some(s) => Some(s.clone()),
        }
    }
    pub fn set(&mut self, key: &str, value: Value) {
        self.data.insert(key.into(), value);
    }
}
