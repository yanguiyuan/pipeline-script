use crate::context::Context;
use crate::error::PipelineResult;
use crate::expr::Expr;
use crate::module::FnDef;
use std::any::Any;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::sync::{Arc, RwLock, Weak};

#[derive(Debug, Clone)]
pub enum Dynamic {
    Unit,
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Variable(String),
    FnPtr(Box<FnPtr>),
    Array(Vec<Value>),
    Map(HashMap<Dynamic, Value>),
    Struct(Box<Struct>),
    Native(Arc<RwLock<dyn Any + Send + Sync>>),
}
#[derive(Debug, Clone)]
pub struct Struct {
    name: String,
    props: HashMap<String, Value>,
}
#[derive(Clone, Debug)]
pub enum SignalType {
    Break,
    Continue,
    Return(Box<Value>),
}
#[derive(Debug, Clone)]
pub enum Value {
    Constant(Dynamic),
    Immutable(Arc<RwLock<Dynamic>>),
    Mutable(Arc<RwLock<Dynamic>>),
    Refer(Weak<RwLock<Dynamic>>),
    Signal(SignalType),
}
#[derive(Debug, Clone)]
pub struct WrapValue {
    name: Option<String>,
    value: Value,
}

impl WrapValue {
    pub fn new(value: Value) -> Self {
        Self { name: None, value }
    }
    pub fn into_value(self) -> Value {
        self.value
    }
    pub fn is_immutable(&self) -> bool {
        self.value.is_immutable()
    }
    pub fn as_dynamic(&self) -> Dynamic {
        self.value.as_dynamic()
    }
    pub fn clone_value(&self) -> Value {
        self.value.clone()
    }
    pub fn has_name(&self) -> bool {
        self.name.is_some()
    }
    pub fn get_name(&self) -> Option<&str> {
        match &self.name {
            None => None,
            Some(s) => Some(s),
        }
    }
    pub fn with_name(name: impl Into<String>, value: Value) -> Self {
        Self {
            name: Some(name.into()),
            value,
        }
    }
}

impl From<Value> for WrapValue {
    fn from(value: Value) -> Self {
        WrapValue::new(value)
    }
}
impl Value {
    pub fn with_mutable(v: Dynamic) -> Self {
        Value::Mutable(Arc::new(RwLock::new(v)))
    }
    pub fn with_immutable(v: Dynamic) -> Self {
        Value::Immutable(Arc::new(RwLock::new(v)))
    }
    pub fn as_string(&self) -> Option<String> {
        self.as_dynamic().as_string()
    }
    pub fn as_float(&self) -> Option<f64> {
        self.as_dynamic().as_float()
    }
    pub fn as_bool(&self) -> Option<bool> {
        self.as_dynamic().as_bool()
    }
    pub fn as_integer(&self) -> Option<i64> {
        self.as_dynamic().as_integer()
    }
    pub fn is_immutable(&self) -> bool {
        matches!(self, Value::Immutable(_) | Value::Constant(_))
    }
    pub fn is_mutable(&self) -> bool {
        matches!(self, Value::Mutable(_))
    }
    pub fn can_mutable(&self) -> bool {
        matches!(self, Value::Mutable(_) | Value::Refer(_))
    }

    pub fn as_dynamic(&self) -> Dynamic {
        match self {
            Value::Immutable(d) => {
                let r = d.read().unwrap();
                r.clone()
            }
            Value::Mutable(d) => {
                let r = d.read().unwrap();
                r.clone()
            }
            Value::Constant(d) => d.clone(),
            Value::Refer(r) => r.upgrade().unwrap().read().unwrap().clone(),
            _ => panic!("signal cannot as dynamic"),
        }
    }
    pub fn get_mut_arc(&self) -> Arc<RwLock<Dynamic>> {
        match self {
            Value::Immutable(_) => {
                panic!("can not be mutable")
            }
            Value::Mutable(d) => d.clone(),
            Value::Refer(r) => r.upgrade().unwrap(),
            _ => panic!("signal cannot as mut arc"),
        }
    }
    pub fn as_arc(&self) -> Arc<RwLock<Dynamic>> {
        match self {
            Value::Immutable(d) => d.clone(),
            Value::Mutable(d) => d.clone(),
            Value::Refer(r) => r.upgrade().unwrap(),
            Value::Constant(d)=>Arc::new(RwLock::new(d.clone())),
            t => {
                println!("{t:?}");
                panic!("signal cannot as arc"); }
        }
    }
    pub fn as_weak(&self) -> Weak<RwLock<Dynamic>> {
        match self {
            Value::Immutable(_) => {
                panic!("can not be weak pointer")
            }
            Value::Mutable(d) => Arc::downgrade(d),
            Value::Refer(r) => r.clone(),
            _ => panic!("signal cannot as weak"),
        }
    }
}

impl From<Weak<RwLock<Dynamic>>> for Value {
    fn from(value: Weak<RwLock<Dynamic>>) -> Self {
        Value::Refer(value)
    }
}

impl From<Dynamic> for Value {
    fn from(value: Dynamic) -> Self {
        Value::Constant(value)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Constant(Dynamic::Unit)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Constant(Dynamic::Integer(value))
    }
}
impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Constant(Dynamic::Float(value))
    }
}
impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Constant(Dynamic::String(value))
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Constant(Dynamic::Boolean(value))
    }
}

impl From<Value> for String {
    fn from(value: Value) -> Self {
        value.as_string().unwrap()
    }
}

impl From<Value> for i64 {
    fn from(value: Value) -> Self {
        value.as_integer().unwrap()
    }
}

impl From<Value> for f64 {
    fn from(value: Value) -> Self {
        value.as_float().unwrap()
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        value.as_bool().unwrap()
    }
}
impl Struct {
    pub fn new(name: String, props: HashMap<String, Value>) -> Self {
        Self { name, props }
    }
    pub fn get_prop(&self, name: &str) -> Option<Value> {
        self.props.get(name).cloned()
    }
}
#[derive(Debug, Clone)]
pub struct FnPtr {
    pub name: String,
    pub params: Vec<Expr>,
    is_defer: bool,
    pub fn_def: Option<FnDef>,
}

impl FnPtr {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            params: vec![],
            fn_def: None,
            is_defer: false,
        }
    }
    pub fn is_defer(&self) -> bool {
        self.is_defer
    }
    pub fn set_defer(&mut self, defer: bool) {
        self.is_defer = defer
    }
    pub fn set_params(&mut self, params: &[Expr]) {
        params.clone_into(&mut self.params);
    }
    pub fn set_fn_def(&mut self, fn_def: &FnDef) {
        self.fn_def = Some(fn_def.clone())
    }
    pub fn call(&mut self, ctx: &mut Context) -> PipelineResult<Value> {
        let fn_def = self.fn_def.clone();
        match fn_def {
            None => {
                // let expr=FnCallExpr{ name: self.name.clone(), args: self.params.clone() }
                // engine.eval_fn_call_expr_from_ast(ctx,expr)
                todo!()
            }
            Some(f) => {
                let blocks = f.body;
                let mut r = Value::with_immutable(Dynamic::Unit);
                for i in blocks {
                    r = ctx.eval_stmt(&i)?;
                }
                Ok(r)
            }
        }
    }
}
impl From<&str> for Dynamic {
    fn from(value: &str) -> Self {
        Dynamic::String(String::from(value))
    }
}
impl From<String> for Dynamic {
    fn from(value: String) -> Self {
        Dynamic::String(value)
    }
}

impl From<Dynamic> for String {
    fn from(value: Dynamic) -> Self {
        value.as_string().unwrap()
    }
}
impl From<Dynamic> for bool {
    fn from(value: Dynamic) -> Self {
        value.as_bool().unwrap()
    }
}
impl From<Dynamic> for i64 {
    fn from(value: Dynamic) -> Self {
        value.as_integer().unwrap()
    }
}
impl From<Dynamic> for f64 {
    fn from(value: Dynamic) -> Self {
        value.as_float().unwrap()
    }
}
impl From<i64> for Dynamic {
    fn from(value: i64) -> Self {
        Dynamic::Integer(value)
    }
}

impl From<bool> for Dynamic {
    fn from(value: bool) -> Self {
        Dynamic::Boolean(value)
    }
}

impl Display for Dynamic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Dynamic::Unit => {
                write!(f, "Unit")
            }
            Dynamic::Integer(i) => {
                write!(f, "{i}")
            }
            Dynamic::Float(f0) => {
                write!(f, "{f0}")
            }
            Dynamic::String(s) => {
                write!(f, "{s}")
            }
            Dynamic::Boolean(b) => {
                write!(f, "{b}")
            }
            Dynamic::Variable(a) => {
                write!(f, "Variable({a})")
            }
            Dynamic::FnPtr(p) => write!(f, "function {:}", p.name),
            Dynamic::Array(v) => {
                write!(f, "[").expect("write失败");
                for (i, a) in v.iter().enumerate() {
                    write!(f, "{}", a.as_dynamic()).expect("write失败");
                    if i < v.len() - 1 {
                        write!(f, ",").expect("write失败");
                    }
                }
                write!(f, "]")
            }
            Dynamic::Map(v) => {
                write!(f, "{{").expect("write失败");
                for (i, a) in v.iter().enumerate() {
                    write!(f, "{}:{}", a.0, a.1.as_dynamic()).expect("write失败");
                    if i < v.len() - 1 {
                        write!(f, ",").expect("write失败");
                    }
                }
                write!(f, "}}")
            }
            Dynamic::Struct(s) => {
                write!(f, "{}{{", s.name).expect("write失败");
                for (i, a) in s.props.iter().enumerate() {
                    write!(f, "{}:{}", a.0, a.1.as_dynamic()).expect("write失败");
                    if i < s.props.len() - 1 {
                        write!(f, ",").expect("write失败");
                    }
                }
                write!(f, "}}")
            }
            Dynamic::Native(_) => {
                write!(f, "Native Value")
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Immutable(i) => std::fmt::Display::fmt(&i.read().unwrap(), f),
            Value::Mutable(m) => std::fmt::Display::fmt(&m.read().unwrap(), f),
            Value::Refer(r) => std::fmt::Display::fmt(&r.upgrade().unwrap().read().unwrap(), f),
            Value::Signal(s) => write!(f, "{:?}", s),
            Value::Constant(c) => write!(f, "{}", c),
        }
    }
}

impl Eq for Dynamic {}
impl Hash for Dynamic {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Dynamic::Unit => {}
            Dynamic::Integer(i) => i.hash(state),
            Dynamic::String(s) => s.hash(state),
            Dynamic::Boolean(b) => b.hash(state),
            _ => {}
        }
    }
}
impl Mul for Dynamic {
    type Output = Dynamic;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Dynamic::Integer(i) => match rhs {
                Dynamic::Integer(r) => Dynamic::Integer(i * r),
                Dynamic::Float(r) => Dynamic::Float(i as f64 * r),
                t => panic!("Integer can not mul {}", t.type_name()),
            },
            Dynamic::Float(f) => {
                let t = rhs.as_float().unwrap();
                Dynamic::Float(f * t)
            }
            _ => panic!("不能进行相乘操作"),
        }
    }
}

impl Div for Dynamic {
    type Output = Dynamic;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Dynamic::Integer(i) => match rhs {
                Dynamic::Integer(r) => Dynamic::Integer(i / r),
                Dynamic::Float(r) => Dynamic::Float(i as f64 / r),
                t => panic!("Integer can not div {}", t.type_name()),
            },
            Dynamic::Float(f) => match rhs {
                Dynamic::Integer(r) => Dynamic::Float(f / r as f64),
                Dynamic::Float(r) => Dynamic::Float(f / r),
                t => panic!("Float can not div {}", t.type_name()),
            },
            _ => panic!("不能进行相乘操作"),
        }
    }
}

impl Rem for Dynamic {
    type Output = Dynamic;

    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Dynamic::Integer(i) => {
                let r = rhs.as_integer().unwrap();
                Dynamic::Integer(i % r)
            }
            Dynamic::Float(f) => {
                let t = rhs.as_float().unwrap();
                Dynamic::Float(f % t)
            }
            _ => panic!("不能进行相乘操作"),
        }
    }
}
impl PartialEq<Self> for Dynamic {
    fn eq(&self, rhs: &Self) -> bool {
        match self {
            Dynamic::Integer(i) => {
                let r = rhs.as_integer().unwrap();
                i.eq(&r)
            }
            Dynamic::Float(f) => {
                let t = rhs.as_float().unwrap();
                f.eq(&t)
            }
            Dynamic::String(s) => {
                let o = rhs.as_string().unwrap();
                s.eq(&o)
            }
            _ => panic!("不能进行相等操作"),
        }
    }
}

impl PartialEq<Self> for Value {
    fn eq(&self, other: &Self) -> bool {
        let left = self.as_dynamic();
        let right = other.as_dynamic();
        left == right
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let t = self.as_dynamic();
        t.hash(state);
    }
}
impl PartialOrd for Dynamic {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match self {
            Dynamic::Integer(i) => {
                let r = rhs.as_integer().unwrap();
                i.partial_cmp(&r)
            }
            Dynamic::Float(f) => {
                let t = rhs.as_float().unwrap();
                f.partial_cmp(&t)
            }
            _ => panic!("不能进行比较操作"),
        }
    }
}
impl Add for Dynamic {
    type Output = Dynamic;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Dynamic::Integer(i) => {
                if rhs.is_integer() {
                    let r = rhs.as_integer().unwrap();
                    Dynamic::Integer(i + r)
                } else if rhs.is_string() {
                    let r = rhs.as_string().unwrap();
                    Dynamic::String(format!("{}{}", i, r))
                } else {
                    panic!("cannot add")
                }
            }
            Dynamic::Float(f) => {
                let t = rhs.as_float().unwrap();
                Dynamic::Float(f + t)
            }
            Dynamic::String(s) => match rhs {
                Dynamic::String(t) => {
                    let r = format!("{}{}", s, t);
                    Dynamic::String(r)
                }
                Dynamic::Integer(i) => {
                    let r = format!("{}{}", s, i);
                    Dynamic::String(r)
                }
                Dynamic::Float(f) => {
                    let r = format!("{}{}", s, f);
                    Dynamic::String(r)
                }
                t => panic!("{t} cannot do str append"),
            },
            _ => panic!("不能进行相加操作"),
        }
    }
}

impl Sub for Dynamic {
    type Output = Dynamic;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Dynamic::Integer(i) => {
                let r = rhs.as_integer().unwrap();
                Dynamic::Integer(i - r)
            }
            Dynamic::Float(f) => {
                let t = rhs.as_float().unwrap();
                Dynamic::Float(f - t)
            }
            _ => panic!("不能进行相加操作"),
        }
    }
}
impl Dynamic {
    pub fn type_name(&self) -> String {
        match self {
            Dynamic::Unit => "Uint".into(),
            Dynamic::Integer(_) => "Int".into(),
            Dynamic::Float(_) => "Float".into(),
            Dynamic::String(_) => "String".into(),
            Dynamic::Boolean(_) => "Bool".into(),
            Dynamic::Variable(_) => "Variable".into(),
            Dynamic::FnPtr(_) => "Function".into(),
            Dynamic::Array(_) => "Array".into(),
            Dynamic::Map(_) => "Map".into(),
            Dynamic::Native(_) => "Native".into(),
            Dynamic::Struct(s) => s.name.clone(),
        }
    }
    pub fn is_variable(&self) -> bool {
        matches!(self, Dynamic::Variable(_))
    }
    pub fn is_string(&self) -> bool {
        matches!(self, Dynamic::String(_))
    }
    pub fn is_fn_ptr(&self) -> bool {
        matches!(self, Dynamic::FnPtr(_))
    }
    #[allow(unused)]
    pub fn is_integer(&self) -> bool {
        matches!(self, Dynamic::Integer(_))
    }
    #[allow(unused)]
    pub fn is_float(&self) -> bool {
        matches!(self, Dynamic::Float(_))
    }
    pub fn is_array(&self) -> bool {
        matches!(self, Dynamic::Array(_))
    }
    pub fn is_map(&self) -> bool {
        matches!(self, Dynamic::Map(_))
    }
    pub fn as_variable(&self) -> Option<String> {
        match self {
            Dynamic::Variable(s) => Some(s.clone()),
            _ => None,
        }
    }
    pub fn as_string(&self) -> Option<String> {
        match self {
            Dynamic::String(s) => Some(s.clone()),
            _ => None,
        }
    }
    pub fn as_fn_ptr(&self) -> Option<Box<FnPtr>> {
        match self {
            Dynamic::FnPtr(f) => Some(f.clone()),
            _ => None,
        }
    }
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Dynamic::Boolean(i) => Some(*i),
            _ => None,
        }
    }
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Dynamic::Integer(i) => Some(*i),
            _ => None,
        }
    }
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Dynamic::Float(i) => Some(*i),
            _ => None,
        }
    }
    pub fn as_mut_array(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            Dynamic::Array(i) => Some(i),
            _ => None,
        }
    }
    pub fn as_array(&mut self) -> Option<Vec<Value>> {
        match self {
            Dynamic::Array(i) => Some(i.clone()),
            _ => None,
        }
    }
    pub fn as_mut_map(&mut self) -> Option<&mut HashMap<Dynamic, Value>> {
        match self {
            Dynamic::Map(i) => Some(i),
            _ => None,
        }
    }
    pub fn as_struct(&self) -> Option<Box<Struct>> {
        match self {
            Dynamic::Struct(i) => Some(i.clone()),
            _ => None,
        }
    }
    pub fn as_native(&self) -> Option<Arc<RwLock<dyn Any + Send + Sync>>> {
        match self {
            Dynamic::Native(a) => Some(a.clone()),
            _ => None,
        }
    }
    pub fn convert_float(&self) -> Option<f64> {
        match self {
            Dynamic::Integer(i) => Some(*i as f64),
            Dynamic::Float(i) => Some(*i),
            _ => None,
        }
    }
}
