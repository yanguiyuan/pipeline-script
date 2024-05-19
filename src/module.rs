use crate::context::{Context, ContextKey, ContextValue, Scope};
use crate::error::PipelineError::StaticFunctionUndefined;
use crate::error::{PipelineError, PipelineResult};
use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::types::{FnPtr, SignalType, Value, WrapValue};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, RwLock};

pub trait NativeFunction<Marker> {
    fn into_pipe_function(self) -> Arc<PipeFn>;
}
trait NativeType {}
impl NativeType for String {}
impl NativeType for i64 {}
impl NativeType for f64 {}

type PipeFn = dyn Send + Sync + Fn(&mut Context, Vec<Value>) -> PipelineResult<Value>;
#[derive(Clone)]
pub enum Function {
    Native(Arc<PipeFn>),
    Script(Box<FnDef>),
    Method(Box<FnDef>),
}
#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    default: Option<Expr>,
    pub declaration_type: String,
}

impl VariableDeclaration {
    pub fn new(name: String, dec: String) -> Self {
        Self {
            name,
            default: None,
            declaration_type: dec,
        }
    }
    pub fn has_default(&self) -> bool {
        self.default.is_some()
    }
    pub fn get_default(&self) -> Option<&Expr> {
        match &self.default {
            None => None,
            Some(s) => Some(s),
        }
    }
    pub fn set_default(&mut self, expr: Expr) {
        self.default = Some(expr)
    }
    pub fn with_default(name: String, dec: String, default: Expr) -> Self {
        Self {
            name,
            declaration_type: dec,
            default: Some(default),
        }
    }
}
#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub return_type: String,
    pub args: Vec<VariableDeclaration>,
    pub body: Vec<Stmt>,
}

impl FnDef {
    pub fn new(
        name: String,
        args: Vec<VariableDeclaration>,
        body: Vec<Stmt>,
        return_type: String,
    ) -> Self {
        Self {
            name,
            args,
            body,
            return_type,
        }
    }
}
impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(_) => {
                write!(f, "Native Function")
            }
            Function::Script(s) => {
                write!(f, "{s:?}")
            }
            Function::Method(m) => {
                write!(f, "{m:?}")
            }
        }
    }
}
#[derive(Clone, Debug)]
pub struct Module {
    name: String,
    functions: HashMap<String, Function>,
    classes: HashMap<String, Class>,
    block: Vec<Stmt>,
    submodules: HashMap<String, Box<Module>>,
}
#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    attributions: Vec<VariableDeclaration>,
    methods: HashMap<String, Function>,
    static_methods: HashMap<String, Function>,
}

impl Class {
    pub fn new(name: &str, attributions: Vec<VariableDeclaration>) -> Self {
        Self {
            name: name.to_string(),
            attributions,
            methods: HashMap::new(),
            static_methods: HashMap::new(),
        }
    }
    pub fn get_attributions(&self) -> &Vec<VariableDeclaration> {
        &self.attributions
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
    pub fn get_static_function(&self, name: &str) -> Option<Function> {
        self.static_methods.get(name).cloned()
    }
    pub fn register_method(&mut self, name: String, method: Function) {
        self.methods.insert(name, method);
    }
    pub fn register_static_method(&mut self, name: String, method: Function) {
        self.static_methods.insert(name, method);
    }
}
impl Function {
    pub fn is_script(&self) -> bool {
        matches!(self, Function::Script(_))
    }
    pub fn get_ptr(&self) -> FnPtr {
        match self {
            Function::Native(_) => {
                todo!()
            }
            Function::Script(fd) => {
                let mut ptr = FnPtr::new(fd.name.as_str());
                ptr.set_fn_def(fd);
                ptr
            }
            Function::Method(_) => {
                todo!()
            }
        }
    }
    pub fn call(&self, ctx: &mut Context, args: Vec<WrapValue>) -> PipelineResult<Value> {
        let mut scope = Scope::new();
        let old_scope = ctx.get_scope();
        scope.set_parent(old_scope);
        let mut ctx = Context::with_value(
            ctx,
            ContextKey::Scope,
            ContextValue::Scope(Arc::new(RwLock::new(scope))),
        );
        match self {
            Function::Native(n) => (*n)(&mut ctx, args.iter().map(|e| e.clone_value()).collect()),
            Function::Script(s) => {
                let scope = ctx.get_scope();
                let mut scope = scope.write().unwrap();
                let mut has_name = false;
                // 先对参数赋默认值
                for i in s.args.iter() {
                    if i.has_default() {
                        let result = ctx.eval_expr(i.get_default().unwrap())?;
                        scope.set(i.name.as_str(), result);
                    }
                }
                for (index, i) in s.args.iter().enumerate() {
                    if index >= args.len() {
                        break;
                    }
                    if !has_name && args[index].has_name() {
                        has_name = true;
                    }
                    if !has_name {
                        scope.set(i.name.as_str(), args[index].clone_value());
                    } else {
                        scope.set(args[index].get_name().unwrap(), args[index].clone_value());
                        // 对错误进行处理，如果没有名字，返回错误，命名参数一旦使用后端不得出现匿名的下标参数
                    }
                }
                drop(scope);
                for i in &s.body {
                    let r = ctx.eval_stmt(i)?;
                    if let Value::Signal(SignalType::Return(return_value)) = r {
                        return Ok(*return_value);
                    }
                }
                Ok(().into())
            }
            Function::Method(s) => {
                let scope = ctx.get_scope();
                let mut scope = scope.write().unwrap();
                scope.set("this", args.first().unwrap().clone_value());
                for i in s.args.iter() {
                    if i.has_default() {
                        let result = ctx.eval_expr(i.get_default().unwrap())?;
                        scope.set(i.name.as_str(), result);
                    }
                }
                let mut has_name = false;
                for (index, i) in s.args.iter().enumerate() {
                    if index + 1 >= args.len() {
                        break;
                    }
                    if !has_name && args[index + 1].has_name() {
                        has_name = true;
                    }
                    if !has_name {
                        scope.set(i.name.as_str(), args[index + 1].clone_value());
                    } else {
                        scope.set(
                            args[index + 1].get_name().unwrap(),
                            args[index + 1].clone_value(),
                        );
                        // 对错误进行处理，如果没有名字，返回错误，命名参数一旦使用后端不得出现匿名的下标参数
                    }
                }
                drop(scope);
                for i in &s.body {
                    let r = ctx.eval_stmt(i)?;
                    if let Value::Signal(SignalType::Return(return_value)) = r {
                        return Ok(*return_value);
                    }
                }
                Ok(().into())
            }
        }
    }
}
impl Module {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            block: vec![],
            submodules: HashMap::new(),
        }
    }
    pub fn get_class(&self, class_name: &str) -> Option<&Class> {
        return self.classes.get(class_name);
    }
    pub fn get_functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }
    pub fn register_function(&mut self, name: &str, f: &Function) {
        self.functions.insert(name.into(), f.clone());
    }
    pub fn push_block(&mut self, block: Vec<Stmt>) {
        block.into_iter().for_each(|e| self.block.push(e))
    }
    pub fn run_block(&self, ctx: &mut Context) {
        for i in &self.block {
            ctx.eval_stmt(i).unwrap();
        }
    }
    pub fn get_block(&self) -> &Vec<Stmt> {
        &self.block
    }
    pub fn get_classes(&self) -> &HashMap<String, Class> {
        &self.classes
    }
    pub fn get_class_function(&self, class_name: &str, function_name: &str) -> Option<Function> {
        let class_result = self.classes.get(class_name);
        return match class_result {
            None => None,
            Some(class) => {
                let function_result = class.methods.get(function_name);
                function_result.cloned()
            }
        };
    }
    pub fn register_class_method(
        &mut self,
        class_name: impl AsRef<str>,
        method_name: impl Into<String>,
        method: Function,
    ) {
        let class_result = self.classes.get_mut(class_name.as_ref()).unwrap();
        class_result.register_method(method_name.into(), method)
    }
    pub fn register_class(&mut self, class: Class) {
        self.classes.insert(class.name.clone(), class);
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
    pub fn merge(&mut self, module: &Module) {
        for (k, v) in &module.functions {
            if !self.functions.contains_key(k) {
                self.functions.insert(k.clone(), v.clone());
            }
        }
        for (name, class) in &module.classes {
            self.classes.insert(name.clone(), class.clone());
        }
    }
    pub fn get_submodule(&self, name: &str) -> &Module {
        let m = self.submodules.get(name).unwrap();
        m
    }
    pub fn merge_into_main(&mut self, name: &str) {
        let m = self.submodules.get(name).unwrap();
        let m = m.clone();
        self.merge(&m);
    }
    pub fn register_native_function<A: 'static, F>(&mut self, name: impl Into<String>, f: F)
    where
        F: NativeFunction<A>,
    {
        self.functions
            .insert(name.into(), Function::Native(f.into_pipe_function()));
    }
    pub fn register_pipe_function(
        &mut self,
        name: impl Into<String>,
        f: impl Send + Sync + Fn(&mut Context, Vec<Value>) -> PipelineResult<Value> + 'static,
    ) {
        let a: Arc<PipeFn> = Arc::new(f);
        self.functions.insert(name.into(), Function::Native(a));
    }
    pub fn register_script_function(&mut self, name: impl Into<String>, f: FnDef) {
        self.functions
            .insert(name.into(), Function::Script(Box::new(f)));
    }
    pub fn register_submodule(&mut self, name: impl Into<String>, module: Module) {
        self.submodules.insert(name.into(), Box::new(module));
    }
    pub fn call(
        &self,
        ctx: &mut Context,
        function_name: impl Into<String>,
        args: Vec<WrapValue>,
    ) -> PipelineResult<Value> {
        let name = function_name.into();
        let pos = ctx
            .get(ContextKey::Position)
            .unwrap()
            .as_position()
            .unwrap();
        let f = self.functions.get(name.clone().as_str());
        // 处理类静态方法调用
        if name.contains("::") {
            let split = name.split("::").collect::<Vec<_>>();
            let class = self.get_class(split.first().unwrap()).unwrap();
            let function = class.get_static_function(split[1]);
            return match function {
                None => Err(StaticFunctionUndefined(
                    (*split.first().unwrap()).into(),
                    split[1].into(),
                    pos,
                )),
                Some(f) => f.call(ctx, args),
            };
        }
        match f {
            None => {
                if args.is_empty() {
                    return Err(PipelineError::FunctionUndefined(name, pos));
                }
                let type_name = args[0].as_dynamic().type_name();
                let fun = self.get_class_function(type_name.as_str(), name.as_str());
                match fun {
                    None => Err(PipelineError::FunctionUndefined(name, pos)),
                    Some(fun) => fun.call(ctx, args),
                }
            }
            Some(f) => f.call(ctx, args),
        }
    }
    pub fn get_function(&self, name: impl Into<String>) -> Option<Function> {
        let r = self.functions.get(name.into().as_str());
        r.cloned()
    }
}
impl<
        T: Fn(A, B) -> Ret + 'static + Send + Sync,
        A: NativeType + From<Value>,
        B: NativeType + From<Value>,
        Ret: NativeType + From<Value>,
    > NativeFunction<(A, B)> for T
where
    Value: From<Ret>,
{
    fn into_pipe_function(self) -> Arc<PipeFn> {
        Arc::new(
            move |_: &mut Context, args: Vec<Value>| -> PipelineResult<Value> {
                let mut it = args.iter();
                let a = it.next().unwrap().clone().into();
                let b = it.next().unwrap().clone().into();
                let r = self(a, b);
                Ok(r.into())
            },
        )
    }
}
impl<T: Fn(A) + 'static + Send + Sync, A: NativeType + From<Value>> NativeFunction<A> for T {
    fn into_pipe_function(self) -> Arc<PipeFn> {
        Arc::new(
            move |_: &mut Context, args: Vec<Value>| -> PipelineResult<Value> {
                self(args[0].clone().into());
                Ok(().into())
            },
        )
    }
}
