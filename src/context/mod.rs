use crate::ast::module::Module;
use crate::ast::r#type::Type;
use crate::ast::type_alias::TypeAlias;
use crate::context::key::ContextKey;
use crate::context::scope::Scope;
use crate::context::value::ContextValue;
use crate::llvm::builder::Builder;
use crate::llvm::context::LLVMContext;
use crate::llvm::module::LLVMModule;
use crate::llvm::types::LLVMType;
use crate::llvm::value::fucntion::FunctionValue;
use crate::llvm::value::LLVMValue;
use slotmap::DefaultKey;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};

pub mod key;
pub mod scope;
pub mod value;

#[derive(Clone, Debug)]
pub struct Context {
    parent: Option<Box<Context>>,
    key: ContextKey,
    value: ContextValue,
}

impl Context {
    pub fn background() -> Self {
        Self {
            parent: None,
            key: ContextKey::SymbolTable,
            value: ContextValue::SymbolTable(Rc::new(Mutex::new(HashMap::new()))),
        }
    }
    pub fn with_builder(parent: &Context, builder: Builder) -> Self {
        Self {
            parent: Some(Box::new(parent.clone())),
            key: ContextKey::Builder,
            value: ContextValue::Builder(Arc::new(builder)),
        }
    }
    pub fn with_module_slot_map(parent: &Context, t: slotmap::SlotMap<DefaultKey, Module>) -> Self {
        Self::with_value(
            parent,
            ContextKey::ModuleSlotMap,
            ContextValue::ModuleSlotMap(Arc::new(RwLock::new(t))),
        )
    }
    pub fn apply_mut_module(&self, key: DefaultKey, apply: impl Fn(&mut Module)) {
        let slot_map = self.get(ContextKey::ModuleSlotMap).unwrap();
        match slot_map {
            ContextValue::ModuleSlotMap(slot_map) => {
                let mut slot_map = slot_map.write().unwrap();
                let module = slot_map.get_mut(key).unwrap();
                apply(module)
            }
            _ => panic!("not a module slot map"),
        }
    }
    pub fn get_module_slot_map(&self) -> Arc<RwLock<slotmap::SlotMap<DefaultKey, Module>>> {
        match self.get(ContextKey::ModuleSlotMap) {
            Some(ContextValue::ModuleSlotMap(slot_map)) => slot_map.clone(),
            _ => Arc::new(RwLock::new(slotmap::SlotMap::new())),
        }
    }

    // pub fn apply_module(&self,key:DefaultKey,mut apply:impl FnMut(&Module)){
    //     let slot_map = self.get(ContextKey::ModuleSlotMap).unwrap();
    //     match slot_map{
    //         ContextValue::ModuleSlotMap(slot_map)=>{
    //             let slot_map = slot_map.read().unwrap();
    //             let module = slot_map.get(key).unwrap();
    //             apply(module)
    //         }
    //         _=>panic!("not a module slot map")
    //     }
    // }
    pub fn register_module(&self, module: Module) -> DefaultKey {
        let slot_map = self.get(ContextKey::ModuleSlotMap).unwrap();
        match slot_map {
            ContextValue::ModuleSlotMap(slot_map) => {
                let mut slot_map = slot_map.write().unwrap();
                slot_map.insert(module)
            }
            _ => panic!("not a module slot map"),
        }
    }
    pub fn with_type(parent: &Context, name: String, ty: Type) -> Self {
        Self::with_value(parent, ContextKey::Type(name), ContextValue::Type(ty))
    }
    pub fn get_builder(&self) -> Arc<Builder> {
        match self.get(ContextKey::Builder) {
            Some(ContextValue::Builder(b)) => b.clone(),
            _ => panic!("not a builder"),
        }
    }
    pub fn get_current_function(&self) -> FunctionValue {
        match self.get(ContextKey::Function) {
            Some(ContextValue::Function(b)) => b.clone(),
            _ => panic!("not a function"),
        }
    }
    pub fn get_current_function_type(&self) -> &Type {
        match self.get(ContextKey::Type("current_function".into())) {
            Some(ContextValue::Type(ty)) => ty,
            _ => panic!("not a function"),
        }
    }
    pub fn with_scope(parent: &Context, scope: Scope) -> Self {
        Self::with_value(parent, ContextKey::Scope, ContextValue::Scope(scope))
    }
    pub fn with_type_table(parent: &Context, t: HashMap<Type, LLVMType>) -> Self {
        Self::with_value(
            parent,
            ContextKey::TypeTable,
            ContextValue::TypeTable(Rc::new(RwLock::new(t))),
        )
    }
    pub fn create_llvm_context() -> Self {
        let llvm_ctx = LLVMContext::new();
        let module = llvm_ctx.create_module("main");
        let ctx = Context {
            parent: None,
            key: ContextKey::LLVMContext,
            value: ContextValue::LLVMContext(Rc::new(Mutex::new(LLVMContext::new()))),
        };
        Self {
            parent: Some(Box::new(ctx)),
            key: ContextKey::LLVMModule,
            value: ContextValue::LLVMModule(Rc::new(RwLock::new(module))),
        }
    }
    pub fn with_function(parent: &Context, f: FunctionValue) -> Self {
        Self::with_value(parent, ContextKey::Function, ContextValue::Function(f))
    }
    pub fn with_flag(parent: &Context, key: impl Into<String>, flag: bool) -> Self {
        Self::with_value(
            parent,
            ContextKey::Flag(key.into()),
            ContextValue::Flag(Arc::new(RwLock::new(flag))),
        )
    }
    pub fn get_llvm_module(&self) -> Rc<RwLock<LLVMModule>> {
        match self.get(ContextKey::LLVMModule) {
            Some(ContextValue::LLVMModule(m)) => m.clone(),
            _ => panic!("not a llvm module"),
        }
    }
    pub fn get_llvm_context(&self) -> Rc<Mutex<LLVMContext>> {
        match self.get(ContextKey::LLVMContext) {
            Some(ContextValue::LLVMContext(c)) => c.clone(),
            _ => panic!("not a llvm context"),
        }
    }

    pub fn with_local(parent: &Context, local: Vec<String>) -> Self {
        Self::with_value(
            parent,
            ContextKey::LocalVariable,
            ContextValue::LocalVariable(Arc::new(RwLock::new(local))),
        )
    }
    pub fn get_type_binding_functions(&self, name: &str) -> Vec<crate::ast::function::Function> {
        let slot_map = self.get_module_slot_map();
        let slot_map = slot_map.read().unwrap();
        let mut result = Vec::new();

        for module in slot_map.values() {
            for (_, function) in module.get_functions() {
                if function.has_binding() && function.get_binding() == name {
                    result.push(function);
                }
            }
        }

        result
    }

    pub fn with_capture(parent: &Context) -> Self {
        Self::with_value(
            parent,
            ContextKey::CaptureVariable,
            ContextValue::CaptureVariable(Arc::new(RwLock::new(vec![]))),
        )
    }
    pub fn set_flag(&self, key: impl Into<String>, flag: bool) {
        match self.get(ContextKey::Flag(key.into())) {
            Some(ContextValue::Flag(f)) => {
                let mut f = f.write().unwrap();
                *f = flag;
            }
            _ => panic!("not a flag"),
        }
    }
    pub fn get_flag(&self, key: impl Into<String>) -> Option<bool> {
        match self.get(ContextKey::Flag(key.into())) {
            Some(ContextValue::Flag(f)) => {
                let f = f.read().unwrap();
                Some(*f)
            }
            _ => None,
        }
    }
    pub fn with_value(parent: &Context, key: ContextKey, value: ContextValue) -> Self {
        Self {
            parent: Some(Box::new(parent.clone())),
            key,
            value,
        }
    }

    pub fn get(&self, key: ContextKey) -> Option<&ContextValue> {
        if self.key == key {
            return Some(&self.value);
        }
        match &self.parent {
            None => None,
            Some(parent) => parent.get(key),
        }
    }
    pub fn get_scope(&self) -> Scope {
        match self.get(ContextKey::Scope) {
            Some(ContextValue::Scope(s)) => s.clone(),
            _ => panic!("not a scope"),
        }
    }
    pub fn set_symbol(&self, name: String, v: LLVMValue) {
        let symbol_table = self.get(ContextKey::SymbolTable).unwrap().as_symbol_table();
        let mut symbol_table = symbol_table.lock().unwrap();
        symbol_table.insert(name, v);
    }
    pub fn get_symbol(&self, name: impl AsRef<str>) -> Option<LLVMValue> {
        let symbol_table = self.get(ContextKey::SymbolTable).unwrap().as_symbol_table();
        let symbol_table = symbol_table.lock().unwrap();
        symbol_table.get(name.as_ref()).cloned()
    }
    pub fn get_type(&self, t: &Type) -> Option<LLVMType> {
        match self.get(ContextKey::TypeTable) {
            Some(ContextValue::TypeTable(tt0)) => {
                let tt = tt0.read().unwrap();
                let r = tt.get(t);
                r.cloned()
            }
            _ => panic!("not a type table"),
        }
    }
    pub fn register_type(&self, ty: &Type, llvm_ty: &LLVMType) {
        match self.get(ContextKey::TypeTable) {
            Some(ContextValue::TypeTable(tt0)) => {
                let mut tt = tt0.write().unwrap();
                tt.insert(ty.clone(), llvm_ty.clone());
            }
            _ => panic!("not a type table"),
        }
    }
    pub fn get_alias_type(&self, name: impl AsRef<str>) -> Option<Type> {
        match self.get(ContextKey::AliasType) {
            Some(ContextValue::AliasType(t)) => {
                let t = t.read().unwrap();
                t.get(name.as_ref()).cloned()
            }
            _ => panic!("not a symbol type"),
        }
    }
    pub fn set_alias_type(&self, name: String, t0: Type) {
        match self.get(ContextKey::AliasType) {
            Some(ContextValue::AliasType(t)) => {
                let mut t = t.write().unwrap();
                t.insert(name, t0);
            }
            _ => panic!("not a symbol type"),
        }
    }
    pub fn get_symbol_type(&self, name: impl AsRef<str>) -> Option<Type> {
        match self.get(ContextKey::SymbolType) {
            Some(ContextValue::SymbolType(t)) => {
                let t = t.read().unwrap();
                let r = t.get(name.as_ref());
                match r {
                    None => self.parent.clone()?.parent?.get_symbol_type(name),
                    Some(s) => Some(s.clone()),
                }
            }
            _ => None,
        }
    }
    pub fn try_add_local(&self, name: String) {
        if let Some(ContextValue::LocalVariable(local)) = self.get(ContextKey::LocalVariable) {
            let mut local = local.write().unwrap();
            local.push(name);
        }
    }
    pub fn add_capture(&self, name: String, ty: Type) {
        match self.get(ContextKey::CaptureVariable) {
            Some(ContextValue::CaptureVariable(local)) => {
                let mut local = local.write().unwrap();
                local.push((name, ty));
            }
            _ => panic!("not a local variable"),
        }
    }
    pub fn is_local_variable(&self, name: impl AsRef<str>) -> bool {
        match self.get(ContextKey::LocalVariable) {
            Some(ContextValue::LocalVariable(local)) => {
                let local = local.read().unwrap();
                local.contains(&name.as_ref().to_string())
            }
            _ => panic!("not a local variable"),
        }
    }
    pub fn get_captures(&self) -> Option<Vec<(String, Type)>> {
        match self.get(ContextKey::CaptureVariable) {
            Some(ContextValue::CaptureVariable(local)) => {
                let local = local.read().unwrap();
                Some(local.clone())
            }
            _ => panic!("not a local variable"),
        }
    }
    pub fn get_function(&self, name: impl AsRef<str>) -> Option<crate::ast::function::Function> {
        let slot_map = self.get_module_slot_map();
        let slot_map = slot_map.read().unwrap();
        for module in slot_map.values() {
            if let Some(fun) = module.get_function(name.as_ref()) {
                return Some(fun.clone());
            }
        }
        None
    }
    pub fn set_symbol_type(&self, name: String, t0: Type) {
        match self.get(ContextKey::SymbolType) {
            Some(ContextValue::SymbolType(t)) => {
                let mut t = t.write().unwrap();
                t.insert(name, t0);
            }
            _ => panic!("not a symbol type"),
        }
    }
    pub fn get_type_alias(&self, name: impl AsRef<str>) -> Option<TypeAlias> {
        let slot_map = self.get_module_slot_map();
        let slot_map = slot_map.read().unwrap();

        for module in slot_map.values() {
            if let Some(ty) = module.get_type_alias(name.as_ref()) {
                return Some(ty.clone());
            }
        }

        None
    }
}
