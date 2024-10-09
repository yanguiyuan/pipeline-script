use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use wrap_llvm::builder::Builder;
use wrap_llvm::function::Function;
use wrap_llvm::types::LLVMType;
use wrap_llvm::value::LLVMValue;
use crate::context::key::ContextKey;
use crate::context::scope::Scope;
use crate::context::value::ContextValue;
use crate::core::value::Value;
use crate::parser::r#type::Type;

pub mod key;
pub mod value;
pub mod scope;

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
            value: ContextValue::SymbolTable(Arc::new(RwLock::new(HashMap::new()))),
        }
    }
    pub fn with_builder(parent: &Context, builder: Builder) -> Self {
        Self {
            parent: Some(Box::new(parent.clone())),
            key: ContextKey::Builder,
            value: ContextValue::Builder(Arc::new(builder)),
        }
    }
    pub fn get_builder(&self)->Arc<Builder>{
        match self.get(ContextKey::Builder) {
            Some(ContextValue::Builder(b))=>b.clone(),
            _=>panic!("not a builder")
        }
    }
    pub fn get_current_function(&self)->Function{
         match self.get(ContextKey::Function) {
             Some(ContextValue::Function(b))=>b.clone(),
             _=>panic!("not a function")
         }
    }
    pub fn with_scope(parent: &Context,scope:Scope) -> Self {
        Self::with_value(parent, ContextKey::Scope, ContextValue::Scope(scope))
    }
    pub fn with_type_table(parent: &Context, t:HashMap<Type,LLVMType>) -> Self {
        Self::with_value(parent, ContextKey::TypeTable, ContextValue::TypeTable(Arc::new(RwLock::new(t))))
    }
    pub fn with_function(parent: &Context, f:Function) -> Self {
        Self::with_value(parent, ContextKey::Function, ContextValue::Function(f))
    }
    pub fn with_flag(parent: &Context,key:impl Into<String>, flag: bool) -> Self {
        Self::with_value(parent, ContextKey::Flag(key.into()), ContextValue::Flag(Arc::new(RwLock::new(flag))))
    }
    pub fn set_flag(&self,key:impl Into<String>, flag: bool) {
        match self.get(ContextKey::Flag(key.into())) {
            Some(ContextValue::Flag(f)) => {
                let mut f = f.write().unwrap();
                *f = flag;
            }
            _ => panic!("not a flag"),
        }
    }
    pub fn get_flag(&self,key:impl Into<String>)->bool{
        match self.get(ContextKey::Flag(key.into())) {
            Some(ContextValue::Flag(f)) => {
                let f = f.read().unwrap();
                *f
            }
            _ => panic!("not a flag"),
        }
    }
    pub fn with_value(parent: &Context, key: ContextKey, value: ContextValue) -> Self {
        Self {
            parent: Some(Box::new(parent.clone())),
            key,
            value,
        }
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
    pub fn get_scope(&self)->Scope{
        match self.get(ContextKey::Scope) {
            Some(ContextValue::Scope(s))=>s.clone(),
            _=>panic!("not a scope")
        }
    }
    pub fn set_symbol(&self, name: String, v: Value) {
        let mut symbol_table = self.get(ContextKey::SymbolTable).unwrap().as_symbol_table();
        let mut symbol_table = symbol_table.write().unwrap();
        symbol_table.insert(name, v);
    }
    pub fn get_symbol(&self, name: impl AsRef<str>) -> Option<Value> {
        let symbol_table = self.get(ContextKey::SymbolTable).unwrap().as_symbol_table();
        let symbol_table = symbol_table.read().unwrap();
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
    pub fn register_type(&self, ty:&Type,llvm_ty: &LLVMType){
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
                let r  = t.get(name.as_ref());
                match r {
                    None => self.parent.clone()?.parent?.get_symbol_type(name),
                    Some(s) => Some(s.clone())
                }
            }
            _ => panic!("not a symbol type"),
        }
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
}
