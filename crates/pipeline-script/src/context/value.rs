use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use wrap_llvm::builder::Builder;
use wrap_llvm::function::Function;
use crate::context::scope::Scope;
use crate::core::value::Value;
use crate::parser::r#type::Type;

#[derive(Debug, Clone)]
pub enum ContextValue {
    Background,
    Builder(Arc<Builder>),
    SymbolType(Arc<RwLock<HashMap<String,Type>>>),
    SymbolTable(Arc<RwLock<HashMap<String, Value>>>),
    Scope(Scope),
    Flag(Arc<RwLock<bool>>),
    Function(Function),
    Type(Type)
}

impl ContextValue {
    pub fn as_builder(&self)->Arc<Builder>{
        match self {
            ContextValue::Builder(b)=>b.clone(),
            _=>panic!("not a builder")
        }
    }
    pub fn as_type(&self)->Type{
        match self {
            ContextValue::Type(t)=>t.clone(),
            _=>panic!("not a type")
        }
    }
    pub fn as_symbol_table(&self)->Arc<RwLock<HashMap<String, Value>>>{
        match self {
            ContextValue::SymbolTable(t)=>t.clone(),
            _=>panic!("not a symbol table")
        }
    }
}