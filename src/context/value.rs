use crate::context::scope::Scope;
use crate::core::value::Value;
use crate::llvm::builder::Builder;
use crate::llvm::context::LLVMContext;
use crate::llvm::function::Function;
use crate::llvm::module::LLVMModule;
use crate::llvm::types::LLVMType;
use crate::parser::module::Module;
use crate::parser::r#type::Type;
use slotmap::DefaultKey;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};

#[derive(Debug, Clone)]
pub enum ContextValue {
    Background,
    Builder(Arc<Builder>),
    SymbolType(Arc<RwLock<HashMap<String, Type>>>),
    AliasType(Arc<RwLock<HashMap<String, Type>>>),
    SymbolTable(Rc<Mutex<HashMap<String, Value>>>),
    LLVMContext(Rc<Mutex<LLVMContext>>),
    LLVMModule(Rc<RwLock<LLVMModule>>),
    ModuleSlotMap(Arc<RwLock<slotmap::SlotMap<DefaultKey, Module>>>),
    // 编译阶段使用
    Scope(Scope),
    Flag(Arc<RwLock<bool>>),
    Function(Function),
    // 编译阶段使用,存储当前函数类型
    Type(Type),
    // 预处理阶段分析闭包捕获的变量
    LocalVariable(Arc<RwLock<Vec<String>>>),
    CaptureVariable(Arc<RwLock<Vec<(String, Type)>>>),
    TypeTable(Rc<RwLock<HashMap<Type, LLVMType>>>),
}

impl Default for ContextValue {
    fn default() -> Self {
        Self::Background
    }
}

impl ContextValue {
    pub fn as_builder(&self) -> Arc<Builder> {
        match self {
            ContextValue::Builder(b) => b.clone(),
            _ => panic!("not a builder"),
        }
    }
    pub fn as_module(&self) -> Rc<RwLock<LLVMModule>> {
        match self {
            ContextValue::LLVMModule(m) => m.clone(),
            _ => panic!("not a module"),
        }
    }
    pub fn as_type(&self) -> Type {
        match self {
            ContextValue::Type(t) => t.clone(),
            _ => panic!("not a type"),
        }
    }
    #[allow(unused)]
    pub fn as_type_table(&self) -> Rc<RwLock<HashMap<Type, LLVMType>>> {
        match self {
            ContextValue::TypeTable(t) => t.clone(),
            _ => panic!("not a type"),
        }
    }
    pub fn as_symbol_table(&self) -> Rc<Mutex<HashMap<String, Value>>> {
        match self {
            ContextValue::SymbolTable(t) => t.clone(),
            _ => panic!("not a symbol table"),
        }
    }
}
