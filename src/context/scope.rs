use crate::core::value::Value;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;

#[derive(Clone, Debug)]
pub struct Scope {
    symbol_table: Rc<RwLock<HashMap<String, Value>>>,
}
impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            symbol_table: Rc::new(RwLock::new(HashMap::new())),
        }
    }
    pub fn set(&self, name: impl Into<String>, value: Value) {
        let mut symbol_table = self.symbol_table.write().unwrap();
        symbol_table.insert(name.into(), value);
    }
    pub fn has(&self, name: impl AsRef<str>) -> bool {
        let symbol_table = self.symbol_table.read().unwrap();
        symbol_table.contains_key(name.as_ref())
    }
    pub fn get(&self, name: impl AsRef<str>) -> Option<Value> {
        let symbol_table = self.symbol_table.read().unwrap();
        symbol_table.get(name.as_ref()).cloned()
    }
}
