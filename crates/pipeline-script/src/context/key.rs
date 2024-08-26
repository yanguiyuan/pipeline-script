#[derive(Clone, Debug)]
pub enum ContextKey {
    Background,
    Builder,
    SymbolTable,
    SymbolType,
    Flag(String),
    Type,
    Function,
    Scope,
}

impl PartialEq for ContextKey {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ContextKey::Background => {
                match other {
                    ContextKey::Background => true,
                    _=>false
                }
            }
            ContextKey::Builder => {
                match other {
                    ContextKey::Builder => true,
                    _=>false
                }
            }
            ContextKey::Type => {
                match other {
                    ContextKey::Type => true,
                    _=>false
                }
            }
            ContextKey::SymbolTable => {
                match other {
                    ContextKey::SymbolTable => true,
                    _=>false
                }
            }
            ContextKey::Function => {
                match other {
                    ContextKey::Function => true,
                    _=>false
                }
            }
            ContextKey::Scope => {
                match other {
                    ContextKey::Scope => true,
                    _=>false
                }
            }
            ContextKey::Flag(f) => {
                match other {
                    ContextKey::Flag(o)=>f==o,
                    _=>false
                }
            }
            ContextKey::SymbolType => {
                match other {
                    ContextKey::SymbolType => true,
                    _=>false
                }
            }
            _=>todo!("eq")
        }
    }
}