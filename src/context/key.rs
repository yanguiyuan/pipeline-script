#[derive(Clone, Debug)]
pub enum ContextKey {
    Background,
    Builder,
    SymbolTable,
    LLVMContext,
    LLVMModule,
    SymbolType,
    ModuleSlotMap,
    /// 预处理阶段闭包捕获变量分析
    LocalVariable,
    CaptureVariable,
    AliasType,
    // 编译阶段，当前函数类型
    Type(String),
    Flag(String),
    TypeTable,
    Function,
    Scope,
}

impl Default for ContextKey {
    fn default() -> Self {
        Self::Background
    }
}

impl PartialEq for ContextKey {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ContextKey::Background => match other {
                ContextKey::Background => true,
                _ => false,
            },
            ContextKey::Builder => match other {
                ContextKey::Builder => true,
                _ => false,
            },
            ContextKey::TypeTable => match other {
                ContextKey::TypeTable => true,
                _ => false,
            },
            ContextKey::SymbolTable => match other {
                ContextKey::SymbolTable => true,
                _ => false,
            },
            ContextKey::Function => match other {
                ContextKey::Function => true,
                _ => false,
            },
            ContextKey::Scope => match other {
                ContextKey::Scope => true,
                _ => false,
            },
            ContextKey::Flag(f) => match other {
                ContextKey::Flag(o) => f == o,
                _ => false,
            },
            ContextKey::SymbolType => match other {
                ContextKey::SymbolType => true,
                _ => false,
            },
            ContextKey::AliasType => match other {
                ContextKey::AliasType => true,
                _ => false,
            },
            ContextKey::LocalVariable => match other {
                ContextKey::LocalVariable => true,
                _ => false,
            },
            ContextKey::CaptureVariable => match other {
                ContextKey::CaptureVariable => true,
                _ => false,
            },
            ContextKey::Type(t) => match other {
                ContextKey::Type(o) => t == o,
                _ => false,
            },
            ContextKey::LLVMContext => match other {
                ContextKey::LLVMContext => true,
                _ => false,
            },
            ContextKey::LLVMModule => match other {
                ContextKey::LLVMModule => true,
                _ => false,
            },
            ContextKey::ModuleSlotMap => match other {
                ContextKey::ModuleSlotMap => true,
                _ => false,
            },
        }
    }
}
