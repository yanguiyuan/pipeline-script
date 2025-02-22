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
            ContextKey::Background => matches!(other, ContextKey::Background),
            ContextKey::Builder => matches!(other, ContextKey::Builder),
            ContextKey::TypeTable => matches!(other, ContextKey::TypeTable),
            ContextKey::SymbolTable => matches!(other, ContextKey::SymbolTable),
            ContextKey::Function => matches!(other, ContextKey::Function),
            ContextKey::Scope => matches!(other, ContextKey::Scope),
            ContextKey::Flag(f) => match other {
                ContextKey::Flag(o) => f == o,
                _ => false,
            },
            ContextKey::SymbolType => matches!(other, ContextKey::SymbolType),
            ContextKey::AliasType => matches!(other, ContextKey::AliasType),
            ContextKey::LocalVariable => matches!(other, ContextKey::LocalVariable),
            ContextKey::CaptureVariable => matches!(other, ContextKey::CaptureVariable),
            ContextKey::Type(t) => match other {
                ContextKey::Type(o) => t == o,
                _ => false,
            },
            ContextKey::LLVMContext => matches!(other, ContextKey::LLVMContext),
            ContextKey::LLVMModule => matches!(other, ContextKey::LLVMModule),
            ContextKey::ModuleSlotMap => matches!(other, ContextKey::ModuleSlotMap),
        }
    }
}
