use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int8,
    Int16,
    Int32,
    Int64,
    Float,
    Double,
    String,
    Bool,
    Pointer(Box<Type>),
    // 泛型 例如：Array<Int32>,即Generic(Array,[Int32])
    Generic(Box<Type>, Vec<Type>),
    Alias(String),
    Struct(Option<String>, Vec<(String, Type)>),
    Function(Box<Type>, Vec<Type>),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Closure {
        name: Option<String>,
        ptr: (Box<Type>, Vec<Type>),
        env: Vec<(String, Type)>,
    },
    Any,
    Unit,
    ArrayVarArg(Box<Type>),
    VarArg,
    Module,
}
impl From<&str> for Type {
    fn from(s: &str) -> Self {
        match s {
            "Unit" => Type::Unit,
            "Int8" => Type::Int8,
            "Int16" => Type::Int16,
            "Int32" => Type::Int32,
            "Int64" => Type::Int64,
            "Float" => Type::Float,
            "Double" => Type::Double,
            "String" => Type::String,
            "Bool" => Type::Bool,
            "Any" => Type::Any,
            "Pointer" => Type::Pointer(Box::new(Type::Any)),
            //            t=>Type::Struct(t.into()),
            _ => panic!("Unknown type: {}", s),
        }
    }
}

impl From<LLVMType> for Type {
    fn from(value: LLVMType) -> Self {
        match value {
            LLVMType::Int1(_) => Type::Bool,
            LLVMType::Int8(_) => Type::Int8,
            LLVMType::Int16(_) => Type::Int16,
            LLVMType::Int32(_) => Type::Int32,
            LLVMType::Int64(_) => Type::Int64,
            LLVMType::Float(_) => Type::Float,
            LLVMType::Double(_) => Type::Double,
            LLVMType::Pointer(e, _) => Type::Pointer(Box::new((*e).into())),
            LLVMType::Unit(_) => Type::Any,
            t => panic!("{t:?}"),
        }
    }
}
impl From<String> for Type {
    fn from(s: String) -> Self {
        match s.as_str() {
            "Unit" => Type::Unit,
            "Int16" => Type::Int16,
            "Int8" => Type::Int8,
            "Int32" => Type::Int32,
            "Int64" => Type::Int64,
            "Float" => Type::Float,
            "Double" => Type::Double,
            "String" => Type::String,
            "Bool" => Type::Bool,
            "Any" => Type::Any,
            s if s.starts_with("*") => {
                let s = s.strip_prefix("*").unwrap();
                let t = s.into();
                Type::Pointer(Box::new(t))
            }

            s if s.starts_with("[]") => {
                let s = s.strip_prefix("[]").unwrap();
                let t = s.into();
                Type::Array(Box::new(t))
            }
            s if s.starts_with("..") => {
                let s = s.strip_prefix("..").unwrap();
                let t = s.into();
                Type::ArrayVarArg(Box::new(t))
            }
            t => Type::Alias(t.into()),
        }
    }
}
impl Type {
    pub fn get_struct_name(&self) -> Option<&str> {
        match self {
            Type::Struct(name, _) => name.as_deref(),
            _ => None,
        }
    }
    pub fn id(&self) -> i32 {
        match self {
            Type::Unit => 0,
            Type::Int8 => 1,
            Type::Int16 => 2,
            Type::Int32 => 3,
            Type::Int64 => 4,
            Type::Float => 5,
            Type::Double => 6,
            Type::String => 7,
            Type::Bool => 8,
            // Type::Any => 9,
            t => panic!("{t:?}"),
        }
    }
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Type::Int32 | Type::Int64 | Type::Float | Type::Double | Type::String | Type::Bool
        )
    }
    pub fn as_struct(&self) -> Option<&Vec<(String, Type)>> {
        match self {
            Type::Struct(_, m) => Some(m),
            _ => None,
        }
    }
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function(_, _))
    }
    pub fn is_module(&self) -> bool {
        matches!(self, Type::Module)
    }
    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }
    pub fn is_array_vararg(&self) -> bool {
        matches!(self, Type::ArrayVarArg(_))
    }
    pub fn with_return_type(&self, return_type: Type) -> Type {
        match self {
            Type::Function(_, args) => Type::Function(Box::new(return_type), args.clone()),
            _ => panic!("Not a function type"),
        }
    }
    pub fn get_struct_field(&self, name: impl AsRef<str>) -> Option<(usize, Type)> {
        match self {
            Type::Struct(_, m) => {
                for (i, (field_name, t)) in m.iter().enumerate() {
                    if field_name == name.as_ref() {
                        return Some((i, t.clone()));
                    }
                }
                None
            }
            _ => None,
        }
    }
    pub fn get_function_arg_count(&self) -> usize {
        match self {
            Type::Function(_, args) => args.len(),
            _ => panic!("Not a function type"),
        }
    }
    pub fn get_function_arg_type(&self, index: usize) -> Option<Type> {
        match self {
            Type::Function(_, args) => args.get(index).cloned(),
            _ => panic!("Not a function type"),
        }
    }
    pub fn is_i8(&self) -> bool {
        matches!(self, Type::Int8)
    }
    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any)
    }
    pub fn is_string(&self) -> bool {
        matches!(self, Type::String)
    }
    pub fn is_i16(&self) -> bool {
        matches!(self, Type::Int16)
    }
    pub fn is_i32(&self) -> bool {
        matches!(self, Type::Int32)
    }
    pub fn is_i64(&self) -> bool {
        matches!(self, Type::Int64)
    }
    pub fn is_struct(&self) -> bool {
        matches!(self, Type::Struct(_, _))
    }
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }

    pub fn as_llvm_type(&self) -> LLVMType {
        match self {
            Type::Int8 => Global::i8_type(),
            Type::Int16 => Global::i16_type(),
            Type::Int32 => Global::i32_type(),
            Type::Int64 => Global::i64_type(),
            Type::Unit => Global::unit_type(),
            Type::Pointer(i) => Global::pointer_type(i.as_llvm_type()),
            Type::Any => Global::struct_type(vec![
                Global::i32_type(),
                Global::pointer_type(Global::i8_type()),
            ]),
            Type::Array(t) => Global::array_type(t.as_llvm_type()),
            Type::String => Global::pointer_type(Global::i8_type()),
            Type::Struct(_, s) => {
                let mut v = vec![];
                for (_, t) in s.iter() {
                    v.push(t.as_llvm_type());
                }
                Global::struct_type(v)
            }
            Type::Function(ret, args) => {
                let mut v = vec![];
                for t in args.iter() {
                    v.push(t.as_llvm_type());
                }
                Global::function_type(ret.as_llvm_type(), v)
            }
            Type::ArrayVarArg(t) => Global::pointer_type(t.as_llvm_type()),
            _ => panic!("Unknown type: {:?}", self),
        }
    }
    pub fn get_alias_name(&self) -> Option<String> {
        match self {
            Type::Alias(s) => Some(s.clone()),
            _ => None,
        }
    }
    pub fn get_struct_fields(&self) -> Option<&Vec<(String, Type)>> {
        match self {
            Type::Struct(_, s) => Some(s),
            _ => None,
        }
    }
    pub fn is_closure(&self) -> bool {
        matches!(self, Type::Closure { .. })
    }
    pub fn is_alias(&self) -> bool {
        matches!(self, Type::Alias(_))
    }
    pub fn get_function_return_type(&self) -> Option<Type> {
        match self {
            Type::Function(r, _) => Some(*r.clone()),
            _ => None,
        }
    }
    pub fn get_element_type(&self) -> Option<&Type> {
        match self {
            Type::Array(t) => Some(t),
            Type::Pointer(t) => Some(t),
            Type::ArrayVarArg(t) => Some(t),
            _ => None,
        }
    }
    pub fn get_env_type(&self) -> Option<Type> {
        match self {
            Type::Closure {
                name: _,
                ptr: _,
                env,
            } => Some(Type::Pointer(Box::new(Type::Struct(None, env.clone())))),
            _ => None,
        }
    }
    pub fn get_closure_name(&self) -> Option<String> {
        match self {
            Type::Closure {
                name,
                ptr: _,
                env: _,
            } => name.clone(),
            _ => None,
        }
    }
    pub fn get_closure_fn_gen_type(&self) -> Option<Type> {
        match self {
            Type::Closure { name: _, ptr, env } => {
                let mut gen_fn_params_type = ptr.1.clone();
                gen_fn_params_type.push(Type::Pointer(Box::new(Type::Struct(None, env.clone()))));
                Some(Type::Function(ptr.0.clone(), gen_fn_params_type))
            }
            _ => None,
        }
    }
}
