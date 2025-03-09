use crate::llvm::global::Global;
use crate::llvm::types::LLVMType;
use std::collections::HashMap;
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
    // 引用类型和指针类型底层一致，但是使用时默认获取值
    Ref(Box<Type>),
    // 泛型 例如：Array<Int32>,即Generic(Array,[Int32])
    Generic(Box<Type>, Vec<Type>),
    Alias(String),
    Struct(Option<String>, Vec<(String, Type)>),
    // 枚举类型，Option<String>是枚举名称，Vec<(String, Option<Type>)>是枚举变体列表
    // 每个变体包含名称和可选的关联类型
    Enum(Option<String>, Vec<(String, Option<Type>)>),
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
            "Ref" => Type::Ref(Box::new(Type::Any)),
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
            Type::Array(_) => Some("Array"),
            _ => None,
        }
    }
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Int8 | Type::Int16 | Type::Int32 | Type::Int64)
    }
    pub fn get_enum_variant_type(&self,name:&str) -> Option<Type> {
        match self {
            Type::Enum(_, variants) => {
                for (name0, t) in variants {
                    if name0 == name {
                        return t.clone();
                    }
                }
                None
            },
            _ => None,
        }
    }
    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float)
    }
    pub fn is_double(&self) -> bool {
        matches!(self, Type::Double)
    }
    pub fn id(&self) -> i32 {
        match self {
            Type::Unit => 0,
            Type::Int8 => 1,
            Type::Pointer(t) if t.is_i8() => 2,
            Type::Int16 => 3,
            Type::Pointer(t) if t.is_i16() => 4,
            Type::Int32 => 5,
            Type::Pointer(t) if t.is_i32() => 6,
            Type::Int64 => 7,
            Type::Pointer(t)|Type::Ref(t) if t.is_i64() => 8,
            Type::Float => 9,
            Type::Pointer(t) if t.is_float() => 10,
            Type::Double => 11,
            Type::Pointer(t) if t.is_double() => 12,
            Type::String => 13,
            Type::Bool => 15,
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
    pub fn is_ref(&self) -> bool {
        matches!(self, Type::Ref(_))
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
            Type::Pointer(t) => t.get_struct_field(name),
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
            _ => None,
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
            Type::Float => Global::float_type(),
            Type::Double => Global::double_type(),
            Type::Bool => Global::i8_type(),
            Type::String => Global::pointer_type(Global::i8_type()),
            Type::Pointer(t) => Global::pointer_type(t.as_llvm_type()),
            Type::Array(t) => Global::pointer_type(t.as_llvm_type()),
            Type::Struct(_, _) => Global::pointer_type(Global::i8_type()),
            Type::Enum(_, t) => {
                // 枚举类型编译为包含标签和数据的结构体
                // 标签是一个整数，表示枚举变体的索引
                // 数据是一个联合体，包含所有变体的数据
                
                // 创建枚举结构体类型
                let fields = vec![
                    // 标签字段，用于区分不同的变体
                    Global::i32_type(),
                    // 数据字段，使用i64作为默认类型
                    Global::i64_type(),
                ];
                
                Global::struct_type(fields)
            },
            Type::Function(ret, args) => {
                let mut v = vec![];
                for t in args.iter() {
                    v.push(t.as_llvm_type());
                }
                Global::function_type(ret.as_llvm_type(), v)
            }
            Type::ArrayVarArg(t) => Global::pointer_type(t.as_llvm_type()),
            Type::Ref(t) => Global::pointer_type(t.as_llvm_type()),
            Type::Any => Global::struct_type(vec![
                Global::i32_type(),
                Global::pointer_type(Global::i8_type()),
            ]),
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
            Type::Ref(t) => Some(t),
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
    pub fn try_replace_alias(&mut self, alias_map: &HashMap<String, Type>) {
        match self {
            Type::Alias(s) => {
                let t = alias_map.get(s).unwrap();
                *self = t.clone();
            }
            Type::Struct(_, s) => {
                for (_, t) in s.iter_mut() {
                    t.try_replace_alias(alias_map);
                }
            }
            Type::Function(_, args) => {
                for t in args.iter_mut() {
                    t.try_replace_alias(alias_map);
                }
            }
            Type::Array(t) => {
                t.try_replace_alias(alias_map);
            }
            Type::Pointer(t) => {
                t.try_replace_alias(alias_map);
            }
            Type::ArrayVarArg(t) => {
                t.try_replace_alias(alias_map);
            }
            Type::Closure { ptr, env, .. } => {
                ptr.0.try_replace_alias(alias_map);
                ptr.1
                    .iter_mut()
                    .for_each(|t| t.try_replace_alias(alias_map));
                env.iter_mut()
                    .for_each(|(_, t)| t.try_replace_alias(alias_map));
            }
            Type::Generic(t, _) => {
                t.try_replace_alias(alias_map);
            }
            Type::Map(k, v) => {
                k.try_replace_alias(alias_map);
                v.try_replace_alias(alias_map);
            }
            Type::Enum(_, variants) => {
                for (_, t) in variants.iter_mut() {
                    if let Some(ty) = t {
                        ty.try_replace_alias(alias_map);
                    }
                }
            }
            _ => {}
        }
    }
    pub fn has_alias(&self) -> bool {
        // 递归判断是否存在Alias
        match self {
            Type::Alias(_) => true,
            Type::Struct(_, s) => {
                for (_, t) in s.iter() {
                    if t.has_alias() {
                        return true;
                    }
                }
                false
            }
            Type::Function(_, args) => {
                for t in args.iter() {
                    if t.has_alias() {
                        return true;
                    }
                }
                false
            }
            Type::Array(t) => {
                if t.has_alias() {
                    return true;
                }
                false
            }
            Type::Pointer(t) => {
                if t.has_alias() {
                    return true;
                }
                false
            }
            Type::ArrayVarArg(t) => {
                if t.has_alias() {
                    return true;
                }
                false
            }
            Type::Closure { ptr, env, .. } => {
                if ptr.0.has_alias()
                    || ptr.1.iter().any(|t| t.has_alias())
                    || env.iter().any(|(_, t)| t.has_alias())
                {
                    return true;
                }
                false
            }
            Type::Generic(t, _) => {
                if t.has_alias() {
                    return true;
                }
                false
            }
            Type::Map(k, v) => {
                if k.has_alias() || v.has_alias() {
                    return true;
                }
                false
            }
            Type::Enum(_, variants) => {
                for (_, t) in variants.iter() {
                    if let Some(ty) = t {
                        if ty.has_alias() {
                            return true;
                        }
                    }
                }
                false
            }
            _ => false,
        }
    }
    pub fn as_str(&self) -> String {
        match self {
            Type::Alias(s) => s.clone(),
            Type::Struct(_, s) => {
                let mut v = vec![];
                for (_, t) in s.iter() {
                    v.push(t.as_str());
                }
                v.join(".")
            }
            Type::Function(_, args) => {
                let mut v = vec![];
                for t in args.iter() {
                    v.push(t.as_str());
                }
                v.join(".")
            }
            Type::Array(t) => {
                format!("[]{}", t.as_str())
            }
            Type::Pointer(t) => {
                format!("*{}", t.as_str())
            }
            Type::Ref(t) => {
                format!("&{}", t.as_str())
            }
            Type::ArrayVarArg(t) => {
                format!("..{}", t.as_str())
            }
            Type::Closure {
                name,
                ptr: _,
                env: _,
            } => {
                format!("{}", name.as_deref().unwrap_or(""))
            }
            Type::VarArg => "..".to_string(),
            Type::Generic(t, _) => {
                format!("{}", t.as_str())
            }
            Type::Map(k, v) => {
                format!("Map<{},{}>", k.as_str(), v.as_str())
            }
            Type::Enum(name, variants) => {
                let mut result = String::new();
                if let Some(name) = name {
                    result.push_str(name);
                } else {
                    result.push_str("Enum");
                }
                
                if !variants.is_empty() {
                    result.push_str("<");
                    for (i, (variant_name, variant_type)) in variants.iter().enumerate() {
                        if i > 0 {
                            result.push_str(",");
                        }
                        result.push_str(variant_name);
                        if let Some(ty) = variant_type {
                            result.push_str(&format!("({})", ty.as_str()));
                        }
                    }
                    result.push_str(">");
                }
                result
            }
            Type::Module => "Module".to_string(),
            Type::Int8 => "Int8".to_string(),
            Type::Int16 => "Int16".to_string(),
            Type::Int32 => "Int32".to_string(),
            Type::Int64 => "Int64".to_string(),
            Type::Float => "Float".to_string(),
            Type::Double => "Double".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Any => "Any".to_string(),
            Type::Unit => "Unit".to_string(),
        }
    }
    /// 获取函数名称，仅在类型是由特定函数生成的函数类型时有效
    pub fn get_function_name(&self) -> Option<&str> {
        // 这个方法实际上不能简单地从类型中获取函数名称
        // 因为标准函数类型 Type::Function 并不存储函数名
        // 我们将返回 None，由调用代码处理这种情况
        None
    }
    pub fn is_enum(&self) -> bool {
        matches!(self, Type::Enum(_, _))
    }
    
    pub fn get_enum_name(&self) -> Option<&str> {
        match self {
            Type::Enum(name, _) => name.as_ref().map(|s| s.as_str()),
            _ => None,
        }
    }
    
    pub fn get_enum_variants(&self) -> Option<&Vec<(String, Option<Type>)>> {
        match self {
            Type::Enum(_, variants) => Some(variants),
            _ => None,
        }
    }
}
