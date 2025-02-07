use crate::parser::r#type::Type;

#[derive(Debug,Clone)]
pub enum Data{
    String(String),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
    Boolean(bool),
    Usize(usize),
    IDSet(Vec<String>),
    Array(Vec<Data>),
    Map(Vec<(Data,Data)>),
    Type(Type),
    None,
}
impl Data {
    pub fn as_str(&self) -> Option<&str>{
        match self {
            Data::String(s) => Some(s),
            _ => None,
        }
    }
    pub fn as_bool(&self) -> Option<bool>{
        match self {
            Data::Boolean(b) => Some(*b),
            _ => None,
        }
    }
    pub fn as_type(&self) -> Option<&Type>{
        match self {
            Data::Type(t) => Some(t),
            _ => None,
        }
    }
    pub fn as_i64(&self) -> Option<i64>{
        match self {
            Data::Int64(i) => Some(*i),
            _ => None,
        }
    }
}
impl From<String> for Data{
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<i32> for Data{
    fn from(value: i32) -> Self {
        Self::Int32(value)
    }
}