use crate::parser::r#type::Type;

#[derive(Debug)]
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

impl From<String> for Data{
    fn from(value: String) -> Self {
        Self::String(value)
    }
}