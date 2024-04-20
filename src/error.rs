use crate::position::Position;

pub type PipelineResult<T>=Result<T,PipelineError>;
#[derive(Debug,Clone)]
pub enum PipelineError{
    FunctionUndefined(String),
    VariableUndefined(String),
    ExpectedType(String),
    UnexpectedType(String),
    UnexpectedToken(crate::token::Token,Position),
    UnusedKeyword(String),
    UnknownModule(String),
    UndefinedOperation(String)
}

