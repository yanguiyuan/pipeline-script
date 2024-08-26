use crate::lexer::position::Position;

#[derive(Debug, Clone)]
pub enum Error {
    TypeError(String, Position),
    FunctionUndefined(String, Position),
    ClassUndefined(String, Position),
    StaticFunctionUndefined(String, String, Position),
    VariableUndefined(String, Position),
    MismatchedType(String, String, Position),
    AssignToImmutableVariable(String, Position),
    MapKeyNotExist(String, String, Position),
    ExpectedType(String),
    UnexpectedType(String),
    UnexpectedToken(String, String, Position),
    UnusedKeyword(String, Position),
    UnknownModule(String, Position),
    UndefinedOperation(String),
}
