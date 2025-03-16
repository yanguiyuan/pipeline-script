use crate::ast::r#type::Type;
use crate::core::error::Error;
use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::parser::Parser;

impl Parser {
    pub fn parse_simple_type(&mut self) -> crate::core::result::Result<Type> {
        let (token, p0) = self.token_stream.next_token();

        match &token {
            Token::Dot => {
                let _ = self.parse_special_token(Token::Dot)?;
                let ty = self.parse_type()?;
                Ok(Type::ArrayVarArg(Box::new(ty)))
            }
            Token::Identifier(id) => match id.as_str() {
                "Any" => Ok(Type::Any),
                "Unit" => Ok(Type::Unit),
                "Int8" => Ok(Type::Int8),
                "Int16" => Ok(Type::Int16),
                "Int32" => Ok(Type::Int32),
                "Int64" => Ok(Type::Int64),
                "Float" => Ok(Type::Float),
                "Double" => Ok(Type::Double),
                "Bool" => Ok(Type::Bool),
                "String" => Ok(Type::String),
                "Pointer" => {
                    if self.token_stream.peek().0 != Token::Less {
                        return Ok(Type::Pointer(Box::new(Type::Any)));
                    }
                    self.parse_generic_type(Type::Pointer)
                }
                "Array" => self.parse_generic_type(Type::Array),
                "Fn" => {
                    self.parse_special_token(Token::BraceLeft)?;
                    let mut param_type = vec![];

                    loop {
                        let peek = self.token_stream.peek().0;
                        match peek {
                            Token::BraceRight => {
                                self.parse_special_token(Token::BraceRight)?;
                                break;
                            }
                            Token::Comma => {
                                self.parse_special_token(Token::Comma)?;
                            }
                            Token::Identifier(_) => {
                                let ty = self.parse_type()?;
                                param_type.push(ty);
                            }
                            t => {
                                return Err(Error::UnexpectedToken(
                                    t.to_string(),
                                    "Identifier or special token".into(),
                                    p0,
                                ))
                            }
                        }
                    }

                    Ok(Type::Function(Box::new(Type::Unit), param_type))
                }
                name => Ok(Type::Alias(name.into())),
            },
            Token::BracketLeft => {
                self.parse_special_token(Token::BracketLeft)?;
                let ty = self.parse_type()?;
                self.parse_special_token(Token::BracketRight)?;
                Ok(Type::Array(Box::new(ty)))
            }
            _ => Err(Error::UnexpectedToken(
                token.to_string(),
                "Identifier or special token".into(),
                p0,
            )),
        }
    }
    pub fn parse_type(&mut self) -> crate::core::result::Result<Type> {
        let ty = self.parse_simple_type()?;

        // 检查是否有泛型参数
        if !self.try_parse_token(Token::Less) {
            return Ok(ty);
        }

        // 解析泛型参数列表
        let mut list = vec![];
        let t0 = self.parse_type()?;
        list.push(t0);

        // 解析结束符
        self.parse_special_token(Token::Greater)?;

        Ok(Type::Generic(Box::new(ty), list))
    }

    pub fn parse_type_name(&mut self) -> crate::core::result::Result<(String, Position)> {
        let (token, p0) = self.token_stream.next_token();

        match &token {
            Token::BracketLeft => {
                let p1 = self.parse_special_token(Token::BracketRight)?;
                let (name, p2) = self.parse_identifier()?;
                Ok((format!("[]{}", name), p0 + p1 + p2))
            }
            Token::Dot => {
                let p0 = self.parse_special_token(Token::Dot)?;
                let (name, p1) = self.parse_identifier()?;
                Ok((format!("..{}", name), p0 + p1))
            }
            Token::Identifier(id) => Ok((id.to_string(), p0)),
            _ => Err(Error::UnexpectedToken(
                token.to_string(),
                "Identifier or special token".into(),
                p0,
            )),
        }
    }
    fn parse_generic_type<F>(&mut self, constructor: F) -> crate::core::result::Result<Type>
    where
        F: FnOnce(Box<Type>) -> Type,
    {
        self.parse_special_token(Token::Less)?;
        let el_ty = self.parse_type()?;
        self.parse_special_token(Token::Greater)?;
        Ok(constructor(Box::new(el_ty)))
    }
    pub(crate) fn parse_generic_list(&mut self) -> crate::core::result::Result<Vec<Type>> {
        let mut list = vec![];

        // 解析开始符号
        self.parse_special_token(Token::Less)?;

        // 解析第一个类型
        let ty = self.parse_type()?;
        list.push(ty);

        // 解析剩余的类型
        loop {
            let (peek, _) = self.token_stream.peek();

            match peek {
                // 如果是 '|'，则继续解析下一个类型
                Token::Vertical => {
                    self.parse_special_token(Token::Vertical)?;
                    let ty = self.parse_type()?;
                    list.push(ty);
                }
                // 如果是 '>'，则结束解析
                Token::Greater => {
                    self.parse_special_token(Token::Greater)?;
                    break;
                }
                // 其他情况，报错
                t => {
                    return Err(Error::UnexpectedToken(
                        t.to_string(),
                        "| or >".into(),
                        Position::none(),
                    ))
                }
            }
        }

        Ok(list)
    }
    pub(crate) fn parse_type_generics(&mut self) -> crate::core::result::Result<Vec<Type>> {
        let mut type_generics = vec![];

        if self.try_parse_token(Token::Less) {
            loop {
                let ty = self.parse_type()?;
                type_generics.push(ty);
                if !self.try_parse_token(Token::Comma) {
                    break;
                }
            }
            self.parse_special_token(Token::Greater)?;
        }

        Ok(type_generics)
    }
}
