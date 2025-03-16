use crate::ast::stmt::StmtNode;
use crate::context::Context;
use crate::core::error::Error;
use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::parser::Parser;

pub fn is_enum(ctx: &Context, id: &str) -> bool {
    ctx.get_type_alias(id)
        .map_or(false, |ty| ty.get_type().is_enum())
}

impl Parser {
    fn parse_token<T, F>(
        &mut self,
        expected: &str,
        validator: F,
    ) -> crate::core::result::Result<(T, Position)>
    where
        F: FnOnce(&Token) -> Option<T>,
    {
        let (token, pos) = self.token_stream.next_token();

        match validator(&token) {
            Some(value) => Ok((value, pos)),
            None => Err(Error::UnexpectedToken(
                token.to_string(),
                expected.into(),
                pos,
            )),
        }
    }

    pub fn parse_identifier(&mut self) -> crate::core::result::Result<(String, Position)> {
        self.parse_token("Identifier", |token| {
            if let Token::Identifier(id) = token {
                Some(id.clone())
            } else {
                None
            }
        })
    }

    pub fn parse_keyword(&mut self, keyword: &str) -> crate::core::result::Result<Position> {
        let expected = format!("Keyword({})", keyword);
        let (_, pos) = self.parse_token(&expected, |token| {
            if let Token::Keyword(k) = token {
                if k == keyword {
                    Some(())
                } else {
                    None
                }
            } else {
                None
            }
        })?;
        Ok(pos)
    }

    pub fn parse_special_token(&mut self, token: Token) -> crate::core::result::Result<Position> {
        let expected = token.to_string();
        let (_, pos) =
            self.parse_token(&expected, |t| if t == &token { Some(()) } else { None })?;
        Ok(pos)
    }
    pub(crate) fn parse_block(
        &mut self,
        ctx: &Context,
    ) -> crate::core::result::Result<Vec<StmtNode>> {
        let mut result = vec![];
        self.parse_special_token(Token::ParenLeft)?;
        loop {
            let (token, _) = self.token_stream.peek();
            match token {
                Token::ParenRight => {
                    self.parse_special_token(Token::ParenRight)?;
                    break;
                }
                _ => {
                    result.push(self.parse_stmt(ctx).unwrap());
                }
            }
        }
        Ok(result)
    }
    pub(crate) fn try_parse_token(&mut self, token: Token) -> bool {
        let (token0, _) = self.token_stream.peek();
        if token0 == token {
            self.token_stream.next_token();
            return true;
        }
        false
    }
    pub fn next_is(&mut self, token: Token) -> bool {
        self.token_stream.peek().0 == token
    }
}
