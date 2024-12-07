use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::lexer::Lexer;

pub struct TokenStream {
    tokenizer: Lexer,
    peek: Option<(Token, Position)>,
}

impl Iterator for TokenStream {
    type Item = (Token, Position);

    fn next(&mut self) -> Option<Self::Item> {
        match self.next() {
            (Token::Eof, _) => None,
            t => Some(t),
        }
    }
}

impl TokenStream {
    #[allow(unused)]
    pub fn new(lexer: Lexer) -> Self {
        Self {
            tokenizer: lexer,
            peek: None,
        }
    }
    pub fn is_eof(&self) -> bool {
        self.peek.is_none() && self.tokenizer.is_eof()
    }
    #[allow(unused)]
    pub fn set_lexer(&mut self, lexer: Lexer) {
        self.tokenizer = lexer;
    }
    pub fn next(&mut self) -> (Token, Position) {
        if self.peek.is_some() {
            return self.peek.take().unwrap_or((Token::Eof, Position::none()));
        }
        self.tokenizer
            .next()
            .unwrap_or((Token::Eof, Position::none()))
    }
    #[allow(unused)]
    pub fn peek(&mut self) -> (Token, Position) {
        if self.peek.is_some() {
            return self.peek.clone().unwrap();
        }
        let o = self.tokenizer.next();
        self.peek.clone_from(&o);
        o.unwrap_or((Token::Eof, Position::none()))
    }
}
