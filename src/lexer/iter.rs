use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::lexer::Lexer;

pub struct TokenStream {
    tokenizer: Lexer,
    peek: Vec<(Token, Position)>,
}

impl Iterator for TokenStream {
    type Item = (Token, Position);

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            (Token::Eof, _) => None,
            t => Some(t),
        }
    }
}

impl TokenStream {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            tokenizer: lexer,
            peek: Vec::new(),
        }
    }
    pub fn is_eof(&self) -> bool {
        self.peek.is_empty() && self.tokenizer.is_eof()
    }
    pub fn set_lexer(&mut self, lexer: Lexer) {
        self.tokenizer = lexer;
    }
    pub fn next_token(&mut self) -> (Token, Position) {
        if !self.peek.is_empty() {
            return self.peek.remove(0);
        }
        self.tokenizer
            .next_token()
            .unwrap_or((Token::Eof, Position::none()))
    }
    pub fn peek(&mut self) -> (Token, Position) {
        self.peek_nth(0)
    }
    pub fn peek_nth(&mut self, n: usize) -> (Token, Position) {
        // 确保缓存中有足够的元素
        while self.peek.len() <= n {
            match self.tokenizer.next_token() {
                Some(token) => self.peek.push(token),
                None => self.peek.push((Token::Eof, Position::none())),
            }
        }

        // 返回第n个元素的克隆
        self.peek[n].clone()
    }

    // 获取从当前位置开始的n个token
    pub fn peek_n(&mut self, n: usize) -> Vec<(Token, Position)> {
        // 确保缓存中有足够的元素
        while self.peek.len() < n {
            match self.tokenizer.next_token() {
                Some(token) => self.peek.push(token),
                None => self.peek.push((Token::Eof, Position::none())),
            }
        }

        // 返回前n个元素的克隆
        self.peek.iter().take(n).cloned().collect()
    }
}
