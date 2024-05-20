use crate::position::Position;
use crate::token::Token;
use std::ops::Add;

#[derive(Debug, Clone)]
pub struct Lexer {
    module_name: String,
    chars: Vec<char>,
    index: usize,
    col: usize,
    row: usize,
    keywords: Vec<&'static str>,
}
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

impl IntoIterator for Lexer {
    type Item = (Token, Position);
    type IntoIter = TokenStream;

    fn into_iter(self) -> Self::IntoIter {
        TokenStream {
            tokenizer: self,
            peek: None,
        }
    }
}
impl Lexer {
    pub fn new(module_name: impl Into<String>) -> Self {
        Self {
            chars: vec![],
            index: 0,
            col: 1,
            row: 1,
            module_name: module_name.into(),
            keywords: vec![
                "let", "fn", "fun", "return", "if", "while", "import", "else", "val", "var",
                "break", "continue", "for", "in", "class", "static","trait"
            ],
        }
    }
    #[allow(unused)]
    pub fn get_module_name(&self) -> String {
        self.module_name.clone()
    }
    #[allow(unused)]
    pub fn line(&self, line: usize) -> String {
        let mut s = String::new();
        let mut current_line = 1;
        for i in &self.chars {
            if i == &'\n' {
                current_line += 1;
                continue;
            }
            if current_line == line {
                s.push(*i);
            }
            if current_line > line {
                break;
            }
        }
        s
    }
    #[allow(unused)]
    pub fn set_chars(&mut self, chars: Vec<char>) {
        self.chars = chars;
    }
    pub fn next(&mut self) -> Option<(Token, Position)> {
        loop {
            match self.current_char() {
                None => return None,
                Some(c) => {
                    let peek = self.peek_char().unwrap_or('\0');
                    match (c, peek) {
                        ('.', p) if !p.is_numeric() => {
                            let r = Some((Token::Dot, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('0'..='9' | '.', _) => {
                            if c == '.' && !peek.is_numeric() {
                                continue;
                            }
                            return self.scan_number();
                        }
                        ('a'..='z' | 'A'..='Z', _) => {
                            let ident = self.scan_identifier();
                            let clone = ident.clone().unwrap();
                            let ident_str = clone.0.get_identifier_value();
                            if self.keywords.contains(&ident_str) {
                                return Some((Token::Keyword(String::from(ident_str)), clone.1));
                            }
                            return ident;
                        }

                        ('(', _) => {
                            let r = Some((Token::BraceLeft, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        (')', _) => {
                            let r = Some((Token::BraceRight, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('{', _) => {
                            let r = Some((Token::ParenthesisLeft, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('}', _) => {
                            let r = Some((Token::ParenthesisRight, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('[', _) => {
                            let r = Some((Token::SquareBracketLeft, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        (']', _) => {
                            let r = Some((Token::SquareBracketRight, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        (':', ':') => {
                            let r = Some((Token::ScopeSymbol, self.with_pos(1)));
                            self.next_char();
                            self.next_char();
                            return r;
                        }
                        (':', _) => {
                            let r = Some((Token::Colon, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        (',', _) => {
                            let r = Some((Token::Comma, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('|', _) => {
                            let r = Some((Token::BitOr, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('!', '=') => {
                            let r = Some((Token::NotEqual, self.with_pos(2)));
                            self.next_char();
                            self.next_char();
                            return r;
                        }
                        ('=', '=') => {
                            let r = Some((Token::Equal, self.with_pos(2)));
                            self.next_char();
                            self.next_char();
                            return r;
                        }
                        ('-', '>') => {
                            let r = Some((Token::Arrow, self.with_pos(2)));
                            self.next_char();
                            self.next_char();
                            return r;
                        }
                        ('=', _) => {
                            let r = Some((Token::Assign, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('>', _) => {
                            let r = Some((Token::Greater, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('<', _) => {
                            let r = Some((Token::Less, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('+', _) => {
                            let r = Some((Token::Plus, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('-', _) => {
                            let r = Some((Token::Minus, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('*', _) => {
                            let r = Some((Token::Mul, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        ('%', _) => {
                            let r = Some((Token::Mod, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }

                        ('"', _) => {
                            let r = self.scan_string('"');
                            self.increase_index();
                            return r;
                        }
                        ('\'', _) => {
                            let r = self.scan_string('\'');
                            self.increase_index();
                            return r;
                        }
                        (' ' | '\r' | ';' | '\t', _) => {
                            self.next_char();
                        }
                        ('\n', _) => {
                            // self.row+=1;
                            self.next_char();
                        }
                        ('/', '/') => {
                            while self.current_char() != Some('\n') {
                                self.next_char();
                            }
                        }
                        ('/', '*') => {
                            while self.current_char() != Some('*') || self.peek_char() != Some('/')
                            {
                                self.next_char();
                            }
                            self.next_char();
                            self.next_char();
                        }
                        ('/', _) => {
                            let r = Some((Token::Div, self.with_pos(1)));
                            self.next_char();
                            return r;
                        }
                        t => {
                            println!("{:?}", t);
                            return None;
                        }
                    }
                }
            }
        }
    }
    fn with_pos(&self, span: usize) -> Position {
        Position::new(self.index, span, self.row, self.col, &self.module_name)
    }
    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.index + 1).copied()
    }
    fn next_char(&mut self) -> Option<char> {
        let c = self.peek_char();
        self.increase_index();
        c
    }
    fn current_char(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }
    fn increase_index(&mut self) {
        if self.chars.get(self.index).unwrap() == &'\n' {
            self.row += 1;
            self.col = 0;
        }
        self.index = self.index.add(1);
        self.col += 1;
    }
    fn scan_number(&mut self) -> Option<(Token, Position)> {
        let mut v = String::new();
        let mut pos = self.with_pos(0);
        let mut is_decimal = false;
        while let Some(c) = self.current_char() {
            if c == '.' && !is_decimal {
                let next = self.peek_char().unwrap();
                if !next.is_alphabetic() {
                    v.push(c);
                    self.increase_index();
                    is_decimal = true;
                    continue;
                }
            }
            if !c.is_numeric() {
                break;
            }
            v.push(c);
            self.increase_index();
        }
        pos.set_span(v.len());
        if is_decimal {
            let f: f64 = v.parse().unwrap();
            return Some((Token::Float(f), pos));
        }
        let i: i64 = v.parse().unwrap();
        Some((Token::Int(i), pos))
    }
    fn scan_identifier(&mut self) -> Option<(Token, Position)> {
        let mut v = String::new();
        let mut pos = self.with_pos(0);
        while let Some(c) = self.current_char() {
            if !c.is_alphabetic() && !c.is_numeric() {
                break;
            }
            v.push(c);
            self.increase_index();
        }
        pos.set_span(v.len());
        Some((Token::Identifier(v), pos))
    }
    fn scan_string(&mut self, prefix: char) -> Option<(Token, Position)> {
        let mut v = String::new();
        let mut pos = self.with_pos(0);
        self.increase_index();
        while let Some(c) = self.current_char() {
            if c == prefix {
                break;
            }
            v.push(c);
            self.increase_index();
        }

        pos.set_span(v.len() + 2);
        Some((Token::String(v), pos))
    }
    #[allow(unused)]
    pub fn get_source(&self) -> Vec<char> {
        self.chars.clone()
    }
    pub fn from_script(module_name: impl Into<String>, script: impl AsRef<str>) -> Self {
        let script = script.as_ref().chars().collect();
        let mut lexer = Lexer::new(module_name);
        lexer.set_chars(script);
        lexer
    }
}
