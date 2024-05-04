#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    String(String),
    Int(i64),
    Float(f64),
    Identifier(String),
    /// 关键字
    Keyword(String),
    /// (
    BraceLeft,
    /// )
    BraceRight,
    /// [
    SquareBracketLeft,
    /// ]
    SquareBracketRight,
    /// {
    ParenthesisLeft,
    /// }
    ParenthesisRight,
    /// .
    Dot,
    /// :
    Colon,
    //::
    ScopeSymbol,
    /// =
    Assign,
    /// ,
    Comma,
    /// +
    Plus,
    ///-
    Minus,
    /// *
    Mul,
    /// /
    Div,
    /// %
    Mod,
    /// >
    Greater,
    /// <
    Less,
    /// ==
    Equal,
    /// !=
    NotEqual,
    ///->
    Arrow,
    Eof,
}

impl Token {
    #[allow(unused)]
    pub fn token_id(&self) -> i8 {
        match self {
            Token::String(_) => 0,
            Token::Int(_) => 1,
            Token::Float(_) => 2,
            Token::Identifier(_) => 3,
            Token::Keyword(_) => 4,
            Token::BraceLeft => 5,
            Token::BraceRight => 6,
            Token::SquareBracketLeft => 7,
            Token::SquareBracketRight => 8,
            Token::ParenthesisLeft => 9,
            Token::ParenthesisRight => 10,
            Token::Dot => 11,
            Token::Comma => 12,
            Token::Eof => 13,
            Token::Colon => 14,
            Token::Assign => 15,
            Token::Plus => 16,
            Token::Mul => 17,
            Token::Greater => 18,
            Token::Less => 19,
            Token::Equal => 20,
            Token::Minus => 21,
            Token::Div => 22,
            Token::Mod => 23,
            Token::ScopeSymbol => 24,
            Token::NotEqual => 25,
            Token::Arrow => 26,
        }
    }
    pub fn is_colon(&self) -> bool {
        matches!(self, Token::Colon)
    }
    pub fn is_parenthesis_left(&self) -> bool {
        matches!(self, Token::ParenthesisLeft)
    }
    pub fn is_keyword(&self, keyword: &str) -> bool {
        match self {
            Token::Keyword(k) => {
                if k == keyword {
                    return true;
                }
                false
            }
            _ => false,
        }
    }
    pub fn get_identifier_value(&self) -> &str {
        return match self {
            Token::Identifier(s) => s.as_str(),
            _ => "",
        };
    }
}
