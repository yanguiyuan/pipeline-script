use std::fmt::{Display, Formatter};

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
    /// |
    BitOr,
    /// !
    Negate,
    /// &&
    And,
    /// ||
    Or,
    /// @
    Annotation,
    Eof,
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::String(s) => write!(f, "String({s})"),
            Token::Int(i) => write!(f, "Int({i})"),
            Token::Float(f0) => write!(f, "Float({f0})"),
            Token::Identifier(i) => write!(f, "Identifier({i})"),
            Token::Keyword(kw) => write!(f, "Keyword({kw})"),
            Token::BraceLeft => write!(f, "Symbol(()"),
            Token::BraceRight => write!(f, "Symbol())"),
            Token::SquareBracketLeft => write!(f, "Symbol([)"),
            Token::SquareBracketRight => write!(f, "Symbol(])"),
            Token::ParenthesisLeft => write!(f, "Symbol({{)"),
            Token::ParenthesisRight => write!(f, "Symbol(}})"),
            Token::Dot => write!(f, "Symbol(.)"),
            Token::Colon => write!(f, "Symbol(:)"),
            Token::ScopeSymbol => write!(f, "Symbol(::)"),
            Token::Assign => write!(f, "Symbol(=)"),
            Token::Comma => write!(f, "Symbol(,)"),
            Token::Plus => write!(f, "Symbol(+)"),
            Token::Minus => write!(f, "Symbol(-)"),
            Token::Mul => write!(f, "Symbol(*)"),
            Token::Div => write!(f, "Symbol(/)"),
            Token::Mod => write!(f, "Symbol(%)"),
            Token::Greater => write!(f, "Symbol(>)"),
            Token::Less => write!(f, "Symbol(<)"),
            Token::Equal => write!(f, "Symbol(==)"),
            Token::NotEqual => write!(f, "Symbol(!=)"),
            Token::Arrow => write!(f, "Symbol(->)"),
            Token::BitOr => write!(f, "Symbol(|)"),
            Token::Eof => write!(f, "EOF"),
            Token::Negate => write!(f, "Symbol(!)"),
            Token::And => write!(f, "Symbol(&&)"),
            Token::Or => write!(f, "Symbol(||)"),
            Token::Annotation => write!(f, "Symbol(@)"),
        }
    }
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
            Token::BitOr => 27,
            Token::Negate => 28,
            Token::And => 29,
            Token::Or => 30,
            Token::Annotation => 31,
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
    pub fn is_assign(&self) -> bool {
        matches!(self, Token::Assign)
    }
    pub fn get_identifier_value(&self) -> &str {
        return match self {
            Token::Identifier(s) => s.as_str(),
            _ => "",
        };
    }
}
