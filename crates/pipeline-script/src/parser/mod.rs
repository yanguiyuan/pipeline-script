use crate::core::error::Error;
use crate::core::result::Result;
use crate::lexer::iter::TokenStream;
use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::lexer::Lexer;
use crate::parser::declaration::VariableDeclaration;
use crate::parser::expr::{Argument, Expr, FnCallExpr, Op, StructExpr};
use std::collections::HashMap;

use crate::parser::function::Function;
use crate::parser::module::Module;
use crate::parser::r#struct::{Struct, StructField};
use crate::parser::r#type::Type;
use crate::parser::stmt::{IfBranchStmt, IfStmt, Stmt};

use self::expr::ExprNode;
use self::stmt::StmtNode;

mod class;
pub mod declaration;
pub mod expr;
pub mod function;
pub mod module;
mod peg;
pub mod stmt;
pub mod r#struct;
pub mod r#type;

pub struct Parser {
    token_stream: TokenStream,
    module: Module,
    function_keyword: String,
}
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let name = lexer.get_file_name();
        Self {
            token_stream: TokenStream::new(lexer),
            module: Module::new(name),
            function_keyword: "fn".into(),
        }
    }
    pub fn parse(&mut self) -> Result<&Module> {
        while !self.token_stream.is_eof() {
            let stmt = self.parse_stmt()?;
            if stmt.is_noop() {
                break;
            }
            self.module.add_stmt(stmt);
        }
        return Ok(&self.module);
    }
    pub fn parse_stmt(&mut self) -> Result<StmtNode> {
        loop {
            let (token, _) = self.token_stream.peek();
            return Ok(match token {
                Token::Keyword(k) => match k.as_str() {
                    "var" => self.parse_var_stmt()?,
                    "val" => self.parse_val_stmt()?,
                    "if" => self.parse_if_stmt().unwrap(),
                    "while" => self.parse_while_stmt()?,
                    "return" => self.parse_return_stmt()?,
                    "break" => self.parse_break_stmt()?,
                    "continue" => self.parse_continue_stmt()?,
                    "struct" => {
                        self.parse_struct()?;
                        continue;
                    }
                    "enum" => {
                        self.parse_enum();
                        continue;
                    }
                    "trait" => {
                        self.parse_trait();
                        continue;
                    }
                    "extern" => {
                        self.parse_extern_function_declaration()?;
                        continue;
                    }
                    t => {
                        if t == self.function_keyword.as_str() {
                            self.parse_function().unwrap();
                            continue;
                        }
                        todo!()
                    }
                },
                Token::Eof => StmtNode::new(Stmt::Noop, Position::none()),
                _ => {
                    let e0 = self.parse_expr().unwrap();
                    let p0 = e0.position();
                    let (peek, p1) = self.token_stream.peek();
                    if let Token::Assign = peek {
                        let p0 = self.parse_special_token(Token::Assign)?;
                        let e1 = self.parse_expr()?;
                        let p2 = e1.position();
                        return Ok(StmtNode::new(
                            Stmt::Assign(Box::new(e0), Box::new(e1)),
                            p0 + p1 + p2,
                        ));
                    }
                    return Ok(StmtNode::new(Stmt::EvalExpr(Box::new(e0)), p0));
                }
            });
        }
    }
    fn parse_extern_function_declaration(&mut self) -> Result<()> {
        self.parse_keyword("extern")?;
        let fun = self.parse_function_declaration()?;
        let fun = fun.with_extern(true);
        self.module.register_function(&fun.name(), fun);
        Ok(())
    }
    fn parse_param_list(&mut self) -> Result<Vec<VariableDeclaration>> {
        let mut v = vec![];
        loop {
            let (token, pos) = self.token_stream.peek();
            match token {
                Token::Identifier(id) => {
                    let _ = self.parse_identifier()?;
                    let _ = self.parse_special_token(Token::Colon)?;
                    let ty = self.parse_type()?;
                    v.push(VariableDeclaration::new(id).with_type(ty));
                    continue;
                }
                Token::Comma => {
                    self.parse_special_token(Token::Comma)?;
                    continue;
                }
                Token::BraceRight | Token::Vertical => {
                    break;
                }
                t => panic!("unexpected token {:?} at {:?}", t, pos),
            }
        }
        Ok(v)
    }
    fn parse_continue_stmt(&mut self) -> Result<StmtNode> {
        todo!()
    }
    fn parse_break_stmt(&mut self) -> Result<StmtNode> {
        todo!()
    }
    fn parse_struct(&mut self) -> Result<()> {
        let _ = self.parse_keyword("struct")?;
        let (struct_name, _) = self.parse_identifier()?;
        let _ = self.parse_special_token(Token::ParenLeft)?;
        let mut fields = vec![];
        loop {
            let (token, _) = self.token_stream.peek();
            match token {
                Token::Identifier(id) => {
                    let _ = self.parse_identifier()?;
                    let _ = self.parse_special_token(Token::Colon)?;
                    let (type_name, _) = self.parse_type_name()?;
                    fields.push(StructField::new(id, type_name.into()));
                    continue;
                }
                Token::Comma => {
                    self.parse_special_token(Token::Comma)?;
                    continue;
                }
                Token::ParenRight => {
                    self.parse_special_token(Token::ParenRight)?;
                    break;
                }
                _ => break,
            }
        }
        let struct_declaration = Struct::new(struct_name.clone(), fields);
        self.module
            .register_struct(&struct_name, struct_declaration);
        Ok(())
    }
    fn parse_enum(&mut self) {}
    fn parse_trait(&mut self) {}
    fn parse_function(&mut self) -> Result<()> {
        let mut fun = self.parse_function_declaration().unwrap();
        let block = self.parse_block().unwrap();
        fun.set_body(block);
        self.module.register_function(&fun.name(), fun);
        Ok(())
    }
    fn try_parse_token(&mut self, token: Token) -> bool {
        let (token0, _) = self.token_stream.peek();
        if token0 == token {
            self.token_stream.next();
            return true;
        }
        false
    }
    fn parse_function_declaration(&mut self) -> Result<Function> {
        let mut fun = Function::default();
        let fun_name = self.function_keyword.clone();
        let _ = self.parse_keyword(&fun_name)?;
        let (mut name, _) = self.parse_identifier()?;
        if self.try_parse_token(Token::Dot) {
            let (name0, _) = self.parse_identifier()?;
            fun.set_binding_struct(name);
            name = name0;
        }
        // 解析泛型
        let mut list = vec![];
        if self.try_parse_token(Token::Less) {
            let ty= self.parse_type()?;
            list.push(ty);
            self.parse_special_token(Token::Greater)?;
        }
        let _ = self.parse_special_token(Token::BraceLeft)?;
        let param_list = self.parse_param_list()?;
        let _ = self.parse_special_token(Token::BraceRight)?;
        if self.try_parse_token(Token::Arrow) {
            let ret_ty= self.parse_type()?;
            return Ok(fun
                .with_name(name)
                .with_args(param_list)
                .with_generic_list(list)
                .with_return_type(ret_ty));
        }
        Ok(fun
            .with_name(name)
            .with_args(param_list)
            .with_generic_list(list)
            .with_return_type("Unit".into()))
    }
    fn parse_block(&mut self) -> Result<Vec<StmtNode>> {
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
                    result.push(self.parse_stmt().unwrap());
                }
            }
        }
        Ok(result)
    }
    fn parse_return_stmt(&mut self) -> Result<StmtNode> {
        let p0 = self.parse_keyword("return")?;
        if self.token_stream.peek().0 == Token::ParenRight {
            return Ok(StmtNode::new(Stmt::Return(Box::new(Expr::None.into())), p0));
        }
        let expr = self.parse_expr()?;
        Ok(StmtNode::new(Stmt::Return(Box::new(expr)), p0))
    }
    pub fn parse_while_stmt(&mut self) -> Result<StmtNode> {
        let mut p0 = self.parse_keyword("while")?;
        let p1 = self.parse_special_token(Token::BraceLeft)?;
        let expr = self.parse_expr()?;
        let p2 = expr.position();
        let p3 = self.parse_special_token(Token::BraceRight)?;
        let block = self.parse_block()?;
        for s in block.iter() {
            p0 = p0 + s.position();
        }
        Ok(StmtNode::new(
            Stmt::While(Box::new(expr), block),
            p0 + p1 + p2 + p3,
        ))
    }
    pub fn parse_if_stmt(&mut self) -> Result<StmtNode> {
        let mut branches = vec![];
        let mut else_body = None;
        let (b, pos) = self.parse_if_branch()?;
        branches.push(b);

        loop {
            let (peek, _) = self.token_stream.peek();
            match peek.clone() {
                Token::Keyword(k) if k == "else" => {
                    self.token_stream.next();
                    let (peek0, _) = self.token_stream.peek();
                    if let Token::ParenLeft = peek0 {
                        let blocks = self.parse_block()?;
                        else_body = Some(blocks);
                        break;
                    }
                    let (b0, _) = self.parse_if_branch()?;
                    branches.push(b0);
                }
                _ => break,
            }
        }
        Ok(StmtNode::new(
            Stmt::If(Box::new(IfStmt::new(branches, else_body))),
            pos,
        ))
    }
    fn parse_if_branch(&mut self) -> Result<(IfBranchStmt, Position)> {
        let mut p0 = self.parse_keyword("if")?;
        let p1 = self.parse_special_token(Token::BraceLeft)?;
        let e = self.parse_expr()?;
        let p2 = e.position();
        let p3 = self.parse_special_token(Token::BraceRight)?;
        let block = self.parse_block()?;
        for b in block.iter() {
            p0 = p0 + b.position();
        }
        let branch = IfBranchStmt::new(e, block);
        Ok((branch, p0 + p1 + p2 + p3))
    }
    pub fn parse_var_stmt(&mut self) -> Result<StmtNode> {
        let p0 = self.parse_keyword("var")?;
        let (name, _) = self.parse_identifier()?;
        let p2 = self.parse_special_token(Token::Colon)?;
        let (type_name, p3) = self.parse_type_name()?;
        let p4 = self.parse_special_token(Token::Assign)?;
        let expr = self.parse_expr()?;
        let p5 = expr.position();
        Ok(StmtNode::new(
            Stmt::VarDecl(
                VariableDeclaration::new(name)
                    .with_type(type_name.into())
                    .with_default(expr),
            ),
            p0 + p2 + p3 + p4 + p5,
        ))
    }
    pub fn parse_val_stmt(&mut self) -> Result<StmtNode> {
        let p0 = self.parse_keyword("val")?;
        let (name, mut p1) = self.parse_identifier()?;
        let mut vd = VariableDeclaration::new(name);
        if self.token_stream.peek().0 == Token::Colon {
            p1 = p1 + self.parse_special_token(Token::Colon)?;
            let (type_name, p3) = self.parse_type_name()?;
            p1 = p1 + p3;
            vd = vd.with_type(type_name.into())
        }
        let p4 = self.parse_special_token(Token::Assign)?;
        let expr = self.parse_expr()?;
        let p5 = expr.position();
        Ok(StmtNode::new(
            Stmt::ValDecl(vd.with_default(expr)),
            p0 + p1 + p4 + p5,
        ))
    }
    pub fn parse_chain_expr(&mut self) -> Result<ExprNode> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            let (peek, _) = self.token_stream.peek();
            expr = match peek {
                Token::Dot => {
                    self.parse_special_token(Token::Dot)?;
                    let (name, pos) = self.parse_identifier()?;
                    ExprNode::from(Expr::Member(Box::new(expr), name)).with_position(pos)
                }
                Token::BracketLeft => {
                    self.parse_special_token(Token::BracketLeft)?;
                    let index = self.parse_expr()?;
                    let pos = index.position();
                    self.parse_special_token(Token::BracketRight)?;
                    ExprNode::from(Expr::Index(Box::new(expr), Box::new(index))).with_position(pos)
                }
                _ => break,
            };
        }

        Ok(expr)
    }
    fn parse_fn_args(&mut self) -> Result<(Vec<Argument>, Position)> {
        let mut p0 = self.parse_special_token(Token::BraceLeft)?;
        let mut args = vec![];
        loop {
            let (peek, p1) = self.token_stream.peek();
            p0 = p0 + p1;
            match peek {
                Token::BraceRight => {
                    let p2 = self.parse_special_token(Token::BraceRight)?;
                    p0 = p0 + p2;
                    break;
                }
                Token::Comma => {
                    let p2 = self.parse_special_token(Token::Comma)?;
                    p0 = p0 + p2;
                    continue;
                }
                _ => {
                    let expr = self.parse_expr()?;
                    args.push(Argument::new(expr));
                }
            }
        }
        Ok((args, p0))
    }
    pub fn parse_primary_expr(&mut self) -> Result<ExprNode> {
        let (token, mut pos) = self.token_stream.next();
        match token {
            Token::Vertical => {
                let peek = self.token_stream.peek().0;
                let mut l = vec![];
                if peek != Token::Vertical {
                    l = self.parse_param_list()?;
                }
                let p1 = self.parse_special_token(Token::Vertical)?;
                let block = self.parse_block()?;
                Ok(ExprNode::from(Expr::Closure(l, block, vec![])).with_position(p1 + pos))
            }
            Token::Identifier(id) => {
                let (peek, _) = self.token_stream.peek();
                match peek {
                    Token::BraceLeft => {
                        let (args, p0) = self.parse_fn_args()?;
                        pos = pos + p0;
                        Ok(ExprNode::from(Expr::FnCall(FnCallExpr { name: id, args }))
                            .with_position(pos))
                    }
                    Token::ParenLeft => {
                        // 解析结构体构造
                        let p0 = self.parse_special_token(Token::ParenLeft)?;
                        let mut fields = HashMap::new();
                        loop {
                            let (peek, p1) = self.token_stream.peek();
                            pos = pos + p1;
                            match peek {
                                Token::ParenRight => {
                                    let p2 = self.parse_special_token(Token::ParenRight)?;
                                    pos = pos + p2;
                                    break;
                                }
                                Token::Comma => {
                                    let p2 = self.parse_special_token(Token::Comma)?;
                                    pos = pos + p2;
                                    continue;
                                }
                                Token::Identifier(ident) => {
                                    let (_, _) = self.parse_identifier()?;
                                    let _ = self.parse_special_token(Token::Colon)?;
                                    let expr = self.parse_expr()?;
                                    let _ = expr.position();
                                    fields.insert(ident, expr);
                                }
                                _ => todo!("parse primary expr"),
                            }
                        }

                        pos = pos + p0;
                        Ok(ExprNode::from(Expr::Struct(StructExpr::new(id, fields)))
                            .with_position(pos))
                    }
                    _ => Ok(ExprNode::from(Expr::Variable(id)).with_position(pos)),
                }
            }
            Token::Int(n) => Ok(ExprNode::from(Expr::Int(n)).with_position(pos)),
            Token::String(s) => Ok(ExprNode::from(Expr::String(s)).with_position(pos)),
            Token::Boolean(b) => Ok(ExprNode::from(Expr::Boolean(b)).with_position(pos)),
            Token::BraceLeft => {
                let expr = self.parse_expr()?;
                self.parse_special_token(Token::BraceRight)?;
                Ok(ExprNode::from(Expr::BraceExpr(Box::new(expr))).with_position(pos))
            }
            Token::BitAnd => {
                let expr = self.parse_primary_expr()?;
                let _ = expr.position();
                Ok(ExprNode::from(Expr::Address(Box::new(expr))).with_position(pos))
            }
            Token::BracketLeft => {
                let mut v = vec![];
                loop {
                    let expr = self.parse_expr()?;
                    pos = pos + expr.position();
                    v.push(expr);
                    let (token, p1) = self.token_stream.peek();
                    pos = pos + p1;
                    match token {
                        Token::BracketRight => {
                            let p2 = self.parse_special_token(Token::BracketRight)?;
                            return Ok(ExprNode::from(Expr::Array(v)).with_position(pos + p2));
                        }
                        Token::Comma => {
                            let p2 = self.parse_special_token(Token::Comma)?;
                            pos = pos + p2;
                            continue;
                        }
                        _ => {}
                    }
                }
            }
            _ => Err(Error::UnexpectedToken(
                token.to_string(),
                "PrimaryExpr(?)".into(),
                pos,
            )),
        }
    }
    pub fn parse_fact_expr(&mut self) -> Result<ExprNode> {
        let expr = self.parse_chain_expr()?;
        let _ = expr.position();
        let (token, _) = self.token_stream.peek();
        match token {
            Token::BraceLeft => {
                let (mut args, _) = self.parse_fn_args()?;
                args.insert(0, Argument::new(expr.get_member_root()));
                Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                    name: expr.get_member_name(),
                    args,
                })))
            }
            Token::BracketLeft => {
                let p0 = self.parse_special_token(Token::BracketLeft)?;
                let index = self.parse_expr()?;
                let p1 = index.position();
                let p2 = self.parse_special_token(Token::BracketRight)?;
                Ok(ExprNode::from(Expr::Index(Box::new(expr), Box::new(index)))
                    .with_position(p0 + p1 + p2))
            }

            _ => Ok(expr),
        }
    }
    pub fn parse_term(&mut self) -> Result<ExprNode> {
        let expr0 = self.parse_fact_expr()?;
        let p0 = expr0.position();
        let (token, _) = self.token_stream.peek();
        match token {
            Token::Star => {
                let p1 = self.parse_special_token(Token::Star)?;
                let expr1 = self.parse_term()?;
                let p2 = expr1.position();
                Ok(
                    ExprNode::from(Expr::Binary(Op::Mul, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0 + p1 + p2),
                )
            }
            Token::Slash => {
                let p0 = self.parse_special_token(Token::Slash)?;
                let expr1 = self.parse_term()?;
                Ok(
                    ExprNode::from(Expr::Binary(Op::Div, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            _ => Ok(expr0),
        }
    }

    pub fn parse_expr0(&mut self) -> Result<ExprNode> {
        let expr0 = self.parse_term()?;
        let (token, _) = self.token_stream.peek();
        match token {
            Token::Plus => {
                let p0 = self.parse_special_token(Token::Plus)?;
                let expr1 = self.parse_expr()?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Plus, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::Minus => {
                let p0 = self.parse_special_token(Token::Minus)?;
                let expr1 = self.parse_expr()?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Minus, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            _ => Ok(expr0),
        }
    }
    pub fn parse_expr(&mut self) -> Result<ExprNode> {
        let expr0 = self.parse_expr0()?;
        let (token, _) = self.token_stream.peek();
        match token {
            Token::Equal => {
                let p0 = self.parse_special_token(Token::Equal)?;
                let expr1 = self.parse_expr()?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Equal, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::NotEqual => {
                let p0 = self.parse_special_token(Token::NotEqual)?;
                let expr1 = self.parse_expr()?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::NotEqual, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::Less => {
                let p0 = self.parse_special_token(Token::Less)?;
                let expr1 = self.parse_expr()?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Less, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::Greater => {
                let p0 = self.parse_special_token(Token::Greater)?;
                let expr1 = self.parse_expr()?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Greater, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            _ => Ok(expr0),
        }
    }
    pub fn parse_keyword(&mut self, keyword: &str) -> Result<Position> {
        let (token, pos) = self.token_stream.next();
        match token {
            Token::Keyword(k) => {
                if k == keyword {
                    return Ok(pos);
                }
                Err(Error::UnexpectedToken(
                    format!("Keyword({k})"),
                    format!("Keyword({keyword})"),
                    pos,
                ))
            }
            _ => Err(Error::UnexpectedToken(
                token.to_string(),
                "Keyword(?)".into(),
                pos,
            )),
        }
    }
    pub fn parse_identifier(&mut self) -> Result<(String, Position)> {
        let (token, pos) = self.token_stream.next();
        match token {
            Token::Identifier(id) => Ok((id, pos)),
            _ => Err(Error::UnexpectedToken(
                token.to_string(),
                "Identifier(?)".into(),
                pos,
            )),
        }
    }
    pub fn parse_special_token(&mut self, token: Token) -> Result<Position> {
        let (t, pos) = self.token_stream.next();
        if t == token {
            return Ok(pos);
        } else {
            Err(Error::UnexpectedToken(
                t.to_string(),
                token.to_string(),
                pos,
            ))
        }
    }
    pub fn parse_simple_type(&mut self) -> Result<Type>{
        let (token, p0) = self.token_stream.next();
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
                "Array"=>{
                    self.parse_special_token(Token::Less)?;
                    let el_ty = self.parse_type()?;
                    self.parse_special_token(Token::Greater)?;
                    Ok(Type::Array(Box::new(el_ty)))
                },
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
                                    "Identifier(?)".into(),
                                    p0,
                                ))
                            }
                        }
                    }
                    Ok(Type::Function(Box::new(Type::Unit), param_type))
                }
                name => Ok(Type::Alias(name.into()))
            },
            Token::BracketLeft => {
                self.parse_special_token(Token::BracketLeft)?;
                let ty = self.parse_type()?;
                self.parse_special_token(Token::BracketRight)?;
                return Ok(Type::Array(Box::new(ty)));
            }
            _ => Err(Error::UnexpectedToken(
                token.to_string(),
                "Identifier(?)".into(),
                p0,
            )),
        }
    }
    pub fn parse_type(&mut self) -> Result<Type> {
        let ty = self.parse_simple_type()?;
        let mut list = vec![];
        if self.try_parse_token(Token::Less){
            let t0 = self.parse_type()?;
            list.push(t0);
            let _ = self.parse_special_token(Token::Greater);
        }
        Ok(Type::Generic(Box::new(ty), list))
    }

    pub fn parse_type_name(&mut self) -> Result<(String, Position)> {
        let (token, p0) = self.token_stream.next();
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
                "Identifier(?)".into(),
                p0,
            )),
        }
    }
}
// use std::{env, fs};
//
// use crate::error::{PipelineError, PipelineResult};
// use crate::expr::Expr::{Binary, FnCall};
// use crate::expr::{Argument, Expr, FnCallExpr, FnClosureExpr, Op};
// use crate::lexer::{Lexer, TokenStream};
// use crate::module::{Class, FnDef, Function, Module, VariableDeclaration};
// use crate::position::Position;
// use crate::stmt::{IfBranchStmt, IfStmt, Stmt};
// use crate::token::Token;
// use std::sync::{Arc, RwLock};
//
// pub struct PipelineParser {
//     token_stream: TokenStream,
//     #[allow(unused)]
//     module: Arc<RwLock<Module>>,
// }
// impl PipelineParser {
//     pub fn new(lexer: Lexer, module: Arc<RwLock<Module>>) -> Self {
//         Self {
//             module,
//             token_stream: TokenStream::new(lexer),
//         }
//     }
//     pub fn get_module(&self) -> Arc<RwLock<Module>> {
//         self.module.clone()
//     }
//     pub fn parse_stmt_blocks(&mut self) -> PipelineResult<Vec<Stmt>> {
//         let mut v = vec![];
//         loop {
//             let stmt = self.parse_stmt()?;
//             if stmt.is_noop() {
//                 break;
//             }
//             v.push(stmt)
//         }
//         Ok(v)
//     }
//
//     pub fn parse_stmt(&mut self) -> PipelineResult<Stmt> {
//         loop {
//             let (token, pos) = self.token_stream.peek();
//             return match token {
//                 Token::Keyword(k) => match k.as_str() {
//                     "var" => return self.parse_var_stmt(),
//                     "val" => return self.parse_val_stmt(),
//                     "fun" => {
//                         self.parse_function()?;
//                         continue;
//                     }
//                     "return" => self.parse_return_stmt(),
//                     "if" => self.parse_if_stmt(),
//                     "while" => self.parse_while_stmt(),
//                     "import" => self.parse_import_stmt(),
//                     "break" => {
//                         self.token_stream.next();
//                         Ok(Stmt::Break(pos))
//                     }
//                     "continue" => {
//                         self.token_stream.next();
//                         Ok(Stmt::Continue(pos))
//                     }
//                     "for" => self.parse_for_loop(),
//                     "class" => {
//                         self.parse_class()?;
//                         continue;
//                     }
//                     "struct"=>{
//                         self.parse_struct()?;
//                         continue;
//                     }
//                     "trait" => {
//                         self.parse_trait()?;
//                         continue;
//                     }
//                     t => Err(PipelineError::UnusedKeyword(t.into(), pos)),
//                 },
//                 Token::Annotation => {
//                     self.token_stream.next();
//                     let _ = self.parse_identifier()?;
//                     let _ = self.parse_function();
//                     continue;
//                 }
//                 Token::Eof => Ok(Stmt::Noop),
//                 Token::ParenthesisRight => Ok(Stmt::Noop),
//                 _ => self.parse_expr_stmt(),
//             };
//         }
//     }
//     ///
//     /// struct <STRUCT_NAME>{
//     ///     <FIELD_NAME>:<TYPE>
//     ///     ...
//     /// }
//     pub fn parse_struct(&mut self) -> PipelineResult<()> {
//         self.parse_keyword("struct")?;
//         self.parse_identifier()?;
//         self.parse_special_token(Token::ParenthesisLeft)?;
//
//          loop {
//             let (peek, pos) = self.token_stream.peek();
//             match peek {
//                 Token::ParenthesisRight => {
//
//                 }
//             }
//          }
//     }
//     pub fn parse_trait(&mut self) -> PipelineResult<()> {
//         self.parse_keyword("trait")?;
//         self.parse_identifier()?;
//         self.parse_special_token(Token::ParenthesisLeft)?;
//         loop {
//             let (peek, pos) = self.token_stream.peek();
//             match peek {
//                 Token::Keyword(keyword) => {
//                     if keyword.eq("fun") {
//                         self.token_stream.next();
//                         self.parse_identifier()?;
//                         self.parse_fn_def_args()?;
//                         if let Token::Colon = self.token_stream.peek().0 {
//                             self.token_stream.next();
//                             self.parse_identifier()?;
//                         }
//                     } else {
//                         return Err(PipelineError::UnusedKeyword(keyword, pos));
//                     }
//                 }
//                 Token::ParenthesisRight => break,
//                 t => {
//                     return Err(PipelineError::UnexpectedToken(
//                         t.to_string(),
//                         "Keyword(fun)".into(),
//                         pos,
//                     ))
//                 }
//             }
//         }
//         self.parse_special_token(Token::ParenthesisRight)?;
//         Ok(())
//     }
//     pub fn parse_class(&mut self) -> PipelineResult<()> {
//         self.parse_keyword("class")?;
//         let (class_name, class_name_pos) = self.parse_identifier()?;
//         let mut pos = class_name_pos.clone();
//         self.parse_special_token(Token::BraceLeft)?;
//         // 解析构造函数参数也即类属性声明
//         let mut attributions = vec![];
//         loop {
//             let (peek_token, _) = self.token_stream.peek();
//             match peek_token {
//                 Token::BraceRight => break,
//                 Token::Comma => {
//                     self.token_stream.next();
//                     pos.add_span(1)
//                 }
//                 _ => {
//                     let (attribution_name, attribution_name_pos) = self.parse_identifier()?;
//                     self.parse_special_token(Token::Colon)?;
//                     let (attribution_type, attribution_type_pos) = self.parse_identifier()?;
//                     pos.add_span(attribution_name_pos.span + attribution_type_pos.span + 1);
//                     if let Token::Assign = self.token_stream.peek().0 {
//                         self.token_stream.next();
//                         let e = self.parse_expr()?;
//                         attributions.push(VariableDeclaration::with_default(
//                             attribution_name,
//                             attribution_type,
//                             e,
//                         ));
//                     } else {
//                         attributions
//                             .push(VariableDeclaration::new(attribution_name, attribution_type));
//                     }
//                 }
//             }
//         }
//         self.parse_special_token(Token::BraceRight)?;
//         let mut class_declaration = Class::new(class_name.as_str(), attributions);
//         if self.token_stream.peek().0.is_parenthesis_left() {
//             self.parse_special_token(Token::ParenthesisLeft)?;
//             while self.token_stream.peek().0.is_keyword("fun")
//                 || self.token_stream.peek().0.is_keyword("static")
//             {
//                 let peek = self.token_stream.peek().0;
//                 if peek.is_keyword("fun") {
//                     self.parse_keyword("fun")?;
//                     let fn_def = self.parse_fn_def()?;
//                     let method = Function::Method(Box::new(fn_def.clone()));
//                     class_declaration.register_method(fn_def.name.clone(), method);
//                 } else {
//                     self.parse_keyword("static")?;
//                     if let Token::ParenthesisLeft = self.token_stream.peek().0 {
//                         self.parse_special_token(Token::ParenthesisLeft)?;
//                         while self.token_stream.peek().0.is_keyword("fun") {
//                             self.parse_keyword("fun")?;
//                             let fn_def = self.parse_fn_def()?;
//                             let method = Function::Script(Box::new(fn_def.clone()));
//                             class_declaration.register_static_method(fn_def.name.clone(), method);
//                         }
//                         self.parse_special_token(Token::ParenthesisRight)?;
//                     }
//                 }
//             }
//             self.parse_special_token(Token::ParenthesisRight)?;
//         }
//         self.module
//             .write()
//             .unwrap()
//             .register_class(class_declaration);
//         Ok(())
//     }
//     #[allow(unused)]
//     fn parse_keyword(&mut self, target: &str) -> PipelineResult<(String, Position)> {
//         let (next, pos) = self.token_stream.next();
//         if let Token::Keyword(keyword) = next {
//             return Ok((keyword, pos));
//         }
//         Err(PipelineError::UnexpectedToken(
//             next.to_string(),
//             Token::Keyword(target.into()).to_string(),
//             pos,
//         ))
//     }
//     fn parse_identifier(&mut self) -> PipelineResult<(String, Position)> {
//         let (next, pos) = self.token_stream.next();
//         if let Token::Identifier(identifier) = next {
//             return Ok((identifier, pos));
//         }
//         Err(PipelineError::UnexpectedToken(
//             next.to_string(),
//             "Identifier(?)".into(),
//             pos,
//         ))
//     }
//     pub fn parse_for_loop(&mut self) -> PipelineResult<Stmt> {
//         let (ret, mut pos) = self.token_stream.next();
//         if let Token::Keyword(s) = ret.clone() {
//             if s != "for" {
//                 return Err(PipelineError::UnusedKeyword(s, pos));
//             }
//             self.parse_special_token(Token::BraceLeft)?;
//             let (one, pos1) = self.token_stream.next();
//             let (peek, _) = self.token_stream.peek();
//             let mut other = None;
//             if let Token::Comma = peek {
//                 self.token_stream.next();
//                 let (two, _) = self.token_stream.next();
//                 other = Some(two.get_identifier_value().to_string())
//             }
//             let (in_token, pos2) = self.token_stream.next();
//             if let Token::Keyword(s0) = in_token {
//                 if s0 != "in" {
//                     return Err(PipelineError::UnusedKeyword(s, pos));
//                 }
//                 let expr = self.parse_expr()?;
//                 self.parse_special_token(Token::BraceRight)?;
//                 self.parse_special_token(Token::ParenthesisLeft)?;
//                 let blocks = self.parse_stmt_blocks()?;
//                 self.parse_special_token(Token::ParenthesisRight)?;
//                 pos.add_span(pos1.span + pos2.span + expr.position().span + 2);
//                 for p in &blocks {
//                     pos.add_span(p.position().span);
//                 }
//                 return Ok(Stmt::ForIn(
//                     one.get_identifier_value().to_string(),
//                     other,
//                     Box::new(expr),
//                     blocks,
//                     pos,
//                 ));
//             }
//         }
//         Err(PipelineError::UnexpectedToken(
//             ret.to_string(),
//             "Keyword(for)".into(),
//             pos,
//         ))
//     }
//     pub fn parse_module(&mut self, module_name: impl AsRef<str>) -> PipelineResult<Option<Module>> {
//         let mut current_dir = match env::current_dir() {
//             Ok(path) => path,
//             Err(_) => {
//                 return Ok(None);
//             }
//         };
//         #[allow(unused_assignments)]
//         let mut script = String::new();
//         current_dir.push(format!("{}.ppl", module_name.as_ref()));
//         if current_dir.exists() {
//             script = fs::read_to_string(current_dir).unwrap();
//         } else {
//             let home_dir = dirs::home_dir().expect("无法获取用户根目录");
//             let file_path = home_dir
//                 .join(".pipeline/package")
//                 .join(format!("{}.ppl", module_name.as_ref()));
//             let read_result = fs::read_to_string(file_path);
//             match read_result {
//                 Ok(r) => {
//                     script = r;
//                 }
//                 Err(_) => return Ok(None),
//             }
//         }
//         // 打印当前工作目录
//         let id = module_name.as_ref();
//
//         let lexer = Lexer::from_script(id, script);
//         let mut m = Module::new(id);
//         m.register_class(Class::new("Array", vec![]));
//         m.register_class(Class::new("Int", vec![]));
//         let mut parser = PipelineParser::new(lexer, Arc::new(RwLock::new(m)));
//         parser.parse_stmt_blocks()?;
//         let m0 = parser.get_module().read().unwrap().clone();
//         Ok(Some(m0))
//     }
//     pub fn parse_import_stmt(&mut self) -> PipelineResult<Stmt> {
//         let (ret, mut pos) = self.token_stream.next();
//         if let Token::Keyword(s) = ret.clone() {
//             if s != "import" {
//                 return Err(PipelineError::UnusedKeyword(s, pos));
//             }
//             let (next, pos1) = self.token_stream.next();
//             return match next {
//                 Token::Identifier(id) => {
//                     pos += pos1;
//                     let m = self.parse_module(id.clone())?;
//                     if let Some(m) = m {
//                         self.module
//                             .write()
//                             .unwrap()
//                             .register_submodule(m.get_name(), m);
//                     }
//                     Ok(Stmt::Import(id, pos))
//                 }
//                 t => Err(PipelineError::UnexpectedToken(
//                     t.to_string(),
//                     "Identifier(?)".into(),
//                     pos1,
//                 )),
//             };
//         }
//         Err(PipelineError::UnexpectedToken(
//             ret.to_string(),
//             "Keyword(import)".into(),
//             pos,
//         ))
//     }
//     fn parse_if_branch(&mut self) -> PipelineResult<(IfBranchStmt, Position)> {
//         let (ret, mut pos) = self.token_stream.next();
//         if let Token::Keyword(s) = ret.clone() {
//             if s != "if" {
//                 return Err(PipelineError::UnusedKeyword(s, pos));
//             }
//             let (_, p1) = self.parse_special_token(Token::BraceLeft)?;
//             pos += p1;
//             let expr = self.parse_expr()?;
//             let (_, p1) = self.parse_special_token(Token::BraceRight)?;
//             pos += p1;
//             pos += expr.position();
//             self.parse_special_token(Token::ParenthesisLeft)?;
//             let blocks = self.parse_stmt_blocks()?;
//             self.parse_special_token(Token::ParenthesisRight)?;
//             for i in &blocks {
//                 pos.add_span(i.position().span)
//             }
//             return Ok((IfBranchStmt::new(expr, blocks), pos));
//         }
//         Err(PipelineError::UnexpectedToken(
//             ret.to_string(),
//             "Keyword(if)".into(),
//             pos,
//         ))
//     }
//
//     pub fn parse_if_stmt(&mut self) -> PipelineResult<Stmt> {
//         let mut branches = vec![];
//         let mut else_body = None;
//         let (b, pos) = self.parse_if_branch()?;
//         branches.push(b);
//
//         loop {
//             let (peek, _) = self.token_stream.peek();
//             match peek.clone() {
//                 Token::Keyword(k) if k == "else" => {
//                     self.token_stream.next();
//                     let (peek0, _) = self.token_stream.peek();
//                     if let Token::ParenthesisLeft = peek0 {
//                         self.parse_special_token(Token::ParenthesisLeft)?;
//                         let blocks = self.parse_stmt_blocks()?;
//                         self.parse_special_token(Token::ParenthesisRight)?;
//                         else_body = Some(blocks);
//                         break;
//                     }
//                     let (b0, _) = self.parse_if_branch()?;
//                     branches.push(b0);
//                 }
//                 _ => break,
//             }
//         }
//         Ok(Stmt::If(Box::new(IfStmt::new(branches, else_body)), pos))
//     }
//     pub fn parse_while_stmt(&mut self) -> PipelineResult<Stmt> {
//         let (ret, mut pos) = self.token_stream.next();
//         if let Token::Keyword(s) = ret.clone() {
//             if s != "while" {
//                 return Err(PipelineError::UnusedKeyword(s, pos));
//             }
//             let expr = self.parse_expr()?;
//             pos.add_span(expr.position().span);
//             self.parse_special_token(Token::ParenthesisLeft)?;
//             let blocks = self.parse_stmt_blocks()?;
//             self.parse_special_token(Token::ParenthesisRight)?;
//             for i in &blocks {
//                 pos.add_span(i.position().span)
//             }
//             return Ok(Stmt::While(Box::new(expr), blocks, pos));
//         }
//         Err(PipelineError::UnexpectedToken(
//             ret.to_string(),
//             "Keyword(while)".into(),
//             pos,
//         ))
//     }
//     pub fn parse_return_stmt(&mut self) -> PipelineResult<Stmt> {
//         let (ret, mut pos) = self.token_stream.next();
//         if let Token::Keyword(s) = ret.clone() {
//             if s != "return" {
//                 return Err(PipelineError::UnusedKeyword(s, pos));
//             }
//             let peek = self.token_stream.peek();
//
//             let mut expr=match peek.0 {
//                 Token::Identifier(_)|Token::String(_)|Token::Int(_)|Token::Float(_)=> self.parse_expr()?,
//                 _=>Expr::None(Position::none())
//             };
//
//             pos.add_span(expr.position().span);
//             return Ok(Stmt::Return(Box::new(expr), pos));
//         }
//         Err(PipelineError::UnexpectedToken(
//             ret.to_string(),
//             "Keyword(return)".into(),
//             pos,
//         ))
//     }
//     fn parse_var_stmt(&mut self) -> PipelineResult<Stmt> {
//         let (token, mut pos) = self.token_stream.next();
//         if let Token::Keyword(_) = token {
//             let (token1, pos0) = self.token_stream.next();
//             if let Token::Identifier(ident) = token1 {
//                 pos += pos0;
//                 if let Token::Colon = self.token_stream.peek().0 {
//                     self.parse_special_token(Token::Colon)?;
//                     self.parse_type_comment()?;
//                 }
//                 self.parse_special_token(Token::Assign)?;
//                 pos.add_span(1);
//                 let expr = self.parse_expr()?;
//                 pos.add_span(expr.position().span);
//                 return Ok(Stmt::Var(Box::new((ident, expr)), pos));
//             }
//             return Err(PipelineError::UnexpectedToken(
//                 token1.to_string(),
//                 "Identifier(?)".into(),
//                 pos0,
//             ));
//         }
//         Err(PipelineError::UnexpectedToken(
//             token.to_string(),
//             "Keyword(var)".into(),
//             pos,
//         ))
//     }
//     fn parse_type_comment(&mut self) -> PipelineResult<()> {
//         self.parse_identifier()?;
//         if let Token::Less = self.token_stream.peek().0 {
//             self.parse_special_token(Token::Less)?;
//             self.parse_type_comment()?;
//             self.parse_special_token(Token::Greater)?;
//         }
//         if let Token::BitOr = self.token_stream.peek().0 {
//             self.parse_special_token(Token::BitOr)?;
//             self.parse_type_comment()?;
//         }
//         Ok(())
//     }
//     fn parse_val_stmt(&mut self) -> PipelineResult<Stmt> {
//         let (token, mut pos) = self.token_stream.next();
//         if let Token::Keyword(_) = token {
//             let (token1, pos0) = self.token_stream.next();
//             if let Token::Identifier(ident) = token1 {
//                 pos += pos0;
//                 if let Token::Colon = self.token_stream.peek().0 {
//                     self.parse_special_token(Token::Colon)?;
//                     self.parse_type_comment()?;
//                 }
//                 self.parse_special_token(Token::Assign)?;
//                 pos.add_span(1);
//                 let expr = self.parse_expr()?;
//                 pos.add_span(expr.position().span);
//                 return Ok(Stmt::Val(Box::new((ident, expr)), pos));
//             }
//             return Err(PipelineError::UnexpectedToken(
//                 token1.to_string(),
//                 "Identifier(?)".into(),
//                 pos0,
//             ));
//         }
//         Err(PipelineError::UnexpectedToken(
//             token.to_string(),
//             "Keyword(val)".into(),
//             pos,
//         ))
//     }
//     fn parse_fn_def(&mut self) -> PipelineResult<FnDef> {
//         let (function_name, _) = self.parse_identifier()?;
//         let (function_params, _) = self.parse_fn_def_args()?;
//         let mut return_type = String::from("Unit");
//         if self.token_stream.peek().0.is_colon() {
//             self.parse_special_token(Token::Colon)?;
//             (return_type, _) = self.parse_identifier()?;
//         }
//         let mut stmts = vec![];
//         if self.token_stream.peek().0.is_assign() {
//             self.parse_special_token(Token::Assign)?;
//             let expr = self.parse_expr()?;
//             let pos = expr.position();
//             let stmt = Stmt::Return(Box::new(expr), pos);
//             stmts.push(stmt);
//         } else if self.token_stream.peek().0.is_parenthesis_left() {
//             self.parse_special_token(Token::ParenthesisLeft)?;
//             stmts = self.parse_stmt_blocks()?;
//             self.parse_special_token(Token::ParenthesisRight)?;
//         }
//         Ok(FnDef::new(
//             function_name.clone(),
//             function_params,
//             stmts,
//             return_type,
//         ))
//     }
//     pub fn parse_function(&mut self) -> PipelineResult<()> {
//         self.parse_keyword("fun")?;
//         let (one_name, _) = self.parse_identifier()?;
//         let b = self.try_parse_special_token(Token::Dot);
//         if b {
//             let fn_def = self.parse_fn_def()?;
//             let function = Function::Method(Box::new(fn_def.clone()));
//             self.module
//                 .write()
//                 .unwrap()
//                 .register_class_method(one_name, fn_def.name, function);
//             return Ok(());
//         }
//         let (function_params, _) = self.parse_fn_def_args()?;
//         let mut return_type = String::from("Unit");
//         if self.token_stream.peek().0.is_colon() {
//             self.parse_special_token(Token::Colon)?;
//             (return_type, _) = self.parse_identifier()?;
//         }
//         let mut stmts = vec![];
//         if self.token_stream.peek().0.is_assign() {
//             self.parse_special_token(Token::Assign)?;
//             let expr = self.parse_expr()?;
//             let pos = expr.position();
//             let stmt = Stmt::Return(Box::new(expr), pos);
//             stmts.push(stmt);
//         } else if self.token_stream.peek().0.is_parenthesis_left() {
//             self.parse_special_token(Token::ParenthesisLeft)?;
//             stmts = self.parse_stmt_blocks()?;
//             self.parse_special_token(Token::ParenthesisRight)?;
//         }
//         let function_def = FnDef::new(one_name.clone(), function_params, stmts, return_type);
//         self.module
//             .write()
//             .unwrap()
//             .register_script_function(function_def.name.clone(), function_def);
//         Ok(())
//     }
//     pub fn try_parse_special_token(&mut self, target: Token) -> bool {
//         let (token, _) = self.token_stream.peek();
//         match token {
//             t if t.token_id() == target.token_id() => {
//                 self.token_stream.next();
//                 true
//             }
//             _ => false,
//         }
//     }
//     pub fn parse_fn_def_args(&mut self) -> PipelineResult<(Vec<VariableDeclaration>, Position)> {
//         let start = self.parse_special_token(Token::BraceLeft)?;
//         let mut v = vec![];
//         let mut p = start.1;
//         loop {
//             let (token, pos) = self.token_stream.peek();
//             match token {
//                 Token::BraceRight => break,
//                 Token::Comma => {
//                     self.token_stream.next();
//                     p.add_span(1);
//                     continue;
//                 }
//                 Token::Identifier(_) => {
//                     let (dec, pos) = self.parse_variable_declaration()?;
//                     v.push(dec);
//                     p.add_span(pos.span)
//                 }
//                 Token::Assign => {
//                     self.token_stream.next();
//                     let e = self.parse_expr()?;
//                     v.last_mut().unwrap().set_default(e);
//                 }
//                 t => {
//                     return Err(PipelineError::UnexpectedToken(
//                         t.to_string(),
//                         "Symbol())|Symbol(,)".into(),
//                         pos,
//                     ))
//                 }
//             }
//         }
//         self.parse_special_token(Token::BraceRight)?;
//         p.add_span(1);
//         Ok((v, p))
//     }
//     pub fn parse_variable_declaration(
//         &mut self,
//     ) -> PipelineResult<(VariableDeclaration, Position)> {
//         let (next, mut pos) = self.token_stream.next();
//         if let Token::Identifier(s) = next.clone() {
//             self.parse_special_token(Token::Colon)?;
//             let (next1, pos1) = self.token_stream.next();
//             if let Token::Identifier(s1) = next1 {
//                 pos.add_span(1 + pos1.span);
//                 return Ok((VariableDeclaration::new(s, s1), pos));
//             }
//             return Err(PipelineError::UnexpectedToken(
//                 next1.to_string(),
//                 "Identifier(?)".into(),
//                 pos1,
//             ));
//         }
//         Err(PipelineError::UnexpectedToken(
//             next.to_string(),
//             "Identifier(?)".into(),
//             pos,
//         ))
//     }
//
//     pub fn parse_expr_stmt(&mut self) -> PipelineResult<Stmt> {
//         let lhs = self.parse_expr_call_chain()?;
//         let (token, mut pos0) = self.token_stream.peek();
//         Ok(match token {
//             Token::Assign => {
//                 self.token_stream.next();
//                 let expr = self.parse_expr()?;
//                 pos0.add_span(expr.position().span);
//                 if let Expr::Index(target, index, mut pos1) = lhs {
//                     pos1.add_span(pos0.span);
//                     return Ok(Stmt::IndexAssign(target, index, Box::new(expr), pos1));
//                 }
//                 return Ok(Stmt::Assign(Box::new((lhs.clone(), expr)), pos0));
//             }
//             Token::ParenthesisLeft => {
//                 let mut v = vec![];
//                 let (peek, mut pos1) = self.token_stream.peek();
//                 if Token::ParenthesisLeft == peek {
//                     self.parse_special_token(Token::ParenthesisLeft).unwrap();
//                     let blocks = self.parse_stmt_blocks()?;
//                     for stmt in &blocks {
//                         pos1.add_span(stmt.position().span)
//                     }
//                     let fn_def = FnDef::new("".to_string(), vec![], blocks, "Any".into());
//                     pos1.add_span(1);
//                     pos1.add_span(pos1.span);
//                     v.push(Argument::new(Expr::FnClosure(
//                         FnClosureExpr {
//                             def: fn_def,
//                             is_outside: true,
//                         },
//                         pos1.clone(),
//                     )));
//                     self.parse_special_token(Token::ParenthesisRight).unwrap();
//                 }
//                 let mut fn_call_expr = FnCallExpr {
//                     name: "".into(),
//                     args: vec![],
//                 };
//                 match lhs {
//                     // a()格式
//                     Expr::Variable(s, _) => {
//                         fn_call_expr.name = s;
//                     }
//                     // a[index]()格式，b指index,n指a
//                     Expr::MemberAccess(b, n, _) => {
//                         fn_call_expr.name = n;
//                         v.insert(0, Argument::new(*b))
//                     }
//                     _ => panic!("only variable and member_access expected"),
//                 }
//                 fn_call_expr.args = v;
//
//                 Stmt::EvalExpr(Box::new(Expr::FnCall(fn_call_expr, pos1.clone())), pos1)
//                 // return  Ok(Stmt::EvalExpr(Box::new(
//                 //     Expr::FnCall(FnCallExpr{
//                 //     name: "".to_string(),
//                 //     args: vec![],
//                 // },pos1.clone())), pos1))
//             }
//             Token::BraceLeft => {
//                 let (mut args, pos) = self.parse_fn_call_args()?;
//                 let mut fn_call_expr = FnCallExpr {
//                     name: "".into(),
//                     args: vec![],
//                 };
//                 match lhs {
//                     // a()格式
//                     Expr::Variable(s, _) => {
//                         fn_call_expr.name = s;
//                     }
//                     // a[index]()格式，b指index,n指a
//                     Expr::MemberAccess(b, n, _) => {
//                         fn_call_expr.name = n;
//                         args.insert(0, Argument::new(*b))
//                     }
//                     _ => panic!("only variable and member_access expected"),
//                 }
//                 fn_call_expr.args = args;
//
//                 Stmt::EvalExpr(Box::new(Expr::FnCall(fn_call_expr, pos.clone())), pos)
//             }
//             _ => {
//                 let p = lhs.position();
//                 return Ok(Stmt::EvalExpr(Box::new(lhs), p));
//             }
//         })
//     }
//     /// 返回值中第一个Vec<Argument>是指所有的位置参数+具名参数，位置参数在前，具名参数在后
//     pub fn parse_fn_call_args(&mut self) -> PipelineResult<(Vec<Argument>, Position)> {
//         let mut v = vec![];
//         let mut has_name = false;
//
//         let (_, mut p) = self.parse_special_token(Token::BraceLeft)?;
//         loop {
//             let (peek, p0) = self.token_stream.peek();
//             if peek == Token::BraceRight {
//                 p += p0;
//                 self.token_stream.next();
//                 break;
//             }
//             let expr = self.parse_expr()?;
//             let expr_pos = expr.position();
//             p += expr_pos;
//             let (token, pos) = self.token_stream.peek();
//             match token {
//                 Token::BraceRight => {
//                     if has_name {
//                         panic!("具名参数后不能接位置参数");
//                     }
//                     v.push(Argument::new(expr));
//                     p += pos;
//                     self.token_stream.next();
//                     break;
//                 }
//                 Token::Comma => {
//                     v.push(Argument::new(expr));
//                     self.token_stream.next();
//                 }
//                 Token::Assign => {
//                     if !has_name {
//                         has_name = true;
//                     }
//                     self.token_stream.next();
//                     let expr1 = self.parse_expr()?;
//                     let ident = expr.try_get_variable_name().unwrap();
//                     v.push(Argument::with_name(ident, expr1));
//                     if let Token::Comma = self.token_stream.peek().0 {
//                         self.token_stream.next();
//                     }
//                 }
//                 _ => {
//                     return Err(PipelineError::UnexpectedToken(
//                         token.to_string(),
//                         "Symbol(=)|Symbol(,)|Symbol())".into(),
//                         pos,
//                     ))
//                 }
//             }
//         }
//
//         let (peek, mut pos1) = self.token_stream.peek();
//         if Token::ParenthesisLeft == peek {
//             self.parse_special_token(Token::ParenthesisLeft).unwrap();
//             let blocks = self.parse_stmt_blocks()?;
//             for stmt in &blocks {
//                 pos1.add_span(stmt.position().span)
//             }
//             let fn_def = FnDef::new("".to_string(), vec![], blocks, "Any".into());
//             pos1.add_span(1);
//             p.add_span(pos1.span);
//             v.push(Argument::new(Expr::FnClosure(
//                 FnClosureExpr {
//                     def: fn_def,
//                     is_outside: true,
//                 },
//                 pos1,
//             )));
//             self.parse_special_token(Token::ParenthesisRight).unwrap();
//         }
//
//         Ok((v, p))
//     }
//     /// 解析基础表达式，不包含运算符，比如"Hello",1,1.5,a
//     fn parse_primary(&mut self) -> PipelineResult<Expr> {
//         let (token, mut pos) = self.token_stream.peek();
//         match token {
//             Token::String(s) => {
//                 self.token_stream.next();
//                 Ok(Expr::StringConstant(s, pos))
//             }
//             Token::Int(i) => {
//                 self.token_stream.next();
//                 Ok(Expr::IntConstant(i, pos))
//             }
//             Token::Float(f) => {
//                 self.token_stream.next();
//                 Ok(Expr::FloatConstant(f, pos))
//             }
//             Token::Negate => {
//                 self.token_stream.next();
//                 let expr = self.parse_expr()?;
//                 pos += expr.position();
//                 Ok(Expr::Unary(Op::Negate, Box::new(expr), pos))
//             }
//             //  解析变量，可以包含 bar::a 类型和a[0]类型
//             Token::Identifier(ident) => {
//                 self.token_stream.next();
//                 let (peek, mut pos1) = self.token_stream.peek();
//                 match peek {
//                     // bar::a类型
//                     Token::ScopeSymbol => {
//                         self.token_stream.next();
//                         // 获取::后部分标识符a
//                         let (next, pos2) = self.token_stream.next();
//                         let fc_name = next.get_identifier_value();
//                         let (args, pos3) = self.parse_fn_call_args().unwrap();
//                         let name = ident + "::" + fc_name;
//                         let mut p = pos.clone();
//                         p.add_span(pos2.span + 2);
//                         let fn_expr = FnCallExpr { name, args };
//                         pos.add_span(pos2.span + pos3.span + 2);
//                         Ok(Expr::FnCall(fn_expr, pos))
//                     }
//                     // a[index]类型
//                     Token::SquareBracketLeft => {
//                         self.token_stream.next();
//                         // 解析下标值index
//                         let e = self.parse_math_expr()?;
//                         // 解析 ] 右终结符
//                         self.parse_special_token(Token::SquareBracketRight)?;
//                         pos1.add_span(1 + e.position().span + 1);
//                         Ok(Expr::Index(
//                             Box::new(Expr::Variable(ident, pos)),
//                             Box::new(e),
//                             pos1,
//                         ))
//                     }
//                     _ => Ok(Expr::Variable(ident, pos)),
//                 }
//             }
//             Token::BraceLeft => {
//                 let (vd, mut vd_pos) = self.parse_fn_def_args()?;
//                 let (_, arrow_pos) = self.parse_special_token(Token::Arrow)?;
//                 vd_pos += arrow_pos;
//                 if let Token::ParenthesisLeft = self.token_stream.peek().0 {
//                     vd_pos += self.token_stream.peek().1;
//                     self.token_stream.next();
//                     let blocks = self.parse_stmt_blocks()?;
//                     let (_, right_pos) = self.parse_special_token(Token::ParenthesisRight)?;
//                     vd_pos += right_pos;
//                     let fn_def = FnDef {
//                         name: "".to_string(),
//                         return_type: "Unit".to_string(),
//                         args: vd,
//                         body: blocks,
//                     };
//                     Ok(Expr::FnClosure(
//                         FnClosureExpr {
//                             def: fn_def,
//                             is_outside: false,
//                         },
//                         vd_pos,
//                     ))
//                 } else {
//                     let e = self.parse_expr()?;
//                     let pos = e.position();
//                     vd_pos += pos.clone();
//                     let fn_def = FnDef {
//                         name: "".to_string(),
//                         return_type: "Unit".to_string(),
//                         args: vd,
//                         body: vec![Stmt::EvalExpr(Box::new(e), pos)],
//                     };
//                     Ok(Expr::FnClosure(
//                         FnClosureExpr {
//                             def: fn_def,
//                             is_outside: false,
//                         },
//                         vd_pos,
//                     ))
//                 }
//             }
//             _ => Err(PipelineError::UnexpectedToken(
//                 token.to_string(),
//                 "String(?)|Int(?)|Float(?)|Identifier(?)".into(),
//                 pos,
//             )),
//         }
//     }
//
//     fn parse_expr_call_chain(&mut self) -> PipelineResult<Expr> {
//         // 解析 "Hello",1,1.23,a,a[index]等类型
//         let mut lhs = self.parse_primary()?;
//         // 尝试解析链式调用如 a[index],a[index][index]..,a(),a()(),a[index](),a[index].b()
//         loop {
//             let (peek, _) = self.token_stream.peek();
//             match peek {
//                 //解析a.b().c()函数调用
//                 Token::Dot => {
//                     self.token_stream.next();
//                     let (next, pos0) = self.token_stream.next();
//                     let name = next.get_identifier_value();
//                     let (peek1, _) = self.token_stream.peek();
//                     if let Token::BraceLeft = peek1 {
//                         let (args, pos1) = self.parse_fn_call_args()?;
//                         let mut fn_call = FnCallExpr {
//                             name: name.into(),
//                             args,
//                         };
//                         // a.b() 中b实际上是个成员函数成员函数的声明是b(self,params)实际上会将调用者作为第一个参数进行特殊处理
//                         // a.b()最后会转变成b(a)的形式
//                         let mut p = lhs.position();
//                         p.add_span(1 + pos0.span + pos1.span);
//                         fn_call.args.insert(0, Argument::new(lhs));
//                         let expr = FnCall(fn_call, p);
//                         lhs = expr;
//                     } else {
//                         let mut pos00 = lhs.position();
//                         pos00.add_span(1 + pos0.span);
//                         let member_access =
//                             Expr::MemberAccess(Box::new(lhs.clone()), name.into(), pos00);
//                         lhs = member_access;
//                     }
//                 }
//                 Token::BraceLeft => {
//                     let (mut args, pos) = self.parse_fn_call_args()?;
//                     let mut fn_call_expr = FnCallExpr {
//                         name: "".into(),
//                         args: vec![],
//                     };
//                     match lhs.clone() {
//                         Expr::Variable(s, _) => {
//                             fn_call_expr.name = s;
//                         }
//                         Expr::MemberAccess(b, n, _) => {
//                             fn_call_expr.name = n;
//                             args.insert(0, Argument::new(*b))
//                         }
//                         _ => panic!("only variable and member_access expected"),
//                     }
//                     fn_call_expr.args = args;
//                     lhs = Expr::FnCall(fn_call_expr, lhs.position() + pos)
//                 }
//                 Token::SquareBracketLeft => {
//                     let mut pos1 = lhs.position();
//                     self.token_stream.next();
//                     let e = self.parse_math_expr()?;
//                     self.parse_special_token(Token::SquareBracketRight)?;
//                     pos1.add_span(1 + e.position().span + 1);
//                     lhs = Expr::Index(Box::new(lhs), Box::new(e), pos1)
//                 }
//                 _ => return Ok(lhs),
//             }
//         }
//     }
//
//     fn parse_term(&mut self) -> PipelineResult<Expr> {
//         let lhs = self.parse_expr_call_chain()?;
//         let (token, mut pos) = self.token_stream.peek();
//         match token {
//             Token::Mul => {
//                 self.token_stream.next();
//                 let right = self.parse_term()?;
//                 pos.add_span(1 + right.position().span + lhs.position().span);
//                 Ok(Binary(Op::Mul, Box::new(lhs), Box::new(right), pos))
//             }
//             Token::Div => {
//                 self.token_stream.next();
//                 let right = self.parse_term()?;
//                 pos.add_span(1 + right.position().span + lhs.position().span);
//                 Ok(Binary(Op::Div, Box::new(lhs), Box::new(right), pos))
//             }
//             Token::Mod => {
//                 self.token_stream.next();
//                 let right = self.parse_term()?;
//                 pos.add_span(1 + right.position().span + lhs.position().span);
//                 Ok(Binary(Op::Mod, Box::new(lhs), Box::new(right), pos))
//             }
//             _ => Ok(lhs),
//         }
//     }
//     fn parse_math_expr(&mut self) -> PipelineResult<Expr> {
//         let lhs = self.parse_term()?;
//         let next = self.token_stream.peek();
//         match next.0 {
//             Token::Plus => {
//                 self.token_stream.next();
//                 let mut pos = lhs.position();
//                 let rhs = self.parse_expr()?;
//                 pos.add_span(1 + rhs.position().span);
//                 Ok(Expr::Binary(Op::Plus, Box::new(lhs), Box::new(rhs), pos))
//             }
//             Token::Minus => {
//                 self.token_stream.next();
//                 let mut pos = lhs.position();
//                 let rhs = self.parse_expr()?;
//                 pos.add_span(1 + rhs.position().span);
//                 Ok(Expr::Binary(Op::Minus, Box::new(lhs), Box::new(rhs), pos))
//             }
//             _ => Ok(lhs),
//         }
//     }
//     fn parse_array(&mut self) -> PipelineResult<Expr> {
//         let (_, mut pos) = self.token_stream.next();
//         let mut v = vec![];
//         loop {
//             let (peek, _) = self.token_stream.peek();
//             match peek {
//                 Token::Comma => {
//                     self.token_stream.next();
//                     pos.add_span(1);
//                     continue;
//                 }
//                 Token::SquareBracketRight => {
//                     self.token_stream.next();
//                     pos.add_span(1);
//                     break;
//                 }
//                 _ => {
//                     let e = self.parse_expr()?;
//                     v.push(e.clone());
//                     pos.add_span(e.position().span);
//                 }
//             }
//         }
//         Ok(Expr::Array(v, pos))
//     }
//     pub fn parse_map(&mut self) -> PipelineResult<Expr> {
//         let (_, mut pos) = self.token_stream.next();
//         let mut v = vec![];
//         loop {
//             let (peek, _) = self.token_stream.peek();
//             match peek {
//                 Token::Comma => {
//                     self.token_stream.next();
//                     pos.add_span(1);
//                     continue;
//                 }
//                 Token::ParenthesisRight => {
//                     self.token_stream.next();
//                     pos.add_span(1);
//                     break;
//                 }
//                 _ => {
//                     let e = self.parse_expr()?;
//                     pos.add_span(e.position().span);
//                     self.parse_special_token(Token::Colon)?;
//                     let rhs = self.parse_expr()?;
//                     pos.add_span(rhs.position().span + 1);
//                     v.push((e.clone(), rhs.clone()));
//                 }
//             }
//         }
//         Ok(Expr::Map(v, pos))
//     }
//     pub fn parse_logical_expr(&mut self) -> PipelineResult<Expr> {
//         let (peek, _) = self.token_stream.peek();
//         if peek == Token::SquareBracketLeft {
//             return self.parse_array();
//         }
//         if peek == Token::ParenthesisLeft {
//             return self.parse_map();
//         }
//         let lhs = self.parse_math_expr()?;
//         let next = self.token_stream.peek();
//         match next.0 {
//             Token::Greater => {
//                 self.token_stream.next();
//                 let mut pos = lhs.position();
//                 let rhs = self.parse_math_expr()?;
//                 pos.add_span(1 + rhs.position().span);
//                 Ok(Expr::Binary(Op::Greater, Box::new(lhs), Box::new(rhs), pos))
//             }
//             Token::Less => {
//                 self.token_stream.next();
//                 let mut pos = lhs.position();
//                 let rhs = self.parse_math_expr()?;
//                 pos.add_span(1 + rhs.position().span);
//                 Ok(Expr::Binary(Op::Less, Box::new(lhs), Box::new(rhs), pos))
//             }
//             Token::Equal => {
//                 self.token_stream.next();
//                 let mut pos = lhs.position();
//                 let rhs = self.parse_math_expr()?;
//                 pos.add_span(1 + rhs.position().span);
//                 Ok(Expr::Binary(Op::Equal, Box::new(lhs), Box::new(rhs), pos))
//             }
//             Token::NotEqual => {
//                 self.token_stream.next();
//                 let mut pos = lhs.position();
//                 let rhs = self.parse_math_expr()?;
//                 pos.add_span(1 + rhs.position().span);
//                 Ok(Expr::Binary(
//                     Op::NotEqual,
//                     Box::new(lhs),
//                     Box::new(rhs),
//                     pos,
//                 ))
//             }
//             _ => Ok(lhs),
//         }
//     }
//     pub fn parse_expr(&mut self) -> PipelineResult<Expr> {
//         let expr = self.parse_logical_expr()?;
//         let (peek, pos) = self.token_stream.peek();
//         match peek {
//             Token::And => {
//                 self.token_stream.next();
//                 let rhs = self.parse_logical_expr()?;
//                 let pos0 = expr.position() + pos + rhs.position();
//                 Ok(Expr::Binary(Op::And, Box::new(expr), Box::new(rhs), pos0))
//             }
//             Token::Or => {
//                 self.token_stream.next();
//                 let rhs = self.parse_logical_expr()?;
//                 let pos0 = expr.position() + pos + rhs.position();
//                 Ok(Expr::Binary(Op::Or, Box::new(expr), Box::new(rhs), pos0))
//             }
//             _ => Ok(expr),
//         }
//     }
//     pub fn parse_special_token(&mut self, rhs: Token) -> PipelineResult<(Token, Position)> {
//         let (token, pos) = self.token_stream.next();
//         match token {
//             t if t.token_id() == rhs.token_id() => Ok((t, pos)),
//             _ => Err(PipelineError::UnexpectedToken(
//                 token.to_string(),
//                 rhs.to_string(),
//                 pos,
//             )),
//         }
//     }
// }
