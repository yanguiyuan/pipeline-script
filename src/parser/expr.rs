use crate::ast::expr::{Argument, Expr, ExprNode, FunctionCall, Op, StructExpr};
use crate::context::Context;
use crate::core::error::Error;
use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::parser::helper::{get_struct_from_context, is_enum};
use crate::parser::Parser;
use std::collections::HashMap;

impl Parser {
    pub fn parse_primary_expr(&mut self, ctx: &Context) -> crate::core::result::Result<ExprNode> {
        let (token, mut pos) = self.token_stream.next_token();
        match token {
            Token::Not => {
                let expr = self.parse_primary_expr(ctx)?;
                Ok(ExprNode::from(Expr::Unary(Op::Not, Box::new(expr))).with_position(pos))
            }
            Token::Minus => {
                let expr = self.parse_primary_expr(ctx)?;
                Ok(ExprNode::from(Expr::Unary(Op::Negate, Box::new(expr))).with_position(pos))
            }
            Token::Vertical => {
                let peek = self.token_stream.peek().0;
                let mut l = vec![];
                if peek != Token::Vertical {
                    l = self.parse_param_list(ctx)?;
                }
                let p1 = self.parse_special_token(Token::Vertical)?;
                let block = self.parse_block(ctx)?;
                Ok(ExprNode::from(Expr::Closure(l, block, vec![])).with_position(p1 + pos))
            }
            Token::Identifier(id) => {
                // 检查是否是枚举变体访问
                if is_enum(ctx, &id) && self.try_parse_token(Token::Dot) {
                    return self.parse_enum_variant(ctx, &id, pos);
                }

                let mut generics = vec![];
                loop {
                    let (peek, _) = self.token_stream.peek();
                    match peek {
                        Token::ScopeSymbol => {
                            let _ = self.parse_special_token(Token::ScopeSymbol)?;
                            let list = self.parse_generic_list()?;
                            let (args, _) = self.parse_fn_args(ctx)?;
                            return Ok(ExprNode::from(Expr::FnCall(FunctionCall {
                                name: id,
                                args,
                                is_method: false,
                                generics: list,
                                type_generics: vec![],
                            }))
                            .with_position(pos));
                        }
                        Token::BraceLeft => {
                            let (args, p0) = self.parse_fn_args(ctx)?;
                            pos += p0;
                            return Ok(ExprNode::from(Expr::FnCall(FunctionCall {
                                name: id,
                                args,
                                is_method: false,
                                generics,
                                type_generics: vec![],
                            }))
                            .with_position(pos));
                        }
                        Token::Less => {
                            let mut flag = false;
                            ctx.apply_module(self.module, |m| {
                                let r = m.get_struct(&id);
                                if r.is_some() {
                                    flag = true;
                                }
                            });
                            if flag {
                                generics = self.parse_generic_list()?;
                            } else {
                                return Ok(ExprNode::from(Expr::Variable(id)).with_position(pos));
                            }
                        }
                        Token::ParenLeft => {
                            // 解析结构体构造
                            let p0 = self.parse_special_token(Token::ParenLeft)?;
                            let mut fields = HashMap::new();
                            loop {
                                let (peek, p1) = self.token_stream.peek();
                                pos += p1;
                                match peek {
                                    Token::ParenRight => {
                                        let p2 = self.parse_special_token(Token::ParenRight)?;
                                        pos += p2;
                                        break;
                                    }
                                    Token::Comma => {
                                        let p2 = self.parse_special_token(Token::Comma)?;
                                        pos += p2;
                                        continue;
                                    }
                                    Token::Identifier(ident) => {
                                        let (_, _) = self.parse_identifier()?;
                                        let _ = self.parse_special_token(Token::Colon)?;
                                        let expr = self.parse_expr(ctx)?;
                                        let _ = expr.position();
                                        fields.insert(ident, expr);
                                    }
                                    _ => todo!("parse primary expr"),
                                }
                            }

                            pos += p0;
                            return Ok(ExprNode::from(Expr::Struct(
                                StructExpr::new(id, fields).with_generics(generics),
                            ))
                            .with_position(pos));
                        }
                        Token::Dot if !generics.is_empty() => {
                            let _ = self.parse_special_token(Token::Dot)?;
                            let (static_method, p1) = self.parse_identifier()?;
                            pos += p1;
                            let (args, p2) = self.parse_fn_args(ctx).unwrap();
                            pos += p2;
                            return Ok(ExprNode::from(Expr::FnCall(FunctionCall {
                                name: format!("{}.{}", id, static_method),
                                args,
                                is_method: false,
                                generics: vec![],
                                type_generics: generics,
                            })));
                        }
                        _ => return Ok(ExprNode::from(Expr::Variable(id)).with_position(pos)),
                    }
                }
            }
            Token::Int(n) => Ok(ExprNode::from(Expr::Int(n)).with_position(pos)),
            Token::String(s) => Ok(ExprNode::from(Expr::String(s)).with_position(pos)),
            Token::Boolean(b) => Ok(ExprNode::from(Expr::Boolean(b)).with_position(pos)),
            Token::Float(n) => Ok(ExprNode::from(Expr::Float(n)).with_position(pos)),
            Token::BraceLeft => {
                let expr = self.parse_expr(ctx)?;
                self.parse_special_token(Token::BraceRight)?;
                Ok(ExprNode::from(Expr::Brace(Box::new(expr))).with_position(pos))
            }
            Token::BitAnd => {
                let expr = self.parse_primary_expr(ctx)?;
                let _ = expr.position();
                Ok(ExprNode::from(Expr::Address(Box::new(expr))).with_position(pos))
            }
            Token::BracketLeft => {
                let mut v = vec![];
                loop {
                    let expr = self.parse_expr(ctx)?;
                    pos += expr.position();
                    v.push(expr);
                    let (token, p1) = self.token_stream.peek();
                    pos += p1;
                    match token {
                        Token::BracketRight => {
                            let p2 = self.parse_special_token(Token::BracketRight)?;
                            return Ok(ExprNode::from(Expr::Array(v)).with_position(pos + p2));
                        }
                        Token::Comma => {
                            let p2 = self.parse_special_token(Token::Comma)?;
                            pos += p2;
                            continue;
                        }
                        _ => {}
                    }
                }
            }
            _ => Err(Error::UnexpectedToken(
                token.to_string(),
                "Expect Primary Expr".into(),
                pos,
            )),
        }
    }
    fn parse_enum_variant(
        &mut self,
        ctx: &Context,
        enum_name: &str,
        pos: Position,
    ) -> crate::core::result::Result<ExprNode> {
        let (variant_name, variant_pos) = self.parse_identifier()?;

        // 检查是否有关联值
        if self.try_parse_token(Token::BraceLeft) {
            // 解析关联值表达式
            let value_expr = self.parse_expr(ctx)?;
            self.parse_special_token(Token::BraceRight)?;

            // 创建枚举变体表达式
            let enum_variant =
                Expr::EnumVariant(enum_name.into(), variant_name, Some(Box::new(value_expr)));

            Ok(ExprNode::new(enum_variant).with_position(pos + variant_pos))
        } else {
            // 没有关联值的枚举变体
            let enum_variant = Expr::EnumVariant(enum_name.into(), variant_name, None);

            Ok(ExprNode::new(enum_variant).with_position(pos + variant_pos))
        }
    }
    pub fn parse_fact_expr(&mut self, ctx: &Context) -> crate::core::result::Result<ExprNode> {
        let expr = self.parse_chain_expr(ctx)?;
        let p0 = expr.position();
        let (token, _) = self.token_stream.peek();
        match token {
            Token::BraceLeft => {
                let (mut args, p1) = self.parse_fn_args(ctx)?;
                let p1 = p1 + p0;
                let mut name = expr.get_member_name();
                let mut is_method = false;

                // 检查是否是结构体方法调用
                if expr.is_member() {
                    let root = expr.get_member_root();
                    if let Expr::Variable(root_name) = &root.expr {
                        // 检查是否是已知的结构体
                        if let Some(_) = get_struct_from_context(ctx, root_name) {
                            name = format!("{}.{}", root_name, name);
                            args.insert(0, Argument::new(root));
                            is_method = true;
                        } else {
                            // 普通成员访问，保持原始函数名，添加self参数
                            args.insert(0, Argument::new(root));
                            is_method = true;
                        }
                    }
                }

                Ok(ExprNode::from(Expr::FnCall(FunctionCall {
                    name,
                    generics: vec![],
                    is_method,
                    args,
                    type_generics: vec![],
                }))
                .with_position(p1))
            }
            Token::BracketLeft => {
                let p0 = self.parse_special_token(Token::BracketLeft)?;
                let index = self.parse_expr(ctx)?;
                let p1 = index.position();
                let p2 = self.parse_special_token(Token::BracketRight)?;
                Ok(ExprNode::from(Expr::Index(Box::new(expr), Box::new(index)))
                    .with_position(p0 + p1 + p2))
            }

            _ => Ok(expr),
        }
    }
    pub fn parse_term(&mut self, ctx: &Context) -> crate::core::result::Result<ExprNode> {
        let expr0 = self.parse_fact_expr(ctx)?;
        let p0 = expr0.position();
        let (token, _) = self.token_stream.peek();
        match token {
            Token::Star => {
                let p1 = self.parse_special_token(Token::Star)?;
                let expr1 = self.parse_term(ctx)?;
                let p2 = expr1.position();
                Ok(
                    ExprNode::from(Expr::Binary(Op::Mul, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0 + p1 + p2),
                )
            }
            Token::Slash => {
                let p0 = self.parse_special_token(Token::Slash)?;
                let expr1 = self.parse_term(ctx)?;
                Ok(
                    ExprNode::from(Expr::Binary(Op::Div, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            _ => Ok(expr0),
        }
    }

    pub fn parse_expr0(&mut self, ctx: &Context) -> crate::core::result::Result<ExprNode> {
        let expr0 = self.parse_term(ctx)?;
        let (token, _) = self.token_stream.peek();
        match token {
            Token::Plus => {
                let p0 = self.parse_special_token(Token::Plus)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Plus, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::Minus => {
                let p0 = self.parse_special_token(Token::Minus)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Minus, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            _ => Ok(expr0),
        }
    }
    pub fn parse_expr(&mut self, ctx: &Context) -> crate::core::result::Result<ExprNode> {
        let expr0 = self.parse_expr0(ctx)?;
        let (token, _) = self.token_stream.peek();
        match token {
            Token::Equal => {
                let p0 = self.parse_special_token(Token::Equal)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Equal, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::NotEqual => {
                let p0 = self.parse_special_token(Token::NotEqual)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::NotEqual, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::Less => {
                let p0 = self.parse_special_token(Token::Less)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Less, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            Token::LessEqual => {
                let p0 = self.parse_special_token(Token::LessEqual)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(ExprNode::new(Expr::Binary(
                    Op::LessEqual,
                    Box::new(expr0),
                    Box::new(expr1),
                ))
                .with_position(p0))
            }
            Token::GreaterEqual => {
                let p0 = self.parse_special_token(Token::GreaterEqual)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(ExprNode::new(Expr::Binary(
                    Op::GreaterEqual,
                    Box::new(expr0),
                    Box::new(expr1),
                ))
                .with_position(p0))
            }
            Token::Greater => {
                let p0 = self.parse_special_token(Token::Greater)?;
                let expr1 = self.parse_expr(ctx)?;
                Ok(
                    ExprNode::new(Expr::Binary(Op::Greater, Box::new(expr0), Box::new(expr1)))
                        .with_position(p0),
                )
            }
            _ => Ok(expr0),
        }
    }
    pub fn parse_chain_expr(&mut self, ctx: &Context) -> crate::core::result::Result<ExprNode> {
        let mut expr = self.parse_primary_expr(ctx).unwrap();

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
                    let index = self.parse_expr(ctx)?;
                    let pos = index.position();
                    self.parse_special_token(Token::BracketRight)?;
                    ExprNode::from(Expr::Index(Box::new(expr), Box::new(index))).with_position(pos)
                }
                _ => break,
            };
        }

        Ok(expr)
    }
}
