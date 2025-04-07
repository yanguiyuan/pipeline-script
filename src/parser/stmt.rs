use crate::ast::declaration::VariableDeclaration;
use crate::ast::expr::{Expr, ExprNode};
use crate::ast::stmt::{IfBranchStmt, IfStmt, MatchBranch, Stmt, StmtNode};
use crate::context::Context;
use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::parser::Parser;

impl Parser {
    pub fn parse_match_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        // 解析match关键字
        let pos = self.parse_keyword("match")?;

        // 解析匹配的表达式
        self.parse_special_token(Token::BraceLeft)?;
        let expr = self.parse_expr(ctx)?;
        self.parse_special_token(Token::BraceRight)?;

        // 解析匹配体
        self.parse_special_token(Token::ParenLeft)?;

        // 解析匹配分支
        let mut branches = vec![];
        loop {
            let (token, _) = self.token_stream.peek();
            match token {
                Token::ParenRight => {
                    self.parse_special_token(Token::ParenRight)?;
                    break;
                }
                _ => {
                    // 解析模式
                    let pattern = self.parse_expr(ctx)?;

                    // 解析箭头 (->)
                    self.parse_special_token(Token::Arrow)?;

                    // 解析分支体
                    let mut body = vec![];

                    // 检查是否是代码块
                    let (token, _) = self.token_stream.peek();
                    if token == Token::ParenLeft {
                        body = self.parse_block(ctx)?;
                    } else {
                        // 单个语句
                        let stmt = self.parse_stmt(ctx)?;
                        body.push(stmt);
                    }

                    // 添加分支
                    branches.push(MatchBranch::new(pattern, body));

                    // 检查是否有逗号
                    if self.try_parse_token(Token::Comma) {
                        continue;
                    }
                }
            }
        }

        // 创建match语句
        let match_stmt = Stmt::Match(Box::new(expr), branches);
        Ok(StmtNode::new(match_stmt, pos))
    }

    // 解析if let语句
    pub fn parse_if_let_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        // 解析if关键字
        let pos = self.parse_keyword("if")?;

        // 解析let关键字
        self.parse_keyword("let")?;

        // 解析左括号
        self.parse_special_token(Token::BraceLeft)?;

        // 解析模式
        let pattern = self.parse_expr(ctx)?;

        // 解析等号
        self.parse_special_token(Token::Assign)?;

        // 解析匹配的表达式
        let expr = self.parse_expr(ctx)?;

        // 解析右括号
        self.parse_special_token(Token::BraceRight)?;

        // 解析if分支体
        let body = self.parse_block(ctx)?;

        // 解析else分支（如果有）
        let mut else_body = None;
        if self.try_parse_token(Token::Keyword("else".to_string())) {
            else_body = Some(self.parse_block(ctx)?);
        }

        // 创建if let语句
        let if_let_stmt = Stmt::IfLet(Box::new(pattern), Box::new(expr), body, else_body);
        Ok(StmtNode::new(if_let_stmt, pos))
    }
    pub fn parse_if_const_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        // 解析if关键字
        let pos = self.parse_keyword("if")?;
        let s = self.val_keyword.clone();
        // 解析const关键字
        self.parse_keyword(s.as_str())?;

        // 解析左括号
        self.parse_special_token(Token::BraceLeft)?;

        // 解析模式
        let pattern = self.parse_expr(ctx)?;

        // 解析等号
        self.parse_special_token(Token::Assign)?;

        // 解析匹配的表达式
        let expr = self.parse_expr(ctx)?;

        // 解析右括号
        self.parse_special_token(Token::BraceRight)?;

        // 解析if分支体
        let body = self.parse_block(ctx)?;

        // 解析else分支（如果有）
        let mut else_body = None;
        if self.try_parse_token(Token::Keyword("else".to_string())) {
            else_body = Some(self.parse_block(ctx)?);
        }

        // 创建if let语句
        let if_let_stmt = Stmt::IfConst(Box::new(pattern), Box::new(expr), body, else_body);
        Ok(StmtNode::new(if_let_stmt, pos))
    }
    pub fn parse_var_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        let keyword = self.var_keyword.clone();
        self.parse_decl_stmt(ctx, &keyword, |vd, _| Stmt::VarDecl(vd))
    }

    pub fn parse_val_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        let keyword = self.val_keyword.clone();
        self.parse_decl_stmt(ctx, &keyword, |vd, _| Stmt::ValDecl(vd))
    }
    pub(crate) fn parse_if_branch(
        &mut self,
        ctx: &Context,
    ) -> crate::core::result::Result<(IfBranchStmt, Position)> {
        let mut p0 = self.parse_keyword("if")?;
        let p1 = self.parse_special_token(Token::BraceLeft)?;
        let e = self.parse_expr(ctx).unwrap();
        let p2 = e.position();
        let p3 = self.parse_special_token(Token::BraceRight)?;
        let block = self.parse_block(ctx)?;
        for b in block.iter() {
            p0 += b.position();
        }
        let branch = IfBranchStmt::new(e, block);
        Ok((branch, p0 + p1 + p2 + p3))
    }
    fn parse_decl_stmt(
        &mut self,
        ctx: &Context,
        keyword: &str,
        builder: fn(VariableDeclaration, ExprNode) -> Stmt,
    ) -> crate::core::result::Result<StmtNode> {
        let p0 = self.parse_keyword(keyword)?;
        let (name, mut p1) = self.parse_identifier()?;
        let mut vd = VariableDeclaration::new(name);

        // 处理类型注解
        if let Token::Colon = self.token_stream.peek().0 {
            p1 += self.parse_special_token(Token::Colon)?;
            let ty = self.parse_type()?;
            vd = vd.with_type(ty);
        }

        // 处理赋值表达式
        let p4 = self.parse_special_token(Token::Assign)?;
        let expr = self.parse_expr(ctx)?;
        let p5 = expr.position();

        Ok(StmtNode::new(
            builder(vd.with_default(expr.clone()), expr),
            p0 + p1 + p4 + p5,
        ))
    }
    pub(crate) fn parse_return_stmt(
        &mut self,
        ctx: &Context,
    ) -> crate::core::result::Result<StmtNode> {
        let p0 = self.parse_keyword("return")?;
        if self.token_stream.peek().0 == Token::ParenRight {
            return Ok(StmtNode::new(Stmt::Return(Box::new(Expr::None.into())), p0));
        }
        let expr = self.parse_expr(ctx).expect("parse return stmt error");
        Ok(StmtNode::new(Stmt::Return(Box::new(expr)), p0))
    }
    pub fn parse_while_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        let mut p0 = self.parse_keyword("while")?;
        let p1 = self.parse_special_token(Token::BraceLeft)?;
        let expr = self.parse_expr(ctx)?;
        let p2 = expr.position();
        let p3 = self.parse_special_token(Token::BraceRight)?;
        let block = self.parse_block(ctx)?;
        for s in block.iter() {
            p0 += s.position();
        }
        Ok(StmtNode::new(
            Stmt::While(Box::new(expr), block),
            p0 + p1 + p2 + p3,
        ))
    }
    pub fn parse_if_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        let mut branches = vec![];
        let mut else_body = None;
        let (b, pos) = self.parse_if_branch(ctx).unwrap();
        branches.push(b);

        loop {
            let (peek, _) = self.token_stream.peek();
            match peek.clone() {
                Token::Keyword(k) if k == "else" => {
                    self.token_stream.next_token();
                    let (peek0, _) = self.token_stream.peek();
                    if let Token::ParenLeft = peek0 {
                        let blocks = self.parse_block(ctx)?;
                        else_body = Some(blocks);
                        break;
                    }
                    let (b0, _) = self.parse_if_branch(ctx)?;
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
    pub fn parse_for_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        self.parse_keyword("for")?;
        self.parse_special_token(Token::BraceLeft)?;
        let (var_name, _) = self.parse_identifier()?;
        let _ = self.parse_keyword("in");
        let e0 = self.parse_expr(ctx)?;
        self.parse_special_token(Token::BraceRight)?;
        let body = self.parse_block(ctx)?;
        Ok(StmtNode::new(
            Stmt::ForIn(var_name, Box::new(e0), body),
            Position::none(),
        ))
    }
    pub(crate) fn parse_continue_stmt(&mut self) -> crate::core::result::Result<StmtNode> {
        todo!()
    }
    pub(crate) fn parse_break_stmt(&mut self) -> crate::core::result::Result<StmtNode> {
        self.parse_keyword("break")?;
        Ok(StmtNode::new(Stmt::Break, Position::none()))
    }
    pub fn parse_stmt(&mut self, ctx: &Context) -> crate::core::result::Result<StmtNode> {
        loop {
            let (token, _) = self.token_stream.peek();
            return Ok(match token {
                Token::Keyword(k) => match k.as_str() {
                    t if t == self.var_keyword => self.parse_var_stmt(ctx).unwrap(),
                    t if t == self.val_keyword => self.parse_val_stmt(ctx).unwrap(),
                    "while" => self.parse_while_stmt(ctx).unwrap(),
                    "for" => self.parse_for_stmt(ctx).unwrap(),
                    "return" => self.parse_return_stmt(ctx).unwrap(),
                    "break" => self.parse_break_stmt()?,
                    "continue" => self.parse_continue_stmt()?,
                    "struct" => {
                        self.parse_struct(ctx)?;
                        continue;
                    }
                    "enum" => {
                        self.parse_enum(ctx)?;
                        continue;
                    }
                    "trait" => {
                        self.parse_trait();
                        continue;
                    }
                    "extern" => {
                        self.parse_extern_function_declaration(ctx).unwrap();
                        continue;
                    }
                    "module" => {
                        self.parse_module(ctx)?;
                        continue;
                    }
                    "match" => {
                        return self.parse_match_stmt(ctx);
                    }
                    "if" => {
                        // 检查是否是if let语句
                        let (next_token, _) = self.token_stream.peek_nth(1);
                        if next_token.is_keyword("let") {
                            return self.parse_if_let_stmt(ctx);
                        } else if next_token.is_keyword("const") {
                            return self.parse_if_const_stmt(ctx);
                        } else {
                            return self.parse_if_stmt(ctx);
                        }
                    }
                    t => {
                        if t == self.function_keyword {
                            self.parse_function(ctx).unwrap();
                            continue;
                        }
                        dbg!(t);
                        todo!()
                    }
                },
                Token::Eof | Token::ParenRight => StmtNode::new(Stmt::Noop, Position::none()),
                _ => {
                    let e0 = self.parse_expr(ctx)?;
                    let p0 = e0.position();
                    let (peek, p1) = self.token_stream.peek();
                    if let Token::Assign = peek {
                        let p0 = self.parse_special_token(Token::Assign)?;
                        let e1 = self.parse_expr(ctx)?;
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
}
