use crate::core::error::Error;
use crate::core::result::Result;
use crate::lexer::iter::TokenStream;
use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::lexer::Lexer;
use crate::parser::declaration::VariableDeclaration;
use crate::parser::expr::{Argument, Expr, FnCallExpr, Op, StructExpr};
use crate::parser::function::Function;
use crate::parser::module::Module;
use crate::parser::r#struct::{Struct, StructField};
use crate::parser::r#type::Type;
use crate::parser::stmt::{IfBranchStmt, IfStmt, Stmt};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use std::vec;
use slotmap::DefaultKey;
use crate::context::Context;
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
    module: DefaultKey,
    parent_module: DefaultKey,
    current_module:  DefaultKey,
    function_keyword: String,
}
impl Parser {
    pub fn new(lexer: Lexer,ctx:&Context) -> Self {
        let name = lexer.get_file_name();
        let module = ctx.register_module(Module::new(name));
        Self {
            token_stream: TokenStream::new(lexer),
            current_module: module,
            parent_module: module,
            module,
            function_keyword: "fn".into(),
        }
    }
    pub fn parse_module(&mut self,ctx:&Context) -> Result<()> {
        self.parse_keyword("module")?;
        let (name,_) = self.parse_identifier()?;
        let module = Module::new(name.clone());
        let module = ctx.register_module(module);
        self.parent_module = self.current_module;
        self.current_module = module;
        self.parse_special_token(Token::ParenLeft)?;
        while!self.token_stream.is_eof() {
            let stmt = self.parse_stmt(ctx)?;
            if stmt.is_noop() {
                break;
            }
            // ctx.apply_mut_module(self.current_module,|m|{
            //     m.add_stmt(stmt.clone());
            // });
            let module_slot_map = ctx.get_module_slot_map();
            let mut module_slot_map = module_slot_map.write().unwrap();
            let module = module_slot_map.get_mut(self.current_module).unwrap();
            module.add_stmt(stmt);
            // ctx.get_mut_module(self.current_module).add_stmt(stmt);
        }
        self.parse_special_token(Token::ParenRight)?;
        // ctx.apply_mut_module(self.parent_module,|m|{
        //     m.register_submodule(&name,self.current_module);
        // });
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let module = module_slot_map.get_mut(self.parent_module).unwrap();
        module.register_submodule(&name,self.current_module);
        // ctx.get_mut_module(self.parent_module).register_submodule(&name,self.current_module);
        self.current_module = self.parent_module;
        Ok(())
    }
    pub fn parse(&mut self,ctx:&Context) -> Result<DefaultKey> {
        while !self.token_stream.is_eof() {
            let stmt = self.parse_stmt(ctx)?;
            if stmt.is_noop() {
                break;
            }
            // ctx.get_mut_module(self.module).add_stmt(stmt);
            // ctx.apply_mut_module(self.module,|m|{
            //     m.add_stmt(stmt);
            // })
            let module_slot_map = ctx.get_module_slot_map();
            let mut module_slot_map = module_slot_map.write().unwrap();
            let module = module_slot_map.get_mut(self.module).unwrap();
            module.add_stmt(stmt);
        }
        Ok(self.module)
    }
    pub fn parse_stmt(&mut self,ctx:&Context) -> Result<StmtNode> {
        loop {
            let (token, _) = self.token_stream.peek();
            return Ok(match token {
                Token::Keyword(k) => match k.as_str() {
                    "var" => self.parse_var_stmt(ctx)?,
                    "val" => self.parse_val_stmt(ctx).unwrap(),
                    "if" => self.parse_if_stmt(ctx).unwrap(),
                    "while" => self.parse_while_stmt(ctx)?,
                    "return" => self.parse_return_stmt(ctx)?,
                    "break" => self.parse_break_stmt()?,
                    "continue" => self.parse_continue_stmt()?,
                    "struct" => {
                        self.parse_struct(ctx)?;
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
                        self.parse_extern_function_declaration(ctx).unwrap();
                        continue;
                    }
                    "module"=>{
                        self.parse_module(ctx)?;
                        continue;
                    }
                    t => {
                        if t == self.function_keyword.as_str() {
                            self.parse_function(ctx).unwrap();
                            continue;
                        }
                        todo!()
                    }
                },
                Token::Eof |Token::ParenRight=> StmtNode::new(Stmt::Noop, Position::none()),
                _ => {
                    let e0 = self.parse_expr(ctx).unwrap();
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
    fn parse_extern_function_declaration(&mut self,ctx:&Context) -> Result<()> {
        self.parse_keyword("extern")?;
        let fun = self.parse_function_declaration()?;
        let fun = fun.with_extern(true);
        // ctx.apply_mut_module(self.current_module,|m|{
        //     m.register_function(&fun.name(), fun);
        // });
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let module = module_slot_map.get_mut(self.current_module).unwrap();
        module.register_function(&fun.name(), fun);
        // ctx.get_mut_module(self.current_module).register_function(&fun.name(), fun);
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
    fn parse_struct(&mut self,ctx:&Context) -> Result<()> {
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
        // ctx.get_mut_module(self.current_module).register_struct(&struct_name, struct_declaration);
        // ctx.apply_mut_module(self.current_module,|m|{
        //     m.register_struct(&struct_name, struct_declaration);
        // });
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let module = module_slot_map.get_mut(self.current_module).unwrap();
        module.register_struct(&struct_name, struct_declaration);
        Ok(())
    }
    fn parse_enum(&mut self) {}
    fn parse_trait(&mut self) {}
    fn parse_function(&mut self,ctx:&Context) -> Result<()> {
        let mut fun = self.parse_function_declaration().unwrap();
        let block = self.parse_block(ctx).unwrap();
        fun.set_body(block);
        // ctx.get_mut_module(self.current_module).register_function(&fun.name(), fun);
        // ctx.apply_mut_module(self.current_module,|m|{
        //     m.register_function(&fun.name(), fun);
        // });
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let module = module_slot_map.get_mut(self.current_module).unwrap();
        module.register_function(&fun.name(), fun);
        Ok(())
    }
    fn try_parse_token(&mut self, token: Token) -> bool {
        let (token0, _) = self.token_stream.peek();
        if token0 == token {
            self.token_stream.next_token();
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
            fun.set_binding_struct(&name);
            name = format!("{}.{}", name, name0);
        }
        // 解析泛型
        let mut list = vec![];
        if self.try_parse_token(Token::Less) {
            let ty = self.parse_type()?;
            list.push(ty);
            self.parse_special_token(Token::Greater)?;
        }
        let mut is_template = false;
        if !list.is_empty() {
            is_template = true;
        }
        fun = fun.with_template(is_template);
        let _ = self.parse_special_token(Token::BraceLeft)?;
        let param_list = self.parse_param_list()?;
        let _ = self.parse_special_token(Token::BraceRight)?;
        if self.try_parse_token(Token::Arrow) {
            let ret_ty = self.parse_type().unwrap();
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
    fn parse_block(&mut self,ctx:&Context) -> Result<Vec<StmtNode>> {
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
    fn parse_return_stmt(&mut self,ctx:&Context) -> Result<StmtNode> {
        let p0 = self.parse_keyword("return")?;
        if self.token_stream.peek().0 == Token::ParenRight {
            return Ok(StmtNode::new(Stmt::Return(Box::new(Expr::None.into())), p0));
        }
        let expr = self.parse_expr(ctx)?;
        Ok(StmtNode::new(Stmt::Return(Box::new(expr)), p0))
    }
    pub fn parse_while_stmt(&mut self,ctx:&Context) -> Result<StmtNode> {
        let mut p0 = self.parse_keyword("while")?;
        let p1 = self.parse_special_token(Token::BraceLeft)?;
        let expr = self.parse_expr(ctx)?;
        let p2 = expr.position();
        let p3 = self.parse_special_token(Token::BraceRight)?;
        let block = self.parse_block(ctx)?;
        for s in block.iter() {
            p0 = p0 + s.position();
        }
        Ok(StmtNode::new(
            Stmt::While(Box::new(expr), block),
            p0 + p1 + p2 + p3,
        ))
    }
    pub fn parse_if_stmt(&mut self,ctx:&Context) -> Result<StmtNode> {
        let mut branches = vec![];
        let mut else_body = None;
        let (b, pos) = self.parse_if_branch(ctx)?;
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
    fn parse_if_branch(&mut self,ctx:&Context) -> Result<(IfBranchStmt, Position)> {
        let mut p0 = self.parse_keyword("if")?;
        let p1 = self.parse_special_token(Token::BraceLeft)?;
        let e = self.parse_expr(ctx)?;
        let p2 = e.position();
        let p3 = self.parse_special_token(Token::BraceRight)?;
        let block = self.parse_block(ctx)?;
        for b in block.iter() {
            p0 = p0 + b.position();
        }
        let branch = IfBranchStmt::new(e, block);
        Ok((branch, p0 + p1 + p2 + p3))
    }
    pub fn parse_var_stmt(&mut self,ctx:&Context) -> Result<StmtNode> {
        let p0 = self.parse_keyword("var")?;
        let (name, _) = self.parse_identifier()?;
        let p2 = self.parse_special_token(Token::Colon)?;
        let (type_name, p3) = self.parse_type_name()?;
        let p4 = self.parse_special_token(Token::Assign)?;
        let expr = self.parse_expr(ctx)?;
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
    pub fn parse_val_stmt(&mut self,ctx:&Context) -> Result<StmtNode> {
        let p0 = self.parse_keyword("val")?;
        let (name, mut p1) = self.parse_identifier()?;
        let mut vd = VariableDeclaration::new(name);
        if self.token_stream.peek().0 == Token::Colon {
            p1 = p1 + self.parse_special_token(Token::Colon).unwrap();
            let (type_name, p3) = self.parse_type_name().unwrap();
            p1 = p1 + p3;
            vd = vd.with_type(type_name.into())
        }
        let p4 = self.parse_special_token(Token::Assign).unwrap();
        let expr = self.parse_expr(ctx).unwrap();
        let p5 = expr.position();
        Ok(StmtNode::new(
            Stmt::ValDecl(vd.with_default(expr)),
            p0 + p1 + p4 + p5,
        ))
    }
    pub fn parse_chain_expr(&mut self,ctx:&Context) -> Result<ExprNode> {
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
    fn parse_fn_args(&mut self,ctx:&Context) -> Result<(Vec<Argument>, Position)> {
        let mut p0 = self.parse_special_token(Token::BraceLeft)?;
        let mut args = vec![];
        loop {
            let (peek, p1) = self.token_stream.peek();
            p0 = p0 + p1;
            match peek {
                Token::BraceRight => {
                    let p2 = self.parse_special_token(Token::BraceRight)?;
                    p0 = p0 + p2;
                    // 解析外置闭包
                    let peek0 = self.token_stream.peek().0;
                    if peek0 == Token::ParenLeft {
                        let body = self.parse_block(ctx).unwrap();
                        let expr = Expr::Closure(vec![], body, vec![]);
                        args.push(Argument::new(ExprNode::from(expr)));
                    }
                    break;
                }
                Token::Comma => {
                    let p2 = self.parse_special_token(Token::Comma)?;
                    p0 = p0 + p2;
                    continue;
                }
                _ => {
                    let expr = self.parse_expr(ctx)?;
                    args.push(Argument::new(expr));
                }
            }
        }
        Ok((args, p0))
    }
    fn parse_generic_list(&mut self) -> Result<Vec<Type>> {
        let mut list = vec![];
        let _ = self.parse_special_token(Token::Less)?;
        let ty = self.parse_type()?;
        list.push(ty);
        let mut flag = 0b001 | 0b010; //期望下一个是竖线或者>
        loop {
            let (peek, _) = self.token_stream.peek();
            match peek {
                // 0b001
                Token::Vertical if flag & 0b001 == 0b001 => {
                    let _ = self.parse_special_token(Token::Vertical)?;
                    flag = 0b100;
                    continue;
                }
                // 0b010
                Token::Greater if flag & 0b010 == 0b010 => {
                    let _ = self.parse_special_token(Token::Greater)?;
                    break;
                }
                // 0b100
                _ if flag & 0b100 == 0b100 => {
                    let ty = self.parse_type()?;
                    list.push(ty);
                    flag = 0b010 | 0b001
                }
                t => panic!("unexpected token {:?}", t),
            }
        }
        Ok(list)
    }
    pub fn parse_primary_expr(&mut self,ctx:&Context) -> Result<ExprNode> {
        let (token, mut pos) = self.token_stream.next_token();
        match token {
            Token::Vertical => {
                let peek = self.token_stream.peek().0;
                let mut l = vec![];
                if peek != Token::Vertical {
                    l = self.parse_param_list().unwrap();
                }
                let p1 = self.parse_special_token(Token::Vertical)?;
                let block = self.parse_block(ctx).unwrap();
                Ok(ExprNode::from(Expr::Closure(l, block, vec![])).with_position(p1 + pos))
            }
            Token::Identifier(id) => {
                let (peek, _) = self.token_stream.peek();
                match peek {
                    Token::ScopeSymbol => {
                        let _ = self.parse_special_token(Token::ScopeSymbol)?;
                        let list = self.parse_generic_list().unwrap();
                        let (args, _) = self.parse_fn_args(ctx).unwrap();
                        Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                            name: id,
                            args,
                            is_method: false,
                            generics: list,
                        }))
                        .with_position(pos))
                    }
                    Token::BraceLeft => {
                        let (args, p0) = self.parse_fn_args(ctx).unwrap();
                        pos = pos + p0;
                        Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                            name: id,
                            args,
                            is_method: false,
                            generics: vec![],
                        }))
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
                                    let p2 = self.parse_special_token(Token::Comma).unwrap();
                                    pos = pos + p2;
                                    continue;
                                }
                                Token::Identifier(ident) => {
                                    let (_, _) = self.parse_identifier().unwrap();
                                    let _ = self.parse_special_token(Token::Colon).unwrap();
                                    let expr = self.parse_expr(ctx).unwrap();
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
                let expr = self.parse_expr(ctx)?;
                self.parse_special_token(Token::BraceRight)?;
                Ok(ExprNode::from(Expr::BraceExpr(Box::new(expr))).with_position(pos))
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
    pub fn parse_fact_expr(&mut self,ctx:&Context) -> Result<ExprNode> {
        let expr = self.parse_chain_expr(ctx).unwrap();
        let _ = expr.position();
        let (token, _) = self.token_stream.peek();
        match token {
            Token::BraceLeft => {
                let (mut args, _) = self.parse_fn_args(ctx)?;
                args.insert(0, Argument::new(expr.get_member_root()));
                Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                    name: expr.get_member_name(),
                    generics: vec![],
                    is_method: true,
                    args,
                })))
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
    pub fn parse_term(&mut self,ctx:&Context) -> Result<ExprNode> {
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

    pub fn parse_expr0(&mut self,ctx:&Context) -> Result<ExprNode> {
        let expr0 = self.parse_term(ctx).unwrap();
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
    pub fn parse_expr(&mut self,ctx:&Context) -> Result<ExprNode> {
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
    pub fn parse_keyword(&mut self, keyword: &str) -> Result<Position> {
        let (token, pos) = self.token_stream.next_token();
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
        let (token, pos) = self.token_stream.next_token();
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
        let (t, pos) = self.token_stream.next_token();
        if t == token {
            Ok(pos)
        } else {
            Err(Error::UnexpectedToken(
                t.to_string(),
                token.to_string(),
                pos,
            ))
        }
    }
    pub fn parse_simple_type(&mut self) -> Result<Type> {
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
                    self.parse_special_token(Token::Less)?;
                    let el_ty = self.parse_type()?;
                    self.parse_special_token(Token::Greater)?;
                    Ok(Type::Pointer(Box::new(el_ty)))
                }
                "Array" => {
                    self.parse_special_token(Token::Less)?;
                    let el_ty = self.parse_type()?;
                    self.parse_special_token(Token::Greater)?;
                    Ok(Type::Array(Box::new(el_ty)))
                }
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
                "Identifier(?)".into(),
                p0,
            )),
        }
    }
    pub fn parse_type(&mut self) -> Result<Type> {
        let ty = self.parse_simple_type()?;
        let mut list = vec![];
        if self.try_parse_token(Token::Less) {
            let t0 = self.parse_type()?;
            list.push(t0);
            let _ = self.parse_special_token(Token::Greater);
        }
        if list.is_empty() {
            return Ok(ty);
        }
        Ok(Type::Generic(Box::new(ty), list))
    }

    pub fn parse_type_name(&mut self) -> Result<(String, Position)> {
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
                "Identifier(?)".into(),
                p0,
            )),
        }
    }
}
