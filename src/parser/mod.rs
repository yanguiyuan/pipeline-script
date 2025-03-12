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
use crate::parser::stmt::{IfBranchStmt, IfStmt, MatchBranch, Stmt};
use std::collections::HashMap;

use self::expr::ExprNode;
use self::stmt::StmtNode;
use crate::context::Context;
use slotmap::DefaultKey;
use std::vec;

mod class;
pub mod declaration;
pub mod expr;
pub mod function;
pub mod module;
pub mod stmt;
pub mod r#struct;
pub mod r#type;
pub mod type_alias;

pub struct Parser {
    token_stream: TokenStream,
    module: DefaultKey,
    parent_module: DefaultKey,
    current_module: DefaultKey,
    function_keyword: String,
    var_keyword: String,
    val_keyword: String,
}
impl Parser {
    pub fn new(lexer: Lexer, ctx: &Context) -> Self {
        let name = lexer.get_file_name();
        let module = ctx.register_module(Module::new(name));
        Self {
            token_stream: TokenStream::new(lexer),
            current_module: module,
            parent_module: module,
            module,
            function_keyword: "fn".into(),
            var_keyword: "let".into(),
            val_keyword: "const".into(),
        }
    }
    pub fn parse_module(&mut self, ctx: &Context) -> Result<()> {
        self.parse_keyword("module")?;
        let (name, _) = self.parse_identifier()?;
        let module = Module::new(name.clone());
        let module = ctx.register_module(module);
        self.parent_module = self.current_module;
        self.current_module = module;
        self.parse_special_token(Token::ParenLeft)?;
        while !self.token_stream.is_eof() {
            let stmt = self.parse_stmt(ctx)?;
            if stmt.is_noop() {
                break;
            }
            // let module_slot_map = ctx.get_module_slot_map();
            // let mut module_slot_map = module_slot_map.write().unwrap();
            // let module = module_slot_map.get_mut(self.current_module).unwrap();
            // module.add_stmt(stmt);
            ctx.apply_mut_module(self.current_module, |m| m.add_stmt(stmt.clone()));
        }
        self.parse_special_token(Token::ParenRight)?;
        // let module_slot_map = ctx.get_module_slot_map();
        // let mut module_slot_map = module_slot_map.write().unwrap();
        // let module = module_slot_map.get_mut(self.parent_module).unwrap();
        // module.register_submodule(&name, self.current_module);
        // self.current_module = self.parent_module;
        ctx.apply_mut_module(self.parent_module, |m| {
            m.register_submodule(&name, self.current_module)
        });
        Ok(())
    }
    pub fn parse(&mut self, ctx: &Context) -> Result<DefaultKey> {
        while !self.token_stream.is_eof() {
            let stmt = self.parse_stmt(ctx)?;
            if stmt.is_noop() {
                break;
            }
            // let module_slot_map = ctx.get_module_slot_map();
            // let mut module_slot_map = module_slot_map.write().unwrap();
            // let module = module_slot_map.get_mut(self.module).unwrap();
            // module.add_stmt(stmt);
            ctx.apply_mut_module(self.module, |m| m.add_stmt(stmt.clone()));
        }
        Ok(self.module)
    }
    pub fn parse_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
        loop {
            let (token, _) = self.token_stream.peek();
            return Ok(match token {
                Token::Keyword(k) => match k.as_str() {
                    t if t == self.var_keyword => self.parse_var_stmt(ctx).unwrap(),
                    t if t == self.val_keyword => self.parse_val_stmt(ctx).unwrap(),
                    "while" => self.parse_while_stmt(ctx)?,
                    "for" => self.parse_for_stmt(ctx)?,
                    "return" => self.parse_return_stmt(ctx)?,
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
    pub fn parse_for_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
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
    fn parse_extern_function_declaration(&mut self, ctx: &Context) -> Result<()> {
        self.parse_keyword("extern")?;
        let fun = self.parse_function_declaration(ctx)?;
        let fun = fun.with_extern(true);
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let module = module_slot_map.get_mut(self.current_module).unwrap();
        module.register_function(&fun.name(), fun);
        Ok(())
    }
    fn parse_param_list(&mut self, ctx: &Context) -> Result<Vec<VariableDeclaration>> {
        let mut params = vec![];

        loop {
            let (token, pos) = self.token_stream.peek();

            match token {
                Token::Identifier(_) => {
                    // 解析参数名
                    let (id, _) = self.parse_identifier()?;

                    // 解析类型
                    self.parse_special_token(Token::Colon)?;
                    let ty = self.parse_type()?;

                    // 检查是否有默认值
                    let mut var_decl = VariableDeclaration::new(id).with_type(ty);

                    if self.try_parse_token(Token::Assign) {
                        // 解析默认值表达式
                        let default_expr = self.parse_expr(ctx)?;
                        var_decl = var_decl.with_default(default_expr);
                    }

                    // 添加参数
                    params.push(var_decl);
                }
                Token::Comma => {
                    self.parse_special_token(Token::Comma)?;
                }
                Token::BraceRight | Token::Vertical => {
                    break;
                }
                t => {
                    return Err(Error::UnexpectedToken(
                        t.to_string(),
                        "Identifier, comma, or closing brace".into(),
                        pos,
                    ))
                }
            }
        }

        Ok(params)
    }
    fn parse_continue_stmt(&mut self) -> Result<StmtNode> {
        todo!()
    }
    fn parse_break_stmt(&mut self) -> Result<StmtNode> {
        todo!()
    }
    pub fn next_is(&mut self, token: Token) -> bool {
        self.token_stream.peek().0 == token
    }
    fn parse_struct(&mut self, ctx: &Context) -> Result<()> {
        let _ = self.parse_keyword("struct")?;
        let (struct_name, _) = self.parse_identifier()?;
        let mut generic = vec![];
        if self.next_is(Token::Less) {
            generic = self.parse_generic_list().unwrap();
        }
        let _ = self.parse_special_token(Token::ParenLeft)?;
        let mut fields = vec![];
        loop {
            let (token, _) = self.token_stream.peek();
            match token {
                Token::Identifier(id) => {
                    let _ = self.parse_identifier()?;
                    let _ = self.parse_special_token(Token::Colon)?;
                    let ty = self.parse_type()?;
                    fields.push(StructField::new(id, ty));
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
        let struct_declaration = Struct::new(struct_name.clone(), fields, generic.clone());
        // let module_slot_map = ctx.get_module_slot_map();
        // let mut module_slot_map = module_slot_map.write().unwrap();
        // let module = module_slot_map.get_mut(self.current_module).unwrap();
        // module.register_struct(&struct_name, struct_declaration);
        ctx.apply_mut_module(self.current_module, |m| {
            m.register_struct(&struct_name, struct_declaration.clone())
        });
        Ok(())
    }
    fn parse_enum(&mut self, ctx: &Context) -> Result<()> {
        // 解析enum关键字
        let _ = self.parse_keyword("enum")?;

        // 解析枚举名称
        let (enum_name, _) = self.parse_identifier()?;

        // 解析泛型参数
        let mut generic = vec![];
        if self.try_parse_token(Token::Less) {
            loop {
                let ty = self.parse_type()?;
                generic.push(ty);
                if !self.try_parse_token(Token::Comma) {
                    break;
                }
            }
            self.parse_special_token(Token::Greater)?;
        }

        // 解析枚举体
        self.parse_special_token(Token::ParenLeft)?;

        // 解析枚举变体
        let mut variants = vec![];
        loop {
            let (token, _) = self.token_stream.peek();
            match token {
                Token::ParenRight => {
                    self.parse_special_token(Token::ParenRight)?;
                    break;
                }
                Token::Identifier(_) => {
                    let (variant_name, _) = self.parse_identifier()?;

                    // 检查是否有关联类型
                    let mut variant_type = None;
                    if self.try_parse_token(Token::BraceLeft) {
                        // 解析关联类型
                        let ty = self.parse_type()?;
                        self.parse_special_token(Token::BraceRight)?;
                        variant_type = Some(ty);
                    }

                    variants.push((variant_name, variant_type));

                    // 检查是否有逗号
                    if self.try_parse_token(Token::Comma) {
                        continue;
                    }
                }
                _ => {
                    break;
                }
            }
        }

        // 创建枚举类型并注册到模块
        let enum_type = Type::Enum(Some(enum_name.clone()), variants);

        // 注册枚举类型为别名
        ctx.apply_mut_module(self.current_module, |m| {
            let generic = generic.iter().map(|it| it.as_str()).collect();
            m.register_type_alias(&enum_name, enum_type.clone(), generic)
        });
        Ok(())
    }
    fn parse_trait(&mut self) {}
    fn parse_function(&mut self, ctx: &Context) -> Result<()> {
        let mut fun = self.parse_function_declaration(ctx)?;
        let block = self.parse_block(ctx)?;
        fun.set_body(block);
        // let module_slot_map = ctx.get_module_slot_map();
        // let mut module_slot_map = module_slot_map.write().unwrap();
        // let module = module_slot_map.get_mut(self.current_module).unwrap();
        // module.register_function(&fun.name(), fun);
        ctx.apply_mut_module(self.current_module, |m| {
            m.register_function(&fun.name(), fun.clone())
        });
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
    fn parse_function_declaration(&mut self, ctx: &Context) -> Result<Function> {
        let mut fun = Function::default();
        let fun_name = self.function_keyword.clone();
        let _ = self.parse_keyword(&fun_name)?;
        let (mut name, _) = self.parse_identifier()?;
        // 解析结构体泛型参数
        let mut struct_generics = vec![];
        if self.try_parse_token(Token::Less) {
            loop {
                let ty = self.parse_type()?;
                struct_generics.push(ty);
                if !self.try_parse_token(Token::Comma) {
                    break;
                }
            }
            self.parse_special_token(Token::Greater)?;
        }

        if self.try_parse_token(Token::Dot) {
            let (name0, _) = self.parse_identifier()?;
            fun.set_binding_type(&name);
            fun.set_binding_struct_generics(struct_generics.clone());
            name = format!("{}.{}", name, name0);
        }
        // 解析泛型
        let mut list = vec![];
        if self.try_parse_token(Token::Less) {
            loop {
                let ty = self.parse_type()?;
                list.push(ty);
                if !self.try_parse_token(Token::Comma) {
                    break;
                }
            }
            self.parse_special_token(Token::Greater)?;
        }
        let mut is_template = false;
        if !list.is_empty() || !struct_generics.is_empty() {
            is_template = true;
        }
        fun = fun.with_template(is_template);
        let _ = self.parse_special_token(Token::BraceLeft)?;
        let param_list = self.parse_param_list(ctx)?;
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
    fn parse_block(&mut self, ctx: &Context) -> Result<Vec<StmtNode>> {
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
                    result.push(self.parse_stmt(ctx)?);
                }
            }
        }
        Ok(result)
    }
    fn parse_return_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
        let p0 = self.parse_keyword("return")?;
        if self.token_stream.peek().0 == Token::ParenRight {
            return Ok(StmtNode::new(Stmt::Return(Box::new(Expr::None.into())), p0));
        }
        let expr = self.parse_expr(ctx)?;
        Ok(StmtNode::new(Stmt::Return(Box::new(expr)), p0))
    }
    pub fn parse_while_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
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
    pub fn parse_if_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
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
    fn parse_if_branch(&mut self, ctx: &Context) -> Result<(IfBranchStmt, Position)> {
        let mut p0 = self.parse_keyword("if")?;
        let p1 = self.parse_special_token(Token::BraceLeft)?;
        let e = self.parse_expr(ctx)?;
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
    ) -> Result<StmtNode> {
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

    pub fn parse_var_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
        let keyword = self.var_keyword.clone();
        self.parse_decl_stmt(ctx, &keyword, |vd, _| Stmt::VarDecl(vd))
    }

    pub fn parse_val_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
        let keyword = self.val_keyword.clone();
        self.parse_decl_stmt(ctx, &keyword, |vd, _| Stmt::ValDecl(vd))
    }
    pub fn parse_chain_expr(&mut self, ctx: &Context) -> Result<ExprNode> {
        let mut expr = self.parse_primary_expr(ctx)?;

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
    fn parse_fn_args(&mut self, ctx: &Context) -> Result<(Vec<Argument>, Position)> {
        // 解析左括号
        let mut total_pos = self.parse_special_token(Token::BraceLeft)?;
        let mut args = vec![];

        loop {
            let (peek, peek_pos) = self.token_stream.peek();
            total_pos += peek_pos;

            match peek {
                // 如果是右括号，结束解析
                Token::BraceRight => {
                    let right_pos = self.parse_special_token(Token::BraceRight)?;
                    total_pos += right_pos;

                    // 检查是否有外置闭包
                    if self.token_stream.peek().0 == Token::ParenLeft {
                        let body = self.parse_block(ctx)?;
                        let expr = Expr::Closure(vec![], body, vec![]);
                        args.push(Argument::new(ExprNode::from(expr)));
                    }

                    break;
                }
                // 如果是逗号，继续解析
                Token::Comma => {
                    let comma_pos = self.parse_special_token(Token::Comma)?;
                    total_pos += comma_pos;
                }
                // 如果是标识符，检查是否是命名参数
                Token::Identifier(name) => {
                    // 检查下一个token是否是赋值符号
                    let next_token = self.token_stream.peek_nth(1).0;
                    if next_token == Token::Assign {
                        // 是命名参数
                        let (_, _) = self.parse_identifier()?;
                        self.parse_special_token(Token::Assign)?;
                        let expr = self.parse_expr(ctx)?;
                        args.push(Argument::with_name(name, expr));
                    } else {
                        // 不是命名参数，将标识符视为表达式的一部分
                        let expr = self.parse_expr(ctx)?;
                        args.push(Argument::new(expr));
                    }
                }
                // 其他情况，解析普通参数表达式
                _ => {
                    let expr = self.parse_expr(ctx)?;
                    args.push(Argument::new(expr));
                }
            }
        }

        Ok((args, total_pos))
    }
    fn parse_generic_list(&mut self) -> Result<Vec<Type>> {
        let mut list = vec![];

        // 解析开始符号
        self.parse_special_token(Token::Less)?;

        // 解析第一个类型
        let ty = self.parse_type()?;
        list.push(ty);

        // 解析剩余的类型
        loop {
            let (peek, _) = self.token_stream.peek();

            match peek {
                // 如果是 '|'，则继续解析下一个类型
                Token::Vertical => {
                    self.parse_special_token(Token::Vertical)?;
                    let ty = self.parse_type()?;
                    list.push(ty);
                }
                // 如果是 '>'，则结束解析
                Token::Greater => {
                    self.parse_special_token(Token::Greater)?;
                    break;
                }
                // 其他情况，报错
                t => {
                    return Err(Error::UnexpectedToken(
                        t.to_string(),
                        "| or >".into(),
                        Position::none(),
                    ))
                }
            }
        }

        Ok(list)
    }
    pub fn parse_primary_expr(&mut self, ctx: &Context) -> Result<ExprNode> {
        let (token, mut pos) = self.token_stream.next_token();
        match token {
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
                if self.try_parse_token(Token::Dot) {
                    let (variant_name, variant_pos) = self.parse_identifier()?;

                    // 检查是否有关联值
                    if self.try_parse_token(Token::BraceLeft) {
                        // 解析关联值表达式
                        let value_expr = self.parse_expr(ctx)?;
                        self.parse_special_token(Token::BraceRight)?;

                        // 创建枚举变体表达式
                        let enum_variant =
                            Expr::EnumVariant(id.clone(), variant_name, Some(Box::new(value_expr)));

                        return Ok(ExprNode::new(enum_variant).with_position(pos + variant_pos));
                    } else {
                        // 没有关联值的枚举变体
                        let enum_variant = Expr::EnumVariant(id.clone(), variant_name, None);

                        return Ok(ExprNode::new(enum_variant).with_position(pos + variant_pos));
                    }
                }

                // 检查是否是函数调用
                let peek = self.token_stream.peek().0;
                if peek == Token::BraceLeft {
                    let (args, p0) = self.parse_fn_args(ctx)?;
                    return Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                        name: id,
                        args,
                        is_method: false,
                        generics: vec![],
                    }))
                    .with_position(pos + p0));
                }

                let mut generics = vec![];
                loop {
                    let (peek, _) = self.token_stream.peek();
                    match peek {
                        Token::ScopeSymbol => {
                            let _ = self.parse_special_token(Token::ScopeSymbol)?;
                            let list = self.parse_generic_list()?;
                            let (args, _) = self.parse_fn_args(ctx)?;
                            return Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                                name: id,
                                args,
                                is_method: false,
                                generics: list,
                            }))
                            .with_position(pos));
                        }
                        Token::BraceLeft => {
                            let (args, p0) = self.parse_fn_args(ctx)?;
                            pos += p0;
                            return Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                                name: id,
                                args,
                                is_method: false,
                                generics,
                            }))
                            .with_position(pos));
                        }
                        Token::Less => {
                            generics = self.parse_generic_list()?;
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
    pub fn parse_fact_expr(&mut self, ctx: &Context) -> Result<ExprNode> {
        let expr = self.parse_chain_expr(ctx)?;
        let p0 = expr.position();
        let (token, _) = self.token_stream.peek();
        match token {
            Token::BraceLeft => {
                let (mut args, p1) = self.parse_fn_args(ctx)?;
                let p1 = p1 + p0;
                args.insert(0, Argument::new(expr.get_member_root()));
                Ok(ExprNode::from(Expr::FnCall(FnCallExpr {
                    name: expr.get_member_name(),
                    generics: vec![],
                    is_method: true,
                    args,
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
    pub fn parse_term(&mut self, ctx: &Context) -> Result<ExprNode> {
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

    pub fn parse_expr0(&mut self, ctx: &Context) -> Result<ExprNode> {
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
    pub fn parse_expr(&mut self, ctx: &Context) -> Result<ExprNode> {
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
    fn parse_token<T, F>(&mut self, expected: &str, validator: F) -> Result<(T, Position)>
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

    pub fn parse_identifier(&mut self) -> Result<(String, Position)> {
        self.parse_token("Identifier", |token| {
            if let Token::Identifier(id) = token {
                Some(id.clone())
            } else {
                None
            }
        })
    }

    pub fn parse_keyword(&mut self, keyword: &str) -> Result<Position> {
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

    pub fn parse_special_token(&mut self, token: Token) -> Result<Position> {
        let expected = token.to_string();
        let (_, pos) =
            self.parse_token(&expected, |t| if t == &token { Some(()) } else { None })?;
        Ok(pos)
    }

    fn parse_generic_type<F>(&mut self, constructor: F) -> Result<Type>
    where
        F: FnOnce(Box<Type>) -> Type,
    {
        self.parse_special_token(Token::Less)?;
        let el_ty = self.parse_type()?;
        self.parse_special_token(Token::Greater)?;
        Ok(constructor(Box::new(el_ty)))
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
                    self.parse_generic_type(Type::Pointer)
                }
                "Array" => self.parse_generic_type(Type::Array),
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
                                    "Identifier or special token".into(),
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
                "Identifier or special token".into(),
                p0,
            )),
        }
    }
    pub fn parse_type(&mut self) -> Result<Type> {
        let ty = self.parse_simple_type()?;

        // 检查是否有泛型参数
        if !self.try_parse_token(Token::Less) {
            return Ok(ty);
        }

        // 解析泛型参数列表
        let mut list = vec![];
        let t0 = self.parse_type()?;
        list.push(t0);

        // 解析结束符
        self.parse_special_token(Token::Greater)?;

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
                "Identifier or special token".into(),
                p0,
            )),
        }
    }

    // 解析match语句
    pub fn parse_match_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
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
    pub fn parse_if_let_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
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
    pub fn parse_if_const_stmt(&mut self, ctx: &Context) -> Result<StmtNode> {
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
}
