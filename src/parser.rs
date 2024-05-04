use std::{env, fs};

use crate::error::{PipelineError, PipelineResult};
use crate::expr::Expr::{Binary, FnCall};
use crate::expr::{Expr, FnCallExpr, FnClosureExpr, Op};
use crate::lexer::{Lexer, TokenStream};
use crate::module::{Class, FnDef, Function, Module, VariableDeclaration};
use crate::position::Position;
use crate::stmt::{IfBranchStmt, IfStmt, Stmt};
use crate::token::Token;
use std::sync::{Arc, RwLock};

pub struct PipelineParser {
    token_stream: TokenStream,
    #[allow(unused)]
    module: Arc<RwLock<Module>>,
}
impl PipelineParser {
    pub fn new(lexer: Lexer, module: Arc<RwLock<Module>>) -> Self {
        Self {
            module,
            token_stream: TokenStream::new(lexer),
        }
    }
    // pub fn register_predefined_class(&mut self,class:Class){
    //     self.classes.insert(class.get_name(),class);
    // }
    // pub fn get_classes(&self)->&HashMap<String,Class>{
    //     &self.classes
    // }
    // pub fn set_lexer(&mut self,lexer: Lexer){
    //     self.token_stream.set_lexer(lexer)
    // }
    // pub fn get_fn_lib(&self)->Vec<FnDef>{
    //     self.fn_lib.clone()
    // }
    pub fn get_module(&self) -> Arc<RwLock<Module>> {
        self.module.clone()
    }
    pub fn parse_stmt_blocks(&mut self) -> PipelineResult<Vec<Stmt>> {
        let mut v = vec![];
        loop {
            let stmt = self.parse_stmt()?;
            if stmt.is_noop() {
                break;
            }
            v.push(stmt)
        }
        Ok(v)
    }

    pub fn parse_stmt(&mut self) -> PipelineResult<Stmt> {
        loop {
            let (token, pos) = self.token_stream.peek();
            return match token {
                Token::Keyword(k) => match k.as_str() {
                    "var" => return self.parse_var_stmt(),
                    "val" => return self.parse_val_stmt(),
                    "fun" => {
                        self.parse_function()?;
                        continue;
                    }
                    "return" => self.parse_return_stmt(),
                    "if" => self.parse_if_stmt(),
                    "while" => self.parse_while_stmt(),
                    "import" => self.parse_import_stmt(),
                    "break" => {
                        self.token_stream.next();
                        Ok(Stmt::Break(pos))
                    }
                    "continue" => {
                        self.token_stream.next();
                        Ok(Stmt::Continue(pos))
                    }
                    "for" => self.parse_for_loop(),
                    "class" => {
                        self.parse_class()?;
                        continue;
                    }
                    t => Err(PipelineError::UnusedKeyword(t.into())),
                },
                Token::Eof => Ok(Stmt::Noop),
                Token::ParenthesisRight => Ok(Stmt::Noop),
                _ => self.parse_expr_stmt(),
            };
        }
    }
    pub fn parse_class(&mut self) -> PipelineResult<()> {
        self.parse_keyword("class")?;
        let (class_name, class_name_pos) = self.parse_identifier()?;
        let mut pos = class_name_pos.clone();
        self.parse_special_token(Token::BraceLeft)?;
        let mut attributions = vec![];
        loop {
            let (peek_token, _) = self.token_stream.peek();
            match peek_token {
                Token::BraceRight => break,
                Token::Comma => {
                    self.token_stream.next();
                    pos.add_span(1)
                }
                _ => {
                    let (attribution_name, attribution_name_pos) = self.parse_identifier()?;
                    self.parse_special_token(Token::Colon)?;
                    let (attribution_type, attribution_type_pos) = self.parse_identifier()?;
                    pos.add_span(attribution_name_pos.span + attribution_type_pos.span + 1);
                    attributions.push(VariableDeclaration::new(attribution_name, attribution_type));
                }
            }
        }
        self.parse_special_token(Token::BraceRight)?;
        let mut class_declaration = Class::new(class_name.as_str(), attributions);
        if self.token_stream.peek().0.is_parenthesis_left() {
            self.parse_special_token(Token::ParenthesisLeft)?;
            while self.token_stream.peek().0.is_keyword("fun") {
                self.parse_keyword("fun")?;
                let fn_def = self.parse_fn_def()?;
                let method = Function::Method(Box::new(fn_def.clone()));
                class_declaration.register_method(fn_def.name.clone(), method);
            }
            self.parse_special_token(Token::ParenthesisRight)?;
        }
        self.module
            .write()
            .unwrap()
            .register_class(class_declaration);
        Ok(())
    }
    #[allow(unused)]
    fn parse_keyword(&mut self, target: &str) -> PipelineResult<(String, Position)> {
        let (next, pos) = self.token_stream.next();
        if let Token::Keyword(keyword) = next {
            return Ok((keyword, pos));
        }
        Err(PipelineError::UnexpectedToken(next, pos))
    }
    fn parse_identifier(&mut self) -> PipelineResult<(String, Position)> {
        let (next, pos) = self.token_stream.next();
        if let Token::Identifier(identifier) = next {
            return Ok((identifier, pos));
        }
        Err(PipelineError::UnexpectedToken(next, pos))
    }
    pub fn parse_for_loop(&mut self) -> PipelineResult<Stmt> {
        let (ret, mut pos) = self.token_stream.next();
        if let Token::Keyword(s) = ret.clone() {
            if s != "for" {
                return Err(PipelineError::UnusedKeyword(s));
            }
            let (one, pos1) = self.token_stream.next();
            let (peek, _) = self.token_stream.peek();
            let mut other = None;
            if let Token::Comma = peek {
                self.token_stream.next();
                let (two, _) = self.token_stream.next();
                other = Some(two.get_identifier_value().to_string())
            }
            let (in_token, pos2) = self.token_stream.next();
            if let Token::Keyword(s0) = in_token {
                if s0 != "in" {
                    return Err(PipelineError::UnusedKeyword(s));
                }
                let expr = self.parse_expr_call_chain_exclude_map()?;
                self.parse_special_token(Token::ParenthesisLeft)?;
                let blocks = self.parse_stmt_blocks()?;
                self.parse_special_token(Token::ParenthesisRight)?;
                pos.add_span(pos1.span + pos2.span + expr.position().span + 2);
                for p in &blocks {
                    pos.add_span(p.position().span);
                }
                return Ok(Stmt::ForIn(
                    one.get_identifier_value().to_string(),
                    other,
                    Box::new(expr),
                    blocks,
                    pos,
                ));
            }
        }
        Err(PipelineError::UnexpectedToken(ret, pos))
    }
    pub fn parse_module(&mut self, module_name: impl AsRef<str>) -> PipelineResult<Option<Module>> {
        let mut current_dir = match env::current_dir() {
            Ok(path) => path,
            Err(_) => {
                return Ok(None);
            }
        };
        #[allow(unused_assignments)]
        let mut script = String::new();
        current_dir.push(format!("{}.kts", module_name.as_ref()));
        if current_dir.exists() {
            script = fs::read_to_string(current_dir).unwrap();
        } else {
            let home_dir = dirs::home_dir().expect("无法获取用户根目录");
            let file_path = home_dir
                .join(".pipeline/package")
                .join(format!("{}.kts", module_name.as_ref()));
            let read_result = fs::read_to_string(file_path);
            match read_result {
                Ok(r) => {
                    script = r;
                }
                Err(_) => return Ok(None),
            }
        }
        // 打印当前工作目录
        let id = module_name.as_ref();

        let lexer = Lexer::from_script(id, script);
        let m = Module::new(id);
        let mut parser = PipelineParser::new(lexer, Arc::new(RwLock::new(m)));
        parser.parse_stmt_blocks()?;
        let m0 = parser.get_module().read().unwrap().clone();
        Ok(Some(m0))
    }
    pub fn parse_import_stmt(&mut self) -> PipelineResult<Stmt> {
        let (ret, mut pos) = self.token_stream.next();
        if let Token::Keyword(s) = ret.clone() {
            if s != "import" {
                return Err(PipelineError::UnusedKeyword(s));
            }
            let (next, pos1) = self.token_stream.next();
            return match next {
                Token::Identifier(id) => {
                    pos.add_span(pos1.span);
                    let m = self.parse_module(id.clone())?;
                    if let Some(m) = m {
                        self.module
                            .write()
                            .unwrap()
                            .register_submodule(m.get_name(), m);
                    }
                    Ok(Stmt::Import(id, pos))
                }
                t => Err(PipelineError::UnexpectedToken(t, pos1)),
            };
        }
        Err(PipelineError::UnexpectedToken(ret, pos))
    }
    fn parse_if_branch(&mut self) -> PipelineResult<(IfBranchStmt, Position)> {
        let (ret, mut pos) = self.token_stream.next();
        if let Token::Keyword(s) = ret.clone() {
            if s != "if" {
                return Err(PipelineError::UnusedKeyword(s));
            }
            let expr = self.parse_expr()?;
            pos.add_span(expr.position().span);
            self.parse_special_token(Token::ParenthesisLeft)?;
            let blocks = self.parse_stmt_blocks()?;
            self.parse_special_token(Token::ParenthesisRight)?;
            for i in &blocks {
                pos.add_span(i.position().span)
            }
            return Ok((IfBranchStmt::new(expr, blocks), pos));
        }
        Err(PipelineError::UnexpectedToken(ret, pos))
    }

    pub fn parse_if_stmt(&mut self) -> PipelineResult<Stmt> {
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
                    if let Token::ParenthesisLeft = peek0 {
                        self.parse_special_token(Token::ParenthesisLeft)?;
                        let blocks = self.parse_stmt_blocks()?;
                        self.parse_special_token(Token::ParenthesisRight)?;
                        else_body = Some(blocks);
                        break;
                    }
                    let (b0, _) = self.parse_if_branch()?;
                    branches.push(b0);
                }
                _ => break,
            }
        }
        Ok(Stmt::If(Box::new(IfStmt::new(branches, else_body)), pos))
    }
    pub fn parse_while_stmt(&mut self) -> PipelineResult<Stmt> {
        let (ret, mut pos) = self.token_stream.next();
        if let Token::Keyword(s) = ret.clone() {
            if s != "while" {
                return Err(PipelineError::UnusedKeyword(s));
            }
            let expr = self.parse_expr()?;
            pos.add_span(expr.position().span);
            self.parse_special_token(Token::ParenthesisLeft)?;
            let blocks = self.parse_stmt_blocks()?;
            self.parse_special_token(Token::ParenthesisRight)?;
            for i in &blocks {
                pos.add_span(i.position().span)
            }
            return Ok(Stmt::While(Box::new(expr), blocks, pos));
        }
        Err(PipelineError::UnexpectedToken(ret, pos))
    }
    pub fn parse_return_stmt(&mut self) -> PipelineResult<Stmt> {
        let (ret, mut pos) = self.token_stream.next();
        if let Token::Keyword(s) = ret.clone() {
            if s != "return" {
                return Err(PipelineError::UnusedKeyword(s));
            }
            let expr = self.parse_expr()?;
            pos.add_span(expr.position().span);
            return Ok(Stmt::Return(Box::new(expr), pos));
        }
        Err(PipelineError::UnexpectedToken(ret, pos))
    }
    fn parse_var_stmt(&mut self) -> PipelineResult<Stmt> {
        let (token, mut pos) = self.token_stream.next();
        if let Token::Keyword(_) = token {
            let (token1, pos0) = self.token_stream.next();
            if let Token::Identifier(ident) = token1 {
                pos.add_span(pos0.span);
                self.parse_special_token(Token::Assign)?;
                pos.add_span(1);
                let expr = self.parse_expr()?;
                pos.add_span(expr.position().span);
                return Ok(Stmt::Var(Box::new((ident, expr)), pos));
            }
            return Err(PipelineError::UnexpectedToken(token1, pos0));
        }
        Err(PipelineError::UnexpectedToken(token, pos))
    }
    fn parse_val_stmt(&mut self) -> PipelineResult<Stmt> {
        let (token, mut pos) = self.token_stream.next();
        if let Token::Keyword(_) = token {
            let (token1, pos0) = self.token_stream.next();
            if let Token::Identifier(ident) = token1 {
                pos.add_span(pos0.span);
                self.parse_special_token(Token::Assign)?;
                pos.add_span(1);
                let expr = self.parse_expr()?;
                pos.add_span(expr.position().span);
                return Ok(Stmt::Val(Box::new((ident, expr)), pos));
            }
            return Err(PipelineError::UnexpectedToken(token1, pos0));
        }
        Err(PipelineError::UnexpectedToken(token, pos))
    }
    // pub fn parse_fn_def(&mut self)->PipelineResult<(FnDef,Position)>{
    //     let (next,mut pos)=self.token_stream.next();
    //     match next {
    //         Token::Keyword(s) if s.as_str()=="fn"||s.as_str()=="fun"=>{
    //             pos.add_span(2);
    //             let (next1,pos1)=self.token_stream.next();
    //             if let Token::Identifier(ident)=next1{
    //                 pos.add_span(pos1.span);
    //                 let (dec_args,pos2)=self.parse_fn_def_args()?;
    //                 pos.add_span(pos2.span);
    //                 self.parse_special_token(Token::ParenthesisLeft)?;
    //                 pos.add_span(1);
    //                 let stmts=self.parse_stmt_blocks()?;
    //                 for s in &stmts{
    //                     let pos_t=s.position();
    //                     pos.add_span(pos_t.span);
    //                 }
    //                 self.parse_special_token(Token::ParenthesisRight)?;
    //                 pos.add_span(1);
    //                 return Ok((FnDef::new(ident,dec_args,stmts,"Any".into()),pos))
    //             }
    //             return Err(PipelineError::UnexpectedToken(next1,pos1))
    //         },
    //         _=>{
    //             Err(PipelineError::UnexpectedToken(next,pos))
    //         }
    //     }
    // }
    fn parse_fn_def(&mut self) -> PipelineResult<FnDef> {
        let (function_name, _) = self.parse_identifier()?;
        let (function_params, _) = self.parse_fn_def_args()?;
        let mut return_type = String::from("Unit");
        // let mut return_type_pos=Position::none();
        if self.token_stream.peek().0.is_colon() {
            self.parse_special_token(Token::Colon)?;
            (return_type, _) = self.parse_identifier()?;
        }
        self.parse_special_token(Token::ParenthesisLeft)?;
        let stmts = self.parse_stmt_blocks()?;
        self.parse_special_token(Token::ParenthesisRight)?;
        Ok(FnDef::new(
            function_name.clone(),
            function_params,
            stmts,
            return_type,
        ))
    }
    pub fn parse_function(&mut self) -> PipelineResult<()> {
        self.parse_keyword("fun")?;
        let (one_name, _) = self.parse_identifier()?;
        let b = self.try_parse_special_token(Token::Dot);
        if b {
            let fn_def = self.parse_fn_def()?;
            let function = Function::Method(Box::new(fn_def.clone()));
            self.module
                .write()
                .unwrap()
                .register_class_method(one_name, fn_def.name, function);
            return Ok(());
        }
        let (function_params, _) = self.parse_fn_def_args()?;
        let mut return_type = String::from("Unit");
        // let mut return_type_pos=Position::none();
        if self.token_stream.peek().0.is_colon() {
            self.parse_special_token(Token::Colon)?;
            (return_type, _) = self.parse_identifier()?;
        }
        self.parse_special_token(Token::ParenthesisLeft)?;
        let stmts = self.parse_stmt_blocks()?;
        self.parse_special_token(Token::ParenthesisRight)?;
        let function_def = FnDef::new(one_name.clone(), function_params, stmts, return_type);
        self.module
            .write()
            .unwrap()
            .register_script_function(function_def.name.clone(), function_def);
        Ok(())
    }
    pub fn try_parse_special_token(&mut self, target: Token) -> bool {
        let (token, _) = self.token_stream.peek();
        match token {
            t if t.token_id() == target.token_id() => {
                self.token_stream.next();
                true
            }
            _ => false,
        }
    }
    pub fn parse_fn_def_args(&mut self) -> PipelineResult<(Vec<VariableDeclaration>, Position)> {
        let start = self.parse_special_token(Token::BraceLeft)?;
        let mut v = vec![];
        let mut p = start.1;
        loop {
            let (token, _) = self.token_stream.peek();
            if Token::BraceRight == token {
                break;
            }
            if Token::Comma == token {
                self.parse_special_token(Token::Comma)?;
                p.add_span(1);
                continue;
            }
            let (dec, pos) = self.parse_variable_declaration()?;
            v.push(dec);
            p.add_span(pos.span)
        }
        self.parse_special_token(Token::BraceRight)?;
        p.add_span(1);
        Ok((v, p))
    }
    pub fn parse_variable_declaration(
        &mut self,
    ) -> PipelineResult<(VariableDeclaration, Position)> {
        let (next, mut pos) = self.token_stream.next();
        if let Token::Identifier(s) = next.clone() {
            self.parse_special_token(Token::Colon)?;
            let (next1, pos1) = self.token_stream.next();
            if let Token::Identifier(s1) = next1 {
                pos.add_span(1 + pos1.span);
                return Ok((VariableDeclaration::new(s, s1), pos));
            }
            return Err(PipelineError::UnexpectedToken(next1, pos1));
        }
        Err(PipelineError::UnexpectedToken(next, pos))
    }

    pub fn parse_expr_stmt(&mut self) -> PipelineResult<Stmt> {
        let lhs = self.parse_expr_call_chain()?;
        let (token, mut pos0) = self.token_stream.peek();
        Ok(match token {
            Token::Assign => {
                self.token_stream.next();
                let expr = self.parse_expr()?;
                pos0.add_span(expr.position().span);
                if let Expr::Index(target, index, mut pos1) = lhs {
                    pos1.add_span(pos0.span);
                    return Ok(Stmt::IndexAssign(target, index, Box::new(expr), pos1));
                }
                return Ok(Stmt::Assign(Box::new((lhs.clone(), expr)), pos0));
            }
            Token::BraceLeft => {
                let (mut args, pos) = self.parse_fn_call_args()?;
                let mut fn_call_expr = FnCallExpr {
                    name: "".into(),
                    args: vec![],
                };
                match lhs {
                    Expr::Variable(s, _) => {
                        fn_call_expr.name = s;
                    }
                    Expr::MemberAccess(b, n, _) => {
                        fn_call_expr.name = n;
                        args.insert(0, *b)
                    }
                    _ => panic!("only variable and member_access expected"),
                }
                fn_call_expr.args = args;

                Stmt::EvalExpr(Box::new(Expr::FnCall(fn_call_expr, pos.clone())), pos)
            }
            _ => {
                let p = lhs.position();
                return Ok(Stmt::EvalExpr(Box::new(lhs), p));
            }
        })
    }
    pub fn parse_fn_call_args(&mut self) -> PipelineResult<(Vec<Expr>, Position)> {
        let (_, mut p) = self.parse_special_token(Token::BraceLeft)?;
        let mut v = vec![];
        loop {
            let (peek, p0) = self.token_stream.peek();
            if peek == Token::BraceRight {
                p += p0;
                self.token_stream.next();
                break;
            }
            let expr = self.parse_expr()?;
            v.push(expr.clone());
            let expr_pos = expr.position();
            p += expr_pos;
            let (token, pos) = self.token_stream.peek();
            match token {
                Token::BraceRight => {
                    p += pos;
                    self.token_stream.next();
                    break;
                }
                Token::Comma => {
                    self.token_stream.next();
                }
                _ => return Err(PipelineError::UnexpectedToken(token, pos)),
            }
        }
        let (peek0, _) = self.token_stream.peek();
        if Token::Arrow == peek0 {
            self.parse_special_token(Token::Arrow).unwrap();
            let (peek, mut pos1) = self.token_stream.peek();
            if Token::ParenthesisLeft == peek {
                self.parse_special_token(Token::ParenthesisLeft).unwrap();
                let blocks = self.parse_stmt_blocks()?;
                for stmt in &blocks {
                    pos1.add_span(stmt.position().span)
                }
                let fn_def = FnDef::new("".to_string(), vec![], blocks, "Any".into());
                pos1.add_span(1);
                p.add_span(pos1.span);
                v.push(Expr::FnClosure(FnClosureExpr { def: fn_def }, pos1));
                self.parse_special_token(Token::ParenthesisRight).unwrap();
            }
        }

        Ok((v, p))
    }
    fn parse_primary(&mut self) -> PipelineResult<Expr> {
        let (token, mut pos) = self.token_stream.next();
        match token {
            Token::String(s) => Ok(Expr::StringConstant(s, pos)),
            Token::Int(i) => Ok(Expr::IntConstant(i, pos)),
            Token::Float(f) => Ok(Expr::FloatConstant(f, pos)),
            Token::Identifier(ident) => {
                let (peek, mut pos1) = self.token_stream.peek();
                match peek {
                    Token::ScopeSymbol => {
                        self.token_stream.next();
                        let (next, pos2) = self.token_stream.next();
                        let fc_name = next.get_identifier_value();
                        let (args, pos3) = self.parse_fn_call_args().unwrap();
                        let name = ident + "::" + fc_name;
                        let mut p = pos.clone();
                        p.add_span(pos2.span + 2);
                        let fn_expr = FnCallExpr { name, args };
                        pos.add_span(pos2.span + pos3.span + 2);
                        Ok(Expr::FnCall(fn_expr, pos))
                    }
                    Token::SquareBracketLeft => {
                        self.token_stream.next();
                        let e = self.parse_math_expr()?;
                        self.parse_special_token(Token::SquareBracketRight)?;
                        pos1.add_span(1 + e.position().span + 1);
                        Ok(Expr::Index(
                            Box::new(Expr::Variable(ident, pos)),
                            Box::new(e),
                            pos1,
                        ))
                    }
                    // Token::ParenthesisLeft=>{
                    //     let mut props=HashMap::new();
                    //     self.token_stream.next();
                    //     loop{
                    //         let (peek,pos2)=self.token_stream.peek();
                    //         match peek {
                    //             Token::Comma=>{
                    //                 pos.add_span(1);
                    //                 self.token_stream.next();
                    //                 continue
                    //             }
                    //             Token::ParenthesisRight=>{
                    //                 pos.add_span(1);
                    //                 self.token_stream.next();
                    //                 break;
                    //             }
                    //             Token::Identifier(s)=>{
                    //                 self.token_stream.next();
                    //                 self.parse_special_token(Token::Colon)?;
                    //                 let expr=self.parse_expr();
                    //                 let expr=expr.unwrap();
                    //                 pos.add_span(pos2.span+1+expr.position().span);
                    //                 props.insert(s,expr);
                    //             }
                    //             t=>return Err(PipelineError::UnexpectedToken(t,pos2))
                    //         }
                    //     }
                    //     return Ok(Expr::Struct(StructExpr::new(ident,props),pos))
                    // }
                    _ => Ok(Expr::Variable(ident, pos)),
                }
            }
            _ => Err(PipelineError::UnexpectedToken(token, pos)),
        }
    }
    fn parse_primary_exclude_map(&mut self) -> PipelineResult<Expr> {
        let (token, mut pos) = self.token_stream.next();
        match token {
            Token::String(s) => Ok(Expr::StringConstant(s, pos)),
            Token::Int(i) => Ok(Expr::IntConstant(i, pos)),
            Token::Float(f) => Ok(Expr::FloatConstant(f, pos)),
            Token::Identifier(ident) => {
                let (peek, mut pos1) = self.token_stream.peek();
                match peek {
                    Token::ScopeSymbol => {
                        self.token_stream.next();
                        let (next, pos2) = self.token_stream.next();
                        let fc_name = next.get_identifier_value();
                        let (args, pos3) = self.parse_fn_call_args().unwrap();
                        let name = ident + "::" + fc_name;
                        let mut p = pos.clone();
                        p.add_span(pos2.span + 2);
                        let fn_expr = FnCallExpr { name, args };
                        pos.add_span(pos2.span + pos3.span + 2);
                        Ok(Expr::FnCall(fn_expr, pos))
                    }
                    Token::SquareBracketLeft => {
                        self.token_stream.next();
                        let e = self.parse_math_expr()?;
                        self.parse_special_token(Token::SquareBracketRight)?;
                        pos1.add_span(1 + e.position().span + 1);
                        Ok(Expr::Index(
                            Box::new(Expr::Variable(ident, pos)),
                            Box::new(e),
                            pos1,
                        ))
                    }
                    _ => Ok(Expr::Variable(ident, pos)),
                }
            }
            _ => Err(PipelineError::UnexpectedToken(token, pos)),
        }
    }

    fn parse_expr_call_chain(&mut self) -> PipelineResult<Expr> {
        let mut lhs = self.parse_primary()?;
        loop {
            let (peek, _) = self.token_stream.peek();
            match peek {
                Token::Dot => {
                    self.token_stream.next();
                    let (next, pos0) = self.token_stream.next();
                    let name = next.get_identifier_value();
                    let (peek1, _) = self.token_stream.peek();
                    if let Token::BraceLeft = peek1 {
                        let (args, pos1) = self.parse_fn_call_args()?;
                        let mut fn_call = FnCallExpr {
                            name: name.into(),
                            args,
                        };
                        let mut p = lhs.position();
                        p.add_span(1 + pos0.span + pos1.span);
                        fn_call.args.insert(0, lhs);
                        let expr = FnCall(fn_call, p);
                        lhs = expr;
                    } else {
                        let mut pos00 = lhs.position();
                        pos00.add_span(1 + pos0.span);
                        let member_access =
                            Expr::MemberAccess(Box::new(lhs.clone()), name.into(), pos00);
                        lhs = member_access;
                    }
                }
                Token::BraceLeft => {
                    let (mut args, pos) = self.parse_fn_call_args()?;
                    let mut fn_call_expr = FnCallExpr {
                        name: "".into(),
                        args: vec![],
                    };
                    match lhs.clone() {
                        Expr::Variable(s, _) => {
                            fn_call_expr.name = s;
                        }
                        Expr::MemberAccess(b, n, _) => {
                            fn_call_expr.name = n;
                            args.insert(0, *b)
                        }
                        _ => panic!("only variable and member_access expected"),
                    }
                    fn_call_expr.args = args;
                    lhs = Expr::FnCall(fn_call_expr, lhs.position() + pos)
                }
                Token::SquareBracketLeft => {
                    let mut pos1 = lhs.position();
                    self.token_stream.next();
                    let e = self.parse_math_expr()?;
                    self.parse_special_token(Token::SquareBracketRight)?;
                    pos1.add_span(1 + e.position().span + 1);
                    lhs = Expr::Index(Box::new(lhs), Box::new(e), pos1)
                }
                _ => return Ok(lhs),
            }
        }
    }
    fn parse_expr_call_chain_exclude_map(&mut self) -> PipelineResult<Expr> {
        let mut lhs = self.parse_primary_exclude_map()?;
        loop {
            let (peek, _) = self.token_stream.peek();
            match peek {
                Token::Dot => {
                    self.token_stream.next();
                    let (next, pos0) = self.token_stream.next();
                    let name = next.get_identifier_value();
                    let (peek1, _) = self.token_stream.peek();
                    if let Token::BraceLeft = peek1 {
                        let (args, pos1) = self.parse_fn_call_args()?;
                        let mut fn_call = FnCallExpr {
                            name: name.into(),
                            args,
                        };
                        let mut p = lhs.position();
                        p.add_span(1 + pos0.span + pos1.span);
                        fn_call.args.insert(0, lhs);
                        let expr = FnCall(fn_call, p);
                        lhs = expr;
                    } else {
                        let mut pos00 = lhs.position();
                        pos00.add_span(1 + pos0.span);
                        let member_access =
                            Expr::MemberAccess(Box::new(lhs.clone()), name.into(), pos00);
                        lhs = member_access;
                    }
                }
                Token::BraceLeft => {
                    let (mut args, pos) = self.parse_fn_call_args()?;
                    let mut fn_call_expr = FnCallExpr {
                        name: "".into(),
                        args: vec![],
                    };
                    match lhs.clone() {
                        Expr::Variable(s, _) => {
                            fn_call_expr.name = s;
                        }
                        Expr::MemberAccess(b, n, _) => {
                            fn_call_expr.name = n;
                            args.insert(0, *b)
                        }
                        _ => panic!("only variable and member_access expected"),
                    }
                    fn_call_expr.args = args;
                    lhs = Expr::FnCall(fn_call_expr, pos)
                }
                Token::SquareBracketLeft => {
                    let mut pos1 = lhs.position();
                    self.token_stream.next();
                    let e = self.parse_math_expr()?;
                    self.parse_special_token(Token::SquareBracketRight)?;
                    pos1.add_span(1 + e.position().span + 1);
                    return Ok(Expr::Index(Box::new(lhs), Box::new(e), pos1));
                }
                _ => return Ok(lhs),
            }
        }
    }
    fn parse_term(&mut self) -> PipelineResult<Expr> {
        let lhs = self.parse_expr_call_chain()?;
        let (token, mut pos) = self.token_stream.peek();
        match token {
            Token::Mul => {
                self.token_stream.next();
                let right = self.parse_term()?;
                pos.add_span(1 + right.position().span + lhs.position().span);
                Ok(Binary(Op::Mul, Box::new(lhs), Box::new(right), pos))
            }
            Token::Div => {
                self.token_stream.next();
                let right = self.parse_term()?;
                pos.add_span(1 + right.position().span + lhs.position().span);
                Ok(Binary(Op::Div, Box::new(lhs), Box::new(right), pos))
            }
            Token::Mod => {
                self.token_stream.next();
                let right = self.parse_term()?;
                pos.add_span(1 + right.position().span + lhs.position().span);
                Ok(Binary(Op::Mod, Box::new(lhs), Box::new(right), pos))
            }
            _ => Ok(lhs),
        }
    }
    fn parse_math_expr(&mut self) -> PipelineResult<Expr> {
        let lhs = self.parse_term()?;
        let next = self.token_stream.peek();
        match next.0 {
            Token::Plus => {
                self.token_stream.next();
                let mut pos = lhs.position();
                let rhs = self.parse_expr()?;
                pos.add_span(1 + rhs.position().span);
                Ok(Expr::Binary(Op::Plus, Box::new(lhs), Box::new(rhs), pos))
            }
            Token::Minus => {
                self.token_stream.next();
                let mut pos = lhs.position();
                let rhs = self.parse_expr()?;
                pos.add_span(1 + rhs.position().span);
                Ok(Expr::Binary(Op::Minus, Box::new(lhs), Box::new(rhs), pos))
            }
            _ => Ok(lhs),
        }
    }
    fn parse_array(&mut self) -> PipelineResult<Expr> {
        let (_, mut pos) = self.token_stream.next();
        let mut v = vec![];
        loop {
            let (peek, _) = self.token_stream.peek();
            match peek {
                Token::Comma => {
                    self.token_stream.next();
                    pos.add_span(1);
                    continue;
                }
                Token::SquareBracketRight => {
                    self.token_stream.next();
                    pos.add_span(1);
                    break;
                }
                _ => {
                    let e = self.parse_expr()?;
                    v.push(e.clone());
                    pos.add_span(e.position().span);
                }
            }
        }
        Ok(Expr::Array(v, pos))
    }
    pub fn parse_map(&mut self) -> PipelineResult<Expr> {
        let (_, mut pos) = self.token_stream.next();
        let mut v = vec![];
        loop {
            let (peek, _) = self.token_stream.peek();
            match peek {
                Token::Comma => {
                    self.token_stream.next();
                    pos.add_span(1);
                    continue;
                }
                Token::ParenthesisRight => {
                    self.token_stream.next();
                    pos.add_span(1);
                    break;
                }
                _ => {
                    let e = self.parse_expr()?;
                    pos.add_span(e.position().span);
                    self.parse_special_token(Token::Colon)?;
                    let rhs = self.parse_expr()?;
                    pos.add_span(rhs.position().span + 1);
                    v.push((e.clone(), rhs.clone()));
                }
            }
        }
        Ok(Expr::Map(v, pos))
    }
    pub fn parse_expr(&mut self) -> PipelineResult<Expr> {
        let (peek, _) = self.token_stream.peek();
        if peek == Token::SquareBracketLeft {
            return self.parse_array();
        }
        if peek == Token::ParenthesisLeft {
            return self.parse_map();
        }
        let lhs = self.parse_math_expr()?;
        let next = self.token_stream.peek();
        match next.0 {
            Token::Greater => {
                self.token_stream.next();
                let mut pos = lhs.position();
                let rhs = self.parse_math_expr()?;
                pos.add_span(1 + rhs.position().span);
                Ok(Expr::Binary(Op::Greater, Box::new(lhs), Box::new(rhs), pos))
            }
            Token::Less => {
                self.token_stream.next();
                let mut pos = lhs.position();
                let rhs = self.parse_math_expr()?;
                pos.add_span(1 + rhs.position().span);
                Ok(Expr::Binary(Op::Less, Box::new(lhs), Box::new(rhs), pos))
            }
            Token::Equal => {
                self.token_stream.next();
                let mut pos = lhs.position();
                let rhs = self.parse_math_expr()?;
                pos.add_span(1 + rhs.position().span);
                Ok(Expr::Binary(Op::Equal, Box::new(lhs), Box::new(rhs), pos))
            }
            Token::NotEqual => {
                self.token_stream.next();
                let mut pos = lhs.position();
                let rhs = self.parse_math_expr()?;
                pos.add_span(1 + rhs.position().span);
                Ok(Expr::Binary(
                    Op::NotEqual,
                    Box::new(lhs),
                    Box::new(rhs),
                    pos,
                ))
            }
            _ => Ok(lhs),
        }
    }
    pub fn parse_special_token(&mut self, rhs: Token) -> PipelineResult<(Token, Position)> {
        let (token, pos) = self.token_stream.next();
        match token {
            t if t.token_id() == rhs.token_id() => Ok((t, pos)),
            _ => Err(PipelineError::UnexpectedToken(token, pos)),
        }
    }
    // #[allow(unused)]
    // pub fn from_token_stream(token_stream:TokenStream)->Self{
    //     return Self{ token_stream,fn_lib:vec![],modules:vec![],classes:HashMap::new() }
    // }
}

pub trait Parser {
    fn ident() -> String;
    fn parse(p: &mut PipelineParser) -> PipelineResult<Stmt>;
}
