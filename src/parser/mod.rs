use crate::ast::module::Module;
use crate::ast::r#struct::{Struct, StructField};
use crate::ast::r#type::Type;
use crate::context::Context;
use crate::core::result::Result;
use crate::lexer::iter::TokenStream;
use crate::lexer::token::Token;
use crate::lexer::Lexer;
use slotmap::DefaultKey;
use std::vec;

mod expr;
mod function;
mod helper;
mod ptype;
mod stmt;

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
            ctx.apply_mut_module(self.current_module, |m| m.add_stmt(stmt.clone()));
        }
        self.parse_special_token(Token::ParenRight)?;
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
            ctx.apply_mut_module(self.module, |m| m.add_stmt(stmt.clone()));
        }
        Ok(self.module)
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
        let generic = self.parse_type_generics().unwrap();

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
}
