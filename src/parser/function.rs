use crate::ast::declaration::VariableDeclaration;
use crate::ast::expr::{Argument, Expr, ExprNode};
use crate::ast::function::Function;
use crate::ast::r#type::Type;
use crate::context::Context;
use crate::core::error::Error;
use crate::lexer::position::Position;
use crate::lexer::token::Token;
use crate::parser::Parser;

impl Parser {
    pub(crate) fn parse_fn_args(
        &mut self,
        ctx: &Context,
    ) -> crate::core::result::Result<(Vec<Argument>, Position)> {
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
    pub(crate) fn parse_function(&mut self, ctx: &Context) -> crate::core::result::Result<()> {
        let mut fun = self.parse_function_declaration(ctx)?;
        let block = self.parse_block(ctx)?;
        fun.set_body(block);
        ctx.apply_mut_module(self.current_module, |m| {
            m.register_function(&fun.name(), fun.clone())
        });
        Ok(())
    }

    pub(crate) fn parse_function_declaration(
        &mut self,
        ctx: &Context,
    ) -> crate::core::result::Result<Function> {
        // 初始化函数对象
        let mut fun = Function::default();

        // 1. 解析函数关键字
        let fun_name = self.function_keyword.clone(); // 先克隆避免后续引用冲突
        self.parse_keyword(&fun_name)?;

        // 2. 解析函数名
        let (mut name, _) = self.parse_identifier()?;

        // 3. 解析结构体泛型参数
        let type_generics = self.parse_type_generics()?;

        // 4. 处理绑定类型（如果有）
        let has_binding = self.try_parse_token(Token::Dot);
        if has_binding {
            let (method_name, _) = self.parse_identifier()?;
            fun.set_binding_type(&name);
            fun.set_type_generics(type_generics.clone());
            name = format!("{}.{}", name, method_name);
        }

        // 5. 解析方法泛型（仅当有绑定类型时）
        let mut method_generics = vec![];
        if has_binding && self.try_parse_token(Token::Less) {
            loop {
                let ty = self.parse_type()?;
                method_generics.push(ty);
                if !self.try_parse_token(Token::Comma) {
                    break;
                }
            }
            self.parse_special_token(Token::Greater)?;
        }

        // 6. 确定是否为模板函数
        let is_template = !method_generics.is_empty() || !type_generics.is_empty();
        fun = fun.with_template(is_template);

        // 7. 解析参数列表
        self.parse_special_token(Token::BraceLeft)?;

        // 7.1 检查是否有self参数（仅当有绑定类型时）
        let mut self_type = crate::ast::function::SelfType::None;
        let mut self_param = None;

        if has_binding {
            // 检查第一个参数是否是self相关参数
            let (token, _) = self.token_stream.peek();

            match token {
                Token::Identifier(id) if id == "self" => {
                    // 消费self标识符
                    self.parse_identifier()?;

                    // 设置self类型为值类型
                    self_type = crate::ast::function::SelfType::Value;

                    // 创建self参数
                    let binding_type = fun.get_binding();
                    let self_var = VariableDeclaration::new("self".to_string())
                        .with_type(Type::Alias(binding_type));
                    self_param = Some(self_var);

                    // 检查是否有逗号，表示后面还有其他参数
                    if self.try_parse_token(Token::Comma) {
                        // 继续解析其他参数
                    }
                }
                Token::BitAnd => {
                    // 消费&符号
                    self.parse_special_token(Token::BitAnd)?;

                    // 检查是否是self
                    let (token, _) = self.token_stream.peek();
                    if let Token::Identifier(id) = &token {
                        if id == "self" {
                            // 消费self标识符
                            self.parse_identifier()?;

                            // 设置self类型为引用类型
                            self_type = crate::ast::function::SelfType::Reference;

                            // 创建&self参数
                            let binding_type = fun.get_binding();
                            let self_var = VariableDeclaration::new("self".to_string())
                                .with_type(Type::Pointer(Box::new(Type::Alias(binding_type))));
                            self_param = Some(self_var);
                        } else {
                            return Err(Error::UnexpectedToken(
                                id.clone(),
                                "self".into(),
                                Position::none(),
                            ));
                        }
                    } else {
                        return Err(Error::UnexpectedToken(
                            token.to_string(),
                            "mut or self".into(),
                            Position::none(),
                        ));
                    }

                    // 检查是否有逗号，表示后面还有其他参数
                    if self.try_parse_token(Token::Comma) {
                        // 继续解析其他参数
                    }
                }
                _ => {
                    // 没有self参数，继续正常解析
                }
            }
        }

        // 7.2 解析其他参数
        let mut param_list = self.parse_param_list(ctx)?;

        // 如果有self参数，将其添加到参数列表的开头
        if let Some(self_var) = self_param {
            param_list.insert(0, self_var);
        }

        self.parse_special_token(Token::BraceRight)?;

        // 8. 解析返回类型
        let return_type = if self.try_parse_token(Token::Arrow) {
            self.parse_type()?
        } else {
            "Unit".into()
        };

        // 9. 构建并返回函数对象
        Ok(fun
            .with_name(name)
            .with_args(param_list)
            .with_generic_list(type_generics) // 使用类型泛型作为最终泛型列表
            .with_return_type(return_type)
            .with_self_type(self_type)) // 设置self参数类型
    }
    pub(crate) fn parse_extern_function_declaration(
        &mut self,
        ctx: &Context,
    ) -> crate::core::result::Result<()> {
        self.parse_keyword("extern")?;
        let fun = self.parse_function_declaration(ctx)?;
        let fun = fun.with_extern(true);
        let module_slot_map = ctx.get_module_slot_map();
        let mut module_slot_map = module_slot_map.write().unwrap();
        let module = module_slot_map.get_mut(self.current_module).unwrap();
        module.register_function(&fun.name(), fun);
        Ok(())
    }
    pub(crate) fn parse_param_list(
        &mut self,
        ctx: &Context,
    ) -> crate::core::result::Result<Vec<VariableDeclaration>> {
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
}
