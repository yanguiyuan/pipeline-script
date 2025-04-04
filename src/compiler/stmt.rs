use crate::ast::expr::{Expr, ExprNode};
use crate::ast::r#type::Type;
use crate::ast::stmt::{MatchBranch, Stmt, StmtNode};
use crate::compiler::Compiler;
use crate::context::Context;
use crate::llvm::global::Global;
use crate::llvm::value::LLVMValue;
use llvm_sys::prelude::LLVMBasicBlockRef;

impl Compiler {
    pub(crate) fn compile_stmt(&self, stmt: &StmtNode, ctx: &Context) {
        let builder = ctx.get_builder();
        match &stmt.get_stmt() {
            Stmt::EvalExpr(expr) => {
                self.compile_expr(expr, ctx);
            }
            Stmt::Return(expr) => {
                let v = self.compile_expr(expr, ctx);
                ctx.set_flag("return", true);
                builder.build_return(v);
            }
            Stmt::ValDecl(val) => {
                let t = val.r#type().unwrap();
                let v = self.compile_expr(val.get_default().unwrap(), ctx);
                if t.is_array() {
                    return ctx.set_symbol(val.name(), v);
                }
                if v.is_pointer() {
                    ctx.set_symbol(val.name(), v);
                    return;
                }
                ctx.set_symbol(val.name(), v);
            }
            Stmt::VarDecl(val) => {
                let t = val.r#type().unwrap();
                let element_type = t.get_element_type().unwrap();
                let alloc = builder.build_alloca(val.name(), &self.get_type(ctx, &element_type));
                let default_expr = val.get_default().unwrap();
                let default_value = self.compile_expr(default_expr, ctx);
                let result = alloc.as_reference().unwrap();
                result.store(ctx, default_value);
                // builder.build_store(alloc.clone(), default_value);

                ctx.set_symbol(val.name(), alloc);
            }
            Stmt::Assign(lhs, rhs) => {
                let lhs = self.compile_expr_with_ptr(lhs, ctx);
                let rhs = self.compile_expr(rhs, ctx);
                lhs.as_reference().unwrap().store(ctx, rhs);
            }
            Stmt::If(if_stmt) => {
                let branches = if_stmt.get_branches();
                let current_function = ctx.get_current_function();
                let merge_bb = current_function.append_basic_block("merge");
                for i in branches {
                    let condition = i.get_condition();
                    let condition = self.compile_expr(condition, ctx);
                    let builder = ctx.get_builder();
                    let then_bb = current_function.append_basic_block("then");
                    let else_bb = current_function.append_basic_block("else");
                    builder.build_cond_br(condition, then_bb, else_bb);
                    builder.position_at_end(then_bb);
                    let body = i.get_body();
                    let ctx = Context::with_flag(ctx, "break", false);
                    for i in body {
                        self.compile_stmt(i, &ctx)
                    }
                    if !ctx.get_flag("return").unwrap() && !ctx.get_flag("break").unwrap() {
                        builder.build_br(merge_bb);
                    }
                    builder.position_at_end(else_bb);
                }

                let else_block = if_stmt.get_else_body();
                if let Some(else_block) = else_block {
                    for i in &else_block {
                        self.compile_stmt(i, ctx);
                    }
                }
                builder.build_br(merge_bb);
                builder.position_at_end(merge_bb)
            }
            Stmt::While(condition, body) => {
                let current_function = ctx.get_current_function();
                let while_cond_bb = current_function.append_basic_block("while_cond");
                let while_body_bb = current_function.append_basic_block("while_body");
                let while_exit_bb = current_function.append_basic_block("while_exit");
                builder.build_br(while_cond_bb);
                builder.position_at_end(while_cond_bb);
                let cond = self.compile_expr(condition, ctx);
                builder.build_cond_br(cond, while_body_bb, while_exit_bb);
                builder.position_at_end(while_body_bb);
                // 创建新的上下文，包含循环的基本块
                let ctx = Context::with_loop_block(ctx, "while_exit".to_string(), while_exit_bb);
                let ctx = Context::with_flag(&ctx, "break", false);
                for i in body {
                    self.compile_stmt(i, &ctx);
                }
                if !ctx.get_flag("break").unwrap() {
                    builder.build_br(while_cond_bb);
                }
                builder.position_at_end(while_exit_bb)
            }
            Stmt::ForIn(_, expr, body) => {
                let current_function = ctx.get_current_function();
                let while_cond_bb = current_function.append_basic_block("while_cond");
                let while_body_bb = current_function.append_basic_block("while_body");
                let while_exit_bb = current_function.append_basic_block("while_exit");
                builder.build_br(while_cond_bb);
                builder.position_at_end(while_cond_bb);
                let cond = self.compile_expr(expr, ctx);
                builder.build_cond_br(cond, while_body_bb, while_exit_bb);
                builder.position_at_end(while_body_bb);
                // 创建新的上下文，包含循环的基本块
                let ctx = Context::with_loop_block(ctx, "while_exit".to_string(), while_exit_bb);
                for i in body {
                    self.compile_stmt(i, &ctx);
                }
                builder.build_br(while_cond_bb);
                builder.position_at_end(while_exit_bb)
            }
            Stmt::Noop => {}
            Stmt::Match(expr, branches) => {
                self.compile_match_statement(expr, branches, ctx);
            }
            Stmt::IfLet(pattern, expr, body, else_body) => {
                self.compile_if_let_statement(pattern, expr, body, else_body, ctx);
            }
            Stmt::IfConst(pattern, expr, body, else_body) => {
                self.compile_if_const_statement(pattern, expr, body, else_body, ctx);
            }
            Stmt::Break => {
                let builder = ctx.get_builder();
                // 获取最近的循环退出基本块
                if let Some(while_exit_bb) = ctx.get_loop_block("while_exit") {
                    builder.build_br(while_exit_bb);
                    ctx.set_flag("break", true);
                }
            }
            _ => todo!("compile stmt"),
        }
    }

    // 编译match语句
    fn compile_match_statement(&self, expr: &ExprNode, branches: &[MatchBranch], ctx: &Context) {
        let builder = ctx.get_builder();
        // 编译匹配的表达式
        let value = self.compile_expr(expr, ctx);

        // 创建匹配结束的基本块
        let function = ctx.get_current_function();
        let end_block = function.append_basic_block("match_end");

        // 创建分支基本块
        let mut branch_blocks = vec![];
        for _ in branches.iter() {
            let block = function.append_basic_block("match_branch");
            branch_blocks.push(block);
        }
        self.compile_match_branchs(branches, &value, &branch_blocks, end_block, ctx);
        // 设置当前基本块为匹配结束的基本块
        builder.position_at_end(end_block);
    }
    fn compile_enum_variant_index(
        &self,
        enum_name: &str,
        variant_name: &str,
        ctx: &Context,
    ) -> usize {
        // 获取枚举类型
        let enum_type = ctx.get_type_alias(enum_name).unwrap();
        // 获取变体索引
        let mut variant_index = 0;
        if let Type::Enum(_, variants) = enum_type.get_type() {
            for (j, (name, _)) in variants.iter().enumerate() {
                if name == variant_name {
                    variant_index = j;
                    break;
                }
            }
        }
        variant_index
    }
    fn compile_match_branch_binding(
        &self,
        binding: &Option<Box<ExprNode>>,
        value: &LLVMValue,
        ctx: &Context,
    ) {
        // 如果模式有关联值，需要提取并绑定
        if let Some(binding) = binding {
            if let Expr::Variable(name) = &binding.get_expr() {
                // 获取枚举值的数据（第二个字段）
                let builder = ctx.get_builder();
                let data = builder.build_struct_get(value.as_struct().unwrap(), 1);
                // 将变量添加到上下文
                ctx.set_symbol(name.clone(), data);
            }
        }
    }
    fn compile_match_branch_pointer_binding(
        &self,
        binding: &Option<Box<ExprNode>>,
        value: &LLVMValue,
        ctx: &Context,
    ) {
        if let Some(binding) = binding {
            if let Expr::Variable(name) = &binding.get_expr() {
                // 获取枚举值的数据（第二个字段）
                let data_ptr = value
                    .as_reference()
                    .unwrap()
                    .get_enum_variant_data_ptr()
                    .unwrap();

                // 将变量添加到上下文
                ctx.set_symbol(name.clone(), data_ptr);
            }
        }
    }
    // 编译match分支
    fn compile_match_branchs(
        &self,
        branches: &[MatchBranch],
        value: &LLVMValue,
        branch_blocks: &[LLVMBasicBlockRef],
        end_block: LLVMBasicBlockRef,
        ctx: &Context,
    ) {
        for (branch_index, branch) in branches.iter().enumerate() {
            let pattern = branch.get_pattern();

            // 检查模式是否是枚举变体
            if let Expr::EnumVariant(enum_name, variant_name, binding) = &pattern.get_expr() {
                let builder = ctx.get_builder();
                // 获取变体索引
                let variant_index = self.compile_enum_variant_index(enum_name, variant_name, ctx);
                // 获取枚举值的标签（第一个字段）
                let tag = builder.build_struct_get(value.as_struct().unwrap(), 0);
                // 创建条件：tag == variant_index
                let cond = builder.build_eq(tag, Global::const_i32(variant_index as i32).into());
                // 创建下一个分支的基本块
                let next_block = if branch_index < branches.len() - 1 {
                    branch_blocks[branch_index + 1]
                } else {
                    end_block
                };

                // 创建条件分支
                builder.build_cond_br(cond, branch_blocks[branch_index], next_block);

                // 编译分支体
                builder.position_at_end(branch_blocks[branch_index]);

                // 如果模式有关联值，需要提取并绑定
                self.compile_match_branch_binding(binding, value, ctx);

                // 编译分支体
                for stmt in branch.get_body() {
                    self.compile_stmt(stmt, ctx);
                }

                // 跳转到匹配结束的基本块
                builder.build_br(end_block);
            }
        }
    }
    // 编译if-let语句
    fn compile_if_let_statement(
        &self,
        pattern: &ExprNode,
        expr: &ExprNode,
        body: &[StmtNode],
        else_body: &Option<Vec<StmtNode>>,
        ctx: &Context,
    ) {
        let builder = ctx.get_builder();
        // 编译匹配的表达式
        let value = self.compile_expr_with_ptr(expr, ctx);

        // 创建基本块
        let function = ctx.get_current_function();
        let then_block = function.append_basic_block("if_let_then");
        let else_block = function.append_basic_block("if_let_else");
        let end_block = function.append_basic_block("if_let_end");

        // 检查模式是否是枚举变体
        if let Expr::EnumVariant(enum_name, variant_name, binding) = &pattern.get_expr() {
            // 获取变体索引
            let variant_index = self.compile_enum_variant_index(enum_name, variant_name, ctx);

            // 获取枚举值的标签（第一个字段）

            let tag = value
                .as_reference()
                .unwrap()
                .get_value(ctx)
                .as_enum_variant()
                .unwrap()
                .get_tag();

            // 创建条件：tag == variant_index
            let cond = tag.eq(&Global::const_i32(variant_index as i32));

            // 创建条件分支
            builder.build_cond_br(cond.into(), then_block, else_block);

            // 编译then分支
            builder.position_at_end(then_block);

            // 如果模式有关联值，需要提取并绑定
            self.compile_match_branch_pointer_binding(binding, &value, ctx);

            // 编译then分支体
            for stmt in body {
                self.compile_stmt(stmt, ctx);
            }

            // 跳转到结束基本块
            builder.build_br(end_block);

            // 编译else分支
            builder.position_at_end(else_block);
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    self.compile_stmt(stmt, ctx);
                }
            }
            if !ctx.get_flag("return").unwrap() {
                // 跳转到结束基本块
                builder.build_br(end_block);
            }

            // 设置当前基本块为结束基本块
            builder.position_at_end(end_block);
        }
    }
    fn compile_if_const_statement(
        &self,
        pattern: &ExprNode,
        expr: &ExprNode,
        body: &[StmtNode],
        else_body: &Option<Vec<StmtNode>>,
        ctx: &Context,
    ) {
        let builder = ctx.get_builder();
        // 编译匹配的表达式
        let value = self.compile_expr(expr, ctx);

        // 创建基本块
        let function = ctx.get_current_function();
        let then_block = function.append_basic_block("if_const_then");
        let else_block = function.append_basic_block("if_const_else");
        let end_block = function.append_basic_block("if_const_end");

        // 检查模式是否是枚举变体
        if let Expr::EnumVariant(enum_name, variant_name, binding) = &pattern.get_expr() {
            // 获取变体索引
            let variant_index = self.compile_enum_variant_index(enum_name, variant_name, ctx);

            // 获取枚举值的标签（第一个字段）
            let tag = builder.build_struct_get(value.as_struct().unwrap(), 0);

            // 创建条件：tag == variant_index
            let cond = builder.build_eq(tag, Global::const_i32(variant_index as i32).into());

            // 创建条件分支
            builder.build_cond_br(cond, then_block, else_block);

            // 编译then分支
            builder.position_at_end(then_block);

            // 如果模式有关联值，需要提取并绑定
            self.compile_match_branch_binding(binding, &value, ctx);

            // 编译then分支体
            for stmt in body {
                self.compile_stmt(stmt, ctx);
            }
            if !ctx.get_flag("return").unwrap() {
                // 跳转到结束基本块
                builder.build_br(end_block);
            }

            // 编译else分支
            builder.position_at_end(else_block);
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    self.compile_stmt(stmt, ctx);
                }
            }

            // 跳转到结束基本块
            builder.build_br(end_block);

            // 设置当前基本块为结束基本块
            builder.position_at_end(end_block);
        }
    }
}
