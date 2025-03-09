use crate::compiler::Compiler;
use crate::context::Context;

use crate::llvm::global::Global;
use crate::core::value::Value;
use crate::parser::expr::Expr;
use crate::parser::r#type::Type;
use crate::parser::stmt::{Stmt, StmtNode};

impl Compiler {
    pub(crate) fn compile_stmt(&self, stmt: &StmtNode, ctx: &Context) {
        let builder = ctx.get_builder();
        match &stmt.get_stmt() {
            Stmt::EvalExpr(expr) => {
                self.compile_expr(&expr, ctx);
            }
            Stmt::Return(expr) => {
                let v = self.compile_expr(&expr, ctx);
                ctx.set_flag("return", true);
                builder.build_return(v.value);
            }
            Stmt::ValDecl(val) => {
                let t = val.r#type().unwrap();
                let v = self.compile_expr(val.get_default().unwrap(), ctx);
                if t.is_array() {
                    return ctx.set_symbol(val.name(), Value::new(v.value, t.clone()));
                }
                if v.value.is_pointer() {
                    let v = Value::new(v.value, t.clone());
                    ctx.set_symbol(val.name(), Value::new(v.value, t.clone()));
                    return;
                }
                ctx.set_symbol(val.name(), v);
            }
            Stmt::VarDecl(val) => {
                let t = val.r#type().unwrap();
                dbg!(&t);
                let alloc = builder.build_alloca(val.name(), &self.compile_type(t.get_element_type().unwrap()));
                
                if let Some(default_expr) = val.get_default() {
                    let default_value = self.compile_expr(default_expr, ctx);
                    builder.build_store(alloc, default_value.value);
                }
                
                ctx.set_symbol(val.name(), Value::new(alloc, t));
            }
            Stmt::Assign(lhs, rhs) => {
                let lhs = self.compile_expr_with_ptr(&lhs, ctx);
                let rhs = self.compile_expr(&rhs, ctx);
                builder.build_store(lhs.value, rhs.value);
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
                    builder.build_cond_br(condition.value, then_bb, else_bb);
                    builder.position_at_end(then_bb);
                    let body = i.get_body();
                    for i in body {
                        self.compile_stmt(i, ctx)
                    }
                    builder.build_br(merge_bb);
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
                let cond = self.compile_expr(&condition, ctx);
                builder.build_cond_br(cond.value, while_body_bb, while_exit_bb);
                builder.position_at_end(while_body_bb);
                for i in body {
                    self.compile_stmt(&i, ctx);
                }
                builder.build_br(while_cond_bb);
                builder.position_at_end(while_exit_bb)
            }
            Stmt::ForIn(_, expr, body) => {
                let current_function = ctx.get_current_function();
                let while_cond_bb = current_function.append_basic_block("while_cond");
                let while_body_bb = current_function.append_basic_block("while_body");
                let while_exit_bb = current_function.append_basic_block("while_exit");
                builder.build_br(while_cond_bb);
                builder.position_at_end(while_cond_bb);
                let cond = self.compile_expr(&expr, ctx);
                builder.build_cond_br(cond.value, while_body_bb, while_exit_bb);
                builder.position_at_end(while_body_bb);
                // let var = self.compile_expr_with_ptr(var, ctx);
                for i in body {
                    self.compile_stmt(&i, ctx);
                }
                builder.build_br(while_cond_bb);
                builder.position_at_end(while_exit_bb)
            }
            Stmt::Noop => {}
            Stmt::Match(expr, branches) => {
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
                
                // 为每个分支创建条件判断
                for (i, branch) in branches.iter().enumerate() {
                    let pattern = branch.get_pattern();
                    
                    // 检查模式是否是枚举变体
                    if let Expr::EnumVariant(enum_name, variant_name, _) = &pattern.get_expr() {
                        // 获取枚举类型
                        let enum_type = match ctx.get_type_alias(enum_name) {
                            Some(ty) => ty,
                            None => {
                                // 如果找不到枚举类型，使用默认行为
                                println!("Warning: Enum type '{}' not found", enum_name);
                                // 默认走else分支
                                builder.build_br(end_block);
                                // 设置当前基本块为if_let_end
                                builder.position_at_end(end_block);
                                return;
                            }
                        };
                        
                        // 获取变体索引
                        let mut variant_index = 0;
                        if let Type::Enum(_, variants) = &enum_type {
                            for (j, (name, _)) in variants.iter().enumerate() {
                                if name == variant_name {
                                    variant_index = j;
                                    break;
                                }
                            }
                        }
                        
                        // 获取枚举值的标签（第一个字段）
                        let tag_ptr = builder.build_struct_gep(self.compile_type(&value.ty), value.value, 0);
                        let tag = builder.build_load(Global::i32_type(), tag_ptr);
                        
                        // 创建条件：tag == variant_index
                        let cond = builder.build_eq(tag, Global::const_i32(variant_index as i32));
                        
                        // 创建下一个分支的基本块
                        let next_block = if i < branches.len() - 1 {
                            branch_blocks[i + 1]
                        } else {
                            end_block
                        };
                        
                        // 创建条件分支
                        builder.build_cond_br(cond, branch_blocks[i], next_block);
                        dbg!(i);
                        // 编译分支体
                        builder.position_at_end(branch_blocks[i]);
                        
                        // 如果模式有关联值，需要提取并绑定
                        if let Expr::EnumVariant(_, _, Some(binding)) = &pattern.get_expr() {
                            if let Expr::Variable(name) = &binding.get_expr() {
                                // 获取枚举值的数据（第二个字段）
                                let data_ptr = builder.build_struct_gep(self.compile_type(&value.ty), value.value, 1);
                                let data_type = self.compile_type(&value.ty).get_struct_field_type(1);
                                let data = builder.build_load(data_type.clone(), data_ptr);
                                //
                                // // 绑定变量
                                // let var_ptr = builder.build_alloca(name, &data_type);
                                // builder.build_store(var_ptr, data);
                                dbg!(Type::from(data_type.clone()));
                                // 将变量添加到上下文
                                ctx.set_symbol(name.clone(), Value::new(data,Type::from(data_type)));
                            }
                        }
                        
                        // 编译分支体
                        for stmt in branch.get_body() {
                            self.compile_stmt(stmt, ctx);
                        }
                        
                        // 跳转到匹配结束的基本块
                        builder.build_br(end_block);
                    }
                }
                
                // 设置当前基本块为匹配结束的基本块
                builder.position_at_end(end_block);
            }
            Stmt::IfLet(pattern, expr, body, else_body) => {
                // 编译匹配的表达式
                let value = self.compile_expr(expr, ctx);
                
                // 创建基本块
                let function = ctx.get_current_function();
                let then_block = function.append_basic_block("if_let_then");
                let else_block = function.append_basic_block("if_let_else");
                let end_block = function.append_basic_block("if_let_end");
                
                // 检查模式是否是枚举变体
                if let Expr::EnumVariant(enum_name, variant_name, _) = &pattern.get_expr() {
                    // 获取枚举类型
                    let enum_type = ctx.get_type_alias(enum_name).unwrap();
                    
                    // 获取变体索引
                    let mut variant_index = 0;
                    if let Type::Enum(_, variants) = &enum_type {
                        for (j, (name, _)) in variants.iter().enumerate() {
                            if name == variant_name {
                                variant_index = j;
                                break;
                            }
                        }
                    }
                    
                    // 获取枚举值的标签（第一个字段）
                    let tag_ptr = builder.build_struct_gep(self.compile_type(&value.ty), value.value, 0);
                    let tag = builder.build_load(Global::i32_type(), tag_ptr);
                    
                    // 创建条件：tag == variant_index
                    let cond = builder.build_eq(tag, Global::const_i32(variant_index as i32));
                    
                    // 创建条件分支
                    builder.build_cond_br(cond, then_block, else_block);
                    
                    // 编译then分支
                    builder.position_at_end(then_block);
                    
                    // 如果模式有关联值，需要提取并绑定
                    if let Expr::EnumVariant(_, _, Some(binding)) = &pattern.get_expr() {
                        if let Expr::Variable(name) = &binding.get_expr() {
                            // 获取枚举值的数据（第二个字段）
                            let data_ptr = builder.build_struct_gep(self.compile_type(&value.ty), value.value, 1);
                            dbg!(&data_ptr);
                            let data_type = self.compile_type(&value.ty).get_struct_field_type(1);
                            // let data = builder.build_load(data_type.clone(), data_ptr);
                            
                            // // 绑定变量
                            // let var_ptr = builder.build_alloca(name, &data_type);
                            // builder.build_store(var_ptr, data);
                            
                            // 将变量添加到上下文
                            ctx.set_symbol(name.clone(), Value::new(data_ptr, Type::Ref(Box::new(Type::from(data_type)))));
                        }
                    }
                    
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
                    
                    // 跳转到结束基本块
                    builder.build_br(end_block);
                    
                    // 设置当前基本块为结束基本块
                    builder.position_at_end(end_block);
                }
            }
            _ => todo!("compile stmt"),
        }
    }
}
