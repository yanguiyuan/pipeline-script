mod expr;
mod helper;
mod stmt;
mod r#type;

use crate::context::scope::Scope;
use crate::context::Context;
use std::collections::HashMap;

use crate::llvm::context::LLVMContext;
use crate::llvm::global::Global;
use crate::llvm::module::LLVMModule;

use crate::ast::module::Module;

pub struct Compiler {
    module: Module,
    ctx: LLVMContext,
    llvm_module: LLVMModule,
}

impl Compiler {
    pub fn new(module: Module) -> Self {
        let ctx = LLVMContext::new();
        let llvm_module = ctx.create_module(module.get_name());
        Self {
            ctx,
            module,
            llvm_module,
        }
    }
    pub fn compile(&mut self, ctx: &Context) -> &mut LLVMModule {
        let ctx = Context::with_type_table(ctx, HashMap::new());
        // 编译结构体
        for (name, item) in self.module.get_structs() {
            if !item.generics.is_empty() {
                continue;
            }
            let fields = item.get_fields();
            let mut struct_type = vec![];
            let mut field_index: HashMap<String, usize> = HashMap::new();
            for (i, field) in fields.iter().enumerate() {
                let t = self.get_type(&ctx, &field.field_type);
                struct_type.push(t);
                field_index.insert(field.name.clone(), i);
            }
            let t = item.get_type();
            let t = self.get_type(&ctx, &t);
            self.llvm_module.register_struct(name, field_index, t);
        }
        // 编译枚举
        for (name, item) in self.module.get_type_aliases().iter() {
            let t = self.compile_type(item.get_type());
            self.llvm_module.register_struct(name, HashMap::new(), t);
        }
        // 编译函数声明
        for (name, item) in self.module.get_functions().iter() {
            if item.is_template {
                continue;
            }
            let args = item.args();
            let mut arg_types = vec![];
            let mut is_var_arg = false;
            for arg in args {
                if arg.is_var_arg() {
                    is_var_arg = true;
                    continue;
                }
                let ty = arg.r#type().unwrap();
                let t = self.get_type(&ctx, &ty);
                arg_types.push(t);
            }
            let t;
            let return_type0 = item.return_type();
            let return_type = self.get_type(&ctx, return_type0);
            if is_var_arg {
                t = Global::function_type_with_var_arg(return_type, arg_types);
            } else {
                t = Global::function_type(return_type, arg_types);
            }
            if item.is_extern {
                self.llvm_module.register_extern_function(name, t);
            } else {
                let args = item.args();
                let param_names: Vec<String> =
                    args.iter().map(|arg| arg.name()).collect::<Vec<_>>();
                self.llvm_module.register_function(name, t, param_names);
            }
        }

        let builder = Global::create_builder();
        let ctx = Context::with_builder(&ctx, builder);
        // 编译函数实现
        for (name, item) in self.module.get_functions().iter() {
            if item.is_extern || item.is_template {
                continue;
            }
            let function = self.llvm_module.get_function(name).unwrap();
            let entry = function.append_basic_block("entry");
            let builder = ctx.get_builder();
            builder.position_at_end(entry);
            let ctx = Context::with_function(&ctx, function.clone());
            let ctx = Context::with_type(&ctx, "current_function".into(), item.get_type());
            let ctx = Context::with_scope(&ctx, Scope::new());
            let ctx = Context::with_flag(&ctx, "return", false);

            for stmt in item.body() {
                self.compile_stmt(stmt, &ctx);
            }
            let flag = ctx.get_flag("return").unwrap();
            if !flag {
                builder.build_return_void();
            }
        }
        // 编译主函数
        let main = self.llvm_module.register_function(
            "$Module.main",
            Global::function_type(Global::unit_type(), vec![]),
            vec![],
        );
        let block = self.module.get_global_block();

        let entry = main.append_basic_block("entry");
        let builder = ctx.get_builder();
        builder.position_at_end(entry);
        let ctx = Context::with_function(&ctx, main);
        let ctx = Context::with_scope(&ctx, Scope::new());
        let ctx = Context::with_flag(&ctx, "return", false);
        for stmt in block.iter() {
            self.compile_stmt(stmt, &ctx);
        }
        let flag = ctx.get_flag("return").unwrap();
        if !flag {
            builder.build_return_void();
        }
        &mut self.llvm_module
    }
}
