mod expr;
mod stmt;
mod r#type;

use crate::context::Context;
use crate::llvm::context::LLVMContext;
use crate::llvm::global::Global;
use crate::llvm::module::LLVMModule;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;

use crate::ast::module::Module;
use crate::context::key::ContextKey;
use crate::context::value::ContextValue;
use crate::llvm::types::LLVMType;
use crate::llvm::value::fucntion::FunctionValue;
use crate::llvm::value::LLVMValue;

pub struct Compiler {
    module: Module,
    ctx: LLVMContext,
    builtin_symbol: HashMap<String, LLVMValue>,
    llvm_module: Rc<RwLock<LLVMModule>>,
}

impl Compiler {
    pub fn new(module: Module) -> Self {
        let llvm_ctx = LLVMContext::new();
        let llvm_module = llvm_ctx.create_module(module.get_name());
        Self {
            ctx: llvm_ctx,
            module,
            llvm_module: Rc::new(RwLock::new(llvm_module)),
            builtin_symbol: HashMap::new(),
        }
    }
    pub fn register_builtin_symbol(&mut self, name: &str, ty: LLVMValue) {
        self.builtin_symbol.insert(name.to_string(), ty);
    }
    pub fn register_llvm_function(&mut self, name: &str, ty: LLVMType, arg_names: Vec<String>) {
        self.llvm_module
            .write()
            .unwrap()
            .register_function(name, ty, arg_names);
    }
    pub fn compile(&mut self, ctx: &Context) -> Rc<RwLock<LLVMModule>> {
        let ctx = Context::with_type_table(ctx, HashMap::new());
        // 顶层作用域
        let ctx = Context::with_scope(&ctx);
        let ctx = Context::with_value(
            &ctx,
            ContextKey::LLVMModule,
            ContextValue::LLVMModule(self.llvm_module.clone()),
        );
        // 编译内建符号进入scope
        for (name, ty) in self.builtin_symbol.iter() {
            ctx.set_symbol(name.clone(), ty.clone());
        }
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
            self.llvm_module
                .write()
                .unwrap()
                .register_struct(name, field_index, t);
        }
        // 编译枚举
        for (name, item) in self.module.get_type_aliases().iter() {
            let t = self.compile_type(item.get_type());
            self.llvm_module
                .write()
                .unwrap()
                .register_struct(name, HashMap::new(), t);
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
                t = Global::function_type_with_var_arg(return_type.clone(), arg_types);
            } else {
                t = Global::function_type(return_type.clone(), arg_types);
            }
            let f = if item.is_extern {
                self.llvm_module
                    .write()
                    .unwrap()
                    .register_extern_function(name, t)
            } else {
                let args = item.args();
                let param_names: Vec<String> =
                    args.iter().map(|arg| arg.name()).collect::<Vec<_>>();
                self.llvm_module
                    .write()
                    .unwrap()
                    .register_function(name, t, param_names)
            };
            let function_value = FunctionValue::new(
                f.get_function_ref(),
                name.clone(),
                Box::new(return_type.get_undef()),
                args.iter()
                    .map(|arg| {
                        (
                            arg.name().clone(),
                            self.compile_type(&arg.r#type().unwrap()).get_undef(),
                        )
                    })
                    .collect(),
            );
            ctx.set_symbol(name.clone(), function_value.into());
        }

        let builder = Global::create_builder();
        let ctx = Context::with_builder(&ctx, builder);
        // 编译函数实现
        for (_, item) in self.module.get_functions().iter() {
            if item.is_extern || item.is_template {
                continue;
            }
            let ctx = self.prepare_function(&ctx, item);
            for stmt in item.body() {
                self.compile_stmt(stmt, &ctx);
            }
            let flag = ctx.get_flag("return").unwrap();
            if !flag {
                let builder = ctx.get_builder();
                builder.build_return_void();
            }
        }
        // 编译主函数
        let main = self.llvm_module.write().unwrap().register_function(
            "$Module.main",
            Global::function_type(Global::unit_type(), vec![]),
            vec![],
        );
        let block = self.module.get_global_block();

        let entry = main.append_basic_block("entry");
        let builder = ctx.get_builder();
        builder.position_at_end(entry);
        let function_value = FunctionValue::new(
            main.get_function_ref(),
            "$Module.main".into(),
            Box::new(Global::unit_type().get_undef()),
            vec![],
        );
        let ctx = Context::with_function(&ctx, function_value);
        let ctx = Context::with_scope(&ctx);
        let ctx = Context::with_flag(&ctx, "return", false);
        for stmt in block.iter() {
            self.compile_stmt(stmt, &ctx);
        }
        let flag = ctx.get_flag("return").unwrap();
        if !flag {
            builder.build_return_void();
        }
        self.llvm_module.clone()
    }
    pub fn prepare_function(
        &self,
        ctx: &Context,
        function: &crate::ast::function::Function,
    ) -> Context {
        let function_value = ctx
            .get_symbol(function.name())
            .unwrap()
            .as_function()
            .unwrap();
        let entry = function_value.append_basic_block("entry");
        let builder = ctx.get_builder();
        builder.position_at_end(entry);
        let ctx = Context::with_function(&ctx, function_value.clone());
        let ctx = Context::with_type(&ctx, "current_function".into(), function.get_type());
        let ctx = Context::with_scope(&ctx);
        let ctx = Context::with_flag(&ctx, "return", false);
        // 注册形参进入作用域
        for arg in function.args() {
            let arg_name = arg.name();
            let arg_value = function_value.get_param(arg_name.clone()).unwrap();
            ctx.set_symbol(arg_name, arg_value);
        }
        ctx
    }
}
