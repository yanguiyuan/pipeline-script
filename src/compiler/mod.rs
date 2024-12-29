use crate::context::key::ContextKey;
use crate::context::scope::Scope;
use crate::context::Context;
use std::collections::HashMap;

use crate::llvm::context::LLVMContext;
use crate::llvm::global::Global;
use crate::llvm::module::LLVMModule;
use crate::llvm::types::LLVMType;
use crate::llvm::value::LLVMValue;

use crate::core::value::Value;
use crate::parser::expr::{Expr, ExprNode, Op};
use crate::parser::module::Module;
use crate::parser::r#type::Type;
use crate::parser::stmt::{Stmt, StmtNode};

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
    pub fn compile(&mut self) -> &mut LLVMModule {
        let background = Context::background();
        let ctx = Context::with_type_table(&background, HashMap::new());
        // 编译结构体
        for (name, item) in self.module.get_structs() {
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
        // 编译函数声明
        for (name, item) in self.module.get_functions() {
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
        for (name, item) in self.module.get_functions() {
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
            let flag = ctx.get_flag("return");
            if !flag {
                builder.build_return_void();
            }
        }
        // 编译主函数
        let main = self.llvm_module.register_function(
            "$main.__main__",
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
        for stmt in block {
            self.compile_stmt(stmt, &ctx);
        }
        let flag = ctx.get_flag("return");
        if !flag {
            builder.build_return_void();
        }
        &mut self.llvm_module
    }
    fn compile_type(&self, ty: &Type) -> LLVMType {
        match ty {
            Type::Int8 => Global::i8_type(),
            Type::Int16 => Global::i16_type(),
            Type::Int32 => Global::i32_type(),
            Type::Int64 => Global::i64_type(),
            Type::Unit => Global::unit_type(),
            Type::Pointer(i) => Global::pointer_type(i.as_llvm_type()),
            Type::Any => {
                let v = vec![Global::i32_type(), Global::pointer_type(Global::i8_type())];
                self.ctx.create_named_struct_type("Any", v)
            }
            Type::Array(t) => Global::struct_type(vec![
                Global::i64_type(),
                Global::pointer_type(t.as_llvm_type()),
            ]),
            Type::String => Global::pointer_type(Global::i8_type()),
            Type::Struct(name, s) => match name {
                None => {
                    let mut v = vec![];
                    for (_, t) in s.iter() {
                        v.push(t.as_llvm_type());
                    }
                    Global::struct_type(v)
                }
                Some(name) => {
                    let mut v = vec![];
                    for (_, t) in s.iter() {
                        v.push(t.as_llvm_type());
                    }
                    self.ctx.create_named_struct_type(name, v)
                }
            },
            Type::Function(ret, args) => {
                let mut v = vec![];
                for t in args.iter() {
                    v.push(t.as_llvm_type());
                }
                let t = ret.as_llvm_type();
                Global::struct_type(vec![
                    Global::pointer_type(Global::function_type(t, v)),
                    Global::pointer_type(Global::unit_type()),
                ])
            }
            Type::ArrayVarArg(t) => Global::struct_type(vec![
                Global::i64_type(),
                Global::pointer_type(self.compile_type(t)),
            ]),
            Type::Closure { ptr, env } => {
                let return_ty = self.compile_type(&ptr.0);
                let mut params_ty = vec![];
                for t in &ptr.1 {
                    params_ty.push(self.compile_type(t))
                }
                let mut env_ty = vec![];
                for a in env {
                    env_ty.push(self.compile_type(&a.1));
                }
                Global::pointer_type(Global::struct_type(vec![
                    Global::pointer_type(Global::function_type(return_ty, params_ty)),
                    Global::pointer_type(Global::struct_type(env_ty)),
                ]))
            }
            _ => panic!("Unknown type: {:?}", ty),
        }
    }
    fn get_type(&self, ctx: &Context, ty: &Type) -> LLVMType {
        let r = ctx.get_type(ty);
        match r {
            None => {
                let t = self.compile_type(ty);
                ctx.register_type(ty, &t);
                t
            }
            Some(t) => t,
        }
    }
    fn compile_stmt(&self, stmt: &StmtNode, ctx: &Context) {
        let builder = ctx.get_builder();
        match stmt.get_stmt() {
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
                // let ctx = Context::with_type(ctx, &t);
                let v = self.compile_expr(val.get_default().unwrap(), &ctx);
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
            Stmt::Assign(lhs, rhs) => {
                dbg!(&lhs);
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
                let builder = ctx.get_builder();
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
            Stmt::Noop => {}
            _ => todo!("compile stmt"),
        }
    }

    fn compile_expr_with_ptr(&self, expr: &ExprNode, ctx: &Context) -> Value {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let ty0 = expr.get_type().unwrap();
        match expr.get_expr() {
            Expr::Variable(name) => {
                let scope = ctx.get_scope();
                if scope.has(&name) {
                    return scope.get(name).unwrap();
                }
                let function = ctx.get_current_function();
                let a = function.get_param(name.as_ref());
                if a.is_some() {
                    let a = a.unwrap();
                    if ty0.is_struct() {
                        let ptr = builder.build_alloca(&name, &ty0.as_llvm_type());
                        builder.build_store(ptr, a);
                        ctx.get_scope().set(name, Value::new(ptr, ty0.clone()));
                        return Value::new(ptr, ty0);
                    }
                    return Value::new(a, ty0);
                }
                ctx.get_symbol(name).unwrap()
            }
            Expr::Index(target, index) => {
                let v = self.compile_expr_with_ptr(&target, ctx);
                let index = self.compile_expr(&index, ctx);
                Value::new(
                    builder.build_array_gep(ty0.as_llvm_type(), v.get_value(), index.get_value()),
                    ty0,
                )
            }
            Expr::Member(target, field_name) => {
                let v = self.compile_expr_with_ptr(&target, ctx);
                let ty = v.get_type();
                let (idx, _) = ty.get_struct_field(field_name).unwrap();
                Value::new(
                    builder.build_struct_gep(
                        target.get_type().unwrap().as_llvm_type(),
                        v.get_value(),
                        idx,
                    ),
                    ty0,
                )
            }
            Expr::Struct(_) => {
                let val = self.compile_expr(expr, ctx);
                let element_ty = ty0.get_element_type().unwrap();
                let ptr = builder.build_alloca("", &element_ty.as_llvm_type());
                builder.build_store(ptr, val.get_value());
                Value::new(ptr, ty0)
            }
            t => todo!("compile expr with ptr {t:?}"),
        }
    }
    fn compile_expr(&self, expr: &ExprNode, ctx: &Context) -> Value {
        let binding = ctx.get(ContextKey::Builder).unwrap();
        let builder = binding.as_builder();
        let ty0 = expr.get_type().unwrap();
        match expr.get_expr() {
            Expr::FnCall(fc) => {
                let name = &fc.name;
                let args = &fc.args;
                let mut llvm_args = vec![];
                let function_decl;
                let is_extern;
                let is_fn_param;
                if let Some(function) = self.module.get_function(name) {
                    function_decl = function.get_type();
                    is_extern = function.is_extern;
                    is_fn_param = false;
                } else {
                    let current_function = ctx.get_current_function();
                    let function_index = current_function.get_param_index(name).unwrap();
                    function_decl = ctx
                        .get_current_function_type()
                        .get_function_arg_type(function_index)
                        .unwrap();
                    is_extern = false;
                    is_fn_param = true;
                }
                // 处理传入的参数
                for (index, arg) in args.iter().enumerate() {
                    let t = function_decl.get_function_arg_type(index).unwrap();
                    let mut v = self.compile_expr(&arg.value, &ctx);
                    if is_extern
                        && !t.is_pointer()
                        && (t.is_struct() || t.is_array() || t.is_array_vararg())
                    {
                        let ty = self.get_type(ctx, &t);
                        let v0 = builder.build_alloca("", &ty);
                        builder.build_store(v0, v.value);
                        v = Value::new(v0, Type::Pointer(Box::new(t.clone())))
                    }
                    if t.is_any() {
                        let mut val = v.get_value();
                        if !(v.ty.is_pointer() || v.ty.is_string()) {
                            let ty = v.ty.as_llvm_type();
                            val = builder.build_alloca("", &ty);
                            builder.build_store(val, v.value);
                        }
                        let a = builder.build_alloca("any", &t.as_llvm_type());
                        builder.build_store(
                            a,
                            Global::const_struct(vec![Global::const_i32(v.get_type().id()), val]),
                        );
                        v = Value::new(a, t);
                    }
                    llvm_args.push(v.get_value());
                }
                // 特殊函数转换为指令
                match name.as_str() {
                    "int32" => {
                        let val = llvm_args.first().unwrap().clone();
                        let v = builder.build_zext(val, Global::i32_type());
                        return Value::new(v, Type::Int32);
                    }
                    "int64" => {
                        let val = llvm_args.first().unwrap().clone();
                        let v = builder.build_zext(val, Global::i64_type());
                        return Value::new(v, Type::Int64);
                    }
                    "sizeof" => {
                        if fc.generics.len() != 1 {
                            panic!("sizeof must have one generic");
                        }
                        let generic = &fc.generics[0];
                        let v = Global::sizeof(generic.as_llvm_type());
                        return Value::new(v, Type::Int64);
                    }
                    _ => {}
                }
                if is_fn_param {
                    let fn_struct = ctx.get_current_function().get_param(name).unwrap();
                    let fn_ptr = builder.build_struct_get(fn_struct, 0);
                    let extra_param = builder.build_struct_get(fn_struct, 1);
                    llvm_args.push(extra_param);
                    let v = builder.build_call_fn_ptr(
                        function_decl.as_llvm_type(),
                        fn_ptr,
                        llvm_args.as_mut_slice(),
                        "",
                    );
                    return Value::new(v, function_decl.get_function_return_type().unwrap());
                }
                let func = self.module.get_function(name).unwrap();
                if args.len() < func.args_count() {
                    for i in func.args().iter().skip(args.len()) {
                        let d = i.get_default().unwrap();
                        let v = self.compile_expr(&d, ctx);
                        llvm_args.push(v.get_value());
                    }
                }
                let ty = func.return_type();

                let llvm_func = self.llvm_module.get_function(name).unwrap();
                let v = builder.build_call(llvm_func, llvm_args.as_mut_slice(), "");
                Value::new(v, ty.clone())
            }
            Expr::Int(i) => {
                let v = match ty0 {
                    Type::Int8 => Global::const_i8(i as i8),
                    Type::Int16 => Global::const_i16(i as i16),
                    Type::Int32 => Global::const_i32(i as i32),
                    Type::Int64 => Global::const_i64(i),
                    _ => panic!("Unknown type: {:?}", ty0),
                };
                Value::new(v, ty0)
            }
            Expr::Float(f) => Value::new(Global::const_double(f), ty0),
            Expr::String(s, ..) => {
                let ptr = builder.build_global_string("", s);
                Value::new(ptr, ty0)
            }
            Expr::Binary(op, l, r) => {
                let l = self.compile_expr(&l, ctx).get_value();
                let r = self.compile_expr(&r, ctx).get_value();
                let v = match op {
                    Op::Plus => builder.build_add(l, r),
                    Op::Minus => builder.build_sub(l, r),
                    Op::Mul => builder.build_mul(l, r),
                    Op::Equal => builder.build_eq(l, r),
                    Op::NotEqual => builder.build_neq(l, r),
                    Op::Less => builder.build_less(l, r),
                    Op::Greater => builder.build_greater(l, r),
                    _ => todo!("compile binary"),
                };
                Value::new(v, ty0)
            }
            Expr::Variable(name) => {
                let scope = ctx.get_scope();
                if scope.has(&name) {
                    let ptr = scope.get(name).unwrap();
                    if ptr.get_type() == ty0 {
                        return ptr;
                    }
                    let ty = self.get_type(ctx, &ty0);
                    return Value::new(builder.build_load(ty, ptr.get_value()), ty0);
                }
                let param = ctx.get_current_function().get_param(name.as_ref());
                if param.is_some() {
                    return Value::new(param.unwrap(), ty0);
                }
                let ptr = ctx.get_symbol(&name).unwrap_or_else(|| {
                    let f = self.llvm_module.get_function(name).unwrap().as_ref();
                    return Value::new(LLVMValue::Pointer(f), ty0.clone());
                });
                if ptr.get_type() == ty0 {
                    return ptr;
                }
                if !ty0.is_pointer() {
                    let v0 = builder.build_load(ptr.ty.as_llvm_type(), ptr.value);
                    return Value::new(v0, ty0);
                }
                ptr
            }
            Expr::Array(v) => {
                let l = v.len();
                let mut llvm_args = vec![];
                let t = ty0.get_element_type().unwrap();
                let ty = self.get_type(ctx, t);
                for arg in v {
                    let mut v = self.compile_expr(&arg, &ctx);
                    if t.is_any() {
                        let val = v.get_value();

                        let temp = builder.build_struct_insert(
                            Global::undef(ty.clone()),
                            0,
                            Global::const_i32(v.get_type().id()),
                        );
                        let r = builder.build_struct_insert(temp, 1, val);
                        v = Value::new(r, t.clone());
                    }
                    llvm_args.push(v.get_value());
                }

                let v1 = builder.build_array(ty, llvm_args);
                let ty = self.get_type(ctx, &ty0);
                let tmp =
                    builder.build_struct_insert(Global::undef(ty), 0, Global::const_i64(l as i64));
                let v = builder.build_struct_insert(tmp, 1, v1);
                Value::new(v, ty0)
            }
            Expr::Index(target, index) => {
                let v = self.compile_expr_with_ptr(&target, ctx);
                let index = self.compile_expr(&index, ctx);
                let v =
                    builder.build_array_get(ty0.as_llvm_type(), v.get_value(), index.get_value());
                Value::new(v, ty0)
            }
            Expr::Struct(s) => {
                let (field_index_map, _) = self.llvm_module.get_struct(s.get_name()).unwrap();
                let mut props: Vec<(usize, Value)> = s
                    .get_props()
                    .iter()
                    .map(|(field_name, p)| {
                        let field_index = field_index_map.get(field_name).unwrap();
                        let ty = p.get_type().unwrap();
                        if ty.is_pointer() {
                            let v = self.compile_expr_with_ptr(p, &ctx);
                            return (*field_index, v);
                        }
                        let v = self.compile_expr(p, &ctx);
                        (*field_index, v)
                    })
                    .collect::<Vec<(usize, Value)>>();
                props.sort_by(|a, b| a.0.cmp(&b.0));
                let props = props
                    .into_iter()
                    .map(|(_, v)| v.get_value())
                    .collect::<Vec<LLVMValue>>();
                Value::new(Global::const_struct(props), ty0)
            }
            Expr::Member(target, field_name) => {
                let v = self.compile_expr(&target, ctx);
                let ty = v.get_type();
                if ty.is_pointer() {
                    let ty = ty.get_element_type().unwrap();
                    let val = builder.build_load(ty.as_llvm_type(), v.get_value());
                    let (idx, _) = ty.get_struct_field(field_name).unwrap();
                    let v = builder.build_struct_get(val, idx);
                    let v = Value::new(v, ty0);
                    return v;
                }
                let (idx, _) = ty.get_struct_field(field_name).unwrap();
                let v = builder.build_struct_get(v.get_value(), idx);
                Value::new(v, ty0)
            }
            Expr::Address(target) => self.compile_expr_with_ptr(&target, ctx),
            Expr::None => Value::new(Global::const_unit(), ty0),
            t => panic!("Unknown expr: {:?}", t),
        }
    }
}
