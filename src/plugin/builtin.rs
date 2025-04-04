use crate::ast::r#type::Type;
use crate::core::builtin::{cmd, panic, println};
use crate::core::engine::Engine;
use crate::llvm::global::Global;
use crate::llvm::value::fucntion::FunctionValue;
use crate::llvm::value::LLVMValue;
use crate::plugin::Plugin;
use std::ffi::c_void;

pub struct BuiltinPlugin;
impl Plugin for BuiltinPlugin {
    fn apply(self, e: &mut Engine) {
        e.register_external_function("println", println as *mut c_void);
        e.register_external_function("cmd", cmd as *mut c_void);
        e.register_external_function("panic", panic as *mut c_void);
        e.register_builtin_symbol_type("sizeof", Type::Function(Box::new(Type::Int64), vec![]));
        let sizeof_name = "sizeof".to_string();
        let sizeof_args = vec![];
        let reference = Global::unit_type().get_undef().as_llvm_value_ref();
        let sizeof_reference = LLVMValue::Function(FunctionValue::new(
            reference,
            sizeof_name.clone(),
            Box::new(Global::i64_type().get_undef()),
            sizeof_args,
        ));
        e.register_builtin_symbol("sizeof", sizeof_reference);
    }
}
