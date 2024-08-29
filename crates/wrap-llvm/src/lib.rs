pub mod context;
pub mod module;
pub mod types;
mod executor;
pub mod builder;
pub mod function;
pub mod value;
pub mod global;



#[cfg(test)]
mod tests {
    use std::{ffi::{CString, c_char, c_void, CStr}, ptr};
    use std::task::Context;
    use std::thread::Scope;
    use llvm_sys::core::LLVMDumpModule;
    use llvm_sys::{target::LLVM_InitializeAllTargets, core::{LLVMContextCreate, LLVMDisposeModule, LLVMContextDispose, LLVMCreateMemoryBufferWithContentsOfFile, LLVMCreateMemoryBufferWithMemoryRangeCopy}, ir_reader::LLVMParseIRInContext, prelude::{LLVMContextRef, LLVMModuleRef}};

    use crate::{context::LLVMContext, module};
    use crate::global::Global;

    #[repr(C)]
    struct Any{
        id:i32,
        ptr:*mut i8,
    }
    extern "C" fn println(obj:Any){
        match obj.id {
            7=>{
                println!("A");
                let s = unsafe { CStr::from_ptr(obj.ptr as *const c_char) };
                println!("{}",s.to_str().unwrap());
            }
            4=>{
                let v =obj.ptr as i64;
                println!("{}",v);
            }
            3=>{
                let value = obj.ptr as *mut i32;
                unsafe { println!("{}", *value); }
            }
            t=>todo!("{t}")
        }

    }
    #[test]
    fn it_works() {
        let ctx = LLVMContext::with_jit();
        let llvm_ir = r#"
        ; ModuleID = 'main'
        source_filename = "main"

        @0 = private unnamed_addr constant [13 x i8] c"Hello,World!\00", align 1

        declare void @println({ i32, ptr } %0)

        define void @"$main.__main__"() {
        entry:
            ;%0 = load ptr, ptr @0, align 8
            %any = alloca { i32, ptr }, align 8
            store { i32, ptr } { i32 7, ptr @0 }, ptr %any, align 8
            call void @println(ptr %any)
            ret void
        }
        "#;
        let module = ctx.parse_ir(llvm_ir).expect("解析IR失败");
       let f1 = module.get_function_ref("println");
//        module.dump();
        let exec = module.create_executor().expect("创建执行器失败");

       exec.add_global_mapping(f1,println as *mut c_void);
        exec.run_function("$main.__main__", &mut []);
    }
    #[test]
    fn it_works2(){
        let jit_ctx = LLVMContext::with_jit();
        let mut module = jit_ctx.create_module("main");
        let main  = module.register_function("main",Global::function_type(Global::unit_type(), vec![]),
                                 vec![]);
        let entry = main.append_basic_block("entry");
        let builder = Global::create_builder();
        builder.position_at_end(entry);
        builder.build_return_void();
        builder.build_global_string_ptr("a","Hello,world!");
        module.dump()
    }
}
