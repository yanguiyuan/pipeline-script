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
    use crate::{context::LLVMContext, module};
    use crate::global::Global;
    #[repr(C)]
    struct Array{
        len:i64,
        ptr:*mut Any,
    }
    #[repr(C)]
    struct Any{
        id:i32,
        ptr:*mut c_void,
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
    extern "C" fn append(obj:Array){
        for i in 0..obj.len {
            let obj = unsafe { obj.ptr.offset(i as isize) };
            unsafe {
                match (*obj).id {
                    7 => {
                        let s = unsafe { CStr::from_ptr((*obj).ptr as *const c_char) };
                        print!("{}", s.to_str().unwrap());
                    }
                    4 => {
                        let v = (*obj).ptr as i64;
                        print!("{}", v);
                    }
                    3 => {
                        let value = (*obj).ptr as i32;
                        unsafe { print!("{}", *value); }
                    }
                    t => todo!("{t}")
                }
            }
        }
    }
    #[test]
    fn it_works() {
        let ctx = LLVMContext::with_jit();
        let llvm_ir = r#"
  ; ModuleID = 'main'
source_filename = "main"

%Any = type { i32, ptr }
%Person = type { ptr, i32 }

@0 = private unnamed_addr constant [7 x i8] c"\E5\BC\A0\E4\B8\89\00", align 1
@1 = private unnamed_addr constant [16 x i8] c"\E6\88\91\E7\9A\84\E5\90\8D\E5\AD\97\E6\98\AF\00", align 1

declare void @println(%Any %0)

declare void @print(%Any %0)

define %Person @createPerson() {
entry:
  ret %Person { ptr @0, i32 18 }
}

define void @say(%Person %0) {
entry:
  %1 = extractvalue %Person %0, 0
  %2 = insertvalue %Any { i32 7, ptr undef }, ptr %1, 1
  %3 = alloca [2 x %Any], align 8
  %4 = getelementptr %Any, ptr %3, i32 0
  store %Any { i32 7, ptr @1 }, ptr %4, align 8
  %5 = getelementptr %Any, ptr %3, i32 1
  store %Any %2, ptr %5, align 8
  %6 = insertvalue { i64, ptr } { i64 2, ptr undef }, ptr %3, 1
  %7 = alloca { i64, ptr }, align 8
  store { i64, ptr } %6, ptr %7, align 8
  call void @append(ptr %7)
  ret void
}

declare void @append({i64,ptr} %0)

define void @"$main.__main__"() {
entry:
  %0 = call %Person @createPerson()
  ;%1 = load { ptr, i32 }, %Person %0, align 8
  call void @say(%Person %0)
  ret void
}

        "#;
        let module = ctx.parse_ir(llvm_ir).expect("解析IR失败");
        let f1 = module.get_function_ref("println");
        let f2 = module.get_function_ref("append");
//        module.dump();
        let exec = module.create_executor().expect("创建执行器失败");

        exec.add_global_mapping(f1,println as *mut c_void);
        exec.add_global_mapping(f2,append as *mut c_void);
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
