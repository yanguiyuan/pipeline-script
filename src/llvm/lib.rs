
#[cfg(test)]
mod tests {
    use crate::global::Global;
    use crate::{context::LLVMContext, module};
    use std::{
        ffi::{c_char, c_void, CStr, CString},
        ptr,
    };
    #[repr(C)]
    struct Array {
        len: i64,
        ptr: *mut Any,
    }
    #[repr(C)]
    struct Any {
        id: i32,
        ptr: *mut c_void,
    }
    extern "C" fn println(obj: Array) {
        for i in 0..obj.len {
            let obj = unsafe { (obj.ptr as *mut Any).offset(i as isize) };
            unsafe {
                match (*obj).id {
                    0 => {
                        print!("Unit")
                    }
                    3 => {
                        let value = (*obj).ptr as i32;
                        print!("{}", value);
                    }
                    4 => {
                        let v = (*obj).ptr as i64;
                        print!("{}", v);
                    }
                    7 => {
                        let s = CStr::from_ptr((*obj).ptr as *const c_char);
                        print!("{}", s.to_str().unwrap());
                    }
                    t => todo!("{t}"),
                }
            }
        }
        println!()
    }
    extern "C" fn append(obj: Array) {
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
                        unsafe {
                            print!("{}", value);
                        }
                    }
                    t => todo!("{t}"),
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

%Any.0 = type { i32, ptr }

declare void @println({ i64, ptr } %0)

declare void @print({ i64, ptr } %0)

define void @boo( { ptr, ptr } %0) {
entry:
  %1 = extractvalue { ptr,ptr } %0, 0
  %2 = extractvalue { ptr, ptr } %0, 1
  call void %1(i32 3, i32 5, ptr %2)
  ret void
}

define void @a(i32 %0, i32 %1, ptr %2) {
entry:
  %env = load {i32,i32}, ptr %2
  %3 = extractvalue { i32, i32 } %env, 1
  %4 = extractvalue { i32, i32 } %env, 0
  %5 = add i32 %3, %1
  %6 = add i32 %4, %5
  %7 = add i32 %0, %6
  %ptr = inttoptr i32 %7 to i8*
  %8 = insertvalue %Any.0 { i32 3, ptr undef }, ptr %ptr, 1
  %9 = alloca [1 x %Any.0], align 8
  %10 = getelementptr %Any.0, ptr %9, i32 0
  store %Any.0 %8, ptr %10, align 8
  %11 = insertvalue { i64, ptr } { i64 1, ptr undef }, ptr %9, 1
  %12 = alloca { i64, ptr }, align 8
  store { i64, ptr } %11, ptr %12, align 8
  call void @println(ptr %12)
  ret void
}

declare ptr @append({ i64, ptr } %0)

define void @"$main.__main__"() {
entry:
        %0 = insertvalue { i32, i32 } { i32 12, i32 undef }, i32 10, 1
        %1 = alloca { i32, i32 }, align 8
        store { i32, i32 } %0, ptr %1, align 8
        %2 = insertvalue { ptr, ptr } {  ptr @a, ptr undef }, ptr  %1, 1
  call void @boo({ ptr, ptr } %2)
  ret void
}

        "#;
        let module = ctx.parse_ir(llvm_ir).expect("解析IR失败");
        let f1 = module.get_function_ref("println");
        // let f2 = module.get_function_ref("append");
        module.dump();
        let exec = module.create_executor().expect("创建执行器失败");

        exec.add_global_mapping(f1, println as *mut c_void);
        // exec.add_global_mapping(f2,append as *mut c_void);
        exec.run_function("$main.__main__", &mut []);
    }
    #[test]
    fn it_works2() {
        let jit_ctx = LLVMContext::with_jit();
        let mut module = jit_ctx.create_module("main");
        let main = module.register_function(
            "main",
            Global::function_type(Global::unit_type(), vec![]),
            vec![],
        );
        let entry = main.append_basic_block("entry");
        let builder = Global::create_builder();
        builder.position_at_end(entry);
        builder.build_return_void();
        builder.build_global_string_ptr("a", "Hello,world!");
        module.dump()
    }
}
