use std::ffi::{c_char, CStr};
#[repr(C)]
pub struct Array {
    len: i64,
    ptr: *mut Any,
}
#[repr(C)]
pub struct Any {
    id: i32,
    ptr: *mut i8,
}
pub extern "C" fn len(target:Array)->i64{
    target.len
}
pub extern "C" fn println(obj: Array) {
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
pub extern "C" fn print(obj: Array) {
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
}
pub extern "C" fn append(obj: Array) -> *mut c_char {
    let mut s = String::new();
    for i in 0..obj.len {
        let obj = unsafe { (obj.ptr as *mut Any).offset(i as isize) };
        unsafe {
            match (*obj).id {
                0 => {
                    s.push_str("Unit");
                }
                3 => {
                    let value = (*obj).ptr as i32;
                    s.push_str(&format!("{}", value));
                }
                4 => {
                    let v = (*obj).ptr as i64;
                    s.push_str(&format!("{}", v));
                    print!("{}", v);
                }
                7 => {
                    let s0 = CStr::from_ptr((*obj).ptr as *const c_char);
                    s.push_str(s0.to_str().unwrap());
                }
                t => todo!("{t}"),
            }
        }
    }
    let s = s.leak();
    s.as_ptr() as *mut c_char
}
