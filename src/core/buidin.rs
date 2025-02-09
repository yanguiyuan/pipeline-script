use std::ffi::{c_char, CStr};
use std::process::{Command, Stdio};

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
#[allow(unused)]
pub extern "C" fn len(target: Array) -> i64 {
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
#[allow(unused)]
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
                }
                7 => {
                    let s0 = CStr::from_ptr((*obj).ptr as *const c_char);
                    s.push_str(s0.to_str().unwrap());
                }
                t => todo!("{t}"),
            }
        }
    }
    s.push('\0');
    let s = s.leak();
    s.as_ptr() as *mut c_char
}

pub extern "C" fn cmd(command: *mut c_char) {
    let cmd = unsafe { CStr::from_ptr(command).to_str().unwrap() };
    Command::new("powershell")
        .arg("/C")
        .arg(cmd)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .expect("Failed to execute command");
}
#[allow(unused)]
pub extern "C" fn exit() {
    std::process::exit(0);
}
#[allow(unused)]
pub extern "C" fn get_env(key: *mut c_char) -> *mut c_char {
    let key = unsafe { CStr::from_ptr(key).to_str().unwrap() };
    let mut value = std::env::var(key).unwrap();
    value.push('\0');
    let value = value.leak();
    value.as_ptr() as *mut c_char
}
#[allow(unused)]
pub extern "C" fn set_env(key: *mut c_char, value: *mut c_char) {
    let key = unsafe { CStr::from_ptr(key).to_str().unwrap() };
    let value = unsafe { CStr::from_ptr(value).to_str().unwrap() };
    std::env::set_var(key, value);
}
#[allow(improper_ctypes_definitions)]
pub extern "C" fn call(f : fn(i32) ) {
   f(4)
}