; ModuleID = 'main'
source_filename = "main"

%"Option<Int64>" = type { i32, i64 }
%Any = type { i32, ptr }

declare void @println(%Any %0)

declare void @cmd(ptr %0)


define %"Option<Int64>" @next() {
entry:
  ret %"Option<Int64>" { i32 1, i64 10 }
}

define i64 @"Option<Int64>.unwrap"(%"Option<Int64>" %0) {
entry:
  %1 = extractvalue %"Option<Int64>" %0, 0
  %2 = icmp eq i32 %1, 1
  br i1 %2, label %if_let_then, label %if_let_else

if_let_then:                                      ; preds = %entry
  %3 = extractvalue %"Option<Int64>" %0, 1
  ret i64 %3
  br label %if_let_end

if_let_else:                                      ; preds = %entry
  br label %if_let_end

if_let_end:                                       ; preds = %if_let_else, %if_let_then
  ret i64 1
}


define i1 @"Option<Int64>.is_some"(%"Option<Int64>" %0) {
entry:
  %1 = extractvalue %"Option<Int64>" %0, 0
  %2 = icmp eq i32 %1, 0
  br i1 %2, label %if_let_then, label %if_let_else

if_let_then:                                      ; preds = %entry
  ret i1 false
  br label %if_let_end

if_let_else:                                      ; preds = %entry
  br label %if_let_end

if_let_end:                                       ; preds = %if_let_else, %if_let_then
  ret i1 true
}

declare void @panic(ptr %0)

declare ptr @FormatAppend({ i64, ptr } %0)

define void @"$Module.main"() {
entry:
  %a = alloca %"Option<Int64>", align 8
  %0 = call %"Option<Int64>" @next()
  store %"Option<Int64>" %0, ptr %a, align 4
  %b = alloca i64, align 8
  %1 = load %"Option<Int64>", ptr %a, align 4
  %2 = call i64 @"Option<Int64>.unwrap"(%"Option<Int64>" %1)
  store i64 %2, ptr %b, align 4
  ret void
}

