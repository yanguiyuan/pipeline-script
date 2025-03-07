
%"Vec<Int64>" = type { i64, ptr }
declare void @println(%Any.0 %0)
declare ptr @malloc(i64 %0)
define void @"$Module.main"() {
entry:
  %a = alloca %"Vec<Int64>", align 8
  %0 = alloca i64, i64 10, align 8
  store [10 x i64] [i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8, i64 9, i64 10], ptr %0, align 4
  %1 = insertvalue { i64, ptr } { i64 10, ptr undef }, ptr %0, 1
  store { i64, ptr } %1, ptr %a, align 8
  %2 = getelementptr inbounds { i64, ptr }, ptr %a, i32 0, i32 1
  %3 = load ptr, ptr %2, align 8
  %4 = alloca i64, i64 1, align 8
  store [1 x i64] [i64 100], ptr %4, align 4
  store ptr %4, ptr %3, align 8
  %5 = load %"Vec<Int64>", ptr %a, align 8
  %6 = extractvalue %"Vec<Int64>" %5, 1
  %7 = getelementptr inbounds i64, ptr %6, i64 0
  %8 = load i64, ptr %7, align 4
  %9 = alloca i64, align 8
  store i64 %8, ptr %9, align 4
  %any = alloca { i32, ptr }, align 8
  store { i32, ptr } { i32 4, ptr %9 }, ptr %any, align 8
  call void @println(ptr %any)
  ret void
}



