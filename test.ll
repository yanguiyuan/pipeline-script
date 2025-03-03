declare ptr @malloc(i64 %0)
define void @"$Module.main"() {
entry:
  %0 = call ptr @malloc(i64 mul (i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64), i64 10))
  %1 = insertvalue { i64, ptr } { i64 10, ptr undef }, ptr %0, 1
  %2 = getelementptr inbounds { i64, ptr }, { i64, ptr } %1, i32 0, i32 1
  %3 = load ptr, { i64, ptr } %2, align 8
  %4 = getelementptr i64, ptr %3, i64 0
  %5 = load i64, ptr %4, align 4
  %6 = alloca i64, align 8
  store i64 %5, ptr %6, align 4
  %any = alloca { i32, ptr }, align 8
  store { i32, ptr } { i32 4, ptr %6 }, ptr %any, align 8
  call void @println(ptr %any)
  ret void
}


