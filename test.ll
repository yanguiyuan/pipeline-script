; ModuleID = 'main'
source_filename = "main"

%"Vec<Int64>" = type { ptr, i64, i64 }
%Any.0 = type { i32, ptr }

define i64 @"Vec<Int64>.get"(%"Vec<Int64>" %0, i64 %1) {
entry:
  %2 = extractvalue %"Vec<Int64>" %0, 0
  %3 = getelementptr inbounds i64, ptr %2, i64 %1
  %4 = load i64, ptr %3, align 4
  ret i64 %4
}

declare void @cmd(ptr %0)

declare ptr @FormatAppend({ i64, ptr } %0)

declare ptr @malloc(i64 %0)

declare void @panic(ptr %0)

define %"Vec<Int64>" @"Vec<Int64>.new"() {
entry:
  %0 = call ptr @malloc(i64 mul (i64 ptrtoint (ptr getelementptr (i64, ptr null, i32 1) to i64), i64 10))
  %1 = insertvalue %"Vec<Int64>" undef, ptr %0, 0
  %2 = insertvalue %"Vec<Int64>" %1, i64 0, 1
  %3 = insertvalue %"Vec<Int64>" %2, i64 10, 2
  ret %"Vec<Int64>" %3
}

define void @"Vec<Int64>.push"(ptr %0, i64 %1) {
entry:
  %2 = getelementptr inbounds %"Vec<Int64>", ptr %0, i32 0, i32 0
  %3 = getelementptr inbounds %"Vec<Int64>", ptr %0, i32 0, i32 1
  %4 = load i64, ptr %3, align 4
  %data_ptr = load ptr, ptr %2
  %5 = getelementptr i64, ptr %data_ptr, i64 %4
  store i64 %1, ptr %5, align 4
  %6 = getelementptr inbounds %"Vec<Int64>", ptr %0, i32 0, i32 1
  %7 = getelementptr inbounds %"Vec<Int64>", ptr %0, i32 0, i32 1
  %8 = load i64, ptr %7, align 4
  %9 = add i64 %8, 1
  store i64 %9, ptr %6, align 4
  ret void
}

declare void @println(%Any.0 %0)

define void @"$Module.main"() {
entry:
  %v = alloca %"Vec<Int64>", align 8
  %0 = call %"Vec<Int64>" @"Vec<Int64>.new"()
  store %"Vec<Int64>" %0, ptr %v, align 8
  call void @"Vec<Int64>.push"(ptr %v, i64 10)
  %1 = load %"Vec<Int64>", ptr %v, align 8
  %2 = call i64 @"Vec<Int64>.get"(%"Vec<Int64>" %1, i64 0)
  ret void
}
