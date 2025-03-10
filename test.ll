; ModuleID = 'main'
source_filename = "main"

%Any = type { i32, ptr }
%"Option<Float>" = type { i32, float }
%Any.1 = type { i32, ptr }

declare void @println(%Any %0)

declare ptr @FormatAppend({ i64, ptr } %0)

declare void @cmd(ptr %0)

define void @"$Module.main"() {
entry:
  %b = alloca %"Option<Float>", align 8
  %enum_instance = alloca %"Option<Float>", align 8
  %0 = getelementptr inbounds %"Option<Float>", ptr %enum_instance, i32 0, i32 0
  store i32 1, ptr %0, align 4
  %1 = getelementptr inbounds %"Option<Float>", ptr %enum_instance, i32 0, i32 1
  store float 0x40249999A0000000, ptr %1, align 4
  store ptr %enum_instance, ptr %b, align 8
  %2 = load %"Option<Float>", ptr %b, align 4
  %3 = getelementptr inbounds %"Option<Float>", %"Option<Float>" %2, i32 0, i32 0
  %4 = load i32, %"Option<Float>" %3, align 4
  %5 = icmp eq i32 %4, 1
  br i1 %5, label %if_let_then, label %if_let_else

if_let_then:                                      ; preds = %entry
  %6 = getelementptr inbounds %"Option<Float>", %"Option<Float>" %2, i32 0, i32 1
  %7 = load float, %"Option<Float>" %6, align 4
  %8 = fadd float %7, 1.150000e+01
  store float %8, %"Option<Float>" %6, align 4
  %9 = load float, %"Option<Float>" %6, align 4
  %10 = alloca float, align 4
  store float %9, ptr %10, align 4
  %any = alloca %Any.1, align 8
  %11 = insertvalue { i32, ptr } { i32 9, ptr undef }, ptr %10, 1
  store { i32, ptr } %11, ptr %any, align 8
  call void @println(ptr %any)
  br label %if_let_end

if_let_else:                                      ; preds = %entry
  br label %if_let_end

if_let_end:                                       ; preds = %if_let_else, %if_let_then
  ret void
}
