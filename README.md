脚本引擎运行流程：
分词（产生Token）->解析生成AST->宏函数展开->编译为LLVM IR ->调用LLVM JIT执行器执行

Lexer（分词器）->产生Token,into_iter()返回一个TokenStream
Parser(解析器)->生成AST(Module,Function,Struct,Module,CLass,Stmt,Expr)，parse_file()返回一个Module
Compiler(编译器)->生成LLVM IR,compile_module返回一个LLVMModule

Engine（引擎）->run_function调用LLVM JIT执行器执行LLVMModule,run_file()依次调用分词器，解析器，编译器，按照流程执行

## 难点
宏函数展开
动态参数实现，Any类型，Trait类型