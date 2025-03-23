脚本引擎运行流程：
分词（产生Token）->解析生成AST->宏函数展开->编译为LLVM IR ->调用LLVM JIT执行器执行

Lexer（分词器）->产生Token,into_iter()返回一个TokenStream
Parser(解析器)->生成AST(Module,Function,Struct,Module,CLass,Stmt,Expr)，parse_file()返回一个Module
Compiler(编译器)->生成LLVM IR,compile_module返回一个LLVMModule

Engine（引擎）->run_function调用LLVM JIT执行器执行LLVMModule,run_file()依次调用分词器，解析器，编译器，按照流程执行

## 难点
宏函数展开
动态参数实现，Any类型，Trait类型

## 主要逻辑梳理
1. 变量

变量分为值，引用和指针，const声明的都是值，let会声明一个变量，指针常常作为参数和返回值，目的是兼容ffi的第三方接口
- compile_member和compile_index
分为值和引用，目标是值使用extract_value，返回的是字段的值，引用返回的是字段的指针，member为左值的时候，因为需要得到指针，需要保证目标是引用，如果目标是指针，和引用一同处理，所以只需要考虑Type::ref和Type::pointer这两者使用getelementpointer,其余都是提取值就行，对泛型进行特殊考虑，一般是泛型实例，需要转成非泛型类型，几乎所有泛型的地方都需要获得它的实例类型，可以抽象一个新方法get_instance_type，对于其他类型，返回他们自身，对于泛型实例，返回它的实例，因为ref和pointer处理逻辑是一样的，可以考虑一个通用方法进行判断，get_underlying_type将ref转成pointer,将泛型实例转成实例在进行递归转换，有ref变成pointer.



> 考虑特殊情况self.data[index] = 1,self是引用，data是指针，

对于引用对象，有左值应用和右值引用之分，左值取指针，右值取引用值

以上例子中，self.data[index]是左值，尽量取指针，self（获取变量指针），self.data获取data元素的指针，因为data原本就是一个指针，此处获取的是指针的指针，因为self[index]本质上是对self指针进行偏移index计算，self是指针的时候，该语法成立，但是self实际上是指向一个变量的指针，因此，index只能允许原本值是指针才能进行，在左值语境中，index操作前需要进行一次额外的load。那我们接着考虑index在右值的情况，吧b = self.data[index],self是结构体，self.data取结构体变量获取指针data,进行index操作，获取便宜后的指针，最后进行load.

总结：对于index操作，左值，先load再index;右值，先index再load.

对于member操作，左值使用getelementptr,右值使用提取，target在左值会保证是指针，在右值会保证是结构体值

关于类型系统，原则上，compile_xxx函数的返回值是Value，不包含Type，但是有些时候我们系统进行一些类型约束，比如函数调用，函数的参数可能是分为值类型和引用类型，FunctionCall结构体包含了调用的函数，我们编译的时候可以获取到函数的类型，检查函数参数，检查函数的形参和实参类型是否匹配，此时实参还是expr，当形参是Type::Ref时，调用左值求职，当不是ref的时候调用右值求职，这里需要考虑一下指针的特殊参数类型，是否可以应用右值求职，以下是一个例子：

```rust
const p = malloc()
memread(p)
```

p是一个指针，使用const声明，注册的符号是(p,pointer),如果使用let声明，注册的符号是p,ref(p)指向指针的指针，可以改变指针指向的内容空气，const无法改变指向的内存空间。

当执行到FunctionCall时，p是指针类型，直接执行右值，获取p符号的类型，发现不是ref，不会进行额外求值load，返回指针，OK。

所以对于函数参数来说，也只需要考虑值（左值）类型和引用（右值）类型。

从以上例子也可以看出符号表应该存储Type，因为要知道是否是引用类型，引用类型在右值环境会额外求值，但是能否移除符号表的类型呢，似乎可以，那就是给Value引入RefValue,这似乎局不需要再符号表存储额外的类型信息，符号表就是简单的（String,Value）映射，那函数类型的形参类型声明是否有必要呢，考虑以上例子中的函数，memread,符号是memread，符号表是FunctionValue声明，我们需要获取其函数形参声明，需要根据参数名（默认值系统）和根据下标获取形参类型，以下是对FunctionValue结构体的构思：

```rust
struct FunctionValue{
    reference:LLVMValueRef
    index_map:Hashmap<String,usize>
    param_types:Vec<Type> // 这里的type似乎无法避免，不然如何获取形参类型呢,这里还需要记录形参默认值，能够直接通过Value声明类型呢，用一种新的类型的空值来表示没有默认值，并且这种空值无法被用户声明，避免默认值就是空值的情况，非空值就是有默认值，这里似乎可以参考js的undefined,因此引入UndefValue,但是如何兼容诸多的值类型，比如Int64Value,FloatValue呢，他们也有空值吧，哦，对了，为每个Value定义一个undef方法，llvm-sys似乎可以定义类型空值，因此我们可以约束下Valeu的trait
}

trait IValue {
    // get_type(&self)->Type 该方法似乎没有必要
    fn get_llvm_type(&self,ctx:&Context)->LLVMType
    fn as_ref(&self)->LLVMValueRef
    fn get_undef(&self)-> Self
    // 现在问题是该函数怎么实现，undef声明后的返回值也是LLVMValueRef,如何判断它是不是空类型呢，取看看LLVM-sys是否有支持判断LLVMValueRef是否是空类型。
    // 查阅后发现确实存在对应的函数，LLVMGetUndef和LLVMIsUndef,所以函数值也可以不包括Type了，这样在编译过程中Type已经被消除了。
    fn is_undef(&self)->bool
}
```

其次我们还注意到 一个问题，那就是LLVMType，类型在一个上下文中实际上是单例，通过LLVMXxxType()声明一次后可以无限次使用，而考虑到LLVMType返回的也是一个LLVMTypeRef,因此可能即时无限次调用LLVMType创建类型，都是返回的同一个类型引用。结果确实是对的，因此我对于基本类型直接调用LLVMXxxType返回即可。

对于复杂类型，比如结构体，如何获得他们的LLVMType呢，一个结构体有名字，有字段。这个时候一般是创建命名结构体和非命名结构体两种，如果StructValue有名字则，采用命名结构体，没有，则无命名结构体，考虑到命名结构体创建确实比较复杂，而且创建命名结构体值时需要检查结构体的类型约束，因此需要存储他们的类型，在第一次编译模块时编译所有结构体，存在ctx中， 以后的结构体值有名字时，直接获取结构体类型，进行检查，因此get_llvm_type()应该有一个入参ctx,对于Type的get_llvm_type也应该有一个ctx入参，类型都通通ctx创建，比如ctx.llvm_float_type()


另外，对于更复杂一点的类型和值系统，枚举，llvm本身并没有原生支持枚举，需要我们通过struct进行枚举，我们初步设定枚举的值是tag+字节数组，并通过bitcast进行转换。因此，它的value定义可以是以下的样子：
```rust
struct EnumValue{
    name:(String,String) // 枚举名和变体名
    tag:Int64Value
    data:ArrayValue
}
//其llvm_type应该是一种结构固定的结构体，
LLVMType::Enum(EnumLLVMType)

struct EnumLLVMType{
    reference:LLVMTypeRef
    // 标签和变体映射
    variant_tag:Hashmap<string,Int64Value>
}
```

对value系统进行重设计，目前的value包括type和llvmValue,但是实际上llvmValue就包扩了类型信息，type实际上可以只存在于AST阶段，然后现在构建都是通过builder实现的，但是对于一个IntValue应该可以是啊a.add()另外一个IntValue



```rust
enum Value{
    Bool(BoolValue)
    Int8(Int8Value)
    Int16(Int16Value)
    Int32(Int32Value)
    Int64(Int64Value)
    Float(FloatValue)
    Double(DoubleValue)
    String(StringValue)
    Struct(StructValue)
    Pointer(PointerValue)
    Function(FunctionPointer)
    Array(ArrayValue)
}

struct PointerValue{
    element: Box<Value>
    value:Option<Value>
}

impl PointerValue{
    fn get_value()->Value {
        match self.value
    }
    fn get_element_pointer()
}
struct Int64Value {
    value:LLVMValueRef
}

impl Int64Value {
    fn add(&self,ctx:&Context,value:Int64Value)->Int64Value
    fn add_i8(&self,ctx:&Context,value:Int8Value)
}
```

