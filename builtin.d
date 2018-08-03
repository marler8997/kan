module builtin;

import std.typecons : Flag, Yes, No, scoped;

import more.alloc;
import more.builder;
import more.format : StringSink;

import typecons : Rebindable, rebindable;
import common : from, passfail, unconst, toImmutable, toConst, uarray, toUarray, singleton, quit;
import log;
import syntax : SyntaxNodeType, SyntaxNode, CallSyntaxNode;
import semantics;
import types;// : VoidType, Anything, NumberType, SymbolType, Multi, StringType, PrintableType;
import symtab : SymbolTable;
import mod : sliceModuleBaseName, loadImport, Module;
import analyzer/* : AnalyzeOptions, analyzeSemanticCallPass1,
                  analyzeExpressionPass1, analyzeExpressionPass2, tryAnalyzeToSymbolPass1*/;
import interpreter : Interpreter;
static import interpreter;

passfail checkSyntaxArgCount(SemanticCall call, uint expectedArgCount)
{
    if (call.syntaxArgs.length != expectedArgCount)
    {
        errorf(call.formatLocation, "function '%s requires %s syntax argument(s) but got %s",
            call.formatNameForMessage, expectedArgCount, call.syntaxArgs.length);
        return passfail.fail;
    }
    return passfail.pass;
}
passfail checkSemanticArgCount(SemanticCall call, uint expectedArgCount)
{
    if (call.semanticArgs.length != expectedArgCount)
    {
        errorf(call.formatLocation, "function '%s' requires %s semantic argument(s) but got %s",
            call.formatNameForMessage, expectedArgCount, call.semanticArgs.length);
        return passfail.fail;
    }
    return passfail.pass;
}

/+
Symbol tryArgAsSymbolPass1(IReadonlyScope scope_, SemanticCall call, uint argIndex)
   in { assert(argIndex < call.arguments.length, "code bug"); } do
{
    auto arg = call.arguments[argIndex];
    auto result = tryAnalyzeToSymbolPass1(arg);
    return result.value ? result.value :
        errorfNullable!Symbol(arg.formatLocation(), "the '%s' function requires as symbol for the argument at index %s but got '%s'",
            call.syntaxNode.functionName, argIndex, arg);
}
+/

// TODO: a developer can add analyze-time functions, however,
//       they will need to be in special modules that the compiler
//       must load immediately (never loads them lazily).
//       This is because analyze-time functions need to be available
//       when the call is analyzed, and normal functions can be loaded
//       after the call is analyzed.
//
// TODO:
// let(<symbol> <expression>)  // permanently sets <symbol> to <expressioon>
// set(<symbol> <expression>)  // temporarily set <symbol> to <expression>
//
SemanticFunction tryFindSemanticFunction(const(CallSyntaxNode)* call)
{
    return tryFindSemanticFunction(call.functionName, call.arguments.unconst);
}
SemanticFunction tryFindSemanticFunction(const(char)[] functionName, uarray!SyntaxNode args)
{
    //
    // Syntax Only methods
    //
    if (functionName == "flag")
        return cast()flagFunction.instance.checkAndGet(args);
    if (functionName == "symbol")
        return cast()symbolFunction.instance.checkAndGet(args);
    if (functionName == "enum")
        return cast()enumFunction.instance.checkAndGet(args);
    if (functionName == "dumpSymbolTable")
        return cast()dumpSymbolTableFunction.instance.checkAndGet(args);
    // TODO: maybe call(...) should be a syntaxOnly function?

    //
    // All Other Semantic methods
    //
    if (functionName == "import")
        return cast()importFunction.instance.checkAndGet(args);
    if (functionName == "set")
        return cast()setFunction.instance.checkAndGet(args);
    if (functionName == "setBultinFunction")
        return cast()setBuiltinFunctionFunction.instance.checkAndGet(args);
    if (functionName == "function")
        return cast()functionFunction.instance.checkAndGet(args);
    if (functionName == "call")
        return cast()callFunction.instance.checkAndGet(args);
    if (functionName == "jumpBlock")
        return cast()jumpBlockFunction.instance.checkAndGet(args);
    if (functionName == "jumpLoopIf")
        return cast()jumpLoopIfFunction.instance.checkAndGet(args);
    if (functionName == "foreach")
        return cast()foreachFunction.instance.checkAndGet(args);
    // TODO: look for custom analyze-time functions

    return null;
    // TODO: look for custom analyze-time functions

    return null;
}

class flagFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("flag", SemanticArgType.syntaxNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        if (call.syntaxArgs.length != 1)
            return errorfNodeResult(call.formatLocation, "flag function with multiple arguments not implemented");
        auto syntaxArg = &call.syntaxArgs[0];
        if (syntaxArg.type != SyntaxNodeType.symbol)
            return errorfNodeResult(call.formatLocation, "expected a symbol but got '%s'", syntaxArg.type);
        return NodeResult(new FlagValue(syntaxArg, syntaxArg.source));
    }
}
class symbolFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("symbol", SemanticArgType.syntaxNode, null);
    }
    final static ResultOrError!Symbol interpretPass2(SemanticCall call)
    {
        if (call.syntaxArgs.length != 1)
            return errorfSymbolResult(call.formatLocation,
                "symbol function with more than 1 argument is not implemented");
        auto arg = &call.syntaxArgs[0];
        if (arg.type == SyntaxNodeType.symbol || arg.type == SyntaxNodeType.keyword)
            return ResultOrError!Symbol(new SymbolFromSyntax(arg));
        return errorfSymbolResult(call.formatLocation,
            "symbol function for this type is not supported or not implemented");
    }
    //
    //
    //
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        auto result = symbolFunction.interpretPass2(call);
        if (result.value)
            return NodeResult(result.value);
        assert(result.errorCount > 0, "codebug");
        return NodeResult(result.errorCount);
    }
}
class enumFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("enum", SemanticArgType.syntaxNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        auto symbols = new string[call.syntaxArgs.length];
        foreach (i, arg; call.syntaxArgs)
        {
            symbols[i] = arg.source;
        }
        return NodeResult(new EnumType(call.getSyntaxNode, symbols));
    }
}

class dumpSymbolTableFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("dumpSymbolTable", SemanticArgType.syntaxNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        if (call.syntaxArgs.length != 0)
        {
            assert(0, "dumpSymbolTable function with more than 0 arguments is not implemented");
        }
        scope_.dumpSymbols();
        return NodeResult(new VoidValue(call.getSyntaxNode));
    }
}

/**
This function inserts the module into the current scope using the symbol that matches
the base name of the module, i.e.
````
import(a.b.c)

c.foo // access symbol 'foo' in module a.b.c
````
**/
class importFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("import", SemanticArgType.semiAnalyzedSemanticNode, null);
        /*
        super(immutable FunctionInterface(FunctionType.syntax, [
            immutable Parameter(null, new immutable Multi(SymbolType.instance, positiveRangeFrom(1))),
        ], null, FunctionFlags.none));
        */
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        foreach (arg; call.semanticArgs)
        {
            auto result = tryAnalyzeToSymbolPass1(arg);
            if (!result.value)
                return errorfUint(arg.formatLocation(), "import expects symbols, but got '%s'", arg);

            // TODO: call.semanticArgs[i] = result.value ???
            auto moduleName = result.value.value;
            auto moduleBaseName = sliceModuleBaseName(moduleName);
            scope_.add(moduleBaseName, new LazyImport(call.getSyntaxNode, moduleName));
        }
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        return NodeResult(new VoidValue(call.getSyntaxNode));
    }
}
class LazyImport : LazyNode
{
    const(SyntaxNode)* importSyntaxNode;
    string moduleName;
    this(const(SyntaxNode)* importSyntaxNode, string moduleName)
    {
        this.importSyntaxNode = importSyntaxNode;
        this.moduleName = moduleName;
    }
    protected final override NodeResult doEvaluate()
    {
        // TODO: support errors
        return NodeResult(new ImportedModule(importSyntaxNode, loadImport(moduleName)));
    }
    //
    // SemanticNode methods
    //
    protected final override const(SyntaxNode)* getSyntaxNode() const { return importSyntaxNode; }
    final override void valueFormatter(StringSink sink) const { assert(0, "not implemented"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "codebug: SemanticFunction nodes shouldn't exist at interpret time");
    }
}

class setFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private string symbol;
    private this() immutable
    {
        super("set", SemanticArgType.fullyAnalyzedSemanticNode, [
            immutable NumberedSemanticArgType(SemanticArgType.semiAnalyzedSemanticNode, 1)
        ]);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        if(checkSemanticArgCount(call, 2).failed)
            return 1;

        auto symbolArg = call.semanticArgs[0];
        auto result = tryAnalyzeToSymbolPass1(symbolArg);
        if (!result.value)
            return errorfUint(symbolArg.formatLocation(), "argument 1 of 'set' should be a symbol, but got '%s'", symbolArg);
        //from!"std.stdio".writefln("[DEBUG] set '%s' at '%s', scope=%s",
        //    result.value.value, call.formatLocation, scope_.formatScopeDescription);
        scope_.add(result.value.value, call.semanticArgs[1]);
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        // TODO: maybe return the symbol table entry, make sure it is "ignorable" if we do so
        return NodeResult(new VoidValue(call.getSyntaxNode));
    }
}

class setBuiltinFunctionFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("setBuiltinFunction", SemanticArgType.semiAnalyzedSemanticNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        if(checkSemanticArgCount(call, 1).failed)
            return 1;

        Symbol symbol;
        {
            auto symbolArg = call.semanticArgs[0];
            auto result = tryAnalyzeToSymbolPass1(symbolArg);
            if (!result.value)
                return errorfUint(symbolArg.formatLocation(), "'setBuiltinFunction' requires a symbol but got '%s'", symbolArg);
            symbol = result.value;
        }
        auto function_ = tryGetHiddenBuiltinRegularFunction(symbol.value);
        if (function_ is null)
            return errorfUint(call.formatLocation, "builtin function '%s' does not exist", symbol.value);

        //
        // TODO: verify function_.interface matches the builtin function interface
        //
        scope_.add(symbol.value, function_);
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        // TODO: maybe return the function itself
        return NodeResult(new VoidValue(call.getSyntaxNode));
    }
}

class callFunction : SemanticFunction
{
    enum isSemanticCall = cast(void*)1;
    enum isRegularCall  = cast(void*)2;

    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("call", SemanticArgType.syntaxNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        if (call.syntaxArgs.length == 0)
            return errorfUint(call.formatLocation, "'call' requires at least 1 argument");

        auto funcNode = newSemanticNode(&call.syntaxArgs[0]);
        auto loweredSyntaxArgs = call.syntaxArgs[1 .. $];
        auto result = tryAnalyzeToSymbolPass1(funcNode);
        if (result.value)
        {
            auto semanticFunc = tryFindSemanticFunction(result.value.value, loweredSyntaxArgs);
            if (semanticFunc)
            {
                call.funcData = isSemanticCall;
                auto semanticCall = new SemanticCall(call.getSyntaxNode,
                    semanticFunc, loweredSyntaxArgs);
                call.semanticArgs = new SemanticNode[1].toUarray;
                call.semanticArgs[0] = semanticCall;
                return inTreeOrderAnalyzeSemanticCallPass1(scope_, semanticCall);
            }
        }
        call.funcData = isRegularCall;
        call.semanticArgs = new SemanticNode[call.syntaxArgs.length].toUarray;
        call.semanticArgs[0] = funcNode;
        newSemanticNodesInto(call.semanticArgs[1 .. $], loweredSyntaxArgs);
        uint errorCount = 0;
        foreach (arg; call.semanticArgs)
        {
            errorCount += inTreeOrderAnalyzeExpressionPass1(scope_, arg);
        }
        return errorCount;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        if (call.funcData == isSemanticCall)
        {
            assert(call.semanticArgs.length == 1, "codebug");
            return NodeResult(call.semanticArgs[0]);
        }

        assert(call.funcData == isRegularCall, "codebug");

        {
            auto errorCount = inTreeOrderAnalyzeExpressionPass2(scope_, &call.semanticArgs[0], AnalyzeOptions.none);
            if (errorCount > 0)
                return NodeResult(errorCount);
        }

        auto regularFunction = call.semanticArgs[0].tryAs!RegularFunction;
        if (!regularFunction)
            return errorfNodeResult(call.formatLocation, "first argument of 'call' must be a function, but got '%s'", call.semanticArgs[0]);

        return NodeResult(new RegularCall(call.getSyntaxNode, regularFunction,
            call.syntaxArgs[1 .. $], call.semanticArgs[1 .. $]));
    }
}

struct FunctionStaticData
{
    uarray!SemanticNode nodes;
    uint analyzeIndex;
    uint returnTypeIndex;
    bool externC;
    bool builtin;
    Rebindable!IType returnType;
}
class functionFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("function", SemanticArgType.syntaxNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // do nothing for now, maybe we will need to analyze the function body later?
        // or maybe tha would be done later
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        assert(call.semanticArgs.length == 0, "codebug: why where there semantics nodes created for this?");

        auto syntaxNode = call.getSyntaxNode;
        assert(syntaxNode.type == SyntaxNodeType.call, "codebug");
        auto callSyntaxNode = &syntaxNode.call;
        //from!"std.stdio".writefln("[DEBUG] %sinterpretPass2 function, scope is '%s'", call.formatLocation, scope_.formatScopeDescription);
        //assert(0);
        return NodeResult(new UserDefinedFunction(scope_, callSyntaxNode.base, callSyntaxNode.arguments.unconst));
    }
}

class jumpBlockFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("jumpBlock", SemanticArgType.syntaxNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // do nothing for now...
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        assert(0, "jumpBlock function not implemented");
        /+
        auto block = new SemanticNode();
        block.initializeStatementBlock(
            call.syntaxNode.base,
            BlockFlags.isJumpBlock,
            new JumpBlock(scope_),
            newSemanticNodes(call.syntaxArgs)
        );
        return SemanticCallResult(block);

        /+
        auto data = call.getStaticDataStruct!JumpBlockStaticData();
        if (data.newlyAllocated)
        {
            data.scope_ = new JumpBlockScope(scope_);
            data.nodes = newSemanticNodes(call.syntaxArgs);
        }

        for (; data.nodesAnalyzed < data.nodes.length; data.nodesAnalyzed++)
        {
            auto result = analyzeStatement(data.scope_, &data.nodes[data.nodesAnalyzed]);
            if (result != AnalyzeState.analyzed)
            {
                // TODO: may not be the correct return value
                return SemanticCallResult.noEntryButMoreSymbolsCouldBeAdded;
            }
        }
        from!"std.stdio".writeln("WARNING: jumpBlock not fully implemented");
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
        +/
        +/
    }
}

struct JumpBlockStaticData
{
    IScope scope_;
    size_t nodesAnalyzed;
    SemanticNode[] nodes;
}

class jumpLoopIfFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("jumpLoopIf", SemanticArgType.semiAnalyzedSemanticNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // do nothing for now...
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        assert(0, "jumpLoopIf funciton not implemented");
        /+
        if (call.syntaxArgs.length != 1)
        {
            from!"std.stdio".writefln("Error: function '%s' requires 1 argument but got %s",
                call.syntaxNode.functionName, call.syntaxArgs);
            throw quit;
        }
        auto condition = new SemanticNode();
        condition.initialize(&call.syntaxArgs[0]);

        auto jumpNode = new SemanticNode();
        jumpNode.initializeJumpNode(call.syntaxNode.base, JumpType.loopCurrentBlock, condition);

        return SemanticCallResult(jumpNode);
        +/
    }
}



class foreachFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("foreach", SemanticArgType.semiAnalyzedSemanticNode, null);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // do nothing for now...
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        assert(0, "foreach function not implemented");
        /+
        assertFunctionArgCount(scope_, call, 3);
        auto loopSymbol = argAsSymbol(scope_, call, 0);

        /+
        auto rangeArgument = &call.arguments[1];
        if (!analyzeValue(scope_, rangeArgument, Yes.resolveSymbols, used))
        {
            return SemanticCallResult.noEntryButMoreSymbolsCouldBeAdded;
        }
        auto rangeArgumentTypedValue = rangeArgument.getAnalyzedTypedValue(Yes.resolveSymbols);

        auto rangeArgumentTypeAsRangeType = cast(IRangeType)rangeArgumentTypedValue.type;
        if (rangeArgumentTypeAsRangeType is null)
        {
            from!"std.stdio".writefln("Error: 2nd argument of foreach must be a range, but it's type is '%s'",
                rangeArgumentTypedValue.type.formatType);
            throw quit;
        }
        +/

        from!"std.stdio".writefln("ERROR: foreach not implemented, ignoring it for now");
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
        +/
    }
}

/+
NOTE: Removed since syntax '{...}' creates tuples now
class tupleFunction : SemanticFunction
{
    mixin singleton;

    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call) const
    {
        return ResolveResult(UntypedNoLimitSetType.instance.createTypedValue(call.arguments));
    }
}
+/

//
// Builtin Regular methods that are available without any import
//
class lengthFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("length", immutable RegularFunctionInterface(AnySingleThing.instance, [
            immutable Parameter(null, AnySingleThing.instance),
        ], null, FunctionFlags.none));
    }
    final override void interpret(Interpreter* interpreter, RegularCall call)
    {
        assert(0, "not implemented");
    }
}
class leftIsLessFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("leftIsLess", immutable RegularFunctionInterface(BoolType.instance, [
            immutable Parameter(null, AnySingleThing.instance),
            immutable Parameter(null, AnySingleThing.instance),
        ], null, FunctionFlags.none));
    }
    final override void interpret(Interpreter* interpreter, RegularCall call)
    {
        assert(0, "not implemented");
    }
}
class allocaFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        // !!! TODO: should return a pointer to a buffer with a length
        //           except, the length would be a compile-time reference to the length passed
        //           in as the argument.
        super("alloca", immutable RegularFunctionInterface(VoidPtrType.instance, [
            immutable Parameter(null, UnsignedType.instance),
            // TODO: maybe we need alignment parameter?
        ], null, FunctionFlags.none));
    }
    final override void interpret(Interpreter* interpreter, RegularCall call)
    {
        assert(0, "not implemented");
    }
}
class ptrToFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        // todo: should be a template function whose return type is a pointer
        //       to the argument passed in
        super("ptrTo", immutable RegularFunctionInterface(VoidPtrType.instance, [
            immutable Parameter(null, AnySingleThing.instance),
        ], null, FunctionFlags.none));
    }
    final override void interpret(Interpreter* interpreter, RegularCall call)
    {
        assert(0, "not implemented");
    }
}

//
// Builtin Regular methods
// These functions are implemented by the compiler, but they may be "hidden" in that
// they need to be declared in order to be visible.
//
BuiltinRegularFunction tryGetHiddenBuiltinRegularFunction(string functionName)
{
    if (functionName == "print")
        return cast()printFunction.instance;
    // TODO: look for user-defined functions

    return null;
}
class printFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("print", immutable RegularFunctionInterface(VoidType.instance, [
            immutable Parameter(null, new immutable Multi(PrintableType.instance, positiveRangeFrom(1))),
        ], ["noNewline"], FunctionFlags.none));
    }
    final override void interpret(Interpreter* interpreter, RegularCall call)
    {
        bool addNewline = true;

        uint argumentIndex = 0;
        if (argumentIndex < call.arguments.length)
        {
            auto arg = call.arguments[argumentIndex];
            auto asFlag = cast(FlagValue)arg;
            if (asFlag !is null)
            {
                if (asFlag.name == "noNewline")
                {
                    addNewline = false;
                    argumentIndex++;
                }
                else
                {
                    from!"std.stdio".writefln("Error: unknown flag '%s' for print function", asFlag.name);
                    throw quit;
                }
            }
        }

        foreach (arg; call.arguments[argumentIndex .. $])
        {
            // TODO: use runtime value in the future

            //from!"std.stdio".writefln("typed value is %s", typedValue.type.formatType);
            arg.printFormatter(&from!"std.stdio".stdout.write!(const(char)[]), interpreter);
        }

        if (addNewline)
        {
            from!"std.stdio".writeln();
        }
    }
}

