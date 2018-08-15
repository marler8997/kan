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

passfail checkSyntaxArgCount(const(SemanticCall) call, uint expectedArgCount)
{
    if (call.syntaxArgs.length != expectedArgCount)
    {
        errorf(call.formatLocation, "function '%s requires %s syntax argument(s) but got %s",
            call.formatNameForMessage, expectedArgCount, call.syntaxArgs.length);
        return passfail.fail;
    }
    return passfail.pass;
}
/*
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
*/

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

template tuple(T...)
{
    alias tuple = T;
}

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
SemanticCall tryGetSemanticFunctionFor(const(CallSyntaxNode)* call)
{
    return tryGetSemanticFunctionFor(call.base, call.functionName, call.arguments.unconst);
}
SemanticCall tryGetSemanticFunctionFor(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode args)
{
    foreach (name; tuple!(
        "flag",
        "symbol",
        "enum",
        "dumpSymbolTable",
        "import",
        "set",
        "setBuiltinFunction",
        "function",
        "call"
        //"jumpBlock",
        //"jumpLoopIf",
        //"foreach",
    ))
    {
        if (functionName == name)
            return mixin("new " ~ name ~ "Call(syntaxNode, functionName, args)");
    }

    // TODO: look for custom analyze-time functions

    return null;
}

private class flagCall : BuiltinSemanticCall
{
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        if (syntaxArgs.length != 1)
            return errorfNodeResult(formatLocation, "flag function with multiple arguments not implemented");
        auto syntaxArg = &syntaxArgs[0];
        // TODO: use tryAnalyzeToSymbol instead?
        if (syntaxArg.type != SyntaxNodeType.symbol)
            return errorfNodeResult(formatLocation, "expected a symbol but got '%s'", syntaxArg.type);
        return NodeResult(new FlagValue(syntaxArg, syntaxArg.source));
    }
}

private class symbolCall : BuiltinSemanticCall
{
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        auto result = inTreeOrderInterpretPass2();
        if (result.errorCount > 0)
            return NodeResult(result.errorCount);
        assert(result.value);
        return NodeResult(result.value);
    }
    final ResultOrError!Symbol inTreeOrderInterpretPass2() const
    {
        if (checkSyntaxArgCount(this, 1).failed)
            return ResultOrError!Symbol(1);
        auto result = tryAnalyzeToSymbolPass1(&syntaxArgs[0]);
        if (result.errorCount > 0)
            return ResultOrError!Symbol(result.errorCount);
        if (!result.value)
            return errorfSymbolResult(formatLocation,
                "symbol function for this type is not supported or not implemented");
        return ResultOrError!Symbol(result.value);
    }
}

private class enumCall : BuiltinSemanticCall
{
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        auto symbols = new string[syntaxArgs.length];
        foreach (i, arg; syntaxArgs)
        {
            symbols[i] = arg.source;
        }
        return NodeResult(new EnumType(getSyntaxNode, symbols));
    }
}

private class dumpSymbolTableCall : BuiltinSemanticCall
{
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        if (syntaxArgs.length != 0)
        {
            assert(0, "dumpSymbolTable function with more than 0 arguments is not implemented");
        }
        scope_.dumpSymbols();
        return NodeResult(new VoidValue(syntaxNode));
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
private class importCall : BuiltinSemanticCall
{
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    SymbolTableEntry symbolTableEntry;
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        foreach (i; 0 .. syntaxArgs.length)
        {
            auto syntaxArg = &syntaxArgs[i];
            auto symbolResult = tryAnalyzeToSymbolPass1(syntaxArg);
            if (symbolResult.errorCount > 0)
                return symbolResult.errorCount;
            if (!symbolResult.value)
                return errorfUint(syntaxArg.formatLocation, "import expects symbols, but got '%s'", syntaxArg.type);

            // TODO: call.semanticArgs[i] = result.value ???
            auto moduleName = symbolResult.value.value;
            auto moduleBaseName = sliceModuleBaseName(moduleName);
            this.symbolTableEntry = scope_.tryAddOrPrintError(moduleBaseName, new LazyImport(syntaxNode, moduleName), formatLocation);
            if (!this.symbolTableEntry)
                return 1;
        }
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        return NodeResult(new VoidValue(syntaxNode));
        //return NodeResult(this.symbolTableEntry.unconst);
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
        auto mod = loadImport(importSyntaxNode, moduleName);
        if (!mod)
            return NodeResult(1);
        return NodeResult(new ImportedModule(importSyntaxNode, mod));
    }
    //
    // SemanticNode methods
    //
    protected final override const(SyntaxNode)* getSyntaxNode() const { return importSyntaxNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { assert(0, "not implemented"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "codebug: SemanticFunction nodes shouldn't exist at interpret time");
    }
}

class setCall : BuiltinSemanticCall
{
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    SymbolTableEntry symbolTableEntry;
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        if (checkSyntaxArgCount(this, 2).failed)
            return 1;

        auto symbolSyntaxNode = &syntaxArgs[0];
        auto symbolResult = tryAnalyzeToSymbolPass1(symbolSyntaxNode);
        if (symbolResult.errorCount > 0)
            return symbolResult.errorCount;
        if (!symbolResult.value)
            return errorfUint(symbolSyntaxNode.formatLocation, "argument 1 of 'set' should be a symbol, but got '%s'", symbolSyntaxNode.type);

        auto rhsNode = newSemanticNode(&syntaxArgs[1]);
        {
            auto errorCount = inTreeOrderAnalyzeExpressionPass1(scope_, rhsNode);
            if (errorCount > 0)
                return errorCount;
        }
        this.symbolTableEntry = scope_.tryAddOrPrintError(symbolResult.value.value, rhsNode, formatLocation);
        if (!this.symbolTableEntry)
            return 1;
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        // TODO: maybe return the symbol table entry, make sure it is "ignorable" if we do so
        //return NodeResult(new VoidValue(syntaxNode));
        //return NodeResult(symbolTableEntry.unconst);
        return NodeResult(new SetReturnNode(this.unconst));
    }
}

private class setBuiltinFunctionCall : BuiltinSemanticCall
{
    private SymbolTableEntry symbolTableEntry;
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        if (checkSyntaxArgCount(this, 1).failed)
            return 1;

        auto symbolSyntaxNode = &syntaxArgs[0];
        auto symbolResult = tryAnalyzeToSymbolPass1(symbolSyntaxNode);
        if (symbolResult.errorCount > 0)
            return symbolResult.errorCount;
        if (!symbolResult.value)
            return errorfUint(symbolSyntaxNode.formatLocation, "argument 1 of 'setBuiltinFunction' should be a symbol, but got '%s'", symbolSyntaxNode.type);

        auto function_ = tryGetHiddenBuiltinRegularFunction(symbolResult.value.value);
        if (function_ is null)
            return errorfUint(formatLocation, "builtin function '%s' does not exist", symbolResult.value.value);

        //
        // TODO: verify function_.interface matches the builtin function interface
        //
        this.symbolTableEntry = scope_.tryAddOrPrintError(symbolResult.value.value, function_, formatLocation);
        if (!this.symbolTableEntry)
            return 1;
        return 0;
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        // TODO: maybe return the function itself
        //return NodeResult(new VoidValue(call.getSyntaxNode));
        return NodeResult(symbolTableEntry.unconst);
    }
}

/+
class callFunction : SemanticFunction
{
    enum isSemanticCall = cast(void*)1;
    enum isRegularCall  = cast(void*)2;

    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("call"/*, SemanticArgType.syntaxNode, null*/);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call)
    {
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
+/
private class callCall : BuiltinSemanticCall
{
    uarray!SemanticNode semanticNodes;
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        if (syntaxArgs.length == 0)
            return errorfUint(formatLocation, "'call' requires at least 1 argument");

        // For now we only support regular calls
        semanticNodes = newSemanticNodes(syntaxArgs);
        uint errorCount = 0;
        foreach (semanticNode; semanticNodes)
        {
            errorCount += inTreeOrderAnalyzeExpressionPass1(scope_, semanticNode);
        }
        return errorCount;

        /+
        auto funcNode = newSemanticNode(&syntaxArgs[0]);
        {
            auto errorCount = inTreeOrderAnalyzeExpressionPass1(scope_, funcNode);
            if (errorCount > 0)
                return errorCount;
        }
        auto funcSymbol = tryAnalyzeToSymbolPass1(&syntaxArgs[0]);
        auto loweredSyntaxArgs = syntaxArgs[1 .. $];
        if (funcSymbol)
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
        +/
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        assert(0, "not impl");
        /+
        auto functionArg = newSemanticNode(&syntaxArgs[0]);

        this.regularCall = new RegularCall(syntaxNode, )
    const(SyntaxNode)* syntaxNode;
    uarray!SemanticNode arguments; // TODO: rename to semanticArgs
    string functionNameToResolve;
    RegularFunction function_;
    this(const(SyntaxNode)* syntaxNode, string functionNameToResolve,
        uarray!SyntaxNode syntaxArgs, uarray!SemanticNode arguments)
    {
        this.syntaxNode = syntaxNode;
        this.functionNameToResolve = functionNameToResolve;
        this.arguments = arguments;
    }
    this(const(SyntaxNode)* syntaxNode, RegularFunction function_,
        uarray!SyntaxNode syntaxArgs, uarray!SemanticNode arguments)
    {
        this.syntaxNode = syntaxNode;
        this.function_ = function_;
        this.arguments = arguments;
    }
+/


        // TODO: maybe return the function itself
        //return NodeResult(new VoidValue(call.getSyntaxNode));
        //return NodeResult(symbolTableEntry.unconst);
    }
}

/*
struct FunctionStaticData
{
    uarray!SemanticNode nodes;
    uint analyzeIndex;
    uint returnTypeIndex;
    bool externC;
    bool builtin;
    Rebindable!IType returnType;
}
*/
private class functionCall : BuiltinSemanticCall
{
    private IScope scope_;
    private this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, functionName, syntaxArgs);
    }
    final override uint inTreeOrderInterpretPass1(IScope scope_)
    {
        this.scope_ = scope_;
        // do nothing for now, maybe we will need to analyze the function body later?
        // or maybe tha would be done later
        return 0;
    }
    private final UserDefinedFunction makeFunction() const
    {
        assert(scope_, "codebug: pass1 was not called?");
        assert(syntaxNode.type == SyntaxNodeType.call, "codebug?");
        return new UserDefinedFunction(scope_.unconst, syntaxNode, syntaxNode.call.arguments.unconst);
    }
    final override NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const
    {
        return NodeResult(makeFunction);
    }
    override OptionalResultOrError!RegularFunction tryInterpretToRegularFunction()
    {
        return OptionalResultOrError!RegularFunction(makeFunction);
    }
}

class jumpBlockFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("jumpBlock"/*, SemanticArgType.syntaxNode, null*/);
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
        super("jumpLoopIf"/*, SemanticArgType.semiAnalyzedSemanticNode, null*/);
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
        super("foreach"/*, SemanticArgType.semiAnalyzedSemanticNode, null*/);
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

class assertFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("assert", immutable RegularFunctionInterface(VoidType.instance, [
            //immutable Parameter("condition", AssertableType.instance),
        ], null, FunctionFlags.none));
    }
    final override SemanticNode interpret(Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs)
    {
        if (runtimeArgs.length != 1)
            assert(0, "assert function expects 1 arguments (TODO: this should be checked during semantic analysis");
        auto cond = runtimeArgs[0];
        auto type = cond.getType();
        auto typeAsIConditional = cast(IConditionalType)type;
        if (!typeAsIConditional)
            assert(0, from!"std.format".format("type '%s' does not implement IConditionalType", type));
        auto result = typeAsIConditional.isTrue(cond);
        if (!result)
        {
            errorf(call.formatLocation, "assert failed: %s", call.syntaxNode.source);
            throw quit;
        }
        return null;
    }
}
class equalsFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("equals", immutable RegularFunctionInterface(BoolType.instance, [
            //immutable Parameter(null, new immutable Multi(PrintableType.instance, positiveRangeFrom(1))),
        ], null, FunctionFlags.none));
    }
    final override SemanticNode interpret(Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs)
    {
        if (runtimeArgs.length != 2)
            assert(0, "equals function expects 2 arguments (TODO: this should be checked during semantic analysis");
        auto lhs = runtimeArgs[0];
        auto type = lhs.getType();
        auto typeAsEquatable = cast(IEquatableType)type;
        if (!typeAsEquatable)
            assert(0, from!"std.format".format("lhs type '%s' is not IEquatableType", type));
        return new Bool(call.getSyntaxNode, typeAsEquatable.equals(lhs, runtimeArgs[1]));
    }
}
class lengthFunction : BuiltinRegularFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super("length", immutable RegularFunctionInterface(AnySingleThing.instance, [
            immutable Parameter(null, AnySingleThing.instance),
        ], null, FunctionFlags.none));
    }
    final override SemanticNode interpret(Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs)
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
    final override SemanticNode interpret(Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs)
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
    final override SemanticNode interpret(Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs)
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
    final override SemanticNode interpret(Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs)
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
    final override SemanticNode interpret(Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs)
    {
        bool addNewline = true;

        uint argumentIndex = 0;
        if (argumentIndex < runtimeArgs.length)
        {
            auto arg = runtimeArgs[argumentIndex];
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

        foreach (arg; runtimeArgs[argumentIndex .. $])
        {
            // TODO: use runtime value in the future

            //from!"std.stdio".writefln("typed value is %s", typedValue.type.formatType);
            arg.printFormatter(&from!"std.stdio".stdout.write!(const(char)[]), interpreter);
        }

        if (addNewline)
        {
            from!"std.stdio".writeln();
        }
        return null;
    }
}
