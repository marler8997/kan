module builtin;

import std.typecons : Flag, Yes, No, scoped;

import more.alloc;
import more.builder;

import typecons : Rebindable, rebindable;
import common : from, unconst, toImmutable, toConst, uarray, singleton, quit;
import syntax : SyntaxNodeType, SyntaxNode, CallSyntaxNode, base;
import semantics;
import types;// : VoidType, Anything, NumberType, SymbolType, Multi, StringType, PrintableType;
import symtab : SymbolTable;
import mod : sliceModuleBaseName, loadImport, Module;
import analyzer : AnalyzeState, analyzeStatementNode, analyzeValue, analyzeRuntimeCall;
import interpreter : Interpreter;

void assertFunctionArgCount(IReadonlyScope scope_, SemanticCall* call, uint expectedArgCount)
{
    if (call.arguments.length != expectedArgCount)
    {
        from!"std.stdio".writefln("%sError: the '%s' function requires %s argument%s but got %s",
            scope_.getModule.formatLocation(call.syntaxNode.source), call.syntaxNode.functionName,
            expectedArgCount, (expectedArgCount == 1) ? "" : "s", call.syntaxNode.arguments.length);
        throw quit;
    }
}

string argAsSymbol(IReadonlyScope scope_, SemanticCall* call, uint argIndex)
   in { assert(argIndex < call.arguments.length, "code bug"); } do
{
    auto arg = &call.arguments[argIndex];
    auto symbol = tryEvaluateToSymbol(arg);
    if (symbol is null)
    {
        from!"std.stdio".writefln("%sError: the '%s' function requires as symbol for the argument at index %s but got '%s'",
            scope_.getModule.formatLocation(arg.syntaxNode.source), call.syntaxNode.functionName, argIndex, *arg);
        throw quit;
    }
    return symbol;
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
SemanticFunction tryFindSemanticFunction(const(CallSyntaxNode)* call)
{
    //
    // Syntax Only Functions
    //
    if (call.functionName == "flag")
        return cast()flagFunction.instance.checkAndGet(call);
    if (call.functionName == "symbol")
        return cast()symbolFunction.instance.checkAndGet(call);
    if (call.functionName == "enum")
        return cast()enumFunction.instance.checkAndGet(call);
    if (call.functionName == "dumpSymbolTable")
        return cast()dumpSymbolTableFunction.instance.checkAndGet(call);

    //
    // All Other Semantic Functions
    //
    if (call.functionName == "import")
        return cast()importFunction.instance.checkAndGet(call);
    if (call.functionName == "set")
        return cast()setFunction.instance.checkAndGet(call);
    if (call.functionName == "setBultinFunction")
        return cast()setBuiltinFunctionFunction.instance.checkAndGet(call);
    if (call.functionName == "function")
        return cast()functionFunction.instance.checkAndGet(call);
    if (call.functionName == "call")
        return cast()callFunction.instance.checkAndGet(call);
    if (call.functionName == "jumpBlock")
        return cast()jumpBlockFunction.instance.checkAndGet(call);
    if (call.functionName == "jumpLoopIf")
        return cast()jumpLoopIfFunction.instance.checkAndGet(call);
    if (call.functionName == "foreach")
        return cast()foreachFunction.instance.checkAndGet(call);
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
        super(SemanticArgType.syntaxNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        if (call.syntaxNode.arguments.length != 1)
        {
            assert(0, "not implemented");
        }
        auto arg = call.syntaxNode.arguments[0];
        if (arg.type != SyntaxNodeType.symbol)
        {
            assert(0, "not implemented");
        }
        auto node = new SemanticNode();
        node.initTypedValue(call.syntaxNode.base, FlagType.instance.createTypedValue(arg.source));
        return SemanticCallResult(node);
    }
}
class symbolFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.syntaxNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        if (call.syntaxNode.arguments.length != 1)
        {
            assert(0, "symbol function with more than 1 argument is not implemented");
        }
        auto arg = call.syntaxNode.arguments[0];
        if (arg.type == SyntaxNodeType.symbol || arg.type == SyntaxNodeType.keyword)
        {
            auto node = new SemanticNode();
            node.initSymbol(call.syntaxNode.base, arg.source);
            return SemanticCallResult(node);
        }
        assert(0, "symbol function for this type is not implemented");
    }
}
class enumFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.syntaxNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        auto symbols = new string[call.syntaxNode.arguments.length];
        foreach (i, arg; call.syntaxNode.arguments)
        {
            symbols[i] = arg.source;
        }
        auto node = new SemanticNode();
        node.initTypedValue(call.syntaxNode.base, TypeType.instance.createTypedValue(new EnumType(symbols)));
        return SemanticCallResult(node);
    }
}

class dumpSymbolTableFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.syntaxNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // do nothing, no symbols to add
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        if (call.syntaxNode.arguments.length != 0)
        {
            assert(0, "dumpSymbolTable function with more than 0 arguments is not implemented");
        }
        scope_.dumpSymbols();
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
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
class importFunction : SemanticFunctionThanCanAddSymbols
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.semiAnalyzedSemanticNode, null);
        /*
        super(immutable FunctionInterface(FunctionType.syntax, [
            immutable Parameter(null, new immutable Multi(SymbolType.instance, positiveRangeFrom(1))),
        ], null, FunctionFlags.none));
        */
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        foreach (ref argNode; call.arguments)
        {
            auto symbol = tryEvaluateToSymbol(&argNode);
            if (symbol is null)
            {
                from!"std.stdio".writefln("%sError: the '%s' function only accepts symbols but got '%s'",
                    scope_.getModule.formatLocation(argNode.syntaxNode.source), call.syntaxNode.functionName, argNode);
                throw quit;
            }
            auto moduleBaseName = sliceModuleBaseName(symbol);
            from!"std.stdio".writefln("[DEBUG] import '%s'",moduleBaseName);
            scope_.addUnevaluatedSymbol(moduleBaseName);
        }
        return 0;
    }
    mixin SemanticFunctionThanCanAddSymbolsMixin;
    pragma(inline) final SemanticCallResult interpretWriteableScope(IScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        /+
        foreach (ref argNode; call.arguments)
        {
            auto symbol = tryEvaluateToSymbol(&argNode);
            assert(symbol, "CodeBug: this should have been checked on pass 1");
            auto import_ = loadImport(symbol);
            scope_.evaluated(import_.baseID.toString, import_.asTypedValue);
        }
        +/
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
    }
}
class importEvaluator : UnevaluatedSymbol
{
    SemanticCall* importCall;
    this(IScope evaluateScope, SemanticCall* importCall)
    {
        super(evaluateScope);
        this.importCall = importCall;
    }
    override TypedValue tryEvaluate()
    {
        assert(0, "not implemented");
    }
}

class setFunction : SemanticFunctionThanCanAddSymbols
{
    mixin singleton!(No.ctor);
    private string symbol;
    private this() immutable
    {
        super(SemanticArgType.fullyAnalyzedSemanticNode, [
            immutable NumberedSemanticArgType(SemanticArgType.semiAnalyzedSemanticNode, 1)
        ]);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        assertFunctionArgCount(scope_, call, 2);

        this.symbol = argAsSymbol(scope_, call, 0);
        auto def    = &call.syntaxNode.arguments[1];
        // TODO: if call.arguments[1] is not a typed value, or can't be converted to one,
        //       then I could probably create a new typed value node that wraps the actual value
        //scope_.add(symbol, call.arguments[1].getAnalyzedTypedValue(Yes.resolveSymbols));
        scope_.addUnevaluatedSymbol(symbol);
        return 0;
    }
    mixin SemanticFunctionThanCanAddSymbolsMixin;
    pragma(inline) final SemanticCallResult interpretWriteableScope(IScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        scope_.evaluated(this.symbol, call.arguments[1].getAnalyzedTypedValue(Yes.resolveSymbols));
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
    }
}
class setBuiltinFunctionFunction : SemanticFunctionThanCanAddSymbols
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.semiAnalyzedSemanticNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        assertFunctionArgCount(scope_, call, 1);
        auto symbol = &call.syntaxNode.arguments[0];
        if (symbol.type != SyntaxNodeType.symbol)
        {
            from!"std.stdio".writefln("%sError: the '%s' function requires a symbol as the first argument but got %s",
                scope_.getModule.formatLocation(call.syntaxNode.source), call.syntaxNode.functionName, symbol.type);
            throw quit;
        }
        auto function_ = tryGetHiddenBuiltinRuntimeFunction(symbol.source);
        if (function_ is null)
        {
            from!"std.stdio".writefln("%sError: builtin function '%s' does not exist",
                scope_.getModule.formatLocation(call.syntaxNode.source), call.syntaxNode.functionName);
            throw quit;
        }
        //
        // TODO: verify function_.interface matches the builtin function interface
        //
        scope_.add(symbol.source, BuiltinRuntimeFunctionType.instance.createTypedValue(function_));
        return 0;
    }
    mixin SemanticFunctionThanCanAddSymbolsMixin;
    pragma(inline) final SemanticCallResult interpretWriteableScope(IScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
    }
}
class callFunction : SemanticFunctionThanCanAddSymbols
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.semiAnalyzedSemanticNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        assert(0, "call pass1 not implemented");
        return 0;
    }
    mixin SemanticFunctionThanCanAddSymbolsMixin;
    pragma(inline) final SemanticCallResult interpretWriteableScope(IScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        if (call.syntaxNode.arguments.length == 0)
        {
            from!"std.stdio".writefln("Error: the '%s' function requires at least 1 argument", call.syntaxNode.functionName);
            throw quit;
        }
        auto firstArgNode = call.arguments[0];
        if (!analyzeValue(scope_, &firstArgNode, Yes.resolveSymbols/*, used*/))
        {
            return SemanticCallResult.noEntryButMoreSymbolsCouldBeAdded;
        }
        auto firstArgTypedValue = firstArgNode.getAnalyzedTypedValue(Yes.resolveSymbols);
        /*
        if (firstArgNode.type != SemanticNodeType.typedValue)
        {
            assert(0, "not implemented: first argument of call type semantic node type is " ~ from!"std.conv".to!string(firstArgNode.type));
        }
        */

        // Check if it is a semantic function
        {
            auto asSemanticFunctionType = cast(SemanticFunctionType)firstArgTypedValue.type;
            if (asSemanticFunctionType !is null)
            {
                auto semanticFunction = asSemanticFunctionType.get(firstArgTypedValue.value);

                auto semanticCallNode = new SemanticNode();
                semanticCallNode.nodeType = SemanticNodeType.semanticCall;
                semanticCallNode.semanticCall = SemanticCall(call.syntaxNode,
                    rebindable(SemanticCallType.instance.createTypedValue(&semanticCallNode.semanticCall)),
                    semanticFunction.unconst, call.arguments[1 .. $], call.arguments.length - 1, null);
                // TODO: create a new semantic node call with the proper arguments
                //return semanticFunction.interpret(scope_, call, used);
                return SemanticCallResult(semanticCallNode);
            }
        }

        {
            auto asRuntimeFunctionType = cast(RuntimeFunctionType)firstArgTypedValue.type;
            if (asRuntimeFunctionType !is null)
            {
            //from!"std.stdio".writefln("%s HERE", scope_.getModule.formatLocation(call.syntaxNode.source));
                auto runtimeFunction = asRuntimeFunctionType.get(firstArgTypedValue.value);
                assert(runtimeFunction !is null, "code bug");

                auto runtimeCallNode = new SemanticNode();
                runtimeCallNode.runtimeCall = RuntimeCall(
                    call.syntaxNode,
                    rebindable(RuntimeCallType.instance.createTypedValue(&runtimeCallNode.runtimeCall)),
                    call.arguments[1 .. $],
                    rebindable(firstArgTypedValue));
                runtimeCallNode.nodeType = SemanticNodeType.runtimeCall;
                return SemanticCallResult(runtimeCallNode);
            }
        }

        from!"std.stdio".writefln("Error: first argument of 'call' must be a function but it's type is '%s'",
            firstArgTypedValue.type.formatName);
        throw quit;
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
        super(SemanticArgType.syntaxNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // do nothing for now, maybe we will need to analyze the function body later?
        // or maybe tha would be done later
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        static const(IType) convertReturnType(const(TypedValue) typedValue)
        {
            auto asTypeType = cast(TypeType)typedValue.type;
            if (!asTypeType)
            {
                from!"std.stdio".writefln("Error: expected a return type but got %s", typedValue.type.formatName);
                throw quit;
            }
            return asTypeType.get(typedValue.value);
        }

        auto data = call.getStaticDataStruct!FunctionStaticData();
        if (data.newlyAllocated)
        {
            data.nodes = createSemanticNodes(call.syntaxNode.arguments);

            data.analyzeIndex = 0;
            for (;; data.analyzeIndex++)
            {
                if (data.analyzeIndex >= data.nodes.length)
                {
                    from!"std.stdio".writefln("Error: not enough arguments for '%s'", call.syntaxNode.functionName);
                    throw quit;
                }

                auto result = analyzeValue(scope_, &data.nodes[data.analyzeIndex], Yes.resolveSymbols);

                // NOTE: flags always analyze successfully, so there must be no flags
                if (result != Yes.analyzed)
                {
                    data.returnTypeIndex = data.analyzeIndex;
                    // TODO: result should indicate what kind of semantic error to return
                    assert(0, "not implemented");
                    // return result;
                }

                auto typedValue = data.nodes[data.analyzeIndex].getAnalyzedTypedValue(Yes.resolveSymbols);
                auto asFlagType = cast(FlagType)typedValue.type;
                if (!asFlagType)
                {
                    data.returnTypeIndex = data.analyzeIndex;
                    data.analyzeIndex++;
                    data.returnType = rebindable(convertReturnType(typedValue));
                    break;
                }

                auto flagName = asFlagType.get(typedValue.value);
                if (flagName == "externC")
                    data.externC = true;
                else if (flagName == "builtin")
                    data.builtin = true;
                else
                {
                    from!"std.stdio".writefln("Error: unknown function flag(%s)", flagName);
                    throw quit;
                }
            }
            if (data.returnTypeIndex + 3 != data.nodes.length)
            {
                from!"std.stdio".writefln("Error: invalid number of arguments for the 'function' function");
                throw quit;
            }
        }

        if (data.analyzeIndex == data.returnTypeIndex)
        {
            auto returnTypeNode = data.nodes[data.analyzeIndex];
            auto result = analyzeValue(scope_, &returnTypeNode, Yes.resolveSymbols);
            if (result != Yes.analyzed)
            {
                // TODO: result should indicate what kind of semantic error to return
                assert(0, "not implemented");
                // return result;
            }
            auto typedValue = data.nodes[data.analyzeIndex].getAnalyzedTypedValue(Yes.resolveSymbols);
            data.analyzeIndex++;
            data.returnType = rebindable(convertReturnType(typedValue));
        }
        if (data.analyzeIndex == data.returnTypeIndex + 1)
        {
            auto result = analyzeValue(scope_, &data.nodes[data.analyzeIndex], Yes.resolveSymbols);
            if (result != Yes.analyzed)
            {
                // TODO: result should indicate what kind of semantic error to return
                assert(0, "not implemented");
            }
            data.analyzeIndex++;
        }
        assert(data.analyzeIndex == data.returnTypeIndex + 2);

        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // IMPLEMENT THIS LATER
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        auto parameters = new immutable(Parameter)[0];
        auto flagParameters = new immutable(string)[0];
        auto functionFlags = FunctionFlags.none;

        auto interface_ = immutable RuntimeFunctionInterface(
            data.returnType.val.toImmutable,
            parameters, // parameters not implemented
            flagParameters, //flag parameters not implemented
            functionFlags // not implemented
        );

        if (data.builtin)
        {
            assert(0, "not implemented");
        }

        auto bodyTuple = call.syntaxNode.arguments.last;
        if (bodyTuple.type != SyntaxNodeType.tuple)
        {
            assert(0, "not implemted");
        }
        auto body_ = bodyTuple.tuple.nodes;

        return SemanticCallResult(SemanticNode.newTypedValue(call.syntaxNode.base,
            RuntimeFunctionType.instance.createTypedValue(
                new UserDefinedFunction(scope_.getModule(), interface_, body_))));
    }
}

class jumpBlockFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.syntaxNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // do nothing for now...
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        auto block = new SemanticNode();
        block.initializeStatementBlock(
            call.syntaxNode.base,
            BlockFlags.isJumpBlock,
            new JumpBlock(scope_),
            createSemanticNodes(call.syntaxNode.arguments)
        );
        return SemanticCallResult(block);

        /+
        assert(call.returnValue is null);

        call.returnValue = new SemanticNode();
        call.returnValue.initializeStatementBlock(
            call.syntaxNode.base,
            BlockFlags.isJumpBlock,
            new JumpBlockScope(scope_),
            createSemanticNodes(call.syntaxNode.arguments)
        );
        +/



        /+
        auto data = call.getStaticDataStruct!JumpBlockStaticData();
        if (data.newlyAllocated)
        {
            data.scope_ = new JumpBlockScope(scope_);
            data.nodes = createSemanticNodes(call.syntaxNode.arguments);
        }

        for (; data.nodesAnalyzed < data.nodes.length; data.nodesAnalyzed++)
        {
            auto result = analyzeStatementNode(data.scope_, &data.nodes[data.nodesAnalyzed]);
            if (result != AnalyzeState.analyzed)
            {
                // TODO: may not be the correct return value
                return SemanticCallResult.noEntryButMoreSymbolsCouldBeAdded;
            }
        }
        from!"std.stdio".writeln("WARNING: jumpBlock not fully implemented");
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
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
        super(SemanticArgType.semiAnalyzedSemanticNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // do nothing for now...
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        if (call.syntaxNode.arguments.length != 1)
        {
            from!"std.stdio".writefln("Error: function '%s' requires 1 argument but got %s",
                call.syntaxNode.functionName, call.syntaxNode.arguments);
            throw quit;
        }
        auto condition = new SemanticNode();
        condition.initialize(&call.syntaxNode.arguments[0]);

        auto jumpNode = new SemanticNode();
        jumpNode.initializeJumpNode(call.syntaxNode.base, JumpType.loopCurrentBlock, condition);

        return SemanticCallResult(jumpNode);
    }
}



class foreachFunction : SemanticFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(SemanticArgType.semiAnalyzedSemanticNode, null);
    }
    final override uint interpretPass1(IScope scope_, SemanticCall* call)
    {
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // do nothing for now...
        return 0;
    }
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
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
        //return ResolveResult(TypedValue.void_);
    }
}

/+
NOTE: Removed since syntax '{...}' creates tuples now
class tupleFunction : SemanticFunction
{
    mixin singleton;

    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        return ResolveResult(UntypedNoLimitSetType.instance.createTypedValue(call.arguments));
    }
}
+/

//
// Builtin Runtime Functions
// These functions are implemented by the compiler, but they are "hidden" in that
// they need to be declared in order to be visible.
//
BuiltinRuntimeFunction tryGetHiddenBuiltinRuntimeFunction(string functionName)
{
    if (functionName == "print")
        return cast()printFunction.instance;
    // TODO: look for user-defined functions

    return null;
}
class printFunction : BuiltinRuntimeFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(immutable RuntimeFunctionInterface(VoidType.instance, [
            immutable Parameter(null, new immutable Multi(PrintableType.instance, positiveRangeFrom(1))),
        ], ["noNewline"], FunctionFlags.none));
    }
    final override void interpret(Interpreter* interpreter, RuntimeCall* call)
    {
        bool addNewline = true;

        uint argumentIndex = 0;
        if (argumentIndex < call.arguments.length)
        {
            auto arg = call.arguments[argumentIndex].getAnalyzedTypedValue(Yes.resolveSymbols);
            auto asFlag = arg.tryGetValueAs!FlagType(null);
            if (asFlag !is null)
            {
                if (asFlag == "noNewline")
                {
                    addNewline = false;
                    argumentIndex++;
                }
                else
                {
                    assert(0, "unknown flag for print function");
                }
            }
        }

        foreach (ref arg; call.arguments[argumentIndex .. $])
        {
            // TODO: use runtime value in the future
            auto typedValue = arg.getAnalyzedTypedValue(Yes.resolveSymbols);
            //from!"std.stdio".writefln("typed value is %s", typedValue.type.formatType);
            typedValue.type.formatValue(typedValue.value).toString(
                &from!"std.stdio".stdout.write!(const(char)[]));
        }

        if (addNewline)
        {
            from!"std.stdio".writeln();
        }
    }
}

class leftIsLessFunction : BuiltinRuntimeFunction
{
    mixin singleton!(No.ctor);
    private this() immutable
    {
        super(immutable RuntimeFunctionInterface(BooleanType.instance, [
            immutable Parameter(null, AnySingleThing.instance),
            immutable Parameter(null, AnySingleThing.instance)
        ], null, FunctionFlags.none));
    }
    final override void interpret(Interpreter* interpreter, RuntimeCall* call)
    {
        assert(0, "not implemented");
    }
}
