module builtin;

import std.typecons : Flag, Yes, No, scoped;

import more.alloc;
import more.builder;

import typecons : Rebindable, rebindable;
import common : from, toImmutable, singleton, quit;
import syntax : SyntaxNodeType, SyntaxNode, CallSyntaxNode, base;
import semantics;
import types;// : VoidType, Anything, NumberType, SymbolType, Multi, StringType, PrintableType;
import mod : loadImport;
import analyzer : analyzeValue, analyzeRuntimeCall;
import interpreter : Interpreter;

//
// Syntax Functions
//
// Syntax Functions don't operate on the scope, they simply take syntax, perform some operation on it
// and then return a value.
auto tryFindSyntaxFunction(const(CallSyntaxNode)* call)
{
    auto functionName = call.functionName;
    if (functionName == "flag")
        return &flagFunction;
    if (functionName == "symbol")
        return &symbolFunction;
    if (functionName == "enum")
        return &enumFunction;
    return null;
}
SemanticNode* symbolFunction(const(CallSyntaxNode)* call)
{
    if (call.arguments.length != 1)
    {
        assert(0, "symbol function with more than 1 argument is not implemented");
    }
    auto arg = call.arguments[0];
    if (arg.type == SyntaxNodeType.symbol || arg.type == SyntaxNodeType.keyword)
    {
        auto node = new SemanticNode();
        node.initSymbol(call.base, arg.source);
        return node;
    }
    assert(0, "symbol function for this type is not implemented");
}
SemanticNode* flagFunction(const(CallSyntaxNode)* call)
{
    if (call.arguments.length != 1)
    {
        assert(0, "not implemented");
    }
    auto arg = call.arguments[0];
    if (arg.type != SyntaxNodeType.symbol)
    {
        assert(0, "not implemented");
    }
    auto node = new SemanticNode();
    node.initTypedValue(call.base, FlagType.instance.createTypedValue(arg.source));
    return node;
}
SemanticNode* enumFunction(const(CallSyntaxNode)* call)
{
    auto symbols = new string[call.arguments.length];
    foreach (i, arg; call.arguments)
    {
        symbols[i] = arg.source;
    }
    auto node = new SemanticNode();
    node.initTypedValue(call.base, TypeType.instance.createTypedValue(new EnumType(symbols)));
    return node;
}

void assertFunctionArgCount(IReadOnlyScope scope_, SemanticCall* call, uint expectedArgCount)
{
    if (call.arguments.length != expectedArgCount)
    {
        from!"std.stdio".writefln("%sError: the '%s' function requires %s argument%s but got %s",
            scope_.getModule.formatLocation(call.syntaxNode.source), call.syntaxNode.functionName,
            expectedArgCount, (expectedArgCount == 1) ? "" : "s", call.syntaxNode.arguments.length);
        throw quit;
    }
}

string argAsSymbol(IReadOnlyScope scope_, SemanticCall* call, uint argIndex)
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
    //if (call.functionName == "tuple")
    //    return cast()tupleFunction.instance.checkAndGet(call);
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
    if (call.functionName == "foreach")
        return cast()foreachFunction.instance.checkAndGet(call);
    // TODO: look for custom analyze-time functions

    return null;
    // TODO: look for custom analyze-time functions

    return null;
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
        /*
        super(immutable FunctionInterface(FunctionType.syntax, [
            immutable Parameter(null, new immutable Multi(SymbolType.instance, positiveRangeFrom(1))),
        ], null, FunctionFlags.none));
        */
    }
    final override SemanticCallResult interpret(IScope scope_, SemanticCall* call, Flag!"used" used) const
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
            auto import_ = loadImport(symbol);
            scope_.add(import_.baseID.toString, import_.asTypedValue);
        }
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
        //return ResolveResult(TypedValue.void_);
    }
}
class setFunction : SemanticFunction
{
    mixin singleton;
    final override SemanticCallResult interpret(IScope scope_, SemanticCall* call, Flag!"used" used) const
    {
        assertFunctionArgCount(scope_, call, 2);

        auto symbol = argAsSymbol(scope_, call, 0);
        auto def    = &call.syntaxNode.arguments[1];
        // TODO: if call.arguments[1] is not a typed value, or can't be converted to one,
        //       then I could probably create a new typed value node that wraps the actual value
        scope_.add(symbol, call.arguments[1].getAnalyzedTypedValue(Yes.resolveSymbol));
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
        //return ResolveResult(TypedValue.void_);
    }
}
class setBuiltinFunctionFunction : SemanticFunction
{
    mixin singleton;
    final override SemanticCallResult interpret(IScope scope_, SemanticCall* call, Flag!"used" used) const
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
        return SemanticCallResult(SemanticNode.newVoid(call.syntaxNode.base));
        //return ResolveResult(TypedValue.void_);
    }
}
class callFunction : SemanticFunction
{
    mixin singleton;

    final override SemanticCallResult interpret(IScope scope_, SemanticCall* call, Flag!"used" used) const
    {
        if (call.syntaxNode.arguments.length == 0)
        {
            from!"std.stdio".writefln("Error: the '%s' function requires at least 1 argument", call.syntaxNode.functionName);
            throw quit;
        }
        auto firstArgNode = call.arguments[0];
        if (!analyzeValue(scope_, &firstArgNode, Yes.resolveSymbols, used))
        {
            return SemanticCallResult.noEntryButMoreSymbolsCouldBeAdded;
        }
        auto firstArgTypedValue = firstArgNode.getAnalyzedTypedValue(Yes.resolveSymbol);
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
                    semanticFunction, call.arguments[1 .. $], call.arguments.length - 1, null);
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
            firstArgTypedValue.type.formatType);
        throw quit;
    }
}

class functionFunction : SemanticFunctionCannotAddSymbols
{
    mixin singleton;

    mixin SemanticFunctionCannotAddSymbolsMixin;
    pragma(inline) private final SemanticCallResult interpretReadonlyScope(
        IReadOnlyScope scope_, SemanticCall* call, Flag!"used" used) const
    {
        //
        // process function flags
        //
        uint nextArgIndex = 0;
        Rebindable!TypedValue currentArg = void;

        bool builtin = false;

        for (;; nextArgIndex++)
        {
            if (nextArgIndex >= call.arguments.length)
            {
                from!"std.stdio".writefln("Error: not enough arguments for '%s'", call.syntaxNode.functionName);
                throw quit;
            }

            currentArg = call.arguments[nextArgIndex].getAnalyzedTypedValue(Yes.resolveSymbol);
            auto asFlagType = cast(FlagType)currentArg.type;
            if (!asFlagType)
            {
                break;
            }
            auto flagName = asFlagType.get(currentArg.value);
            if (flagName == "builtin")
            {
                builtin = true;
            }
            else
            {
                from!"std.stdio".writefln("Error: unknown flag(%s)", flagName);
                throw quit;
            }
        }

        //
        // process return type
        //
        Rebindable!IType returnType = void;
        {
            auto asTypeType = cast(TypeType)currentArg.type;
            if (!asTypeType)
            {
                from!"std.stdio".writefln("Error: expected a return type but got %s", currentArg.type.formatType);
                throw quit;
            }
            returnType = asTypeType.get(currentArg.value);
        }

        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // IMPLEMENT THIS LATER
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!
        //assert(0, "not implemented");
        auto parameters = new immutable(Parameter)[0];
        auto flagParameters = new immutable(string)[0];
        auto functionFlags = FunctionFlags.none;

        auto interface_ = immutable RuntimeFunctionInterface(
            returnType.toImmutable, parameters, flagParameters, functionFlags
        );

        if (builtin)
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
        //return ResolveResult(RuntimeFunctionType.instance.createTypedValue(
        //    new UserDefinedFunction(scope_.getModule(), interface_, body_)));
    }
}

class foreachFunction : SemanticFunctionCannotAddSymbols
{
    mixin singleton;

    mixin SemanticFunctionCannotAddSymbolsMixin;
    pragma(inline) private final SemanticCallResult interpretReadonlyScope(
        IReadOnlyScope scope_, SemanticCall* call, Flag!"used" used) const
    {
        assertFunctionArgCount(scope_, call, 3);
        auto loopSymbol = argAsSymbol(scope_, call, 0);

        /+
        auto rangeArgument = &call.arguments[1];
        if (!analyzeValue(scope_, rangeArgument, Yes.resolveSymbols, used))
        {
            return SemanticCallResult.noEntryButMoreSymbolsCouldBeAdded;
        }
        auto rangeArgumentTypedValue = rangeArgument.getAnalyzedTypedValue(Yes.resolveSymbol);

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
class tupleFunction : SemanticFunctionCannotAddSymbols
{
    mixin singleton;

    mixin SemanticFunctionCannotAddSymbols;
    pragma(inline) private final SemanticCallResult interpretReadonlyScope(
        IReadOnlyScope scope_, SemanticCall* call, Flag!"used" used) const
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

        ushort argumentIndex = 0;
        if (argumentIndex < call.arguments.length)
        {
            auto arg = call.arguments[argumentIndex].getAnalyzedTypedValue(Yes.resolveSymbol);
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
            auto typedValue = arg.getAnalyzedTypedValue(Yes.resolveSymbol);
            //from!"std.stdio".writefln("typed value is %s", typedValue.type.formatType);
            auto asPrinterType = cast(IValuePrinter)typedValue.type;
            if (asPrinterType is null)
            {
                from!"std.stdio".writefln("Error: type '%s' does not implement IValuePrinter",
                    typedValue.type.formatType);
                throw quit;
            }
            asPrinterType.print(&from!"std.stdio".stdout.write!(const(char)[]), typedValue.value);
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