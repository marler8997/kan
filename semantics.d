module semantics;

import std.typecons : Flag, Yes, No, scoped;
import std.bitmanip : bitfields;
import std.bigint : BigInt;
import std.string : indexOf;
import std.format : formattedWrite;

import more.alloc : GCDoubler;
import more.builder : Builder;
import more.format : StringSink, DelegateFormatter;

import typecons : Rebindable, rebindable;
import common : isNull, uarray, toUarray, from, singleton, quit;
static import global;
import syntax : SyntaxNodeType, SyntaxNode, KeywordType, StringSyntaxNode, TupleSyntaxNode, CallSyntaxNode;
import types;// : Type, VoidType, NumberType, StringType, ModuleType;
import mod : Module;
import builtin : tryFindSyntaxFunction, tryFindSemanticFunction;
static import interpreter;

enum SemanticNodeType : ubyte
{
    // Means there's nothing to really analyze for the semantic node
    typedValue,
    tuple,
    // Means the node represents a symbol
    symbol,
    syntaxCall,
    semanticCall,
    runtimeCall,
}

struct TypedValueNode
{
    // TODO: need to know if this is a compile-time or runtime value
    const(SyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
}
struct TupleNode
{
    const(SyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    uarray!SemanticNode elements;
    uint analyzeElementsIndex;
}
struct SymbolNode
{
    const(SyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    Rebindable!TypedValue resolved;
    @property string symbolString() const { return asTypedValue.value.string_; }
}
struct SyntaxCall
{
    const(CallSyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    SemanticNode* returnValue;
}
struct SemanticCall
{
    const(CallSyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    SemanticFunction function_;
    uarray!SemanticNode arguments;
    uint analyzeArgumentsIndex;
    SemanticNode* returnValue;
}
enum RuntimeCallAnalyzeState : ubyte
{
    analyzeArguments,
    resolveFunctionSymbol,
    analyzeFunctionValue,
    done,
}
pragma(inline) static bool isDone(const RuntimeCallAnalyzeState state)
{
    return state == RuntimeCallAnalyzeState.done;
}
struct RuntimeCall
{
    const(CallSyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    uarray!SemanticNode arguments;
    Rebindable!TypedValue currentFunctionValue;
    RuntimeFunction function_;
    IType returnType;
    RuntimeCallAnalyzeState analyzeState;
    uint analyzeArgumentsIndex;
    void toString(StringSink sink) const
    {
        sink(syntaxNode.source);
        /*
        formattedWrite("")
        formattedWrite(sink, "CallSemanticNode:%s,functionResolved=%s,returnValue=%s",
            syntaxNode.functionName,
            (function_ is null) ? "no" : "yes",
            returnValue.isNull ? "no" : "yes");
            */
    }
}

struct SemanticNode
{
    static SemanticNode nullValue() { return SemanticNode(null); }
    @property bool isNull() const { return syntaxNode is null; }
    static SemanticNode* newVoid(const(SyntaxNode*) syntaxNode)
    {
        auto node = new SemanticNode();
        node.syntaxNode = syntaxNode;
        node.nodeType = SemanticNodeType.typedValue;
        node.asTypedValue = TypedValue.void_;
        return node;
    }
    static SemanticNode* newTypedValue(const(SyntaxNode*) syntaxNode, TypedValue typedValue)
    {
        auto node = new SemanticNode();
        node.syntaxNode = syntaxNode;
        node.nodeType = SemanticNodeType.typedValue;
        node.asTypedValue = typedValue;
        return node;
    }

    union
    {
        struct
        {
            const(SyntaxNode)* syntaxNode = void;
            private Rebindable!TypedValue asTypedValue = void;
        }
        TypedValueNode typedValue = void;
        TupleNode tuple = void;
        SymbolNode symbol = void;
        SyntaxCall syntaxCall = void;
        SemanticCall semanticCall = void;
        RuntimeCall runtimeCall = void;
    }
    SemanticNodeType nodeType;
    static assert(syntaxNode.offsetof == TypedValueNode.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == TupleNode.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == SymbolNode.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == SyntaxCall.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == SemanticCall.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == RuntimeCall.syntaxNode.offsetof);

    static assert(asTypedValue.offsetof == TypedValueNode.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == TupleNode.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == SymbolNode.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == SyntaxCall.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == SemanticCall.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == RuntimeCall.asTypedValue.offsetof);

    pragma(inline) final void initTypedValue(const(SyntaxNode)* syntaxNode, const(TypedValue) typedValueToSet)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        this.typedValue = TypedValueNode(syntaxNode, rebindable(typedValueToSet));
        this.nodeType = SemanticNodeType.typedValue;
    }
    pragma(inline) final void initSymbol(const(SyntaxNode)* syntaxNode)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        this.symbol = SymbolNode(syntaxNode,
            rebindable(SymbolType.instance.createTypedValue(syntaxNode.source)),
            rebindable(TypedValue.nullValue));
        this.nodeType = SemanticNodeType.symbol;
    }
    pragma(inline) final void initSymbol(const(SyntaxNode)* syntaxNode, string symbol)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        this.symbol = SymbolNode(syntaxNode,
            rebindable(SymbolType.instance.createTypedValue(symbol)),
            rebindable(TypedValue.nullValue));
        this.nodeType = SemanticNodeType.symbol;
    }
    void initializeAsRuntimeCall(const(CallSyntaxNode)* syntaxCall)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        auto arguments = new SemanticNode[syntaxCall.arguments.length].toUarray;
        foreach (i, ref argumentSyntaxNode; syntaxCall.arguments)
        {
            arguments[i].initialize(&argumentSyntaxNode);
        }
        this.runtimeCall = RuntimeCall(syntaxCall,
            rebindable(RuntimeCallType.instance.createTypedValue(&this.runtimeCall)), arguments);
        this.nodeType = SemanticNodeType.runtimeCall;
    }
    final void initialize(const(SyntaxNode)* syntaxNode)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        final switch(syntaxNode.type)
        {
        case SyntaxNodeType.number:
            initTypedValue(syntaxNode, NumberLiteralType.instance.createTypedValue(syntaxNode));
            return;
        case SyntaxNodeType.string_:
            initTypedValue(syntaxNode, StringType.instance.createTypedValue(&syntaxNode.str));
            return;
        case SyntaxNodeType.keyword:
            final switch(syntaxNode.keyword.type)
            {
            case KeywordType.void_ : initTypedValue(syntaxNode, TypedValue.void_);
                break;
            case KeywordType.false_: initTypedValue(syntaxNode, BooleanType.instance.createTypedValue(false));
                break;
            case KeywordType.true_ : initTypedValue(syntaxNode, BooleanType.instance.createTypedValue(true));
                break;
            }
            return;
        case SyntaxNodeType.symbol:
            initSymbol(syntaxNode);
            return;
        case SyntaxNodeType.tuple:
            {
                auto arguments = new SemanticNode[syntaxNode.call.arguments.length].toUarray;
                foreach (i, ref argumentSyntaxNode; syntaxNode.call.arguments)
                {
                    arguments[i].initialize(&argumentSyntaxNode);
                }
                this.tuple = TupleNode(syntaxNode,
                    rebindable(TupleLiteralType.instance.createTypedValue(&this.tuple)),
                    arguments, 0);
                this.nodeType = SemanticNodeType.tuple;
            }
            return;
        case SyntaxNodeType.call:
            {
                auto syntaxFunction = tryFindSyntaxFunction(&syntaxNode.call);
                if (syntaxFunction !is null)
                {
                    this.syntaxCall = SyntaxCall(&syntaxNode.call,
                        rebindable(SyntaxCallType.instance.createTypedValue(&this.syntaxCall)),
                        syntaxFunction(&syntaxNode.call));
                    this.nodeType = SemanticNodeType.syntaxCall;
                    return;
                }
            }
            {
                auto function_ = tryFindSemanticFunction(&syntaxNode.call);
                if (function_ !is null)
                {
                    auto arguments = new SemanticNode[syntaxNode.call.arguments.length].toUarray;
                    foreach (i, ref argumentSyntaxNode; syntaxNode.call.arguments)
                    {
                        arguments[i].initialize(&argumentSyntaxNode);
                    }
                    this.semanticCall = SemanticCall(&syntaxNode.call,
                        rebindable(SemanticCallType.instance.createTypedValue(&this.semanticCall)),
                        function_, arguments, 0, null);//rebindable(TypedValue.nullValue));
                    this.nodeType = SemanticNodeType.semanticCall;
                    return;
                }
            }

            initializeAsRuntimeCall(&syntaxNode.call);
            return;
        }
    }

    // NOTE: it is assumed this node has already been analyzed!
    final inout(TypedValue) getAnalyzedTypedValue(Flag!"resolveSymbol" resolveSymbol) inout
    {
        final switch(nodeType)
        {
        case SemanticNodeType.typedValue:
            return cast(inout(TypedValue))asTypedValue;
        case SemanticNodeType.tuple:
            assert(tuple.analyzeElementsIndex == tuple.elements.length, "code bug");
            return cast(inout(TypedValue))asTypedValue;
        case SemanticNodeType.symbol:
            if (!resolveSymbol)
                return cast(inout(TypedValue))asTypedValue;
            if (symbol.resolved.isNull)
            {
                from!"std.stdio".writefln("symbol %s resolved is null", this.symbol.syntaxNode.source);
                assert(0, "not implemented");
            }
            return cast(inout(TypedValue))symbol.resolved;
        case SemanticNodeType.syntaxCall:
            return syntaxCall.returnValue.getAnalyzedTypedValue(resolveSymbol);
        case SemanticNodeType.semanticCall:
            if (semanticCall.returnValue.isNull)
            {
                assert(0, "code bug: getAnalyzedTypedValue should not be called on a node that is not analyzed");
            }
            return semanticCall.returnValue.getAnalyzedTypedValue(resolveSymbol);
            //return cast(inout(TypedValue))semanticCall.returnValue;
        case SemanticNodeType.runtimeCall:
            return cast(inout(TypedValue))asTypedValue;
        }
    }

    final void toString(StringSink sink)
    {
        // TODO: temporary implementation
        formattedWrite(sink, "SemanticType=%s:SyntaxType=%s:%s",
            nodeType, syntaxNode.type, syntaxNode.source);
    }

}

string tryEvaluateToSymbol(inout(SemanticNode)* node)
{
    return node.getAnalyzedTypedValue(No.resolveSymbol).tryGetValueAs!SymbolType(null);
}

auto peelQualifier(string* symbol)
{
    auto dotIndex = (*symbol).indexOf('.');
    if (-1 == dotIndex)
    {
        auto returnValue = *symbol;
        *symbol = null;
        return returnValue;
    }
    auto returnValue = (*symbol)[0..dotIndex];
    assert(returnValue.length > 0);
    *symbol = (*symbol)[dotIndex + 1 .. $];
    assert((*symbol).length > 0);
    return returnValue;
}

enum SatisfyState : ubyte
{
    satisfied,
    notSatisfied,
    needMoreSymbols,
}
enum ResolveResultEnum : ubyte
{
    haveEntry                         = SatisfyState.satisfied,
    noEntryAndAllSymbolsAdded         = SatisfyState.notSatisfied,
    noEntryButMoreSymbolsCouldBeAdded = SatisfyState.needMoreSymbols,
}
pragma(inline) ResolveResultEnum toResolveResult(const SatisfyState state)
{
    return cast(ResolveResultEnum)state;
}
pragma(inline) SatisfyState toSatisfyState(const ResolveResultEnum result)
{
    return cast(SatisfyState)result;
}

struct ResolveResultTemplate(T)
{
    @property static auto noEntryAndAllSymbolsAdded()
    {
        return ResolveResultTemplate!T(ResolveResultEnum.noEntryAndAllSymbolsAdded);
    }
    @property static auto noEntryButMoreSymbolsCouldBeAdded()
    {
        return ResolveResultTemplate!T(ResolveResultEnum.noEntryButMoreSymbolsCouldBeAdded);
    }

    T entry = void;
    ResolveResultEnum state;
    private this(ResolveResultEnum state) { this.state = state; }
    this(T entry)
        in { assert(!entry.isNull); } do
    {
        this.entry = entry;
        this.state = ResolveResultEnum.haveEntry;
    }
}
alias ResolveResult = ResolveResultTemplate!(const(TypedValue));
alias ResolveTypeResult = ResolveResultTemplate!(const(IType));
alias SemanticCallResult = ResolveResultTemplate!(SemanticNode*);

// An object that contains members accessed via the '.' operator
interface IDotQualifiable
{
    // try to get a symbol table entry that matches the given symbol
    ResolveResult tryGetUnqualified(string symbol);
}

ResolveResult tryGetQualified(IDotQualifiable qualifiable, string symbol)
    in { assert(symbol.length > 0); } do
{
    for (;;)
    {
        auto next = peelQualifier(&symbol);
        auto resolved = qualifiable.tryGetUnqualified(next);
        if (resolved.state != ResolveResultEnum.haveEntry)
        {
            return resolved;
        }
        if (symbol is null)
        {
            return resolved;
        }
        auto resolvedAsQualifiable = resolved.entry.tryAsIDotQualifiable;
        if (resolvedAsQualifiable is null)
        {
            from!"std.stdio".writefln("Error: cannot access member '%s' from an object of type '%s', it doesn't have any dotted members",
                symbol, resolved.entry.formatType);
            throw quit;
        }
        qualifiable = cast()resolvedAsQualifiable;
    }
}

interface IReadOnlyScope : IDotQualifiable
{
    IReadOnlyScope getParent();
    Module getModule();
}
interface IScope : IReadOnlyScope
{
    void add(const(string) symbol, const(TypedValue) value);
}

struct Parameter
{
    string name;

    /*
    Some examples of types:

    bool    "A simple single type"

    {bool,int}  "two ordered types"

    unordered {bool, bool}  "two unordered types"

    { on : bool, value: int}  "two ordered and named types"

    { on: bool, values: int...} "1 bool named 'on' followed by 0 or more int values"
    */
    IType type;
    this(string name, immutable(IType) type) immutable
    {
        this.name = name;
        this.type = type;
    }
}

enum FunctionFlags
{
    none = 0,
    // function is evaluated at analyze time

    syntaxFunction = 0x01,
    analyzeTimeEvaluation = 0x01,
    //generatesSymbols = 0x01,
    //inputsRawSymbols = 0x02,
}

struct RuntimeFunctionInterface
{
    IType returnType;
    Parameter[] parameters;
    string[] flagParameters;
    FunctionFlags flags;
    this(immutable(IType) returnType, immutable(Parameter)[] parameters,
        immutable(string[]) flagParameters, FunctionFlags flags) immutable
    {
        this.returnType = returnType;
        this.parameters = parameters;
        this.flagParameters = flagParameters;
        this.flags = flags;
    }
    @property bool analyzeTimeEvaluation() const
    {
        return 0 != (flags & FunctionFlags.analyzeTimeEvaluation);
    }
    /*
    @property bool inputsRawSymbols() const
    {
        return 0 != (flags & FunctionFlags.inputsRawSymbols);
    }
    */

    bool containsFlag(string flagArgument) const
    {
        foreach (flagParameter; flagParameters)
        {
            if (flagArgument == flagParameter)
            {
                return true;
            }
        }
        return false;
    }

    SatisfyState supports(IScope scope_, RuntimeCall* call, StringSink errorSink) const
    {
        assert(0, "not implemented");
        /+
        static struct ParameterState
        {
            bool used;
        }
        import core.stdc.stdlib : alloca;
        SemanticNode** filteredArgumentsPtr = cast(SemanticNode**)alloca((SemanticNode*).sizeof * call.arguments.length);
        assert(filteredArgumentsPtr, "alloca failed");
        ushort filteredArgumentsCount = 0;
        auto parameterStates = (cast(ParameterState*)alloca(ParameterState.sizeof * parameters.length))
            [0..parameters.length];
        assert(parameterStates.ptr, "alloca failed");

        // first pull out the special functions
        // flag(...)
        // named(<name> <expr>)
        //
        foreach (i, ref arg; call.arguments)
        {
            if (arg.syntaxNode.type != SyntaxNodeType.call)
            {
                filteredArgumentsPtr[filteredArgumentsCount++] = &arg;
            }
            else
            {
                if (arg.syntaxNode.call.functionName == "flag")
                {
                    if (arg.syntaxNode.call.arguments.length != 1 ||
                       arg.syntaxNode.call.arguments[0].type != SyntaxNodeType.symbol)
                    {
                        assert(0, "flag function with non symbol not implemented");
                    }
                    if (!containsFlag(arg.syntaxNode.call.arguments[0].source))
                    {
                        formattedWrite(errorSink, "function does not contain flag(%s)", arg.syntaxNode.call.arguments[0].source);
                        return SatisfyState.notSatisfied;
                    }
                }
                else if (arg.syntaxNode.call.functionName == "named")
                {
                    assert(0, "named not implemented");
                }
                else
                {
                    filteredArgumentsPtr[filteredArgumentsCount++] = &arg;
                }
            }
        }

        static ushort toNext(T)(T[] array, ushort index)
        {
            for (;; index++)
            {
                if (index >= array.length || !array[index].used)
                    return index;
            }
        }

        auto filteredArguments = filteredArgumentsPtr[0..filteredArgumentsCount];

        // go through each parameter and match them up to the corresponding argument
        ushort nextArgIndex = 0;
        ushort nextParamIndex = 0;
        for (;; nextParamIndex++)
        {
            nextParamIndex = toNext(parameterStates, nextParamIndex);
            if (nextParamIndex == parameterStates.length)
            {
                if (nextArgIndex < filteredArguments.length)
                {
                    errorSink("there are too many arguments");
                    return SatisfyState.notSatisfied;
                }
                return SatisfyState.satisfied; // all parameters satisfied
            }
            auto result = parameters[nextParamIndex].type.tryConsume(scope_, filteredArguments, &nextArgIndex);
            final switch(result)
            {
            case SatisfyState.satisfied:
                break;
            case SatisfyState.notSatisfied:
                if (nextArgIndex < filteredArguments.length)
                {
                    from!"std.stdio".writefln("[DEBUG] next arg is '%s'", *filteredArguments[nextArgIndex]);
                }
                formattedWrite(errorSink, "parameter at index %s of type %s is not satisfied",
                    nextParamIndex, parameters[nextParamIndex].type.format);
                return SatisfyState.notSatisfied;
            case SatisfyState.needMoreSymbols:
                return SatisfyState.needMoreSymbols;
            }
        }
        +/
    }
    void toString(StringSink sink) const
    {
        sink("FunctionInterface.toString not implemented");
    }
}

class SemanticFunction
{
    /**
    Note, it is important to know during analysis if a semantic function can add symbols because
    symbol resolution cannot ascend past a scope until all symbols in that scope have been added.
    This is the only way to guarantee that the match always occurs in the innermost scope.
    */
    bool canAddSymbols() const
    {
        return true; // safer to assume functions can add symbols by default
    }
    abstract SemanticCallResult interpret(IScope scope_, SemanticCall* call, Flag!"used" used) const;

    final void printErrorsForInterpret(IReadOnlyScope scope_, SemanticCall* call, Flag!"used" used) const
    {
        from!"std.stdio".writefln("%sError: failed to interpret function '%s' (TODO: implement printing more details)",
            scope_.getModule.formatLocation(call.syntaxNode.source), call.syntaxNode.functionName);
    }
    final auto checkAndGet(const(CallSyntaxNode)* call) inout
    {
        // this method provides an opportunity to check the call
        return this;
        /+
        auto errorBuilder = StringBuilder!(GCDoubler!100)();
        auto result = interface_.supports(scope_, call, &errorBuilder.append);
        if (result == SatisfyState.needMoreSymbols)
        {
            return inout FindAnalyzeTimeFunctionResult(Yes.needMoreSymbols);
            from!"std.stdio".writefln("[DEBUG] function interface check, needs more symbols call=%s",
                *call);
            assert(0, "not implemented");
        }
        if (result != SatisfyState.satisfied)
        {
            // TODO: print nice error message with specific reasons why you can't call this
            //       function with these arguments
            //assert(0, format("function '%s' with arguments %s is not callable with %s",
            //    call.functionName, interface_, call.arguments));

            import std.stdio : writefln;
            writefln("Error: builtiln analyze-time function '%s' is not callable with these arguments because %s:",
                call.syntaxNode.functionName, errorBuilder.data);
            foreach (ref arg; call.arguments)
            {
                writefln("  %s", arg);
            }
            throw quit;
        }
        return inout FindAnalyzeTimeFunctionResult(this);
        +/
    }
}

class SemanticFunctionCannotAddSymbols : SemanticFunction
{
    override bool canAddSymbols() const { return false; }
    override SemanticCallResult interpret(IScope scope_, SemanticCall* call, Flag!"used" used) const
    {
        assert(0, "code bug: semantic function is missing `mixin SemanticFunctionCannotAddSymbolsMixin;`");
    }
}
mixin template SemanticFunctionCannotAddSymbolsMixin()
{
    final override SemanticCallResult interpret(IScope scope_, SemanticCall* call, Flag!"used" used) const
    {
        return interpretReadonlyScope(cast(IReadOnlyScope)scope_, call, used);
    }
}




class RuntimeFunction
{
    IScope containingScope;
    immutable RuntimeFunctionInterface interface_;
    Builder!(RuntimeCall*, GCDoubler!8) calls;
    bool queuedForAnalysis;

    this(IScope containingScope, immutable RuntimeFunctionInterface interface_)
    {
        this.containingScope = containingScope;
        this.interface_ = interface_;
    }
    this(immutable IScope containingScope, immutable RuntimeFunctionInterface interface_) immutable
    {
        this.containingScope = containingScope;
        this.interface_ = interface_;
    }
    void addUsedCall(RuntimeCall* runtimeCall)
    {
        if (!queuedForAnalysis)
        {
            queueForAnalysis();
            this.queuedForAnalysis = true;
        }
        if (global.willOptimize)
        {
            // Only need to add if I'm going to perform optimization later
            calls.append(runtimeCall);
        }
    }

    abstract void queueForAnalysis();
    //abstract void interpret(RuntimeCall* call);
}
class UserDefinedFunction : RuntimeFunction
{
    const uarray!SyntaxNode rawCode;
    uarray!SemanticNode analyzedCode;
    ushort codeAnalyzedCount;
    this(IScope containingScope, immutable RuntimeFunctionInterface interface_,
        const uarray!SyntaxNode rawCode)
    {
        super(containingScope, interface_);
        this.rawCode = rawCode;
    }
    this(immutable IScope containingScope, immutable RuntimeFunctionInterface interface_,
        immutable uarray!SyntaxNode rawCode) immutable
    {
        super(containingScope, interface_);
        this.rawCode = rawCode;
    }
    override void queueForAnalysis()
        in { assert(!queuedForAnalysis && analyzedCode is null); } do
    {
        analyzedCode = new SemanticNode[rawCode.length].toUarray;
        foreach (i, ref rawCodeNode; rawCode)
        {
            analyzedCode[i].initialize(&rawCode[i]);
        }
        containingScope.getModule().addFunctionToAnalyze(this);
    }

    /*
    final override void interpret(RuntimeCall* call)
        in { assert(codeAnalyzedCount == rawCode.length); } do
    {
        interpreter.interpretRuntimeCall(this, call);
    }
    */
}
class BuiltinRuntimeFunction : RuntimeFunction
{
    this(immutable RuntimeFunctionInterface interface_) immutable
    {
        super(Module.builtin, interface_);
    }

    // TODO:
    // This shouldn't take a RuntimeCall, it should take a list of argument
    // values.
    abstract void interpret(interpreter.Interpreter* interpreter, RuntimeCall* call);

    override void queueForAnalysis()
        in { assert(!queuedForAnalysis); } do
    {
        // builtin function don't need to be analyzed
    }
}

// A single contingous range of numbers between 0 (inclusive) and infinity
struct PositiveRange
{
    // TODO: should probably use infinite precision numbers for these values
    private ubyte minValue;
    private Flag!"isInfinite" isInfinite;
    private ubyte maxValue = void;

    auto min() const { return minValue; }
    bool maxIsInfinite() const { return isInfinite; }
    auto max() const in { assert(!maxIsInfinite()); } do { return maxValue; }

    final bool greaterOrEqualToMin(T)(T value) const
    {
        return value >= cast(T)minValue;
    }
    final bool greaterOrEqualToMax(T)(T value) const
    {
        return !isInfinite && value >= cast(T)maxValue;
    }

    final void toString(scope void delegate(const(char)[]) sink)
    {
        if (isInfinite)
            formattedWrite(sink, "[%s-inf]", minValue);
        else
            formattedWrite(sink, "[%s-%s]", minValue, maxValue);
    }
}
auto positiveRangeFrom(T)(T min)
{
    auto minCasted = cast(typeof(PositiveRange.minValue))min;
    assert(minCasted == min, "PositiveRange types need to be modified to use infinite precision");
    return PositiveRange(minCasted, Yes.isInfinite);
}
auto positiveRange(T,U)(T min, U max)
{
    auto minCasted = cast(typeof(PositiveRange.minValue))min;
    assert(minCasted == min, "PositiveRange types need to be modified to use infinite precision");
    return PositiveRange(minCasted, No.isInfinite, max);
}


//
// Data Structures For Compile-Time Evaluation
//
struct Value
{
    union
    {
        bool bool_;
        string string_;
        RuntimeFunction RuntimeFunction_;

        SyntaxNode* SyntaxNodeP_;
        StringSyntaxNode* StringSyntaxNodeP_;

        //SemanticNode* SemanticNodeP_;
        //SemanticNode[] SemanticNodeLR_;
        uarray!SemanticNode uarray_SemanticNode_;
        TupleNode* TupleNodeP_;
        RuntimeCall* RuntimeCallP_;
        SyntaxCall* SyntaxCallP_;
        SemanticCall* SemanticCallP_;

        Module Module_;

        SemanticFunction SemanticFunction_;
        BuiltinRuntimeFunction BuiltinRuntimeFunction_;

        //
        // Types
        //
        IType IType_;
        //TypeType TypeType_;
        TypeClass TypeClass_;
    }
    this(bool bool_) inout { this.bool_ = bool_; }
    this(string string_) inout { this.string_ = string_; }
    this(inout(RuntimeFunction) RuntimeFunction_) inout { this.RuntimeFunction_ = RuntimeFunction_; }

    this(inout(SyntaxNode)* SyntaxNodeP_) inout { this.SyntaxNodeP_ = SyntaxNodeP_; }
    this(inout(StringSyntaxNode)* StringSyntaxNodeP_) inout { this.StringSyntaxNodeP_ = StringSyntaxNodeP_; }

    //this(inout(SemanticNode)* semanticNode) inout { this.semanticNode = semanticNode; }
    //this(inout(SemanticNode)[] SemanticNodeLR_) inout { this.SemanticNodeLR_ = SemanticNodeLR_; }
    this(inout(uarray!SemanticNode) uarray_SemanticNode_) inout { this.uarray_SemanticNode_ = uarray_SemanticNode_; }
    this(inout(TupleNode)* TupleNodeP_) inout { this.TupleNodeP_ = TupleNodeP_; }
    this(inout(RuntimeCall)* RuntimeCallP_) inout { this.RuntimeCallP_ = RuntimeCallP_; }
    this(inout(SyntaxCall)* SyntaxCallP_) inout { this.SyntaxCallP_ = SyntaxCallP_; }
    this(inout(SemanticCall)* SemanticCallP_) inout { this.SemanticCallP_ = SemanticCallP_; }
    this(inout(Module) Module_) inout { this.Module_ = Module_; }
    this(inout(SemanticFunction) SemanticFunction_) inout { this.SemanticFunction_ = SemanticFunction_; }
    this(inout(BuiltinRuntimeFunction) BuiltinRuntimeFunction_) inout { this.BuiltinRuntimeFunction_ = BuiltinRuntimeFunction_; }
    this(inout(IType) IType_) inout { this.IType_ = IType_; }
    //this(inout(TypeType) TypeType_) inout { this.TypeType_ = TypeType_; }
    this(inout(TypeClass) TypeClass_) inout { this.TypeClass_ = TypeClass_; }
}
struct TypedValue
{
    @property static auto nullValue() { return TypedValue(null); }
    @property bool isNull() const { return type is null; }

    @property static auto void_()
    {
        return TypedValue(VoidType.instance);
    }
    @property bool isVoid() const { return this.type is VoidType.instance; }

    Value value = void;
    Rebindable!IType type;
    private this(const(IType) type)
    {
        this.type = type;
    }
    this(const(IType) type, inout(Value) value) inout
    {
        this.type = type;
        this.value = value;
    }
    @property final inout(IDotQualifiable) tryAsIDotQualifiable() inout
    {
        //return type.tryAsIDotQualifiable(value);
        auto converter = cast(IValueToDotQualifiable)type;
        return converter ? converter.tryAsIDotQualifiable(value) : null;
    }
    @property DelegateFormatter formatType() const
    {
        if (type is null)
            assert(0, "not implemented");
        return types.formatType(type);
    }
    @property auto formatValue() const
    {
        if (type is null)
            assert(0, "not implemented");
        return types.formatValue(type, value);
    }
    @property auto tryTypeAs(T)() inout
    {
        return cast(T)type;
    }
    auto tryGetValueAs(T)(typeof(T.get(value)) defaultValue) inout
    {
        auto casted = cast(T)type;
        return casted ? casted.get(value) : defaultValue;
    }
}