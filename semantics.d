module semantics;

import std.typecons : Flag, Yes, No, scoped;
import std.bitmanip : bitfields;
import std.bigint : BigInt;
import std.algorithm : min;
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
import symtab : SymbolTable;
import mod : Module;
import builtin : tryFindSemanticFunction;
static import interpreter;

enum SemanticNodeType : ubyte
{
    // Means there's nothing to really analyze for the semantic node
    typedValue,
    tuple,
    // Means the node represents a symbol
    symbol,
    semanticCall,
    runtimeCall,
    statementBlock,
    jump,
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
struct SemanticCall
{
    const(CallSyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    SemanticFunction function_;
    uarray!SemanticNode arguments;
    uint analyzeArgumentsIndex;
    void* staticData;
    SemanticNode* returnValue;
}
auto getStaticDataStruct(T)(SemanticCall* call) if (is(T == struct))
{
    static struct Wrapper
    {
        bool newlyAllocated;
        T* dataPtr;
        alias dataPtr this;
    }
    if (call.staticData is null)
    {
        auto newT = new T();
        call.staticData = newT;
        return Wrapper(true, newT);
    }
    return Wrapper(false, cast(T*) call.staticData);
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

enum BlockFlags : ubyte
{
    isJumpBlock,
}
struct StatementBlock
{
    const(SyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    IScope scope_;
    uarray!SemanticNode statements;
    uint analyzeStatementsIndex;
    BlockFlags flags;
}

enum JumpType : ubyte
{
    loopCurrentBlock,
    breakCurrentBlock,
}
struct JumpNode
{
    const(SyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    SemanticNode* condition;
    JumpType jumpType;
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
        SemanticCall semanticCall = void;
        RuntimeCall runtimeCall = void;
        StatementBlock statementBlock = void;
        JumpNode jump = void;
    }
    SemanticNodeType nodeType;
    static assert(syntaxNode.offsetof == TypedValueNode.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == TupleNode.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == SymbolNode.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == SemanticCall.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == RuntimeCall.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == StatementBlock.syntaxNode.offsetof);
    static assert(syntaxNode.offsetof == JumpNode.syntaxNode.offsetof);

    static assert(asTypedValue.offsetof == TypedValueNode.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == TupleNode.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == SymbolNode.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == SemanticCall.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == RuntimeCall.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == StatementBlock.asTypedValue.offsetof);
    static assert(asTypedValue.offsetof == JumpNode.asTypedValue.offsetof);

    void nullify() { syntaxNode = null; }

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
    final void initializeAsRuntimeCall(const(CallSyntaxNode)* syntaxCall)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        this.runtimeCall = RuntimeCall(syntaxCall,
            rebindable(RuntimeCallType.instance.createTypedValue(&this.runtimeCall)),
            createSemanticNodes(syntaxCall.arguments));
        this.nodeType = SemanticNodeType.runtimeCall;
    }
    final void initializeStatementBlock(const(SyntaxNode)* syntaxNode, BlockFlags flags, IScope scope_,
        uarray!SemanticNode statements)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        this.statementBlock = StatementBlock(syntaxNode,
            rebindable(StatementBlockType.instance.createTypedValue(&this.statementBlock)),
            scope_, statements, 0, flags);
        this.nodeType = SemanticNodeType.statementBlock;
    }
    final void initializeJumpNode(const(SyntaxNode)* syntaxNode, JumpType jumpType, SemanticNode* condition)
        in { assert(isNull); }
        out { assert(!isNull); } do
    {
        this.jump = JumpNode(syntaxNode,
            rebindable(TypedValue.void_), condition, jumpType);
        this.nodeType = SemanticNodeType.jump;
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
                this.tuple = TupleNode(syntaxNode,
                    rebindable(TupleLiteralType.instance.createTypedValue(&this.tuple)),
                    createSemanticNodes(syntaxNode.tuple.nodes), 0);
                this.nodeType = SemanticNodeType.tuple;
            }
            return;
        case SyntaxNodeType.call:
            {
                auto function_ = tryFindSemanticFunction(&syntaxNode.call);
                if (function_ !is null)
                {
                    auto nodeCount = function_.semanticNodeBufferCountFor(syntaxNode.call.arguments.length);
                    auto arguments = new SemanticNode[nodeCount].toUarray;
                    auto initializedCount = 0;
                    foreach (arg; function_.semanticNodeRange(syntaxNode.call.arguments.length))
                    {
                        arguments[arg.semanticNodeIndex].initialize(&syntaxNode.call.arguments[arg.syntaxNodeIndex]);
                        initializedCount++;
                    }
                    assert(initializedCount <= nodeCount);
                    while(initializedCount < nodeCount)
                    {
                        // TODO: initialize to a void placeholder value
                        assert(0, "not implemented");
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
    final TypedValue getAnalyzedTypedValue(Flag!"resolveSymbols" resolveSymbols)
    {
        final switch(nodeType)
        {
        case SemanticNodeType.typedValue:
            return asTypedValue;
        case SemanticNodeType.tuple:
            assert(tuple.analyzeElementsIndex == tuple.elements.length, "code bug");
            return asTypedValue;
        case SemanticNodeType.symbol:
            if (!resolveSymbols)
                return asTypedValue;
            if (symbol.resolved.isNull)
            {
                from!"std.stdio".writefln("FatalError: symbol %s resolved is null, not implemented", this.symbol.syntaxNode.source);
                //throw quit;
                assert(0);
            }
            return symbol.resolved;
        case SemanticNodeType.semanticCall:
            if (semanticCall.returnValue is null)
            {
                assert(0, "code bug: getAnalyzedTypedValue should not be called on a node that is not analyzed");
            }
            return semanticCall.returnValue.getAnalyzedTypedValue(resolveSymbols);
            //return semanticCall.returnValue;
        case SemanticNodeType.runtimeCall:
            // TODO: need to return the return value as a typed value, not the call itself
            assert(0, "not implemented");
            return asTypedValue;
        case SemanticNodeType.statementBlock:
            assert(0, "getAnalyzedTypedValue statementBlock not implemented");
        case SemanticNodeType.jump:
            assert(0, "getAnalyzedTypedValue jump not implemented");
        }
    }

    final void toString(StringSink sink)
    {
        // TODO: temporary implementation
        formattedWrite(sink, "SemanticType=%s:SyntaxType=%s:%s",
            nodeType, syntaxNode.type, syntaxNode.source);
    }

}

uarray!SemanticNode createSemanticNodes(const uarray!SyntaxNode syntaxNodes)
{
    auto semanticNodes = new SemanticNode[syntaxNodes.length].toUarray;
    foreach (i; 0 .. syntaxNodes.length)
    {
        semanticNodes[i].initialize(&syntaxNodes[i]);
    }
    return semanticNodes;
}

class UnevaluatedSymbol
{
    IScope evaluationScope;
    this(IScope evaluationScope)
    {
        this.evaluationScope = evaluationScope;
    }
    abstract TypedValue tryEvaluate();
    /*
    {
        assert(0, "UnevaluatedSymbol.tryEvaluate not implemented");
    }
    */
}

string tryEvaluateToSymbol(SemanticNode* node)
{
    return node.getAnalyzedTypedValue(No.resolveSymbols).tryGetValueAs!SymbolType(null);
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
    void dumpSymbols() const;
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
        const resolvedAsQualifiable = resolved.entry.tryAsIDotQualifiable;
        if (resolvedAsQualifiable is null)
        {
            from!"std.stdio".writefln("Error: cannot access member '%s' from an object of type '%s', it doesn't have any dotted members",
                symbol, resolved.entry.formatType);
            throw quit;
        }
        qualifiable = cast()resolvedAsQualifiable;
    }
}

interface IReadonlyScope : IDotQualifiable
{
    @property inout(IReadonlyScope) getParent() inout;
    @property inout(Module) asModule() inout;
    @property inout(IScope) asWriteable() inout;
    @property inout(JumpBlock) asJumpBlock() inout;
}
inout(Module) getModule(inout(IReadonlyScope) scope_)
{
    auto next = rebindable(scope_);
    while(true)
    {
        auto module_ = next.asModule;
        if (module_ !is null)
            return cast(inout(Module))module_;
        next = next.getParent;
        if (next is null)
            assert(0, "this scope is not inside a module");
    }
}
inout(JumpBlock) tryGetJumpBlock(inout(IReadonlyScope) scope_)
{
    auto next = rebindable(scope_);
    while(true)
    {
        auto jumpBlock = next.asJumpBlock;
        if (jumpBlock !is null)
            return cast(inout(JumpBlock))jumpBlock;
        next = next.getParent;
        if (next is null)
            return null;
    }
}

interface IScope : IReadonlyScope
{
    void add(const(string) symbol, TypedValue value);
    void evaluated(const(string) symbol, TypedValue value);
}
void addUnevaluatedSymbol(IScope scope_, const(string) symbol)
{
    scope_.add(symbol, TypedValue(UnevaluatedSymbolType.instance, Value()));
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

struct NumberedSemanticArgType
{
    ushort count;
    SemanticArgType type;
    this(SemanticArgType type, ushort count) inout
    in { assert(count > 0); } do
    {
        this.type = type;
        this.count = count;
    }
}
enum SemanticArgType : ubyte
{
    syntaxNode,
    semiAnalyzedSemanticNode,
    fullyAnalyzedSemanticNode,
}
pragma(inline) bool needsSemanticNode(const SemanticArgType type)
{
    return type != SemanticArgType.syntaxNode;
}

class SemanticFunction
{
    SemanticArgType defaultArgType;
    const(NumberedSemanticArgType)[] firstArgTypes;
    // Total number of arguments in firstArgTypes
    private size_t firstArgTypesCount;
    // Saves the number of nodes needed from firstArgTypes
    private size_t firstArgsThatNeedSemanticNodesCount;
    this(SemanticArgType defaultArgType, immutable(NumberedSemanticArgType)[] firstArgTypes) immutable
    {
        this.defaultArgType = defaultArgType;
        this.firstArgTypes = firstArgTypes;
        size_t countArgs = 0;
        size_t countSemanticNodes = 0;
        if (firstArgTypes !is null)
        {
            foreach (argType; firstArgTypes)
            {
                countArgs += argType.count;
                if (argType.type.needsSemanticNode)
                    countSemanticNodes += argType.count;
            }
        }
        this.firstArgTypesCount = countArgs;
        this.firstArgsThatNeedSemanticNodesCount = countSemanticNodes;
    }

    // Returns the number of nodes that need to be allocated for the call
    size_t semanticNodeBufferCountFor(size_t syntaxNodeCount) const
    {
        if (syntaxNodeCount <= firstArgTypesCount)
            return firstArgsThatNeedSemanticNodesCount; // This is always the minimum

        if (defaultArgType.needsSemanticNode)
        {
            return syntaxNodeCount - (firstArgTypesCount - firstArgsThatNeedSemanticNodesCount);
        }
        return firstArgsThatNeedSemanticNodesCount;
    }
    // Returns the number of nodes that need to be analyzed
    size_t semanticNodeAnalyzeCountFor(size_t syntaxNodeCount) const
    {
        if (syntaxNodeCount >= firstArgTypesCount)
        {
            if (defaultArgType.needsSemanticNode)
            {
                return syntaxNodeCount - (firstArgTypesCount - firstArgsThatNeedSemanticNodesCount);
            }
            return firstArgsThatNeedSemanticNodesCount;
        }

        // We don't have all the initial arguments, so we have to count how many
        // semantic nodes we need
        size_t semanticNodeCount = 0;
        for (size_t i = 0; ; i++)
        {
            assert(i < firstArgTypes.length, "code bug");
            auto nextCount = firstArgTypes[i].count;
            if (nextCount >= syntaxNodeCount)
            {
                if (firstArgTypes[i].type.needsSemanticNode)
                    semanticNodeCount += syntaxNodeCount;
                return semanticNodeCount;
            }
            if (firstArgTypes[i].type.needsSemanticNode)
                semanticNodeCount += nextCount;
            syntaxNodeCount -= nextCount;
        }
    }
    // Returns an input range of syntaxNode/semanticNode indices
    auto semanticNodeRange(uint syntaxNodeCount) const
    {
        return NodeIndicesRange(this, syntaxNodeCount);
    }

    /**
    Note, it is important to know during analysis if a semantic function can add symbols because
    symbol resolution cannot ascend past a scope until all symbols in that scope have been added.
    This is the only way to guarantee that the match always occurs in the innermost scope.
    */
    bool canAddSymbols() const
    {
        return false;
    }
    abstract uint interpretPass1(IScope scope_, SemanticCall* call);
    abstract SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const;

    final void printErrorsForInterpret(IReadonlyScope scope_, SemanticCall* call, Flag!"used" used) const
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

struct NodeIndices
{
    uint syntaxNodeIndex;
    uint semanticNodeIndex;
}
struct NodeIndicesRange
{
    const(SemanticFunction) function_;
    uint syntaxNodeCount;
    uint firstArgTypeIndex;
    ushort firstArgCount;
    NodeIndices currentIndices;
    this(const(SemanticFunction) function_, uint syntaxNodeCount)
    {
        this.function_ = function_;
        this.syntaxNodeCount = syntaxNodeCount;
        toNextFirstArg();
    }
    private void toNextFirstArg()
    {
        for(;; firstArgTypeIndex++)
        {
            if (firstArgTypeIndex >= function_.firstArgTypes.length)
            {
                if (!function_.defaultArgType.needsSemanticNode)
                {
                    // skip the rest of the semantic nodes
                    currentIndices.syntaxNodeIndex = syntaxNodeCount;
                }
                break;
            }
            if (function_.firstArgTypes[firstArgTypeIndex].type.needsSemanticNode)
                break;

            // skip syntax nodes
            currentIndices.syntaxNodeIndex += function_.firstArgTypes[firstArgTypeIndex].count;
        }
    }
    bool empty() const { return currentIndices.syntaxNodeIndex >= syntaxNodeCount; }
    NodeIndices front() { return currentIndices; }
    void popFront()
    {
        version(none)
        {
            from!"std.stdio".writefln("+ popFront semantic %s syntax %s",
                currentIndices.semanticNodeIndex, currentIndices.syntaxNodeIndex);
            scope(exit)
            {
                from!"std.stdio".writefln("- popFront semantic %s syntax %s",
                    currentIndices.semanticNodeIndex, currentIndices.syntaxNodeIndex);
            }
        }
        currentIndices.semanticNodeIndex++;
        currentIndices.syntaxNodeIndex++;
        for(;;)
        {
            if (currentIndices.syntaxNodeIndex >= syntaxNodeCount)
                return;

            if (firstArgTypeIndex >= function_.firstArgTypes.length)
                return;

            firstArgCount++;
            if (firstArgCount < function_.firstArgTypes[firstArgTypeIndex].count)
                return;
            firstArgTypeIndex++;
            toNextFirstArg();
            firstArgCount = 0;
        }
    }
}

unittest
{
    //from!"std.stdio".writefln("SEMANTICS!------------------------------");
    //scope(exit) from!"std.stdio".writefln("SEMANTICS DONE!------------------------------");
    static class TestSemanticFunction : SemanticFunction
    {
        this(SemanticArgType defaultArgType, immutable(NumberedSemanticArgType)[] firstArgTypes) immutable
        {
            super(defaultArgType, firstArgTypes);
        }
        override SemanticCallResult interpret(IScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
        {
            assert(0, "code bug: semantic function is missing `mixin SemanticFunctionCannotAddSymbolsMixin;`");
        }
    }
    foreach(foo; [
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, null),
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 1),
        ]),
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 2),
        ]),
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 100),
        ]),
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 1),
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 1),
        ]),
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 2),
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 1),
        ]),
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 1),
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 2),
        ]),
        new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 100),
            immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, 200),
        ]),
    ])
    {
        foreach (syntaxNodeCount; [0, 1, 2, 500])
        {
            assert(0 == foo.semanticNodeBufferCountFor(syntaxNodeCount));
            assert(0 == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
            auto range = foo.semanticNodeRange(syntaxNodeCount);
            assert(range.empty);
        }
    }
    {
        auto foo = new immutable TestSemanticFunction(SemanticArgType.fullyAnalyzedSemanticNode, null);
        foreach (syntaxNodeCount; 0 .. 5)
        {
            assert(syntaxNodeCount == foo.semanticNodeBufferCountFor(syntaxNodeCount));
            assert(syntaxNodeCount == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
            auto range = foo.semanticNodeRange(syntaxNodeCount);
            foreach (i; 0 .. syntaxNodeCount)
            {
                assert(!range.empty);
                assert(range.front.syntaxNodeIndex == i);
                assert(range.front.semanticNodeIndex == i);
                range.popFront();
            }
            assert(range.empty);
        }
    }
    foreach (ushort firstArgSemanticNodeCount; 1 .. 5)
    {
        {
            auto foo = new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
                immutable NumberedSemanticArgType(SemanticArgType.fullyAnalyzedSemanticNode, firstArgSemanticNodeCount),
            ]);
            foreach (syntaxNodeCount; 0 .. firstArgSemanticNodeCount + 1)
            {
                assert(firstArgSemanticNodeCount == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(syntaxNodeCount           == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                foreach (i; 0 .. syntaxNodeCount)
                {
                    assert(!range.empty);
                    assert(range.front.syntaxNodeIndex == i);
                    assert(range.front.semanticNodeIndex == i);
                    range.popFront();
                }
                assert(range.empty);
            }
            foreach (syntaxNodeCount; [firstArgSemanticNodeCount + 1, firstArgSemanticNodeCount + 2, 500])
            {
                assert(firstArgSemanticNodeCount == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(firstArgSemanticNodeCount == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                foreach (i; 0 .. firstArgSemanticNodeCount)
                {
                    assert(!range.empty);
                    assert(range.front.syntaxNodeIndex == i);
                    assert(range.front.semanticNodeIndex == i);
                    range.popFront();
                }
                assert(range.empty);
            }
        }

        foreach (ushort secondArgSyntaxNodeCount; 1 .. 5)
        {
            auto foo = new immutable TestSemanticFunction(SemanticArgType.fullyAnalyzedSemanticNode, [
                immutable NumberedSemanticArgType(SemanticArgType.fullyAnalyzedSemanticNode, firstArgSemanticNodeCount),
                immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, secondArgSyntaxNodeCount),
            ]);
            foreach (syntaxNodeCount; 0 .. firstArgSemanticNodeCount + secondArgSyntaxNodeCount + 1)
            {
                auto maxSemanticNodeIndex = (syntaxNodeCount <= firstArgSemanticNodeCount) ? syntaxNodeCount : firstArgSemanticNodeCount;
                assert(firstArgSemanticNodeCount == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(maxSemanticNodeIndex      == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                foreach (i; 0 .. maxSemanticNodeIndex)
                {
                    assert(!range.empty);
                    assert(range.front.syntaxNodeIndex == i);
                    assert(range.front.semanticNodeIndex == i);
                    range.popFront();
                }
                assert(range.empty);
            }
            foreach (syntaxNodeCount; firstArgSemanticNodeCount + secondArgSyntaxNodeCount + 1 .. firstArgSemanticNodeCount + secondArgSyntaxNodeCount + 5)
            {
                auto extra = syntaxNodeCount - (firstArgSemanticNodeCount + secondArgSyntaxNodeCount);
                assert(firstArgSemanticNodeCount + extra == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(firstArgSemanticNodeCount + extra == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                foreach (i; 0 .. firstArgSemanticNodeCount + extra)
                {
                    assert(!range.empty);
                    if (i < firstArgSemanticNodeCount)
                        assert(range.front.syntaxNodeIndex == i);
                    else
                        assert(range.front.syntaxNodeIndex == secondArgSyntaxNodeCount + i);
                    assert(range.front.semanticNodeIndex == i);
                    range.popFront();
                }
                assert(range.empty);
            }
        }
    }
    foreach (ushort firstArgSyntaxNodeCount; 1 .. 5)
    {
        {
            auto foo = new immutable TestSemanticFunction(SemanticArgType.fullyAnalyzedSemanticNode, [
                immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, firstArgSyntaxNodeCount),
            ]);
            foreach (syntaxNodeCount; 0 .. firstArgSyntaxNodeCount + 1)
            {
                assert(0 == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(0 == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                assert(range.empty);
            }
            foreach (syntaxNodeCount; firstArgSyntaxNodeCount + 1 .. firstArgSyntaxNodeCount + 5)
            {
                assert(syntaxNodeCount - firstArgSyntaxNodeCount == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(syntaxNodeCount - firstArgSyntaxNodeCount == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                foreach (i; 0 .. syntaxNodeCount - firstArgSyntaxNodeCount)
                {
                    assert(!range.empty);
                    assert(range.front.syntaxNodeIndex == i + firstArgSyntaxNodeCount);
                    assert(range.front.semanticNodeIndex == i);
                    range.popFront();
                }
                assert(range.empty);
            }
        }

        foreach (ushort secondArgSemanticNodeCount; 1 .. 5)
        {
            auto foo = new immutable TestSemanticFunction(SemanticArgType.syntaxNode, [
                immutable NumberedSemanticArgType(SemanticArgType.syntaxNode, firstArgSyntaxNodeCount),
                immutable NumberedSemanticArgType(SemanticArgType.fullyAnalyzedSemanticNode, secondArgSemanticNodeCount),
            ]);
            foreach (syntaxNodeCount; 0 .. firstArgSyntaxNodeCount)
            {
                assert(secondArgSemanticNodeCount == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(0                          == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                assert(range.empty);
            }
            foreach (syntaxNodeCount; firstArgSyntaxNodeCount .. firstArgSyntaxNodeCount + secondArgSemanticNodeCount + 5)
            {
                auto maxSemanticNodeIndex = min(syntaxNodeCount, firstArgSyntaxNodeCount + secondArgSemanticNodeCount);
                assert(secondArgSemanticNodeCount                     == foo.semanticNodeBufferCountFor(syntaxNodeCount));
                assert(maxSemanticNodeIndex - firstArgSyntaxNodeCount == foo.semanticNodeAnalyzeCountFor(syntaxNodeCount));
                auto range = foo.semanticNodeRange(syntaxNodeCount);
                foreach (i; firstArgSyntaxNodeCount .. maxSemanticNodeIndex)
                {
                    assert(!range.empty);
                    assert(range.front.syntaxNodeIndex == i);
                    assert(range.front.semanticNodeIndex == i - firstArgSyntaxNodeCount);
                    range.popFront();
                }
                assert(range.empty);
            }
        }
    }
}

class SemanticFunctionThanCanAddSymbols : SemanticFunction
{
    this(SemanticArgType defaultArgType, immutable(NumberedSemanticArgType)[] firstArgTypes) immutable
    {
        super(defaultArgType, firstArgTypes);
    }
    override bool canAddSymbols() const { return true; }
    override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        assert(0, "code bug: semantic function is missing `mixin SemanticFunctionThanCanAddSymbolsMixin;`");
    }
}
mixin template SemanticFunctionThanCanAddSymbolsMixin()
{
    final override SemanticCallResult interpret(IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/) const
    {
        auto writeableScope = scope_.asWriteable;
        if (!writeableScope)
        {
            // TODO: this should be a nice compiler error message
            //       here's an example where this could happen
            //       foo(set(x 100))
            assert(0, "Error: this semantic function requires a writeable scope!");
        }
        return interpretWriteableScope(writeableScope, call/*, used*/);
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
        analyzedCode = createSemanticNodes(rawCode);
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

class JumpBlock : IScope
{
    IReadonlyScope parent;
    SymbolTable symbolTable;
    this(IReadonlyScope parent)
    {
        this.parent = parent;
    }
    //
    // IDotQualifiable functions
    //
    ResolveResult tryGetUnqualified(string symbol)
    {
        // !!!!!!!!!!!!!!!!!!!!!!!
        // TODO: this is probably not right
        // !!!!!!!!!!!!!!!!!!!!!!!
        {
            auto result = symbolTable.get(symbol);
            if (!result.isNull)
            {
                return ResolveResult(result);
            }
        }
        return ResolveResult.noEntryAndAllSymbolsAdded;
    }
    void dumpSymbols() const
    {
        assert(0, "JumpBlock.dumpSymbols not impelemented");
    }
    //
    // IReadonlyScope functions
    //
    @property final inout(IReadonlyScope) getParent() inout { return parent; }
    @property final inout(Module) asModule() inout { return null; }
    @property final inout(IScope) asWriteable() inout { return this; }
    @property final inout(JumpBlock) asJumpBlock() inout { return this; }
    //
    // IScope functions
    //
    void add(const(string) symbol, TypedValue value)
    {
        assert(0, "not implemented");
    }
    void evaluated(const(string) symbol, TypedValue value)
    {
        assert(0, "not implemented");
    }
}


//
// Data Structures For Compile-Time Evaluation
//
struct Value
{
    union
    {
        UnevaluatedSymbol UnevaluatedSymbol_;

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
        SemanticCall* SemanticCallP_;
        StatementBlock* StatementBlockP_;

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
    this(inout(UnevaluatedSymbol) UnevaluatedSymbol_) inout { this.UnevaluatedSymbol_ = UnevaluatedSymbol_; }
    this(UnevaluatedSymbol UnevaluatedSymbol_) { this.UnevaluatedSymbol_ = UnevaluatedSymbol_; }

    this(bool bool_) inout { this.bool_ = bool_; }
    this(bool bool_) { this.bool_ = bool_; }
    this(string string_) inout { this.string_ = string_; }
    this(string string_) { this.string_ = string_; }
    this(inout(RuntimeFunction) RuntimeFunction_) inout { this.RuntimeFunction_ = RuntimeFunction_; }

    this(inout(SyntaxNode)* SyntaxNodeP_) inout { this.SyntaxNodeP_ = SyntaxNodeP_; }
    this(inout(StringSyntaxNode)* StringSyntaxNodeP_) inout { this.StringSyntaxNodeP_ = StringSyntaxNodeP_; }

    //this(inout(SemanticNode)* semanticNode) inout { this.semanticNode = semanticNode; }
    //this(inout(SemanticNode)[] SemanticNodeLR_) inout { this.SemanticNodeLR_ = SemanticNodeLR_; }
    this(inout(uarray!SemanticNode) uarray_SemanticNode_) inout { this.uarray_SemanticNode_ = uarray_SemanticNode_; }
    this(inout(TupleNode)* TupleNodeP_) inout { this.TupleNodeP_ = TupleNodeP_; }
    this(inout(RuntimeCall)* RuntimeCallP_) inout { this.RuntimeCallP_ = RuntimeCallP_; }
    this(inout(SemanticCall)* SemanticCallP_) inout { this.SemanticCallP_ = SemanticCallP_; }
    this(inout(StatementBlock)* StatementBlockP_) inout { this.StatementBlockP_ = StatementBlockP_; }
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
    @property bool isVoid() const { return type.val is VoidType.instance; }
    @property final bool isUnevaluatedSymbol() const { return type.val is UnevaluatedSymbolType.instance; }

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
        auto castedType = cast(IValueToDotQualifiable)type;
        return castedType ? castedType.tryAsIDotQualifiable(value) : null;
    }

    @property final inout(UnevaluatedSymbol) tryAsUnevaluatedSymbol() inout
    {
        return isUnevaluatedSymbol ? value.UnevaluatedSymbol_ : null;
    }

    @property DelegateFormatter formatType() const
    {
        if (type is null)
            assert(0, "not implemented");
        return types.formatName(type);
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