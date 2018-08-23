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

static import global;

import typecons : Rebindable, rebindable;
import common : isNull, passfail, uarray, toUarray, unconst, from, singleton, quit;
import log;
import syntax : SyntaxNodeType, SyntaxNode, KeywordType, KeywordSyntaxNode, StringSyntaxNode, TupleSyntaxNode, CallSyntaxNode;
import types;// : Type, VoidType, NumberType, StringType, ModuleType;
import symtab : SymbolTable;
import mod : Module;
static import builtin;
import builtin : tryGetSemanticFunctionFor;
import pass2 = analyzer.pass2;
static import interpreter;

// !!!!!!
// Note: will support different kinds of visitors
//
interface IHighLevelVisitor
{
    void visit(SymbolTableEntry);
    void visit(SetReturnNode);
    void visit(Tuple);
    void visit(Symbol);
    void visit(RegularCall);
    void visit(SemanticCall);
    void visit(BuiltinType); // TODO: Probably should be Type, not BuiltinType?
    void visit(SemanticFunction);
    void visit(RegularFunction);
    void visit(Value);
    //void visit(IType);
    //void visit(FunctionParameter);
    void visit(LazyNode);
}
class HighLevelVisitorIgnoreByDefault : IHighLevelVisitor
{
    void visit(SymbolTableEntry) { }
    void visit(SetReturnNode) { }
    void visit(Tuple) { }
    void visit(Symbol) { }
    void visit(RegularCall) { }
    void visit(SemanticCall) { }
    void visit(BuiltinType) { }
    void visit(SemanticFunction) { }
    void visit(RegularFunction) { }
    void visit(Value) { }
    //void visit(IType) { }
    //void visit(FunctionParameter) { }
    void visit(LazyNode) { }
}
class HighLevelVisitorNotImplementedByDefault : IHighLevelVisitor
{
    import std.format : format;
    void visit(SymbolTableEntry node) { assert(0, format("%sSymbolTableEntry not implemented", node.formatLocation)); }
    void visit(SetReturnNode node) { assert(0, format("%sSetReturnNode not implemented", node.formatLocation)); }
    void visit(Tuple node) { assert(0, format("%sTuple not implemented", node.formatLocation)); }
    void visit(Symbol node) { assert(0, format("%sSymbol not implemented", node.formatLocation)); }
    void visit(RegularCall node) { assert(0, format("%sRegularCall not implemented", node.formatLocation)); }
    void visit(SemanticCall node) { assert(0, format("%sSemanticCall not implemented", node.formatLocation)); }
    void visit(BuiltinType node) { assert(0, format("%sBuiltinType not implemented", node.formatLocation)); }
    void visit(SemanticFunction node) { assert(0, format("%sSemanticFunction not implemented", node.formatLocation)); }
    void visit(RegularFunction node) { assert(0, format("%sRegularFunction not implemented", node.formatLocation)); }
    void visit(Value node) { assert(0, format("%sValue not implemented", node.formatLocation)); }
    //void visit(IType node) { assert(0, format("%sIType not implemented", node.formatLocation)); }
    //void visit(FunctionParameter node) { assert(0, format("%sFunctionParameter not implemented", node.formatLocation)); }
    void visit(LazyNode node) { assert(0, format("%sLazyNode not implemented", node.formatLocation)); }
}


void newSemanticNodesInto(uarray!SemanticNode into, const uarray!SyntaxNode syntaxNodes)
in { assert(into.length >= syntaxNodes.length); } do
{
    foreach (i; 0 .. syntaxNodes.length)
    {
        into[i] = newSemanticNode(&syntaxNodes[i]);
    }
}
uarray!SemanticNode newSemanticNodes(const uarray!SyntaxNode syntaxNodes)
{
    auto semanticNodes = new SemanticNode[syntaxNodes.length].toUarray;
    newSemanticNodesInto(semanticNodes, syntaxNodes);
    return semanticNodes;
}

SemanticNode newSemanticNode(const(SyntaxNode)* syntaxNode)
{
    final switch(syntaxNode.type)
    {
    case SyntaxNodeType.number:
        return new NumberLiteral(syntaxNode);
    case SyntaxNodeType.string_:
        return new StringLiteral(&syntaxNode.str);
    case SyntaxNodeType.keyword:
        final switch(syntaxNode.keyword.type)
        {
        case KeywordType.void_ : return new VoidKeyword(&syntaxNode.keyword);
            break;
        case KeywordType.false_: return new Bool(syntaxNode, false);
            break;
        case KeywordType.true_ : return new Bool(syntaxNode, true);
            break;
        }
    case SyntaxNodeType.symbol:
        return new SymbolFromSyntax(syntaxNode);
    case SyntaxNodeType.tuple:
        return new TupleFromSyntaxNode(&syntaxNode.tuple, newSemanticNodes(syntaxNode.tuple.nodes));
    case SyntaxNodeType.call:
        {
            auto call = tryGetSemanticFunctionFor(&syntaxNode.call);
            if (call !is null)
            {
                return call;
            }
        }
        return new RegularCall(syntaxNode, syntaxNode.call.functionName,
            /*syntaxNode.call.arguments.unconst, */newSemanticNodes(syntaxNode.call.arguments));
    }
}

Module tryGetModuleFromSource(immutable(char)* source)
{
    if (source == Module.builtinSyntaxSource.ptr)
    {
        return Module.builtin.unconst;
    }
    foreach (mod; global.modules.data)
    {
        if (source >= mod.content.ptr && source < mod.content.ptr + mod.content.length)
            return mod;
    }
    return null;
}
LocationFormatter formatLocation(immutable(char)* source)
{
    auto mod = tryGetModuleFromSource(source);
    assert(mod, "codebug: formatLocation called with string that was not in any module");
    return mod.formatLocation(source);
}
pragma(inline)
LocationFormatter formatLocation(const(SyntaxNode)* node) { return formatLocation(node.source.ptr); }

class SemanticNode : IDotQualifiable
{
    abstract void accept(IHighLevelVisitor visitor);

    final LocationFormatter formatLocation() const
    {
        auto syntaxNode = getSyntaxNode();
        return .formatLocation(getSyntaxNode.source.ptr);
    }

    abstract const(SyntaxNode)* getSyntaxNode() const;
    abstract IType getType() const;
    abstract void valueFormatter(StringSink sink) const;
    abstract void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const;
    //
    // Object methods
    //
    void toString(scope void delegate(in char[]) sink) const
    {
        sink((cast(Object)super).toString());
    }
}
DelegateFormatter formatValue(const(SemanticNode)* node)
{
    return DelegateFormatter(&node.valueFormatter);
}

mixin template SemanticNodeMixin()
{
    final override void accept(IHighLevelVisitor visitor) { visitor.visit(this); }
}

class Tuple : SemanticNode
{
    uarray!SemanticNode nodes;
    this(uarray!SemanticNode nodes)
    {
        this.nodes = nodes;
    }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
    final override IType getType() const { assert(0, "not implemented"); }
}
class TupleFromSyntaxNode : Tuple
{
    const(TupleSyntaxNode)* syntaxNode;
    this(const(TupleSyntaxNode)* syntaxNode, uarray!SemanticNode nodes)
    {
        super(nodes);
        this.syntaxNode = syntaxNode;
    }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("syntax-tuple"); }
    //
    // SemanticNode methods
    //
    override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode.base; }
    final override void valueFormatter(StringSink sink) const { sink("<syntax-tuple>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "not implemented");
    }
}

/**
A VoidKeyword node represents an actual 'void' syntax node.
It can represent a void value, or a void type, or anything else that
accepts a void syntax node.
TODO: Maybe I'll have VoidKeyword inherts from VoidType inherits from VoidValue?
*/
class VoidKeyword : Value
{
    const(KeywordSyntaxNode)* syntaxNode;
    this(const(KeywordSyntaxNode)* syntaxNode)
    {
        this.syntaxNode = syntaxNode;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return VoidType.instance.unconst; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("void"); }
    //
    // SemanticNode methods
    //
    override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode.base; }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { sink("<void>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "not implemented");
    }
}
class VoidValue : Value
{
    const(SyntaxNode)* syntaxNode;
    this(const(SyntaxNode)* syntaxNode)
    {
        this.syntaxNode = syntaxNode;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("void"); }
    //
    // SemanticNode methods
    //
    override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { return VoidType.instance.unconst; }
    final override void valueFormatter(StringSink sink) const { sink("<void>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "not implemented");
    }
}

class SymbolTableEntry : SemanticNode
{
    //IScope setScope;
    string name;
    SemanticNode currentNode;
    enum State
    {
        initial,
        pass2Started,
        pass2Done,
        failed,
    }
    State state;
    this(/*IScope setScope, */string name, SemanticNode currentNode)
    {
        //this.setScope = setScope;
        this.name = name;
        this.currentNode = currentNode;
    }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    { return currentNode.tryGetUnqualified(symbol, fromInside); }
    void scopeDescriptionFormatter(StringSink sink) const { currentNode.scopeDescriptionFormatter(sink); }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
    override const(SyntaxNode)* getSyntaxNode() const { return currentNode.getSyntaxNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { currentNode.valueFormatter(sink); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        currentNode.printFormatter(sink, interpreter);
    }
    override void toString(scope void delegate(in char[]) sink) const
    {
        formattedWrite(sink, "symtab[%s:%s]", name, currentNode);
    }
}

class SetReturnNode : SemanticNode
{
    builtin.setCall set;
    this(builtin.setCall set)
    {
        this.set = set;
    }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    { return set.symbolTableEntry.currentNode.tryGetUnqualified(symbol, fromInside); }
    void scopeDescriptionFormatter(StringSink sink) const { set.symbolTableEntry.currentNode.scopeDescriptionFormatter(sink); }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
    override const(SyntaxNode)* getSyntaxNode() const { return set.symbolTableEntry.currentNode.getSyntaxNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { set.symbolTableEntry.currentNode.valueFormatter(sink); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        set.symbolTableEntry.currentNode.printFormatter(sink, interpreter);
    }
    override void toString(scope void delegate(in char[]) sink) const
    {
        set.symbolTableEntry.toString(sink);
    }
}


class Symbol : SemanticNode
{
    abstract string value() const;
    //
    // IDotQualifiable methods
    //
    void scopeDescriptionFormatter(StringSink sink) const { sink("symbol"); }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { formattedWrite(sink, "<symbol:%s>", value); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "codebug: Symbol nodes shouldn't exist at interpret time");
    }
}
class SymbolFromSyntax : Symbol
{
    const(SyntaxNode)* syntaxNode;
    this(const(SyntaxNode)* syntaxNode)
    {
        this.syntaxNode = syntaxNode;
    }
    //
    // Symbol methods
    //
    final override string value() const { return syntaxNode.source; }
    //
    // SemanticNode methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
}

class Call : SemanticNode
{
    this()
    {
    }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    //
    // SemanticNode methods
    //
    final override void valueFormatter(StringSink sink) const { assert(0, "not implemented"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        // probably an error, I don't think symbols can exist at runtime
        assert(0, "not implemented");
    }
}

DelegateFormatter formatNameForMessage(T)(const(T) obj)
{
    return DelegateFormatter(&obj.nameForErrorMessageFormatter);
}

class SemanticCall : Call
{
    protected const(SyntaxNode)* syntaxNode;
    uarray!SyntaxNode syntaxArgs;
    void* funcData; // Data that the function may need to associate with the call
    this(const(SyntaxNode)* syntaxNode, uarray!SyntaxNode syntaxArgs)
    {
        this.syntaxNode = syntaxNode;
        this.syntaxArgs = syntaxArgs;
        /+
        auto nodeCount = function_.semanticNodeBufferCountFor(syntaxArgs.length);
        this.semanticArgs = new SemanticNode[nodeCount].toUarray;
        auto initializedCount = 0;
        foreach (arg; function_.semanticNodeRange(syntaxArgs.length))
        {
            this.semanticArgs[arg.semanticNodeIndex] = newSemanticNode(&syntaxArgs[arg.syntaxNodeIndex]);
            initializedCount++;
        }
        assert(initializedCount <= nodeCount);
        for (;initializedCount < nodeCount; initializedCount++)
        {
            // TODO: initialize to a void placeholder value
            assert(0, "not implemented");
        }
        +/
    }
    abstract string getFunctionName() const;
    final void nameForErrorMessageFormatter(StringSink sink) const
    {
        //sink(function_.nameForMessages);
        sink(getFunctionName);
    }

    // Assumption: this function should always be called before inTreeOrderInterpretPass1
    abstract uint inTreeOrderInterpretPass1(IScope scope_);
    // Assumption: this function will ONLY be called after inTreeOrderInterpretPass1
    // Assumption: this function will only be called once at most
    // Assumption: this function will not be called if tryInterpretToRegularFunction is called
    abstract NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_) const;
    // Assumption: this function will ONLY be called after inTreeOrderInterpretPass1
    // Assumption: this function will only be called once at most
    // Assumption: this function will not be called if inTreeOrderInterpretPass2 is called
    OptionalResultOrError!RegularFunction tryInterpretToRegularFunction()
    {
        return OptionalResultOrError!RegularFunction(0);
    }
    final ResultOrError!RegularFunction interpretToRegularFunction(LocationFormatter errorLocation)
    {
        auto result = tryInterpretToRegularFunction();
        if (result.errorCount > 0)
            return ResultOrError!RegularFunction(result.errorCount);
        if (result.value is null)
            return errorfResultOrError!(RegularFunction)(errorLocation, "%s is not a function", this);
        return ResultOrError!RegularFunction(result.value);
    }


    //
    // IDotQualifiable methods
    //
    void scopeDescriptionFormatter(StringSink sink) const { sink("semantic call"); }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    //
    //
    override void toString(scope void delegate(in char[]) sink) const
    {
        formattedWrite(sink, "SemanticCall to '%s'", getFunctionName);
    }
}
class BuiltinSemanticCall : SemanticCall
{
    string functionName;
    this(const(SyntaxNode)* syntaxNode, string functionName, uarray!SyntaxNode syntaxArgs)
    {
        super(syntaxNode, syntaxArgs);
        this.functionName = functionName;
    }
    //
    // SemanticCall methods
    //
    final override string getFunctionName() const
    {
        return functionName;
    }
}

/**
A RegularCall is a call to a regular function (i.e. not a "semantic function").
*/
class RegularCall : Call
{
    const(SyntaxNode)* syntaxNode;
    uarray!SemanticNode semanticArgs;
    string functionNameToResolve;
    RegularFunction function_;
    this(const(SyntaxNode)* syntaxNode, string functionNameToResolve,
        /*uarray!SyntaxNode syntaxArgs, */uarray!SemanticNode semanticArgs)
    {
        this.syntaxNode = syntaxNode;
        this.functionNameToResolve = functionNameToResolve;
        this.semanticArgs = semanticArgs;
    }
    this(const(SyntaxNode)* syntaxNode, RegularFunction function_,
        /*uarray!SyntaxNode syntaxArgs, */uarray!SemanticNode semanticArgs)
    {
        this.syntaxNode = syntaxNode;
        this.function_ = function_;
        this.semanticArgs = semanticArgs;
    }
    //
    // IDotQualifiable methods
    //
    void scopeDescriptionFormatter(StringSink sink) const { sink("runtime call"); }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    //
    // Objecte methods
    //
    override void toString(scope void delegate(in char[]) sink) const
    {
        if (functionNameToResolve)
            formattedWrite(sink, "RegularCallTo(%s)", functionNameToResolve);
        else
            formattedWrite(sink, "RegularCall");
    }
}


class Value : SemanticNode
{
    /** Try to interpret the node as a type */
    abstract IType tryAsType() const;
    abstract void serialize(IType type, ubyte[] storage) const;
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
}

class Bool : Value
{
    const(SyntaxNode)* syntaxNode;
    bool value;
    this(const(SyntaxNode)* syntaxNode, bool value)
    {
        this.syntaxNode = syntaxNode;
        this.value = value;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("bool value"); }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { return BoolType.instance.unconst; }
    final override void valueFormatter(StringSink sink) const { sink(value ? "true" : "false"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        sink(value ? "true" : "false");
    }
}

class Number : Value
{
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("number value"); }
}
class NumberLiteral : Number
{
    const(SyntaxNode)* syntaxNode;
    this(const(SyntaxNode)* syntaxNode)
    {
        this.syntaxNode = syntaxNode;
    }
    //
    // Value methods
    //
    final override void serialize(IType type, ubyte[] storage) const
    {
        import serial : trySerializeNumber;
        auto numberType = cast(NumberType)type;
        if (!numberType)
        {
            assert(0, from!"std.format".format("not sure if a number literal can be passed to type '%s'", type));
        }
        auto error = trySerializeNumber(syntaxNode.source, storage);
        if (error)
        {
            errorfNoLocation("failed to serialize number '%s' to %s bytes: %s", syntaxNode.source, storage.length, error);
            assert(0);
        }
    }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { return NumberLiteralType.instance.unconst; }
    final override void valueFormatter(StringSink sink) const { sink(syntaxNode.source); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        sink(syntaxNode.source);
    }
}
class StringLiteral : Value
{
    const(StringSyntaxNode)* syntaxNode;
    this(const(StringSyntaxNode)* syntaxNode)
    {
        this.syntaxNode = syntaxNode;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("string literal"); }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode.base; }
    final override IType getType() const { return StringLiteralType.instance.unconst; }
    final override void valueFormatter(StringSink sink) const { sink(syntaxNode.str); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        sink(syntaxNode.str);
    }
}
class FlagValue : Value
{
    const(SyntaxNode)* syntaxNode;
    string name;
    this(const(SyntaxNode)* syntaxNode, string name)
    {
        this.syntaxNode = syntaxNode;
        this.name = name;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("flag value"); }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { return FlagType.instance.unconst; }
    final override void valueFormatter(StringSink sink) const { formattedWrite(sink, "<flag:%s>", name); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        formattedWrite(sink, "flag(%s)", name);
    }
}
class EnumValue : Value
{
    const(SyntaxNode)* syntaxNode;
    EnumType type;
    string name;
    this(const(SyntaxNode)* syntaxNode, EnumType type, string name)
    {
        this.syntaxNode = syntaxNode;
        this.type = type;
        this.name = name;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("eum value"); }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { return type.unconst; }
    final override void valueFormatter(StringSink sink) const { sink(name); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        sink(name);
    }
}

class BuiltinType : SemanticNode, IType
{
    mixin SemanticNodeMixin;
    //
    // IType methods
    //
    abstract bool supports(SemanticNode node);
    abstract void formatter(StringSink sink) const;
    //
    // IDotQualifiable methods
    //
    OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("type"); }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return Module.getBuiltinSyntaxNode; }
    final override IType getType() const { return TypeType.instance.unconst; }
    final override void valueFormatter(StringSink sink) const { sink("<type>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "codebug: Type nodes shouldn't exist at interpret time");
    }
}

class FunctionParameter : Value
{
    const(SyntaxNode)* syntaxNode;
    RegularFunction func;
    uint paramIndex;
    string name;
    IType type;
    this(const(SyntaxNode)* syntaxNode, RegularFunction func, uint paramIndex, string name, IType type)
    {
        this.syntaxNode = syntaxNode;
        this.func = func;
        this.paramIndex = paramIndex;
        this.name = name;
        this.type = type;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const
    {
        // Maybe a function parameter could be an alias to a type?
        assert(0, "not implemented");
    }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("function parameter"); }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { sink("<func-param>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        interpreter.currentBlock.args[paramIndex].printFormatter(sink, interpreter);
    }
}

// A node that has not been evaluated
class LazyNode : SemanticNode
{
    private SemanticNode evaluated;
    NodeResult tryEvaluate()
    {
        if (!evaluated)
        {
            auto result = doEvaluate();
            if (!result.value)
            {
                assert(result.errorCount > 0, "codebug?");
                return result;
            }
            assert(result.errorCount == 0, "codebug?");
            this.evaluated = result.value;
        }
        return NodeResult(evaluated);
    }
    protected abstract NodeResult doEvaluate();
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    {
        assert(0, "codebug? I don't think tryGetUnqualified should be called on a LazyNode");
    }
    void scopeDescriptionFormatter(StringSink sink) const { sink("lazy node"); }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
}

class ImportedModule : Value
{
    const(SyntaxNode)* importSyntaxNode;
    private Module mod;
    this(const(SyntaxNode)* importSyntaxNode, Module mod)
    {
        this.importSyntaxNode = importSyntaxNode;
        this.mod = mod;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    { return mod.tryGetUnqualified(symbol, fromInside); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("module "); sink(mod.importName); }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return importSyntaxNode; }
    final override IType getType() const { return ModuleType.instance.unconst; }
    final override void valueFormatter(StringSink sink) const { formattedWrite(sink, "<module:%s>", mod.importName); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "codebug: ImportedModule nodes shouldn't exist at interpret time");
    }
    override void toString(scope void delegate(in char[]) sink) const
    {
        formattedWrite(sink, "importedModule(%s)", mod);
    }
}

T tryAs(T)(SemanticNode func)
{
    static class Visitor : HighLevelVisitorIgnoreByDefault
    {
        T result;
        final override void visit(T node) { this.result = node; }
        final override void visit(LazyNode node)
        {
            assert(0, "not implemented");
        }
    }
    scope visitor = new Visitor();
    func.accept(visitor);
    return visitor.result;
}

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

enum BlockFlags : ubyte
{
    isJumpBlock,
}
/+
enum JumpType : ubyte
{
    loopCurrentBlock,
    breakCurrentBlock,
}
struct JumpNode
{
    const(SyntaxNode)* syntaxNode;
    Rebindable!TypedValue asTypedValue;
    SemanticNode condition;
    JumpType jumpType;
}
+/

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

bool valueIsNull(T)(T value)
{
    static if (is(T == class))
    {
        return value is null;
    }
    else
    {
        return value.isNull;
    }
}
struct OptionalResultOrError(T)
{
    uint errorCount;
    T value;
    this(uint errorCount)
    {
        this.errorCount = errorCount;
    }
    this(T value)
    {
        this.errorCount = 0;
        this.value = value;
    }
}
struct ResultOrError(T)
{
    OptionalResultOrError!T obj;
    alias obj this;

    this(uint errorCount)
    in { assert(errorCount > 0, "codebug, neither value or error was given"); } do
    {
        this.errorCount = errorCount;
    }
    this(T value)
    in { assert(!valueIsNull(value), "codebug, cannot set to null"); } do
    {
        this.errorCount = 0;
        this.value = value;
    }
}
alias NodeResult = ResultOrError!SemanticNode;
alias OptionalNodeResult = OptionalResultOrError!SemanticNode;

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
//alias ResolveResult = ResolveResultTemplate!(const(TypedValue));
alias ResolveResult = ResolveResultTemplate!SemanticNode;
alias ResolveTypeResult = ResolveResultTemplate!(const(IType));
alias SemanticCallResult = ResolveResultTemplate!(SemanticNode);

// An object that contains members accessed via the '.' operator
interface IDotQualifiable
{
    // try to get a symbol table entry that matches the given symbol
    OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside);
    void scopeDescriptionFormatter(StringSink sink) const;
}
DelegateFormatter formatScopeDescription(const(IDotQualifiable) qualifiable)
{
    return DelegateFormatter(&qualifiable.scopeDescriptionFormatter);
}

interface IReadonlyScope : IDotQualifiable
{
    void dumpSymbols() const;
    @property inout(IReadonlyScope) getParent() inout;
    @property inout(Module) asModule() inout;
    @property inout(IScope) asWriteable() inout;
    @property inout(JumpBlock) asJumpBlock() inout;
    uint prepareForChildAnalyzePass2();
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
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // TODO: Maybe add should return a reference to the symbol entry in the symbol
    //       table in case the caller ever wants to update it
    SymbolTableEntry tryAdd(string symbol, SemanticNode node);
    //void add(const(string) symbol, SemanticNode node);
    //void evaluated(const(string) symbol, SemanticNode node);
}
SymbolTableEntry tryAddOrPrintError(IScope scope_, string symbol, SemanticNode node, LocationFormatter errorLocation)
{
    auto result = scope_.tryAdd(symbol, node);
    if (!result)
        errorf(errorLocation, "scope of %s already has symbol '%s'", scope_.formatScopeDescription, symbol);
    return result;
}

/**
Used to pass IReadonlyScope to IScope
Use: scope thunkedScope = new ReadonlyScopeThunk(scope_);
*/
class ReadonlyScopeThunk : IScope
{
    IReadonlyScope wrappedScope;
    this(IReadonlyScope wrappedScope) { this.wrappedScope = wrappedScope; }
    //
    // IScope methods
    //
    SymbolTableEntry tryAdd(string symbol, SemanticNode node)
    {
        errorf(node.formatLocation, "this scope has already been analyzed on pass1, no more symbols can be added");
        return null;
    }
    //
    // IReadonlyScope methods
    //
    void dumpSymbols() const { wrappedScope.dumpSymbols(); }
    @property inout(IReadonlyScope) getParent() inout { return wrappedScope.getParent(); }
    @property inout(Module) asModule() inout { return wrappedScope.asModule(); }
    @property inout(IScope) asWriteable() inout { return wrappedScope.asWriteable(); }
    @property inout(JumpBlock) asJumpBlock() inout { return wrappedScope.asJumpBlock(); }
    uint prepareForChildAnalyzePass2() { return wrappedScope.prepareForChildAnalyzePass2(); }
    //
    // IDotQualifiable methods
    //
    OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    { return wrappedScope.tryGetUnqualified(symbol, fromInside); }
    void scopeDescriptionFormatter(StringSink sink) const
    { return wrappedScope.scopeDescriptionFormatter(sink); }
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
struct Parameter2
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
    SemanticNode type;
    this(string name, SemanticNode type)
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

struct RegularFunctionInterface
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

    SatisfyState supports(IScope scope_, RegularCall call, StringSink errorSink) const
    {
        assert(0, "not implemented");
        /+
        static struct ParameterState
        {
            bool used;
        }
        import core.stdc.stdlib : alloca;
        SemanticNode* filteredArgumentsPtr = cast(SemanticNode*)alloca((SemanticNode).sizeof * call.arguments.length);
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

/**
A regular function is a function that can be run at compile-time or runtime, but not
at "analyze-time" (i.e. it is not a "semantic function").
*/
class RegularFunction : SemanticNode
{
    IReadonlyScope containingScope;
    this(IReadonlyScope containingScope)
    {
        this.containingScope = containingScope;
    }
    this(immutable(IReadonlyScope) containingScope) immutable
    {
        this.containingScope = containingScope;
    }
    abstract passfail addAndCheckCall(RegularCall runtimeCall);
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
}

class BuiltinRegularFunction : RegularFunction
{
    string nameForMessages;
    RegularFunctionInterface interface_;
    uint callCount;
    this(string nameForMessages, immutable(RegularFunctionInterface) interface_) immutable
    {
        super(Module.builtin);
        this.nameForMessages = nameForMessages;
        this.interface_ = interface_;
    }

    // TODO: This should probably take an array of arguments rather than a RegularCall
    abstract SemanticNode interpret(interpreter.Interpreter* interpreter, RegularCall call, uarray!SemanticNode runtimeArgs);
    //
    // RegularFunction methods
    //
    final override passfail addAndCheckCall(RegularCall runtimeCall)
    {
        callCount++;
        return passfail.pass;
    }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("builtin function"); }
    //
    // SemanticNode methods
    //
    protected final override const(SyntaxNode)* getSyntaxNode() const { assert(0, "not implemented"); }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { sink("<builtin-function>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        sink("<builtin-function>");
    }
}

class ExternProperties : Value
{
    const(SyntaxNode)* syntaxNode;
    string funcName;
    this(const(SyntaxNode)* syntaxNode, string funcName)
    {
        this.syntaxNode = syntaxNode;
        this.funcName = funcName;
    }
    //
    // Value methods
    //
    final override IType tryAsType() const { return null; }
    final override void serialize(IType type, ubyte[] storage) const { assert(0, "not impl"); }
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside) { assert(0, "not implemented"); }
    void scopeDescriptionFormatter(StringSink sink) const { formattedWrite(sink, "extern(\"%s\")", funcName); }
    //
    // SemanticNode methods
    //
    protected final override const(SyntaxNode)* getSyntaxNode() const { return syntaxNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { formattedWrite(sink, "extern(\"%s\")", funcName); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        formattedWrite(sink, "extern(\"%s\")", funcName);
    }

}

class UserDefinedFunction : RegularFunction, IScope
{
    const(SyntaxNode)* originalDefinitionNode;
    uarray!SyntaxNode defNodes;
    Builder!(RegularCall, GCDoubler!8) calls;
    enum State
    {
        initial,
        pass2Started,
        pass2Done,
        analyzeFailed,
    }
    State state;
    IType returnType;
    uarray!FunctionParameter params;
    Flag!"isExternFunction" isExternFunction;
    union
    {
        struct
        {
            // TODO: abi
            string externName;
        }
        struct
        {
            uarray!SemanticNode bodyNodes;
            SymbolTable bodySymbolTable;
        }
    }

    this(IReadonlyScope containingScope, const(SyntaxNode)* originalDefinitionNode, uarray!SyntaxNode defNodes)
    {
        super(containingScope);
        this.originalDefinitionNode = originalDefinitionNode;
        this.defNodes = defNodes;
    }

    final override passfail addAndCheckCall(RegularCall runtimeCall)
    {
        // Only need to add if I'm going to perform optimization later
        calls.append(runtimeCall);

        if (state == State.initial)
        {
            state = State.pass2Started;
            uint errorCount = pass2.analyzeUserDefinedFunction(this);
            assert(state == State.pass2Started, "codebug");
            state = (errorCount > 0) ? State.analyzeFailed : State.pass2Done;
        }
        if (state == State.analyzeFailed)
            return passfail.fail;
        assert(state == State.pass2Done, "codebug");

        from!"std.stdio".writefln("WARNING: %scheck regular call arguments not implemented",
            runtimeCall.formatLocation());
        return passfail.pass;
    }

    /*
    final override void interpret(RegularCall call)
        in { assert(codeAnalyzedCount == rawCode.length); } do
    {
        interpreter.interpretRegularCall(this, call);
    }
    */
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    {
        if (fromInside)
        {
            assert(state == State.pass2Started, "codebug?");
            return OptionalNodeResult(bodySymbolTable.tryGet(symbol));
        }
        else
        {
            assert(0, "not implemented");
        }
    }
    void scopeDescriptionFormatter(StringSink sink) const { sink("function"); }
    void dumpSymbols() const
    {
        bodySymbolTable.dump();
    }
    //
    // SemanticNode methods
    //
    final override const(SyntaxNode)* getSyntaxNode() const { return originalDefinitionNode; }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { sink("<function>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        sink("<user-defined-function>");
    }
    //
    // IReadonlyScope methods
    //
    @property final inout(IReadonlyScope) getParent() inout { return containingScope; }
    @property final inout(Module) asModule() inout { return null; }
    @property final inout(IScope) asWriteable() inout { return this; }
    @property final inout(JumpBlock) asJumpBlock() inout { return null; }
    final uint prepareForChildAnalyzePass2()
    {
        /*
        I think this could happen if you have a call to a function
        that is defined inside this function. i.e.
        function(auto {} {
            set(foo function(auto {} {}))
            foo() // when this function is analyzed, it should call this function for
                  // the parent function scope
        })
        */
        // I don't actually think we need to do anything here, because if we are analyzing
        // a call to a child function, then it must mean we have already done pass1?  Note
        // that this also means that you cannot call a funciton inside a function outside
        // that function.  This makes sense, if you want to call it outside the funciton, then
        // define it outside the function.
        assert(state == State.pass2Started, "codebug?");
        return 0;
    }
    //
    // IScope methods
    //
    SymbolTableEntry tryAdd(string symbol, SemanticNode node)
    {
        auto result = bodySymbolTable.tryAdd(symbol, node);
        return result.newEntryAdded ? result.entry : null;
    }
    /+
    void evaluated(const(string) symbol, SemanticNode node)
    {
        if (bodySymbolTable.update(symbol, node).failed)
        {
            from!"std.stdio".writefln("Error: CodeBug: attempted to update symbol '%s' but it does not exist!", symbol);
            throw quit;
        }
    }
    +/
}


/+
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
+/

class SemanticFunction : SemanticNode
{
    string nameForMessages;
    /+
    SemanticArgType defaultArgType;
    const(NumberedSemanticArgType)[] firstArgTypes;
    // Total number of arguments in firstArgTypes
    private size_t firstArgTypesCount;
    // Saves the number of nodes needed from firstArgTypes
    private size_t firstArgsThatNeedSemanticNodesCount;
    +/
    this(string nameForMessages/*, SemanticArgType defaultArgType, immutable(NumberedSemanticArgType)[] firstArgTypes*/) immutable
    {
        this.nameForMessages = nameForMessages;
        /+
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
        +/
    }

    /+
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
    +/
    abstract uint inTreeOrderInterpretPass1(IScope scope_, SemanticCall call);
    abstract NodeResult inTreeOrderInterpretPass2(IReadonlyScope scope_, SemanticCall call/*, Flag!"used" used*/) const;

    /+
    final void printErrorsForInterpret(IReadonlyScope scope_, SemanticCall call, Flag!"used" used) const
    {
        errorf(call.formatLocation(), "failed to interpret function '%s' (TODO: implement printing more details)", call);
    }
    +/
    final auto checkAndGet(uarray!SyntaxNode args) inout
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
    //
    // IDotQualifiable methods
    //
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    { assert(0, "not implemented or maybe code bug?"); }
    void scopeDescriptionFormatter(StringSink sink) const { sink("semantic function"); }
    //
    // SemanticNode methods
    //
    mixin SemanticNodeMixin;
    override const(SyntaxNode)* getSyntaxNode() const { assert(0, "not implemented"); }
    final override IType getType() const { assert(0, "not implemented"); }
    final override void valueFormatter(StringSink sink) const { sink("<semantic-function>"); }
    final override void printFormatter(StringSink sink, interpreter.Interpreter* interpreter) const
    {
        assert(0, "codebug: SemanticFunction nodes shouldn't exist at interpret time");
    }
}

/+
struct NodeIndices
{
    Flag!"fullyAnalyzed" fullyAnalyzed;
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
        override NodeResult interpret(IScope scope_, SemanticCall call/*, Flag!"used" used*/) const
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
+/

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
    final OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    {
        if (fromInside)
        {
            // !!!!!!!!!!!!!!!!!!!!!!!
            // TODO: this is probably not right
            // !!!!!!!!!!!!!!!!!!!!!!!
            return OptionalNodeResult(symbolTable.tryGet(symbol));
        }
        else
        {
            assert(0, "not implemented");
        }
    }
    void scopeDescriptionFormatter(StringSink sink) const { sink("jump block"); }
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
    final uint prepareForChildAnalyzePass2()
    {
        assert(0, "not implemented");
    }
    //
    // IScope functions
    //
    SymbolTableEntry tryAdd(string symbol, SemanticNode node)
    {
        assert(0, "not implemented");
    }
    /+
    void evaluated(const(string) symbol, SemanticNode node)
    {
        assert(0, "not implemented");
    }
    +/
}
