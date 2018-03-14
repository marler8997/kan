module types;

import std.typecons : Flag, Yes, No;
import std.format : formattedWrite;

import more.format : StringSink, DelegateFormatter;

import common : uarray, from, singleton;
import log;
import syntax : SyntaxNodeType, SyntaxNode, StringSyntaxNode, TupleSyntaxNode;
import semantics;/* : IScope, SemanticNode, CallSemanticNode, SatisfyState, ResolveResultEnum, IDotQualifiable,
                   positiveRangeFrom, PositiveRange, Function, UserDefinedFunction, Value, TypedValue;*/

string createSemanticNodePtrArrayOnStack(string varName, string semanticNodesVarName)
{
    return
        "import core.stdc.stdlib : alloca;" ~
        "SemanticNode*[] " ~ varName ~ " = (cast(SemanticNode**)alloca((SemanticNode*).sizeof * " ~ semanticNodesVarName
        ~ ".length))[0 .. " ~ semanticNodesVarName ~ ".length];" ~
        "assert(" ~ varName ~ ".ptr, \"alloca failed\");";
}

string mangleValueFieldName(string type)
{
    char[] mangled = new char[type.length + 1];
    foreach (i, c; type)
    {
        if (c == '*')
            mangled[i] = 'P';
        else if (c == '[')
            mangled[i] = 'L';
        else if (c == ']')
            mangled[i] = 'R';
        else if (c == '!')
            mangled[i] = '_';
        else
            mangled[i] = c;
    }
    mangled[type.length] = '_';
    return cast(string)mangled;
}
mixin template valueCreatorAndGetter(string typeModifier, string typeName)
{
    enum code = `
    final auto createTypedValue(` ~ typeModifier ~ ` ` ~ typeName ~ ` value) const
    {
        return ` ~ typeModifier ~ ` TypedValue(this, ` ~ typeModifier ~ ` Value(value));
    }
    final auto get(inout(Value) value) const
    {
        return value.` ~ mangleValueFieldName(typeName) ~ `;
    }
    `;
    //pragma(msg, code);
    mixin(code);
}

interface IValuePrinter
{
    void print(StringSink sink, const(Value) value) const;
}

// The minimum interface an object must support to be considered a type
interface IType
{
    bool supports(SemanticNode* node);

    // These functions aren't necessary for a type definition, but it's very useful for
    // error messages and such
    void formatter(StringSink sink) const;
    void valueFormatter(StringSink sink, const(Value) value) const;
}
@property DelegateFormatter formatType(const(IType) type)
{
    return DelegateFormatter(&type.formatter);
}
@property auto formatValue(const(IType) type, const(Value) value)
{
    static struct Formatter
    {
        const(IType) type;
        const(Value) value;
        void toString(StringSink sink)
        {
            type.valueFormatter(sink, value);
        }
    }
    return Formatter(type, value);
}

interface IValueToDotQualifiable
{
    @property inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const;
}

// A type that when used as a parameter can consume multiple arguments
interface IVariadicType
{
    abstract SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const;
}

interface IRangeType
{
}

// TODO:
// I'm probably going to get rid of this base class
// and split it up into a series of interfaces
interface TypeClass : IType
{
    abstract SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const;
}


class VoidType : TypeType
{
    mixin singleton;

    //
    // IType Functions
    //
    override bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override void formatter(StringSink sink) const { sink("void"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }


    @property final override inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const { return null; }
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        return SatisfyState.satisfied;
    }
}
class AnySingleThing : TypeClass, IType
{
    mixin singleton;

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final void formatter(StringSink sink) const { sink("AnySingleThing"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        if ((*offset) < args.length)
        {
            (*offset)++;
            return SatisfyState.satisfied;
        }
        return SatisfyState.notSatisfied;
    }
}
class NumberLiteralType : TypeClass, IType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "SyntaxNode*");
    /*
    final inout(TypedValue) createTypedValue(inout(SyntaxNode)* syntaxNode) const
    {
        return inout TypedValue(this, inout Value(syntaxNode));
    }
    */

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("NumberLiteral"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}
class SymbolType : TypeClass, IType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("", "string");
    /*
    final TypedValue createTypedValue(string source) const
    {
        return TypedValue(this, Value(source));
    }
    */

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("symbol"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        if (*offset < args.length && args[*offset].syntaxNode.type == SyntaxNodeType.symbol)
        {
            (*offset)++;
            return SatisfyState.satisfied;
        }
        return SatisfyState.notSatisfied;
    }
}
class StringType : TypeClass, IType, IValuePrinter
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "StringSyntaxNode*");
    /*
    final inout(TypedValue) createTypedValue(inout(StringSyntaxNode)* syntaxNode) const
    {
        return inout TypedValue(this, inout Value(syntaxNode));
    }
    */

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("string"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }
    //
    // IValuePrinter Functions
    //
    final void print(StringSink sink, const(Value) value) const
    {
        sink(get(value).str);
    }
    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}
class BooleanType : TypeClass, IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("", "bool");

    @property static TypedValue falseTypedValue()
    {
        return TypedValue(instance, Value(false));
    }
    @property static TypedValue trueTypedValue()
    {
        return TypedValue(instance, Value(true));
    }

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("bool"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}



class TupleLiteralType : IType, IRangeType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "TupleNode*");

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("tuple"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }
}

// TODO: this class could be replaced by TypedNoLimitSetType with
//       the AnySingleThing type
class UntypedNoLimitSetType : TypeClass, IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "uarray!SemanticNode");
    /*
    final inout(TypedValue) createTypedValue(inout(SemanticNode)[] semanticNodes) const
    {
        return inout TypedValue(this, inout Value(semanticNodes));
    }
    */

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("set"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
        /*
        if (*offset >= args.length)
            return SatisfyState.notSatisfied;

        auto arg = args[*offset];
        auto result = arg.evaluateType(scope_);
        final switch(result.state)
        {
        case ResolveResultEnum.haveEntry:
            break;
        case ResolveResultEnum.noEntryAndAllSymbolsAdded:
            return SatisfyState.notSatisfied;
        case ResolveResultEnum.noEntryButMoreSymbolsCouldBeAdded:
            return SatisfyState.needMoreSymbols;
        }

        {
            auto untyped = cast(UntypedNoLimitSetType)result.entry;
            if (untyped)
            {
                (*offset)++;
                return SatisfyState.satisfied;
            }
        }
        assert(0, "not implemented");
        */
    }
}
class TypedNoLimitSetType : TypeClass, IType
{
    static auto singletonOf(T)()
    {
        static immutable instance = new immutable TypedNoLimitSetType(T.instance);
        return instance;
    }
    TypeClass type;
    this(immutable(TypeClass) type) immutable { this.type = type; }
    mixin valueCreatorAndGetter!("inout", "uarray!SemanticNode");
    /*
    final inout(TypedValue) createTypedValue(inout(SemanticNode)[] semanticNodes) const
    {
        assert(0, "not implemenjted");
    }
    */

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        formattedWrite(sink,"TypedNoLimitset(%s)", type.formatType);
    }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
        /*
        if (*offset >= args.length)
            return SatisfyState.notSatisfied;

        auto arg = args[*offset];
        auto result = arg.evaluateType(scope_);
        final switch(result.state)
        {
        case ResolveResultEnum.haveEntry:
            break;
        case ResolveResultEnum.noEntryAndAllSymbolsAdded:
            return SatisfyState.notSatisfied;
        case ResolveResultEnum.noEntryButMoreSymbolsCouldBeAdded:
            return SatisfyState.needMoreSymbols;
        }

        {
            auto untyped = cast(UntypedNoLimitSetType)result.entry;
            if (untyped)
            {
                assert(0, "not implemented");
                /+
                mixin(createSemanticNodePtrArrayOnStack("setNodes", "arg.set.nodes"));
                // make sure this type supports all the nodes in the set
                ushort nodeIndex = 0;
                for (; nodeIndex < arg.set.nodes.length;)
                {
                    auto saveNodeIndex = nodeIndex;
                    auto satisfyResult = type.tryConsume(scope_, setNodes, &nodeIndex);
                    if (satisfyResult != SatisfyState.satisfied)
                    {
                        return satisfyResult;
                    }
                    if (saveNodeIndex == nodeIndex)
                    {
                        return SatisfyState.notSatisfied;
                    }
                }
                (*offset)++;
                return SatisfyState.satisfied;
                +/
            }
        }

        assert(0, from!"std.format".format("not implemented: arg type is %s", type.formatType));
        */
    }
}

/+
class LimitedSetType : TypeClass, IType
{
    /*
    static auto singletonAnyCountOf(T)()
    {
        static immutable instance = new immutable SetType(T.instance, positiveRangeFrom(0));
        return instance;
    }
    */

    Type type;
    PositiveRange countRange;
    this(immutable(Type) type, PositiveRange countRange) immutable
    {
        this.type = type;
        this.countRange = countRange;
    }
    //
    // Type Functions
    //
    @property final override inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const
    {
        assert(0, "not implemented");
    }
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
        /*
        for (ushort consumed = 0; ;consumed++)
        {
            if (countRange.greaterOrEqualToMax(consumed))
            {
                return Yes.satisfied;
            }
            auto saveOffset = *offset;
            // if we don't consume any more arguments
            if (!type.tryConsume(args, offset) || saveOffset == *offset)
            {
                // If consumed >= min, then we are in range and the type is satisfied
                if (countRange.greaterOrEqualToMin(consumed))
                {
                    return Yes.satisfied;
                }
                from!"std.stdio".writefln("Set %s, args not satisfied (consumed %s, countRange %s)", type.classinfo);
                return No.satisfied;
            }
        }
        */
    }
}
+/


// The "StringSink" type is defined as any function that accepts strings
class StringSinkType : TypeClass, IType
{
    mixin singleton;

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("StringSink"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Function
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

// The "Printable" type is defined as the set of values that have a function called "toString" that print the value.
// toString(x, sink)
class PrintableType : TypeClass, IType
{
    mixin singleton;

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override void formatter(StringSink sink) const { sink("Printable"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

class FlagType : TypeClass, IType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("", "string");
    /*
    final TypedValue createTypedValue(string name) const
    {
        return TypedValue(this, Value(name));
    }
    inout(string) get(inout(Value) value) const
    {
        return value.str;
    }
    */

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("flag"); }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

class Multi : TypeClass, IType
{
    static auto singletonAnyCountOf(T)()
    {
        static immutable instance = new immutable Multi(T.instance, positiveRangeFrom(0));
        return instance;
    }

    TypeClass type;
    PositiveRange countRange;
    this(immutable(TypeClass) type, PositiveRange countRange) immutable
    {
        this.type = type;
        this.countRange = countRange;
    }

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        formattedWrite(sink,"multi(%s %s)", type.formatType, countRange);
    }
    final void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* outOffset) const
    {
        auto offset = *outOffset;
        for (ushort consumed = 0; ;consumed++)
        {
            if (countRange.greaterOrEqualToMax(consumed))
            {
                *outOffset = offset;
                return SatisfyState.satisfied;
            }
            auto saveOffset = offset;
            auto result = type.tryConsume(scope_, args, &offset);
            if (result == SatisfyState.needMoreSymbols)
            {
                return result;
            }
            // if we don't consume any more arguments
            if (result == SatisfyState.notSatisfied || saveOffset == offset)
            {
                // If consumed >= min, then we are in range and the type is satisfied
                if (countRange.greaterOrEqualToMin(consumed))
                {
                    *outOffset = offset;
                    return SatisfyState.satisfied;
                }
                verbose(1, "Multi %s, args not satisfied (consumed %s, countRange %s)",
                    type.classinfo, consumed, countRange);
                return SatisfyState.notSatisfied;
            }
        }
    }
}

abstract class ScopeType : TypeClass, IType, IValueToDotQualifiable
{
    //
    // IType Functions
    //
    bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("scope"); }
    void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // IValueToDotQualifiable Functions
    //
    @property abstract inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const;

    //
    // Type Functions
    //
    override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

class ModuleType : ScopeType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "Module");

    //
    // IType Functions
    //
    override bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("module"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // IValueToDotQualifiable Functions
    //
    @property final override inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const
    {
        return get(value);
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

class TypeType : TypeClass, IType, IValueToDotQualifiable
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "IType");

    //
    // IType Functions
    //
    bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("type"); }
    void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // IValueToDotQualifiable Functions
    //
    @property inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const
    {
        return cast(inout(IDotQualifiable))get(value);
    }

    //
    // Type Functions
    //
    override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
        /*
        if (*offset >= args.length)
            return SatisfyState.notSatisfied;

        auto result = args[*offset].evaluateType(scope_);
        if (result.state != ResolveResultEnum.haveEntry)
        {
            if (result.state == ResolveResultEnum.noEntryAndAllSymbolsAdded)
            {
                assert(0, "todo: print good error message and quit");
            }
            return SatisfyState.needMoreSymbols;
        }

        auto asTypeType = cast(TypeType)result.entry;
        if (asTypeType)
        {
            (*offset)++;
            return SatisfyState.satisfied;
        }
        return SatisfyState.notSatisfied;
        */
    }
}


class OptionalType : TypeClass, IType
{
    static auto singletonOf(T)()
    {
        static immutable instance = new immutable OptionalType(T.instance);
        return instance;
    }

    TypeClass type;
    this(immutable(TypeClass) type) immutable { this.type = type; }
    /*
    this(inout(Type) type) inout
    {
        this.type = type;
    }
    */

    //
    // IType Functions
    //
    override bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        formattedWrite(sink,"optional(%s)", type.formatType);
    }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        auto result = type.tryConsume(scope_, args, offset);
        if (result == SatisfyState.notSatisfied)
        {
            return SatisfyState.satisfied;
        }
        return result;
    }
}

class SemanticFunctionType : IType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "SemanticFunction");

    //
    // IType Functions
    //
    bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("SemanticFunction"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }
}

class RuntimeFunctionType : TypeClass, IType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "RuntimeFunction");

    //
    // IType Functions
    //
    bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("RuntimeFunction"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}
class UserDefinedRuntimeFunctionType : RuntimeFunctionType
{
    mixin singleton;

    //mixin valueCreatorAndGetter!("inout", "UserDefinedFunction");

    //
    // IType Functions
    //
    override bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("UserDefinedRuntimeFunction"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

class RuntimeCallType : TypeClass, IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "RuntimeCall*");

    //
    // IType Functions
    //
    bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("call"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}
class SyntaxCallType : IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "SyntaxCall*");
    //
    // IType Functions
    //
    bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("SyntaxCall"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }
}
class SemanticCallType : IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "SemanticCall*");
    //
    // IType Functions
    //
    bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("SemanticCall"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        auto node = get(value);
        formattedWrite(sink, "SemanticCall to %s", node.syntaxNode.functionName);
    }
}

class BuiltinRuntimeFunctionType : RuntimeFunctionType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "BuiltinRuntimeFunction");

    //
    // IType Functions
    //
    override bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("BuiltinRuntimeFunction"); }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

class EnumType : TypeClass, IType, IDotQualifiable, IValuePrinter
{
    string[] symbols;
    this(string[] symbols)
    {
        this.symbols = symbols;
    }

    mixin valueCreatorAndGetter!("", "string");

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        sink("enum");
    }
    override void valueFormatter(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
    }
    //
    // IDotQualifiable Function
    //
    final ResolveResult tryGetUnqualified(string symbol)
    {
        foreach (definitionSymbol; symbols)
        {
            if (symbol == definitionSymbol)
            {
                return ResolveResult(createTypedValue(definitionSymbol));
            }
        }
        return ResolveResult.noEntryAndAllSymbolsAdded;
    }
    //
    // IValuePrinter Functions
    //
    final void print(StringSink sink, const(Value) value) const
    {
        sink(get(value));
    }
    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}
