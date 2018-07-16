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
        //pragma(msg, "` ~ typeName ~ `");
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
}
@property DelegateFormatter formatName(const(IType) type)
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
            auto printer = cast(IValuePrinter)type;
            if (printer)
            {
                printer.print(sink, value);
            }
            else
            {
                sink("[Value of type ");
                sink((cast(Object)type).classinfo.name);
                sink(" that type does not implement IValuePrinter]");
            }
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
    SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const;
}

// A type whose values can be used as conditionals for branching
interface IConditionalType
{
}

interface IRangeType
{
}

// TODO:
// I'm probably going to get rid of this base class
// and split it up into a series of interfaces
interface TypeClass : IType
{
    SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const;
}

class UnevaluatedSymbolType : IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "UnevaluatedSymbol");

    bool supports(SemanticNode* node)
    {
        assert(0, "CodeBug: The supports function probably shouldn't be called on an unevaluated symbol!");
    }
    void formatter(StringSink sink) const
    {
        sink("<unevaluated-symbol>");
    }
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
class NumberLiteralType : TypeClass, IType, IValuePrinter
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
    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
    //
    // IValuePrinter Functions
    //
    final void print(StringSink sink, const(Value) value) const
    {
        sink(get(value).source);
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
        formattedWrite(sink,"TypedNoLimitset(%s)", type.formatName);
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

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}

// The "Integer" type is an ifinite precision whole number
class IntegerType : IType
{
    mixin singleton;

    //mixin valueCreatorAndGetter!("", "string");

    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("integer"); }
}

class UnsignedType : IntegerType
{
    mixin singleton;

    override void formatter(StringSink sink) const { sink("unsigned"); }
}

class UnsignedFixedWidthType(ushort bitWidth) : UnsignedType
{
    mixin singleton;
    override void formatter(StringSink sink) const { formattedWrite(sink, "u%s", bitWidth); }
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
        formattedWrite(sink,"multi(%s %s)", type.formatName, countRange);
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


// A "TypeType" is a "Type" that can be "hold" another type.
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

class TypeTypeTemplate(T) : TypeType
{
    mixin singleton;

    static TypedValue createTypedValue()
    {
        return TypedValue(instance, Value());
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
        formattedWrite(sink,"optional(%s)", type.formatName);
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

    //
    // Type Functions
    //
    final override SatisfyState tryConsume(IScope scope_, SemanticNode*[] args, ushort* offset) const
    {
        assert(0, "not implemented");
    }
}
class SemanticCallType : IType, IValuePrinter
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
    //
    // IValuePrinter functions
    //
    final void print(StringSink sink, const(Value) value) const
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
    void dumpSymbols() const
    {
        assert(0, "EnumType.dumpSymbols not impelemented");
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

class StatementBlockType : IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "StatementBlock*");
    //
    // IType Functions
    //
    final bool supports(SemanticNode* node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        assert(0, "not implemented");
    }
}