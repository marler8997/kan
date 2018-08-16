module types;

import std.typecons : Flag, Yes, No;
import std.format : formattedWrite, format;

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
        "SemanticNode[] " ~ varName ~ " = (cast(SemanticNode*)alloca((SemanticNode).sizeof * " ~ semanticNodesVarName
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
    /+
    //pragma(msg, "valueCreatorAndGetter(` ~ typeModifier ~ `, ` ~ typeName ~ `)");
    final auto createTypedValue(` ~ typeModifier ~ ` ` ~ typeName ~ ` value) const
    {
        return ` ~ typeModifier ~ ` TypedValue(this, ` ~ typeModifier ~ ` Value(value));
    }
    final auto get(inout(Value) value) const
    {
        return value.` ~ mangleValueFieldName(typeName) ~ `;
    }
    +/
    `;
    //pragma(msg, code);
    mixin(code);
}

interface IIgnorable
{
}

interface IValuePrinterType
{
    void print(StringSink sink, const(Value) value) const;
}

// The minimum interface an object must support to be considered a type
interface IType
{
    bool supports(SemanticNode node);

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
            auto printer = cast(IValuePrinterType)type;
            if (printer)
            {
                printer.print(sink, value);
            }
            else
            {
                sink("[Value of type ");
                sink((cast(Object)type).classinfo.name);
                sink(" that type does not implement IValuePrinterType]");
            }
        }
    }
    return Formatter(type, value);
}

/*
interface IValueToDotQualifiable
{
    @property inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const;
}
*/

/+
// A type that when used as a parameter can consume multiple arguments
interface IVariadicType
{
    SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const;
}
+/

// A type whose values can be used as conditionals for branching
interface IConditionalType
{
    bool isTrue(SemanticNode node) const;
}

interface IEquatableType
{
    bool equals(SemanticNode node, SemanticNode rhs);
}
interface IEquatableTypeNumberType
{
    bool equalsNumber(SemanticNode node, const(char)[] number);
}

interface IRangeType
{
}

bool canBeIgnoredAsReturnValue(const(IType) type)
{
    if (type is VoidType.instance)
        return true;
    // TODO: probably support more scenarios
    return false;
}

class VoidType : TypeType, IIgnorable
{
    mixin singleton;

    //
    // IType methods
    //
    override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }

    //
    // Type methods
    //
    final override void formatter(StringSink sink) const { sink("void"); }

    //@property final override inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const { return null; }
    /+
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
    {
        return SatisfyState.satisfied;
    }
    +/
}
class AnySingleThing : IType
{
    mixin singleton;

    //
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final void formatter(StringSink sink) const { sink("AnySingleThing"); }

    //
    // Type methods
    //
    /+
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
    {
        if ((*offset) < args.length)
        {
            (*offset)++;
            return SatisfyState.satisfied;
        }
        return SatisfyState.notSatisfied;
    }
    +/
}

class PtrType : BuiltinType
{
    IType derefType;
    this(IType derefType)
    {
        this.derefType = derefType;
    }
    //
    // IType methods
    //
    final override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        formattedWrite(sink, "ptr(%s)", derefType.formatName);
    }
}

class VoidPtrType : BuiltinType
{
    mixin singleton;
    //
    // IType methods
    //
    final override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("ptrTo(void)"); }
    //
    // IValuePrinterType methods
    //
    final void print(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
        //sink(get(value).source);
    }
}

class NumberType : BuiltinType, IEquatableType, IEquatableTypeNumberType, IValuePrinterType
{
    //mixin singleton;
    //
    // IValuePrinterType methods
    //
    final void print(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
        //sink(get(value).source);
    }
}

class NumberLiteralType : NumberType
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
    // IEquatableTypeNumber methods
    //
    final bool equalsNumber(SemanticNode node, const(char)[] rhsNumber)
    {
        auto nodeAsNumberLiteral = cast(NumberLiteral)node;
        assert(nodeAsNumberLiteral, "codebug");

        auto lhsNumber = nodeAsNumberLiteral.syntaxNode.source;
        auto result = lhsNumber == rhsNumber;
        from!"std.stdio".writefln("[DEBUG] %s == %s ? %s",
            lhsNumber, rhsNumber, result);
        return result;
    }
    //
    // IEquatableType methods
    //
    final bool equals(SemanticNode node, SemanticNode rhs)
    {
        auto nodeAsNumberLiteral = cast(NumberLiteral)node;
        assert(nodeAsNumberLiteral, "codebug");

        auto rhsType = rhs.getType();
        auto rhsTypeAsEquatableNumber = cast(IEquatableTypeNumberType)rhsType;
        if (!rhsTypeAsEquatableNumber)
            assert(0, format("type '%s' does not implement IEquatableTypeNumber", rhsType));
        return rhsTypeAsEquatableNumber.equalsNumber(rhs, nodeAsNumberLiteral.syntaxNode.source);
    }
    //
    // IType methods
    //
    final override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("NumberLiteral"); }
    //
    // IValuePrinterType methods
    //
    /*
    final void print(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
        //sink(get(value).source);
    }
    */
}
class SymbolType : IType
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
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("symbol"); }

    //
    // Type methods
    //
    /+
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
    {
        if (*offset < args.length && args[*offset].syntaxNode.type == SyntaxNodeType.symbol)
        {
            (*offset)++;
            return SatisfyState.satisfied;
        }
        return SatisfyState.notSatisfied;
    }
    +/
}

class StringLiteralType : BuiltinType, IType, IValuePrinterType
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
    // IType methods
    //
    final override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("string"); }
    //
    // IValuePrinterType methods
    //
    final void print(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
        //sink(get(value).str);
    }
}

class StringType : IType, IValuePrinterType
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
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("string"); }
    //
    // IValuePrinterType methods
    //
    final void print(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
        //sink(get(value).str);
    }
}
class BoolType : BuiltinType, IConditionalType, IEquatableType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("", "bool");

    /*
    @property static TypedValue falseTypedValue()
    {
        return TypedValue(instance, Value(false));
    }
    @property static TypedValue trueTypedValue()
    {
        return TypedValue(instance, Value(true));
    }
    */
    //
    // IConditionalType methods
    //
    final bool isTrue(SemanticNode node) const
    {
        auto asBool = cast(Bool)node;
        assert(asBool, "codebug");
        return asBool.value;
    }
    //
    // IEquatableType methods
    //
    final bool equals(SemanticNode node, SemanticNode rhs)
    {
        assert(0, "not implemented");
    }
    //
    // IType methods
    //
    final override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("bool"); }
}

class TupleLiteralType : IType, IRangeType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "TupleNode*");

    //
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("tuple"); }
}

// TODO: this class could be replaced by TypedNoLimitSetType with
//       the AnySingleThing type
class UntypedNoLimitSetType : IType
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
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("set"); }

    //
    // Type methods
    //
    /+
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
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
    +/
}
class TypedNoLimitSetType : IType
{
    static auto singletonOf(T)()
    {
        static immutable instance = new immutable TypedNoLimitSetType(T.instance);
        return instance;
    }
    IType type;
    this(immutable(IType) type) immutable { this.type = type; }
    mixin valueCreatorAndGetter!("inout", "uarray!SemanticNode");
    /*
    final inout(TypedValue) createTypedValue(inout(SemanticNode)[] semanticNodes) const
    {
        assert(0, "not implemenjted");
    }
    */

    //
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        formattedWrite(sink,"TypedNoLimitset(%s)", type.formatName);
    }

    //
    // Type methods
    //
    /+
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
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
    +/
}

/+
class LimitedSetType : IType
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
    // Type methods
    //
    @property final override inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const
    {
        assert(0, "not implemented");
    }
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
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
class StringSinkType : IType
{
    mixin singleton;

    //
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("StringSink"); }
}

// The "Printable" type is defined as the set of values that have a function called "toString" that print the value.
// toString(x, sink)
class PrintableType : IType
{
    mixin singleton;

    //
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }

    //
    // Type methods
    //
    final override void formatter(StringSink sink) const { sink("Printable"); }
}

class FlagType : IType
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
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("flag"); }
}

// The "Integer" type is an ifinite precision whole number
class IntegerType : NumberType, IType
{
    mixin singleton;

    //mixin valueCreatorAndGetter!("", "string");

    //
    // IEquatableTypeNumber methods
    //
    final bool equalsNumber(SemanticNode node, const(char)[] rhsNumber)
    {
        assert(0, "not implemented");
    }
    //
    // IEquatableType methods
    //
    final bool equals(SemanticNode node, SemanticNode rhs)
    {
        assert(0, "not implemented");
    }
    //
    // IType methods
    //
    final override bool supports(SemanticNode node)
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


class Multi : IType
{
    static auto singletonAnyCountOf(T)()
    {
        static immutable instance = new immutable Multi(T.instance, positiveRangeFrom(0));
        return instance;
    }

    IType type;
    PositiveRange countRange;
    this(immutable(IType) type, PositiveRange countRange) immutable
    {
        this.type = type;
        this.countRange = countRange;
    }

    //
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        formattedWrite(sink,"multi(%s %s)", type.formatName, countRange);
    }

    //
    // Type methods
    //
    /+
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* outOffset) const
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
    +/
}

abstract class ScopeType : IType//, IValueToDotQualifiable
{
    //
    // IType methods
    //
    bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("scope"); }

    //
    // IValueToDotQualifiable methods
    //
    //@property abstract inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const;
}

class ModuleType : ScopeType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "Module");

    //
    // IType methods
    //
    override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("module"); }

    //
    // IValueToDotQualifiable methods
    //
    //@property final override inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const
    //{
    //    return get(value);
    //}
}


// A "TypeType" is a "Type" that can be "hold" another type.
class TypeType : IType//, IValueToDotQualifiable
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "IType");

    //
    // IType methods
    //
    bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("type"); }

    //
    // IValueToDotQualifiable methods
    //
    //@property inout(IDotQualifiable) tryAsIDotQualifiable(inout(Value) value) const
    //{
    //    return cast(inout(IDotQualifiable))get(value);
    //}

    //
    // Type methods
    //
    /+
    override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
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
    +/
}

class TypeTypeTemplate(T) : TypeType
{
    mixin singleton;

    static TypedValue createTypedValue()
    {
        assert(0, "not implemented");
        //return TypedValue(instance, Value());
    }
}



class OptionalType : IType
{
    static auto singletonOf(T)()
    {
        static immutable instance = new immutable OptionalType(T.instance);
        return instance;
    }

    IType type;
    this(immutable(IType) type) immutable { this.type = type; }
    /*
    this(inout(Type) type) inout
    {
        this.type = type;
    }
    */

    //
    // IType methods
    //
    override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        formattedWrite(sink,"optional(%s)", type.formatName);
    }

    //
    // Type methods
    //
    /+
    final override SatisfyState tryConsume(IScope scope_, SemanticNode[] args, ushort* offset) const
    {
        auto result = type.tryConsume(scope_, args, offset);
        if (result == SatisfyState.notSatisfied)
        {
            return SatisfyState.satisfied;
        }
        return result;
    }
    +/
}

class SemanticFunctionType : IType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "SemanticFunction");

    //
    // IType methods
    //
    bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("SemanticFunction"); }
}

class RuntimeFunctionType : IType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "RuntimeFunction");

    //
    // IType methods
    //
    bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    override void formatter(StringSink sink) const { sink("RuntimeFunction"); }
}
class UserDefinedRuntimeFunctionType : RuntimeFunctionType
{
    mixin singleton;

    //mixin valueCreatorAndGetter!("inout", "UserDefinedFunction");

    //
    // IType methods
    //
    override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("UserDefinedRuntimeFunction"); }
}

class RuntimeCallType : IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "RuntimeCall");

    //
    // IType methods
    //
    bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("call"); }
}
class SemanticCallType : IType, IValuePrinterType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "SemanticCall");
    //
    // IType methods
    //
    bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("SemanticCall"); }
    //
    // IValuePrinterType functions
    //
    final void print(StringSink sink, const(Value) value) const
    {
        assert(0, "not implemented");
        /*
        auto node = get(value);
        formattedWrite(sink, "SemanticCall to %s", node.syntaxNode.functionName);
        */
    }
}

class BuiltinRuntimeFunctionType : RuntimeFunctionType
{
    mixin singleton;

    mixin valueCreatorAndGetter!("inout", "BuiltinRuntimeFunction");

    //
    // IType methods
    //
    override bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const { sink("BuiltinRuntimeFunction"); }
}

class EnumType : BuiltinType, IType//, IValuePrinterType
{
    EnumValue[] values;
    this(const(SyntaxNode)* syntaxNode, string[] symbols)
    {
        this.values = new EnumValue[symbols.length];
        foreach (i; 0 .. symbols.length)
        {
            this.values[i] = new EnumValue(syntaxNode, this, symbols[i]);
        }
    }

    mixin valueCreatorAndGetter!("", "string");

    //
    // IType methods
    //
    final override bool supports(SemanticNode node)
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
    final override OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    {
        foreach (value; values)
        {
            if (value.name == symbol)
                return OptionalNodeResult(value);
        }
        return OptionalNodeResult(null);
    }
    override void scopeDescriptionFormatter(StringSink sink) const { sink("enum type"); }
    void dumpSymbols() const
    {
        assert(0, "EnumType.dumpSymbols not impelemented");
    }
    //
    // IValuePrinterType methods
    //
    //final void print(StringSink sink, const(Value) value) const
    //{
    //    sink(get(value));
    //}
}

class StatementBlockType : IType
{
    mixin singleton;
    mixin valueCreatorAndGetter!("inout", "StatementBlock*");
    //
    // IType methods
    //
    final bool supports(SemanticNode node)
    {
        assert(0, "not implemented");
    }
    final override void formatter(StringSink sink) const
    {
        assert(0, "not implemented");
    }
}