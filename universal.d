import std.typecons : Flag, Yes, No;

import more.format : StringSink;

import common : unconst, singleton;
import semantics : SemanticNode, IReadonlyScope, IScope, OptionalNodeResult, JumpBlock;
import mod : Module;

struct BuiltinSymbol
{
    string symbol;
    SemanticNode node;

    static import builtin;
    import types;
    __gshared static immutable values = [
        immutable BuiltinSymbol("u8", UnsignedFixedWidthType!8.instance),
        immutable BuiltinSymbol("u32", UnsignedFixedWidthType!32.instance),
        immutable BuiltinSymbol("assert", builtin.assertFunction.instance),
        immutable BuiltinSymbol("equals", builtin.equalsFunction.instance),
        immutable BuiltinSymbol("leftIsLess", builtin.leftIsLessFunction.instance),
        immutable BuiltinSymbol("length", builtin.lengthFunction.instance),
        immutable BuiltinSymbol("alloca", builtin.allocaFunction.instance),
        immutable BuiltinSymbol("ptrTo", builtin.ptrToFunction.instance),
    ];
}

class UniversalScope : IReadonlyScope
{
    mixin singleton;
    //
    // IDotQualifiable methods
    //
    OptionalNodeResult tryGetUnqualified(string symbol, Flag!"fromInside" fromInside)
    {
        //from!"std.stdio".writefln("Universal.tryGetUnqualified(\"%s\")", symbol);
        foreach (ref builtinSymbol; BuiltinSymbol.values)
        {
            if (builtinSymbol.symbol == symbol)
                return OptionalNodeResult(builtinSymbol.node.unconst);
        }
        return OptionalNodeResult(null);
    }
    void scopeDescriptionFormatter(StringSink sink) const { sink("global scope"); }
    void dumpSymbols() const
    {
        assert(0, "UniversalScope.dumpSymbols not impelemented");
    }
    //
    // IReadonlyScope methods
    //
    @property final inout(IReadonlyScope) getParent() inout { return null; }
    // TODO: probably return a pseudo universal-module
    @property final inout(Module) asModule() inout { assert(0, "not implemented"); }
    @property final inout(IScope) asWriteable() inout { return null; }
    @property final inout(JumpBlock) asJumpBlock() inout { return null; }
    final uint prepareForChildAnalyzePass2() { return 0; }
}