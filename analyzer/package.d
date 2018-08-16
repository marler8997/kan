module analyzer;

import std.typecons : Flag, Yes, No;
import std.format : format;

import more.alloc : GCDoubler;
import more.builder : Builder;
import more.format : StringSink;

import common : from, toUarray, quit;
import log;
import syntax : SyntaxNodeType, SyntaxNode, TupleSyntaxNode, KeywordType;
import types;
import semantics;// : peelQualifier, IDotQualifiable, SymbolEntryDirect, SemanticNode, Type;
import mod : Module;
import builtin : tryGetSemanticFunctionFor, symbolCall;



/*
Analyze Contexts
-------------------------------------
#### Statement Context

Example: RegularCall
will cause the function that is called to be analyzed, the call will be added
to the function

Example: SymbolTableEntry
not sure what to do here, I think if you just have a symbol table entry in a
statement context then it might depend on what the actual symbol table entry is.

#### Expression Context

#### Regular Function Call Context

The node is being called as a regular function, so it should be analyzed as a function that will be called.


*/














/**
inTreeOrder* means that the node is being analyzed in the order it appears in the abstract syntax tree.
In this context, you can assume that the parent is either done with or in the same stage of analysis.
You can also assume that this function will only ever be called once for each particular node, there's
no need to cache whether or not the inTreeOrder function has already been called.
*/



// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// TODO: might add "statementContext" to AnalyzeOptions instead of having a separate
//       enforceValidStatement function
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
struct AnalyzeOptions
{
    static AnalyzeOptions none()
    {
        return AnalyzeOptions(cast(Flags)0);
    }
    static AnalyzeOptions makePreventNonFunctionSymbolResolution()
    {
        return AnalyzeOptions(Flags.preventNonFunctionSymbolResolution);
    }

    private enum Flags : ubyte
    {
        preventNonFunctionSymbolResolution = 0b0001,
    }
    Flags flags;

    bool preventNonFunctionSymbolResolution() const
    { return (flags & Flags.preventNonFunctionSymbolResolution) != 0; }
    void enablePreventNonFunctionSymbolResolution()
    { this.flags |= Flags.preventNonFunctionSymbolResolution; }

    void toString(StringSink sink)
    {
        string prefix = "";
        if (preventNonFunctionSymbolResolution)
        {
            sink(prefix);
            prefix = ",";
            sink("preventSymbolRes");
        }
        if (prefix.length == 0)
        {
            sink("none");
        }
    }
}

OptionalResultOrError!Symbol trySemanticCallToSymbol(SemanticCall call)
{
    // only builtin functions that are available in pass 1 may be used here
    // for now, the only function that support this is the symbol function
    {
        auto symbolCall = cast(symbolCall)call;
        if (symbolCall)
        {
            return symbolCall.inTreeOrderInterpretPass2();
        }
    }
    return OptionalResultOrError!Symbol(null);
}
OptionalResultOrError!Symbol tryAnalyzeToSymbol(const(SyntaxNode)* node)
{
    if (node.type == SyntaxNodeType.symbol || node.type == SyntaxNodeType.keyword)
        return OptionalResultOrError!Symbol(new SymbolFromSyntax(node));
    if (node.type == SyntaxNodeType.call)
    {
        auto call = tryGetSemanticFunctionFor(&node.call);
        if (call !is null)
            return trySemanticCallToSymbol(call);
    }
    return OptionalResultOrError!Symbol(null);
}
OptionalResultOrError!Symbol tryAnalyzeToSymbol(SemanticNode node)
{
    static class Visitor : HighLevelVisitorIgnoreByDefault
    {
        OptionalResultOrError!Symbol visitResult;
        final override void visit(Symbol node) { this.visitResult = ResultOrError!Symbol(node); }
        final override void visit(SemanticCall node)
        {
            this.visitResult = trySemanticCallToSymbol(node);
        }
    }
    scope visitor = new Visitor();
    node.accept(visitor);
    if (visitor.visitResult.value)
        return ResultOrError!Symbol(visitor.visitResult.value);
    if (visitor.visitResult.errorCount == 0)
        return ResultOrError!Symbol(1);
    return ResultOrError!Symbol(visitor.visitResult.errorCount);
}

uint analyzeParamPass2(const(SyntaxNode)* paramSyntax, SemanticNode* param)
{
    return errorfUint(formatLocation(paramSyntax.source.ptr), "analyzeParam not implemented");
}


    /+
uint inTreeOrderAnalyzeSemanticCallPass1(IScope scope_, SemanticCall call)
{
    verbose(2, "analyzeSemanticCallPass1 '%s'", call.formatNameForMessage);
    {
        uint errorCount = 0;
        foreach (arg; call.semanticArgs)
        {
            errorCount += inTreeOrderAnalyzeExpressionPass1(scope_, arg);
        }
        if (errorCount > 0)
            return errorCount;
    }
    return call.function_.inTreeOrderInterpretPass1(scope_, call);
}

private NodeResult inTreeOrderAnalyzeSemanticCallPass2(IReadonlyScope scope_, SemanticCall call, AnalyzeOptions analyzeOptions)
{
    verbose(4, "analyzeSemanticCallPass2 %s", call.formatNameForMessage);
    {
        uint errorCount = 0;
        foreach (arg; call.function_.semanticNodeRange(call.syntaxArgs.length))
        {
            if (arg.fullyAnalyzed)
                analyzeOptions.enablePreventNonFunctionSymbolResolution();
            else
                analyzeOptions.enablePreventNonFunctionSymbolResolution();
            errorCount += inTreeOrderAnalyzeExpressionPass2(scope_, &call.semanticArgs[arg.semanticNodeIndex], analyzeOptions);

            //this.semanticArgs[arg.semanticNodeIndex] = newSemanticNode(&syntaxArgs[arg.syntaxNodeIndex]);
            //initializedCount++;
        }
        if (errorCount > 0)
            return NodeResult(errorCount);
    }
    return call.function_.inTreeOrderInterpretPass2(scope_, call/*, analyzeOptions*/);


    /+
    const argumentNodesToAnalyzeCount = call.function_.semanticNodeAnalyzeCountFor(call.syntaxArgs.length);
    if (call.function_.defaultArgType == SemanticArgType.semiAnalyzedSemanticNode)
    {
        analyzeOptions.enablePreventNonFunctionSymbolResolution();
    }
    {
        uint errorCount = 0;
        foreach (i; 0 .. argumentNodesToAnalyzeCount)
        {
            errorCount += inTreeOrderAnalyzeExpressionPass2(scope_, &call.semanticArgs[i], analyzeOptions);
        }
        if (errorCount > 0)
            return NodeResult(errorCount);
    }

    return call.function_.inTreeOrderInterpretPass2(scope_, call/*, analyzeOptions*/);
    +/
}
    +/

/+


NodeResult inTreeOrderAnalyzeSymbolExpressionPass2(IReadonlyScope scope_, string symbol,
    scope LocationFormatter delegate() locationFormatter, AnalyzeOptions analyzeOptions)
{
    verbose(1, "analyzeSymbolPass2 '%s'", symbol);

    auto restOfSymbol = symbol;
    auto firstPart = peelQualifier(&restOfSymbol);

    auto firstPartResolveResult = scope_.tryResolveUnqualified(firstPart);
    if (firstPartResolveResult.errorCount > 0)
        return NodeResult(firstPartResolveResult.errorCount);
    auto firstPartNodeAndScope = firstPartResolveResult.value;
    if (firstPartNodeAndScope.isNull)
        return errorfNodeResult(locationFormatter(), "undefined symbol '%s'", firstPart);

    //auto nextScope = firstPartNodeAndScope.scope_;
    auto nextNode = firstPartNodeAndScope.node;
    for(;;)
    {
        if (restOfSymbol is null)
            return NodeResult(nextNode);
        {
            auto errorCount = symbolRefAnalyzePass2(/*nextScope, */&nextNode, analyzeOptions);
            if (errorCount)
                return NodeResult(errorCount);
        }
        auto nextId = peelQualifier(&restOfSymbol);
        auto nextNodeResult = nextNode.tryGetUnqualified(nextId, No.fromInside);
        if (nextNodeResult.errorCount > 0)
            return NodeResult(nextNodeResult.errorCount);
        if (!nextNodeResult.value)
            return errorfNodeResult(locationFormatter(),  "%s does not have a member named '%s'",
                nextNode.formatScopeDescription, nextId);
        /*
        {
            auto asScope = cast(IReadonlyScope)nextNode;
            if (asScope)
                nextScope = asScope;
        }
        */
        nextNode = nextNodeResult.value;
    }
}
+/


uint analyzeSymbolTableEntryPass2(SymbolTableEntry entry)
{
    if (entry.state == SymbolTableEntry.State.pass2Done)
        return 0;
    if (entry.state == SymbolTableEntry.State.failed)
        return 1;
    if (entry.state == SymbolTableEntry.State.pass2Started)
        assert(0, "not sure what to do here");

    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        SymbolTableEntry entry;
        uint errorCount;
        this(SymbolTableEntry entry) { this.entry = entry; }
        final override void visit(Symbol node)
        {
            /*
            !!!
            auto result = resolveQualified(scope_, node.value, &node.formatLocation);
            if (result.errorCount > 0)
            {
                this.visitResult = OptionalNodeResult(result.errorCount);
                return;
            }
            // TODO: maybe analyze it more?  Not sure how this node is being used though, so
            //       we don't want to do too much
            this.visitResult = OptionalNodeResult(result.value);
            */
            assert(0, "not impl");
        }
        final override void visit(RegularCall node) { }
        final override void visit(Value node) { }
        final override void visit(RegularFunction) { }
        final override void visit(LazyNode node)
        {
            auto result = node.tryEvaluate();
            if (result.errorCount > 0)
            {
                this.errorCount = result.errorCount;
                return;
            }
            assert(result.value, "codebug");
            entry.currentNode = result.value;
        }
    }
    for (;;)
    {
        auto nodeBeforeAnalyzed = entry.currentNode;
        scope visitor = new Visitor(entry);
        //from!"std.stdio".writefln("[DEBUG] analyzeSymbolTableEntryPass2 %s", entry);
        entry.currentNode.accept(visitor);
        if (visitor.errorCount > 0)
            return visitor.errorCount;
        if (entry.currentNode == nodeBeforeAnalyzed)
            return 0;
    }
}

/+
uint inTreeOrderAnalyzeExpressionPass2(IReadonlyScope scope_, SemanticNode* semanticNodeRef, AnalyzeOptions analyzeOptions)
{
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        AnalyzeOptions analyzeOptions;
        OptionalNodeResult visitResult;
        this(IReadonlyScope scope_, AnalyzeOptions analyzeOptions)
        { this.scope_ = scope_; this.analyzeOptions = analyzeOptions; }
        final override void visit(SymbolTableEntry node)
        {
            this.visitResult = OptionalNodeResult(analyzeSymbolTableEntryPass2(node));
        }
        final override void visit(SetReturnNode node)
        {
            //this.visitResult = OptionalNodeResult(analyzeSymbolTableEntryPass2(node.set.symbolTableEntry));
        }
        //final override void visit(Void node) { }
        final override void visit(Symbol node)
        {
            //verbose(0, "[DEBUG] symbol '%s' analyzeOptions=%s", node.value, analyzeOptions);
            if (analyzeOptions.preventNonFunctionSymbolResolution)
                verbose(2, "preventing symbol '%s' resolution", node.value);
            else
            {
                //this.visitResult = inTreeOrderAnalyzeSymbolExpressionPass2(scope_, node.value, &node.formatLocation, analyzeOptions);
                auto result = resolveQualified(scope_, node.value, &node.formatLocation);
                if (result.errorCount > 0)
                {
                    this.visitResult = OptionalNodeResult(result.errorCount);
                    return;
                }
                // TODO: maybe analyze it more?  Not sure how this node is being used though, so
                //       we don't want to do too much
                this.visitResult = OptionalNodeResult(result.value);
            }
        }
        final override void visit(RegularCall node)
        {
            visitResult = OptionalNodeResult(inTreeOrderAnalyzeRegularCallPass2(scope_, node, analyzeOptions));
        }
        final override void visit(SemanticCall node)
        {
            //visitResult = inTreeOrderAnalyzeSemanticCallPass2(scope_, node, analyzeOptions);
            visitResult = node.inTreeOrderInterpretPass2(scope_);
            assert(visitResult.value || visitResult.errorCount > 0, "codebug, analyzeSemantic call must return a node or errors");
        }
        final override void visit(BuiltinType node) { }
        final override void visit(RegularFunction node) { }
        final override void visit(Value node) { }
        //final override void visit(FunctionParameter node) { }
        final override void visit(LazyNode node) { this.visitResult = node.tryEvaluate(); }
    }
    for (;;)
    {
        verbose(3, "analyzeExpressionPass2 %s, analyzeOptions=%s", (*semanticNodeRef), analyzeOptions);
        scope visitor = new Visitor(scope_, analyzeOptions);
        (*semanticNodeRef).accept(visitor);
        if (visitor.visitResult.errorCount > 0 || visitor.visitResult.value is null)
            return visitor.visitResult.errorCount;
        *semanticNodeRef = visitor.visitResult.value;
    }
}
+/

uint enforceValidStatement(IReadonlyScope scope_, SemanticNode semanticNode)
{
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        uint errorCount;
        this(IReadonlyScope scope_) { this.scope_ = scope_; }
        final override void visit(SymbolTableEntry node)
        {
            /**
            TODO: we want to error in this case, however, some functions like set(...)
                  currently return a symbol table result which ARE valid statements.
                  I may need to modify their return object to wrap the SymbolTableEntry
                  with an object that is ignorable.
            */
            this.errorCount = errorfUint(node.formatLocation, "symbol table entries are currently not valid statements");
        }
        final override void visit(SetReturnNode node) { }
        //final override void visit(Symbol node) { /* probably ignore on pass 1*/ }
        final override void visit(RegularCall node)
        {
            assert(node.function_, "codebug");
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            // TODO: UNCOMMENT THIS
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            /+
            if (!node.function_.interface_.returnType.canBeIgnoredAsReturnValue)
            {
                from!"std.stdio".writefln("%sError: function return value of type %s is being ignored",
                    node.function_.formatLocation(), node.function_.interface_.returnType.formatName);
                errorCount = 1;
            }
            +/
        }
        final override void visit(SemanticCall node)
        {
            assert(0, "codebug? Shouldn't the semantic call have been replaced?");
        }
        //final override void visit(BuiltinType node) { /* probably ignore on pass 1*/ }
        //final override void visit(RegularFunction node) { /* probably ignore on pass 1*/ }
        final override void visit(Value node)
        {
            auto type = node.getType();
            if (cast(IIgnorable)type is null)
                errorCount = errorfUint(node.formatLocation,
                    "value of type '%s' is currently being ignored, but it isn't ignorable", type.formatName);
        }
    }
    scope visitor = new Visitor(scope_);
    semanticNode.accept(visitor);
    return visitor.errorCount;
}
