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

/*
The following functions use specific words to mean certain behavior.
---------------------------------------------------------------------
try (return a null TypedValue if not found)
get (search for the symbol in this object only, don't check parent scopes)
resolve (search for the symbol in this scope and any parent scopes)
unqualified (the symbol has no '.', it is the full symbol)
qualified (the symbol may or may not have multiple parts)
*/
struct NodeAndScope
{
    IReadonlyScope scope_;
    SemanticNode node;

    pragma(inline) static NodeAndScope nullValue()
    {
        NodeAndScope value = void;
        value.scope_ = null;
        return value;
    }
    pragma(inline) bool isNull() const { return scope_ is null; }
}
OptionalResultOrError!NodeAndScope tryResolveUnqualified(IReadonlyScope scope_, string unqualifiedSymbol)
{
    for (;;)
    {
        auto result = scope_.tryGetUnqualified(unqualifiedSymbol, Yes.fromInside);
        if (result.errorCount > 0)
            return OptionalResultOrError!NodeAndScope(result.errorCount);
        if (result.value)
            return OptionalResultOrError!NodeAndScope(NodeAndScope(scope_, result.value));
        scope_ = scope_.getParent();
        if (!scope_)
            return OptionalResultOrError!NodeAndScope(NodeAndScope.nullValue);
    }
}




NodeResult resolveQualified(IReadonlyScope scope_, string qualified,
    scope LocationFormatter delegate() locationFormatter)
{
    verbose(1, "resolveQualified '%s'", qualified);

    auto restOfSymbol = qualified;
    auto firstPart = peelQualifier(&restOfSymbol);

    auto firstPartResolveResult = scope_.tryResolveUnqualified(firstPart);
    if (firstPartResolveResult.errorCount > 0)
        return NodeResult(firstPartResolveResult.errorCount);
    auto firstPartNodeAndScope = firstPartResolveResult.value;
    if (firstPartNodeAndScope.isNull)
        return errorfNodeResult(locationFormatter(), "undefined symbol '%s'", firstPart);

    auto nextNode = firstPartNodeAndScope.node;
    for(;;)
    {
        if (restOfSymbol is null)
            return NodeResult(nextNode);
        {
            auto errorCount = analyzeForMemberAccess(&nextNode);
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
        nextNode = nextNodeResult.value;
    }
}

/+
// Find the symbol in the current or a parent scope, do not stop search if a scope
// is unfinished.
// Returns TypedValue.nullValue if no entry is found
const(TypedValue) tryResolveUnqualifiedIgnoreUnfinishedScopes(IReadonlyScope scope_, string unqualifiedSymbol)
{
    for (;;)
    {
        auto result = scope_.tryGetUnqualified(unqualifiedSymbol);
        if (result.state == ResolveResultEnum.haveEntry)
            return result.entry;
        scope_ = scope_.getParent();
        if (!scope_)
            return TypedValue.nullValue;
    }
}
+/

/+
SemanticNode tryResolveQualified(IReadonlyScope scope_, string qualifiedSymbol)
{
    verbose(1, "tryResolveQualified '%s'", qualifiedSymbol);

    string restOfSymbol = qualifiedSymbol;
    auto firstPart = peelQualifier(&restOfSymbol);

    auto firstPartResult = scope_.tryResolveUnqualified(firstPart);
    if (!firstPartResult || restOfSymbol is null)
        return firstPartResult;

    auto firstPartQualifiable = firstPartResult.tryAsIDotQualifiable();
    if (firstPartQualifiable is null)
    {
        /+
        from!"std.stdio".writefln("%sError: cannot access member '%s' from an object of type ?, it doesn't have any dotted members",
            location, restOfSymbol/*, firstPartResult.entry.type.formatName*/);
        throw quit;
        +/
        return null;
    }

    // TODO: maybe add reportErrors to tryGetQualified
    auto secondResult = tryGetQualified(cast()firstPartQualifiable, restOfSymbol);
    //verbose(1, "tryResolveQualified '%s' > %s", qualifiedSymbol, secondResult.state);
    return secondResult;
}
+/

uint analyzeParamPass2(const(SyntaxNode)* paramSyntax, SemanticNode* param)
{
    return errorfUint(formatLocation(paramSyntax.source.ptr), "analyzeParam not implemented");
}

uint analyzeUserDefinedFunctionPass2(UserDefinedFunction func)
{
    // TODO: need to make sure that all parent nodes have been analyzed
    {
        auto errorCount = func.containingScope.prepareForChildAnalyzePass2();
        if (errorCount > 0)
            return errorCount;
    }

    //
    // Analyze function modifiers
    //
        /+

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

        +/
    uint argOffset = 0;
    for (;; argOffset++)
    {
        if (argOffset >= func.defNodes.length)
            return errorfUint(func.formatLocation(), "function is missing returnType and parameters");
        // Check for certain function flags
        break;
    }

    //
    // Analyze returnType/parameters
    //
    auto returnTypeNode = newSemanticNode(&func.defNodes[argOffset++]);
    if (argOffset >= func.defNodes.length)
        return errorfUint(func.formatLocation(), "function is missing parameters");
    const(TupleSyntaxNode)* paramTuple;
    {
        auto paramSyntaxNode = func.defNodes[argOffset++];
        if (paramSyntaxNode.type != SyntaxNodeType.tuple)
            return errorfUint(func.formatLocation(), "expected a parameter tuple but got '%s'", paramSyntaxNode.type);
        paramTuple = &paramSyntaxNode.tuple;
    }

    func.returnType = analyzeToType(func.containingScope, returnTypeNode);
    if (!func.returnType)
        return errorfUint(returnTypeNode.formatLocation(), "expected the return type, but got '%s'", returnTypeNode);

    {
        auto paramTupleSemanticNodes = newSemanticNodes(paramTuple.nodes);
        {
            uint errorCount = 0;
            foreach (node; paramTupleSemanticNodes)
            {
                errorCount += inTreeOrderAnalyzeExpressionPass1(func, node);
            }
            if (errorCount > 0)
                return errorCount;
        }

        auto maxParams = paramTuple.nodes.length / 2; // every parameter needs at least a name/type
        auto paramBuffer = new FunctionParameter[paramTuple.nodes.length / 2];
        uint paramCount = 0;
        for (size_t tupleIndex = 0; ;)
        {
            if (tupleIndex >= paramTuple.nodes.length)
                break;

            SemanticNode type;
            for (;;)
            {
                auto node = paramTupleSemanticNodes[tupleIndex++];
                {
                    auto errorCount = inTreeOrderAnalyzeExpressionPass2(func, &node, AnalyzeOptions.none);
                    if (errorCount > 0)
                        return errorCount;
                }
                // TODO: check if it is some type of modifier
                if (type is null)
                {
                    // TODO: check that it is actually a type
                    type = node;
                    break;
                }
            }
            if (tupleIndex >= paramTuple.nodes.length)
                return errorfUint(paramTuple.base.formatLocation, "last param type is missing a name");

            auto nameNode = paramTupleSemanticNodes[tupleIndex++];
            auto result = tryAnalyzeToSymbolPass1(nameNode);
            if (!result.value)
                return errorfUint(nameNode.formatLocation, "expected symbol but got '%s'", nameNode);
            auto param = new FunctionParameter(nameNode.getSyntaxNode, func, paramCount, result.value.value, type);
            paramBuffer[paramCount++] = param;
            {
                auto entry = func.tryAddOrPrintError(result.value.value, param, result.value.formatLocation);
                if (!entry)
                    return 1;
            }
        }
        func.params = paramBuffer[0 .. paramCount].toUarray;
    }

    //
    // Analyze function body
    //
    if (argOffset < func.defNodes.length)
    {
        const(TupleSyntaxNode)* bodyTuple;
        {
            auto bodySyntaxNode = func.defNodes[argOffset++];
            if (argOffset != func.defNodes.length)
                return errorfUint(formatLocation(&func.defNodes[argOffset]),
                    "too many arguments to 'function'");
            if (bodySyntaxNode.type != SyntaxNodeType.tuple)
                return errorfUint(func.formatLocation(), "expected body of function to be a tuple but got '%s'", bodySyntaxNode.type);
            bodyTuple = &bodySyntaxNode.tuple;
        }

        func.bodyNodes = newSemanticNodes(bodyTuple.nodes);
        {
            uint errorCount = 0;
            foreach (node; func.bodyNodes)
            {
                errorCount += inTreeOrderAnalyzeExpressionPass1(func, node);
            }
            if (errorCount > 0)
                return errorCount;
        }
        {
            uint totalErrorCount = 0;
            foreach (i; 0 .. func.bodyNodes.length)
            {
                auto lastErrorCount = inTreeOrderAnalyzeExpressionPass2(func, &func.bodyNodes[i], AnalyzeOptions.none);
                if (lastErrorCount == 0)
                    totalErrorCount += analyzer.enforceValidStatement(func, func.bodyNodes[i]);
                else
                    totalErrorCount += lastErrorCount;
            }
            if (totalErrorCount > 0)
                return totalErrorCount;
        }
    }
    return 0;
}

uint inTreeOrderAnalyzeRegularCallPass1(IScope scope_, RegularCall call)
{
    verbose(2, "analyzeRegularCallPass1 '%s'", call.formatLocation);
    uint errorCount = 0;
    foreach (arg; call.semanticArgs)
    {
        errorCount += inTreeOrderAnalyzeExpressionPass1(scope_, arg);
    }
    return errorCount;
}

uint inTreeOrderAnalyzeRegularCallPass2(IReadonlyScope scope_, RegularCall call, AnalyzeOptions analyzeOptions)
{
    verbose(2, "analyzeRegularCallPass2 '%s' analyzeOptions=%s", call.formatLocation, analyzeOptions);
    {
        uint errorCount = 0;
        foreach (i; 0 .. call.semanticArgs.length)
        {
            errorCount += inTreeOrderAnalyzeExpressionPass2(scope_, &call.semanticArgs[i], analyzeOptions);
        }
        if (errorCount > 0)
            return errorCount;
    }

    if (!call.function_)
    {
        assert(call.functionNameToResolve, "codebug");
        verbose(5, "resolving function '%s'", call.functionNameToResolve);

        //auto result = inTreeOrderAnalyzeSymbolExpressionPass2(scope_, call.functionNameToResolve, &call.formatLocation, analyzeOptions);
        auto result = resolveQualified(scope_, call.functionNameToResolve, &call.formatLocation);
        if (result.errorCount > 0)
            return result.errorCount;

        //call.function_ = result.value.tryAs!RegularFunction;
        call.function_ = resolveForRegularFunctionCall(scope_, result.value);
        if (!call.function_)
        {
            from!"std.stdio".writefln("Error: symbol '%s' is not a function", call.functionNameToResolve);
            return 1;
        }
    }
    if (call.function_.addAndCheckCall(call).failed)
        return 1; // error

    return 0;
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

uint inTreeOrderAnalyzeExpressionPass1(IScope scope_, SemanticNode semanticNode)
{
    static class Visitor : IHighLevelVisitor
    {
        IScope scope_;
        uint errorCount;
        this(IScope scope_) { this.scope_ = scope_; }
        void visit(SymbolTableEntry node)
        {
            assert(0, "not implemented");
        }
        void visit(Tuple node)
        {
            // TODO: does the tuple have it's own scope?
            foreach (part; node.nodes)
            {
                // !!!! TODO: the tuple may have it's own scope!
                errorCount += inTreeOrderAnalyzeExpressionPass1(scope_, part);
            }
        }
        void visit(SetReturnNode node) { assert(0, "not implemented"); }
        //void visit(Void node) { }
        void visit(Symbol node) { }
        void visit(RegularCall node) { errorCount = inTreeOrderAnalyzeRegularCallPass1(scope_, node); }
        void visit(SemanticCall node) { errorCount = node.inTreeOrderInterpretPass1(scope_); }
        void visit(BuiltinType node) { }
        void visit(SemanticFunction node) { }
        void visit(RegularFunction node) { }
        void visit(Value node) { }
        void visit(FunctionParameter node) { assert(0, "codebug, should be no FunctionParameter on pass1"); }
        void visit(LazyNode node) { assert(0, "codebug, should be no LazyNodes on pass1"); }
    }
    scope visitor = new Visitor(scope_);
    semanticNode.accept(visitor);
    return visitor.errorCount;
}

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

uint analyzeForMemberAccess(SemanticNode* semanticNodeRef)
{
    verbose(3, "analyzeForMemberAccess %s", (*semanticNodeRef));
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        //IReadonlyScope scope_;
        //AnalyzeOptions analyzeOptions;
        OptionalNodeResult visitResult;
        this(/*IReadonlyScope scope_, AnalyzeOptions analyzeOptions*/)
        { /*this.scope_ = scope_; this.analyzeOptions = analyzeOptions;*/ }
        final override void visit(SymbolTableEntry node)
        {
            visitResult.errorCount = analyzeForMemberAccess(&node.currentNode);
        }
        /*
        final override void visit(Symbol node)
        {
            //verbose(0, "[DEBUG] symbol '%s' analyzeOptions=%s", node.value, analyzeOptions);
            if (analyzeOptions.preventNonFunctionSymbolResolution)
                verbose(2, "preventing symbol '%s' resolution", node.value);
            else
                this.visitResult = inTreeOrderAnalyzeSymbolExpressionPass2(scope_, node.value, &node.formatLocation, analyzeOptions);
        }
        final override void visit(RegularCall node)
        {
            visitResult = OptionalNodeResult(inTreeOrderAnalyzeRegularCallPass2(scope_, node, analyzeOptions));
        }
        final override void visit(SemanticCall node)
        {
            visitResult = inTreeOrderAnalyzeSemanticCallPass2(scope_, node, analyzeOptions);
            assert(visitResult.value || visitResult.errorCount > 0, "codebug, analyzeSemantic call must return a node or errors");
        }
        final override void visit(BuiltinType node) { }
        final override void visit(RegularFunction node) { }
        */
        final override void visit(Value node) { }
        //final override void visit(FunctionParameter node) { }
        final override void visit(LazyNode node) { this.visitResult = node.tryEvaluate(); }
    }
    for (;;)
    {
        scope visitor = new Visitor(/*scope_,analyzeOptions */);
        verbose(4, "symbolRefAnalyze %s", (*semanticNodeRef));
        (*semanticNodeRef).accept(visitor);
        if (visitor.visitResult.errorCount > 0 || visitor.visitResult.value is null)
            return visitor.visitResult.errorCount;
        *semanticNodeRef = visitor.visitResult.value;
    }
}
RegularFunction resolveForRegularFunctionCall(IReadonlyScope scope_, SemanticNode node)
{
    verbose(3, "resolveForRegularFunctionCall %s", node);
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        RegularFunction funcResult;
        this(IReadonlyScope scope_)
        { this.scope_ = scope_; }
        final override void visit(SymbolTableEntry node)
        {
            auto result = resolveForRegularFunctionCall(scope_, node.currentNode);
            if (result)
            {
                node.currentNode = result; // update the symbol table entry as well
                this.funcResult = result;
            }
        }
        /*
        final override void visit(Symbol node)
        {
            //verbose(0, "[DEBUG] symbol '%s' analyzeOptions=%s", node.value, analyzeOptions);
            if (analyzeOptions.preventNonFunctionSymbolResolution)
                verbose(2, "preventing symbol '%s' resolution", node.value);
            else
                this.visitResult = inTreeOrderAnalyzeSymbolExpressionPass2(scope_, node.value, &node.formatLocation, analyzeOptions);
        }
        final override void visit(RegularCall node)
        {
            visitResult = OptionalNodeResult(inTreeOrderAnalyzeRegularCallPass2(scope_, node, analyzeOptions));
        }
        */
        final override void visit(SemanticCall node)
        {
            //errorf(node.formatLocation, "semantic call not impl: %s", node.getFunctionName);
            this.funcResult = node.interpretToRegularFunction(node.formatLocation).value;
        }
        //final override void visit(BuiltinType node) { }
        final override void visit(RegularFunction node)
        {
            this.funcResult = node;
        }
        //final override void visit(Value node) { }
        //final override void visit(FunctionParameter node) { }
        //final override void visit(LazyNode node) { this.visitResult = node.tryEvaluate(); }
    }
    scope visitor = new Visitor(scope_);
    node.accept(visitor);
    return visitor.funcResult;
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

OptionalResultOrError!Symbol tryAnalyzeToSymbolPass1(const(SyntaxNode)* node)
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
OptionalResultOrError!Symbol tryAnalyzeToSymbolPass1(SemanticNode node)
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

// Returns: null on error
IType analyzeToType(IReadonlyScope scope_, SemanticNode node)
{
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        IType returnType;
        this(IReadonlyScope scope_) { this.scope_ = scope_; }
        final override void visit(Value node)
        {
            this.returnType = node.tryAsType();
        }
    }
    scope visitor = new Visitor(scope_);
    node.accept(visitor);
    return visitor.returnType;
}