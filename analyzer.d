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
import builtin : symbolFunction;

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
SemanticNode tryResolveUnqualified(IReadonlyScope scope_, string unqualifiedSymbol)
{
    for (;;)
    {
        auto result = scope_.tryGetUnqualified(unqualifiedSymbol);
        if (result)
            return result;
        scope_ = scope_.getParent();
        if (!scope_)
            return null;
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
    func.returnType = newSemanticNode(&func.defNodes[argOffset++]);
    if (argOffset >= func.defNodes.length)
        return errorfUint(func.formatLocation(), "function is missing parameters");
    const(TupleSyntaxNode)* paramTuple;
    {
        auto paramSyntaxNode = func.defNodes[argOffset++];
        if (paramSyntaxNode.type != SyntaxNodeType.tuple)
            return errorfUint(func.formatLocation(), "expected a parameter tuple but got '%s'", paramSyntaxNode.type);
        paramTuple = &paramSyntaxNode.tuple;
    }

    {
        auto errorCount = analyzeExpressionPass2(func.containingScope, &func.returnType, AnalyzeOptions.none);
        if (errorCount > 0)
            return errorCount;
    }
    // TODO: make sure that returnType is a type
    from!"std.stdio".writefln("WARNING: check that the function return type is actually a type");
    /+
        auto asTypeType = cast(TypeType)typedValue.type;
        if (!asTypeType)
        {
            from!"std.stdio".writefln("Error: expected a return type but got %s", typedValue.type.formatName);
            throw quit;
        }
    +/
    {
        auto paramTupleSemanticNodes = newSemanticNodes(paramTuple.nodes);
        {
            uint errorCount = 0;
            foreach (node; paramTupleSemanticNodes)
            {
                errorCount += analyzeExpressionPass1(func, node);
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
                    auto errorCount = analyzeExpressionPass2(func, &node, AnalyzeOptions.none);
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
            func.add(result.value.value, param);
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
                errorCount += analyzeExpressionPass1(func, node);
            }
            if (errorCount > 0)
                return errorCount;
        }
        {
            uint totalErrorCount = 0;
            foreach (i; 0 .. func.bodyNodes.length)
            {
                auto lastErrorCount = analyzeExpressionPass2(func, &func.bodyNodes[i], AnalyzeOptions.none);
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

uint analyzeRegularCallPass1(IScope scope_, RegularCall call)
{
    verbose(2, "analyzeRegularCallPass1 '%s'", call.formatLocation);
    uint errorCount = 0;
    foreach (arg; call.arguments)
    {
        errorCount += analyzeExpressionPass1(scope_, arg);
    }
    return errorCount;
}

uint analyzeRegularCallPass2(IReadonlyScope scope_, RegularCall call, AnalyzeOptions analyzeOptions)
{
    verbose(2, "analyzeRegularCallPass2 '%s' analyzeOptions=%s", call.formatLocation, analyzeOptions);
    {
        uint errorCount = 0;
        foreach (i; 0 .. call.arguments.length)
        {
            errorCount += analyzeExpressionPass2(scope_, &call.arguments[i], analyzeOptions);
        }
        if (errorCount > 0)
            return errorCount;
    }

    if (!call.function_)
    {
        assert(call.functionNameToResolve, "codebug");
        verbose(5, "resolving function '%s'", call.functionNameToResolve);
        auto result = analyzeSymbolExpressionPass2(scope_, call.functionNameToResolve, &call.formatLocation, analyzeOptions);
        if (result.errorCount > 0)
            return result.errorCount;

        call.function_ = result.value.tryAs!RegularFunction;
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

uint analyzeSemanticCallPass1(IScope scope_, SemanticCall call)
{
    verbose(2, "analyzeSemanticCallPass1 '%s'", call.formatNameForMessage);
    uint errorCount = 0;
    foreach (arg; call.semanticArgs)
    {
        errorCount += analyzeExpressionPass1(scope_, arg);
    }
    if (errorCount == 0)
    {
        errorCount += call.function_.interpretPass1(scope_, call);
    }
    return errorCount;
}

private NodeResult analyzeSemanticCallPass2(IReadonlyScope scope_, SemanticCall call, AnalyzeOptions analyzeOptions)
{
    verbose(4, "analyzeSemanticCallPass2 %s", call.formatNameForMessage);
    const argumentNodesToAnalyzeCount = call.function_.semanticNodeAnalyzeCountFor(call.syntaxArgs.length);
    if (call.function_.defaultArgType == SemanticArgType.semiAnalyzedSemanticNode)
    {
        analyzeOptions.enablePreventNonFunctionSymbolResolution();
    }
    {
        uint errorCount = 0;
        foreach (i; 0 .. argumentNodesToAnalyzeCount)
        {
            errorCount += analyzeExpressionPass2(scope_, &call.semanticArgs[i], analyzeOptions);
        }
        if (errorCount > 0)
            return NodeResult(errorCount);
    }

    return call.function_.interpretPass2(scope_, call/*, analyzeOptions*/);
}

NodeResult analyzeSymbolExpressionPass2(IReadonlyScope scope_, string symbol,
    scope LocationFormatter delegate() locationFormatter, AnalyzeOptions analyzeOptions)
{
    verbose(1, "analyzeSymbol '%s'", symbol);

    auto restOfSymbol = symbol;
    auto firstPart = peelQualifier(&restOfSymbol);

    auto resultNode = scope_.tryResolveUnqualified(firstPart);
    if (!resultNode)
        return errorfNodeResult(locationFormatter(), "undefined symbol '%s'", firstPart);

    for(;;)
    {
        {
            auto errorCount = analyzeExpressionPass2(scope_, &resultNode, analyzeOptions);
            if (errorCount)
                return NodeResult(errorCount);
        }
        if (restOfSymbol is null)
            return NodeResult(resultNode);
        auto nextId = peelQualifier(&restOfSymbol);
        auto nextNode = resultNode.tryGetUnqualified(nextId);
        if (!nextNode)
            return errorfNodeResult(locationFormatter(),  "%s does not have a member named '%s'",
                resultNode.formatScopeDescription, nextId);
        resultNode = nextNode;
    }
}

uint analyzeExpressionPass1(IScope scope_, SemanticNode semanticNode)
{
    static class Visitor : IHighLevelVisitor
    {
        IScope scope_;
        uint errorCount;
        this(IScope scope_) { this.scope_ = scope_; }
        void visit(Tuple node)
        {
            // TODO: does the tuple have it's own scope?
            foreach (part; node.nodes)
            {
                // !!!! TODO: the tuple may have it's own scope!
                errorCount += analyzeExpressionPass1(scope_, part);
            }
        }
        //void visit(Void node) { }
        void visit(Symbol node) { }
        void visit(RegularCall node) { errorCount = analyzeRegularCallPass1(scope_, node); }
        void visit(SemanticCall node) { errorCount = analyzeSemanticCallPass1(scope_, node); }
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

uint analyzeExpressionPass2(IReadonlyScope scope_, SemanticNode* semanticNodeRef, AnalyzeOptions analyzeOptions)
{
    verbose(3, "analyzeExpression %s, analyzeOptions=%s", (*semanticNodeRef), analyzeOptions);
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        AnalyzeOptions analyzeOptions;
        OptionalNodeResult visitResult;
        this(IReadonlyScope scope_, AnalyzeOptions analyzeOptions)
        { this.scope_ = scope_; this.analyzeOptions = analyzeOptions; }
        //final override void visit(Void node) { }
        final override void visit(Symbol node)
        {
            verbose(0, "[DEBUG] symbol '%s' analyzeOptions=%s", node.value, analyzeOptions);
            if (analyzeOptions.preventNonFunctionSymbolResolution)
                verbose(2, "preventing symbol '%s' resolution", node.value);
            else
                this.visitResult = analyzeSymbolExpressionPass2(scope_, node.value, &node.formatLocation, analyzeOptions);
        }
        final override void visit(RegularCall node)
        {
            visitResult = OptionalNodeResult(analyzeRegularCallPass2(scope_, node, analyzeOptions));
        }
        final override void visit(SemanticCall node)
        {
            visitResult = analyzeSemanticCallPass2(scope_, node, analyzeOptions);
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
        scope visitor = new Visitor(scope_, analyzeOptions);
        verbose(4, "analyzeExpression %s", (*semanticNodeRef));
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

ResultOrError!Symbol tryAnalyzeToSymbolPass1(SemanticNode node)
{
    static class Visitor : HighLevelVisitorIgnoreByDefault
    {
        OptionalResultOrError!Symbol visitResult;
        final override void visit(Symbol node) { this.visitResult = ResultOrError!Symbol(node); }
        final override void visit(SemanticCall node)
        {
            // only builtin functions that are available in pass 1 may be used here
            // for now, the only function that support this is the symbol function
            if (node.function_ is symbolFunction.instance)
            {
                auto result = symbolFunction.interpretPass2(node);
                if (result.value)
                    this.visitResult = OptionalResultOrError!Symbol(result.value);
                else
                {
                    assert(result.errorCount > 0, "codebug");
                    this.visitResult = OptionalResultOrError!Symbol(result.errorCount);
                }
            }
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