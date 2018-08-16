module analyzer.pass2;

import std.typecons : Flag, Yes, No;

import common : from, uarray, toUarray;
import log;
import syntax : SyntaxNodeType, SyntaxNode, TupleSyntaxNode, KeywordType;
import semantics;
import types : IType;
import mod : Module;
import analyzer : AnalyzeOptions, tryAnalyzeToSymbol;
import pass1 = analyzer.pass1;

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
        /*
        from!"std.stdio".writefln("%sError: cannot access member '%s' from an object of type ?, it doesn't have any dotted members",
            location, restOfSymbol/*, firstPartResult.entry.type.formatName*/);
        throw quit;
        */
        return null;
    }

    // TODO: maybe add reportErrors to tryGetQualified
    auto secondResult = tryGetQualified(cast()firstPartQualifiable, restOfSymbol);
    //verbose(1, "tryResolveQualified '%s' > %s", qualifiedSymbol, secondResult.state);
    return secondResult;
}
+/

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
        final override void visit(SemanticCall node)
        {
            //errorf(node.formatLocation, "semantic call not impl: %s", node.getFunctionName);
            this.funcResult = node.interpretToRegularFunction(node.formatLocation).value;
        }
        final override void visit(RegularFunction node)
        {
            this.funcResult = node;
        }
    }
    scope visitor = new Visitor(scope_);
    node.accept(visitor);
    return visitor.funcResult;
}

// Returns: null on error
IType analyzeToType(IReadonlyScope scope_, SemanticNode node)
{
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        IType returnType;
        this(IReadonlyScope scope_) { this.scope_ = scope_; }
        final override void visit(SymbolTableEntry node)
        {
            auto errorCount = analyzeStatement(scope_, &node.currentNode);
            if (errorCount > 0)
                return;
            this.returnType = analyzeToType(scope_, node.currentNode);
        }
        final override void visit(Symbol node)
        {
            auto result = resolveQualified(scope_, node.value, &node.formatLocation);
            if (result.errorCount > 0)
                return;

            assert(result.value, "codebug: NodeResult should have a value or an error");
            this.returnType = analyzeToType(scope_, result.value);
        }
        final override void visit(Value node)
        {
            this.returnType = node.tryAsType();
        }
        final override void visit(BuiltinType node)
        {
            this.returnType = node;
        }
    }
    scope visitor = new Visitor(scope_);
    node.accept(visitor);
    return visitor.returnType;
}

// Analayze a top-level node of a module or a function
uint analyzeStatement(IReadonlyScope scope_, SemanticNode* semanticNodeRef)
{
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        OptionalNodeResult visitResult;
        bool madeAChange; // TODO:
        this(IReadonlyScope scope_)
        { this.scope_ = scope_; }
        final override void visit(SymbolTableEntry node)
        {
            this.visitResult = OptionalNodeResult(analyzeStatement(scope_, &node.currentNode));
        }
        final override void visit(SetReturnNode node)
        {
            //this.visitResult = OptionalNodeResult(analyzeSymbolTableEntryPass2(node.set.symbolTableEntry));
        }
        //final override void visit(Void node) { }
        final override void visit(Symbol node)
        {
            //this.visitResult = errorfOptionalNodeResult(node.formatLocation, "I don't think a Symbol on it's own is a valid statement");
            /+
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
            +/
        }
        final override void visit(RegularCall node)
        {
            visitResult = OptionalNodeResult(analyzeRegularCall(scope_, node));
        }
        final override void visit(SemanticCall node)
        {
            visitResult = node.inTreeOrderInterpretPass2(scope_);
            assert(visitResult.value || visitResult.errorCount > 0, "codebug, analyzeSemantic call must return a node or errors");
        }
        final override void visit(BuiltinType node) { }
        final override void visit(RegularFunction node) { }
        final override void visit(Value node) { }
        final override void visit(LazyNode node) { this.visitResult = node.tryEvaluate(); }
    }
    for (;;)
    {
        verbose(3, "analyzeStatement %s", (*semanticNodeRef));
        scope visitor = new Visitor(scope_);
        (*semanticNodeRef).accept(visitor);
        if (visitor.visitResult.errorCount > 0)
            return visitor.visitResult.errorCount;
        if (visitor.visitResult.value is null)
        {
            // TODO: check visitor.madeAChange
            return 0;
        }
        *semanticNodeRef = visitor.visitResult.value;
    }
}

uint analyzeRegularFunctionArgument(IReadonlyScope scope_, SemanticNode* semanticNodeRef)
{
    static class Visitor : HighLevelVisitorNotImplementedByDefault
    {
        IReadonlyScope scope_;
        OptionalNodeResult visitResult;
        this(IReadonlyScope scope_)
        { this.scope_ = scope_; }
        final override void visit(SymbolTableEntry node)
        {
            this.visitResult = OptionalNodeResult(analyzeRegularFunctionArgument(scope_, &node.currentNode));
        }
        final override void visit(Value node) { }
        final override void visit(Symbol node)
        {
            auto result = resolveQualified(scope_, node.value, &node.formatLocation);
            if (result.errorCount > 0)
            {
                this.visitResult = OptionalNodeResult(result.errorCount);
                return;
            }
            assert(result.value, "codebug: NodeResult should have a value or an error");
            this.visitResult = OptionalNodeResult(result.value);
        }
        final override void visit(RegularCall node)
        {
            visitResult = OptionalNodeResult(analyzeRegularCall(scope_, node));
        }
        final override void visit(SemanticCall node)
        {
            visitResult = node.inTreeOrderInterpretPass2(scope_);
            assert(visitResult.value || visitResult.errorCount > 0, "codebug, analyzeSemantic call must return a node or errors");
        }
        final override void visit(BuiltinType node) { }
    }
    for (;;)
    {
        verbose(3, "analyzeRegularFunctionArgument %s", (*semanticNodeRef));
        scope visitor = new Visitor(scope_);
        (*semanticNodeRef).accept(visitor);
        if (visitor.visitResult.errorCount > 0 || visitor.visitResult.value is null)
            return visitor.visitResult.errorCount;
        *semanticNodeRef = visitor.visitResult.value;
    }
}

uint analyzeRegularCall(IReadonlyScope scope_, RegularCall call)
{
    verbose(2, "pass2: analyzeRegularCall '%s'", call.formatLocation);
    {
        uint errorCount = 0;
        foreach (i; 0 .. call.semanticArgs.length)
        {
            errorCount += analyzeRegularFunctionArgument(scope_, &call.semanticArgs[i]);
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

uint analyzeUserDefinedFunction(UserDefinedFunction func)
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
                errorCount += pass1.inTreeOrderAnalyzeExpression(func, node);
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
                    auto errorCount = analyzeRegularFunctionArgument(func, &node);
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
            auto result = tryAnalyzeToSymbol(nameNode);
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

    if (argOffset >= func.defNodes.length)
        return errorfUint(func.formatLocation(), "function is missing {body} or extern(...)");

    auto nextSyntaxNode = &func.defNodes[argOffset++];
    if (nextSyntaxNode.type == SyntaxNodeType.tuple)
    {
        func.isExternFunction = No.isExternFunction;
        const(TupleSyntaxNode)* bodyTuple = &nextSyntaxNode.tuple;

        func.bodyNodes = newSemanticNodes(bodyTuple.nodes);
        {
            uint errorCount = 0;
            foreach (node; func.bodyNodes)
            {
                errorCount += pass1.inTreeOrderAnalyzeExpression(func, node);
            }
            if (errorCount > 0)
                return errorCount;
        }
        {
            uint errorCount = 0;
            foreach (i; 0 .. func.bodyNodes.length)
            {
                errorCount += analyzeStatement(func, &func.bodyNodes[i]);
            }
            if (errorCount > 0)
                return errorCount;
        }
    }
    else
    {
        auto externNode = newSemanticNode(nextSyntaxNode);
        {
            scope thunkedScope = new ReadonlyScopeThunk(func.containingScope);
            auto errorCount = pass1.inTreeOrderAnalyzeExpression(thunkedScope, externNode);
            if (errorCount > 0)
                return errorCount;
        }
        {
            auto errorCount = analyzeRegularFunctionArgument(func.containingScope, &externNode);
            if (errorCount > 0)
                return errorCount;
        }
        auto externProps = cast(ExternProperties)externNode;
        if (externProps is null)
            return errorfUint(externNode.formatLocation, "expected extern(...) but got '%s'", externNode);

        func.isExternFunction = Yes.isExternFunction;
        func.externName = externProps.funcName;
    }

    if (argOffset < func.defNodes.length)
        return errorfUint(func.formatLocation, "too many for function definition");

    return 0;
}
