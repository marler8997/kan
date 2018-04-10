module analyzer;

import std.typecons : Flag, Yes, No;
import std.format : format;

import common : from, quit;
import log;
import syntax : SyntaxNodeType, SyntaxNode, KeywordType;
import types;
import semantics;// : peelQualifier, IDotQualifiable, SymbolEntryDirect, SemanticNode, Type;
import mod : Module;

// Used as a template parameter for functions to execute in 2 different modes.
// It executes normaly when using ReportErrors.no, but when it is ReportErrors.yes
// it means semantic analysis has failed and any unresolved analysis should be
// considered errors and reported.
enum ReportErrors {no, yes}

alias NotReporting = ReportErrors.no;
alias Reporting    = ReportErrors.yes;

/**
Resolves the return type to `T` if reportErrors is false, otherwise, resovles to void
*/
template ReportReturn(ReportErrors reportErrors, T)
{
    static if (reportErrors == ReportErrors.yes)
        alias ReportReturn = uint;
    else
        alias ReportReturn = T;
}

enum AnalyzeState : ubyte
{
    notAnalyzed,
    error,
    // The node has added any symbols it could add to the containing scope
    // but it not fully analyzed
    addedSymbols,
    analyzed,
}
bool greaterThan(AnalyzeState left, AnalyzeState right)
{
    return left > right;
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

ResolveResult tryResolveUnqualified(IReadonlyScope scope_, string unqualifiedSymbol)
{
    for (;;)
    {
        auto result = scope_.tryGetUnqualified(unqualifiedSymbol);
        if (result.state != ResolveResultEnum.noEntryAndAllSymbolsAdded)
        {
            return result;
        }
        scope_ = scope_.getParent();
        if (!scope_)
        {
            return ResolveResult.noEntryAndAllSymbolsAdded;
        }
    }
}
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

ReportReturn!(reportErrors, ResolveResult) tryResolveQualified(ReportErrors reportErrors = NotReporting)
    (IReadonlyScope scope_, string qualifiedSymbol)
{
    verbose(1, "tryResolveQualified '%s'", qualifiedSymbol);

    string restOfSymbol = qualifiedSymbol;
    auto firstPart = peelQualifier(&restOfSymbol);

    auto firstPartResult = scope_.tryResolveUnqualified(firstPart);
    if (firstPartResult.state != ResolveResultEnum.haveEntry)
    {
        verbose(1, "tryResolveQualified '%s' > %s", qualifiedSymbol, firstPartResult.state);
        static if (reportErrors)
        {
            if (firstPartResult.state == ResolveResultEnum.noEntryButMoreSymbolsCouldBeAdded)
            {
                // We may not want to print an error here because if more symbols could be added
                // then there should be an error somewhere else

                // check if any parent scopes have the symbol for better analytics
                auto found = scope_.tryResolveUnqualifiedIgnoreUnfinishedScopes(firstPart);
                if (found.isNull)
                {
                    from!"std.stdio".writefln("%sError: undefined symbol '%s'",
                        scope_.getModule().formatLocation(firstPart), firstPart);
                }
                else
                {
                    from!"std.stdio".writefln("%sError: symbol '%s' was found in a parent scope, but inner scope(s) were unfinished",
                        scope_.getModule().formatLocation(firstPart), firstPart);
                }
            }
            else
            {
                from!"std.stdio".writefln("%sError: undefined symbol '%s'",
                    scope_.getModule().formatLocation(firstPart), firstPart);
            }
            return 1;
        }
        else
            return firstPartResult;
    }
    if (restOfSymbol is null)
    {
        verbose(1, "tryResolveQualified '%s' > haveEntry", qualifiedSymbol);
        static if (reportErrors)
            return 0; // no error
        else
            return firstPartResult;
    }

    auto firstPartQualifiable = firstPartResult.entry.tryAsIDotQualifiable();
    if (firstPartQualifiable is null)
    {
        from!"std.stdio".writefln("%sError: cannot access member '%s' from an object of type %s, it doesn't have any dotted members",
            scope_.getModule().formatLocation(restOfSymbol), restOfSymbol, firstPartResult.entry.type.formatName);
        throw quit;
    }

    // TODO: maybe add reportErrors to tryGetQualified
    auto secondResult = tryGetQualified(cast()firstPartQualifiable, restOfSymbol);
    verbose(1, "tryResolveQualified '%s' > %s", qualifiedSymbol, secondResult.state);
    static if (!reportErrors)
        return secondResult;
    else
    {
        if (secondResult.state == ResolveResultEnum.haveEntry)
            return 0; // no error
        if (secondResult.state == ResolveResultEnum.noEntryButMoreSymbolsCouldBeAdded)
        {
            // We probably don't print an error here because if more symbols could be added
            // then there should be an error somewhere else
        }
        else
        {
            from!"std.stdio".writefln("Error: scope '%s' does not contain the qualified symbol '%s'",
                firstPartQualifiable, restOfSymbol);
        }
        return 1;
    }
}
alias tryResolveQualifiedSymbol = tryResolveQualified;

ReportReturn!(reportErrors, void) analyzeRuntimeCall(ReportErrors reportErrors = NotReporting)
    (IReadonlyScope scope_, RuntimeCall* call/*, Flag!"used" used*/)
{
    enum CodeBugInvalidStateMessage = "code bug: this function should not be called in this state";
    static if (!reportErrors)
    {
        static string changeAndGotoRuntimeCallAnalyzeState(RuntimeCallAnalyzeState state)
        {
            auto stateString = from!"std.conv".to!string(state);
            return `
                {
                    static assert(!RuntimeCallAnalyzeState.` ~ stateString ~ `.isDone, "code bug: ` ~ stateString ~ `");
                    call.analyzeState = RuntimeCallAnalyzeState.` ~ stateString ~ `;
                    goto case RuntimeCallAnalyzeState.` ~ stateString ~ `;
                }
        `;
        }
        static string setDoneAnalyzeStateAndReturn(RuntimeCallAnalyzeState state)
        {
            auto stateString = from!"std.conv".to!string(state);
            return `
                {
                    static assert(RuntimeCallAnalyzeState.` ~ stateString ~ `.isDone, "code bug: ` ~ stateString ~ `");
                    call.analyzeState = RuntimeCallAnalyzeState.` ~ stateString ~ `;
                    return;
                }
        `;
        }

        enum finishAndReturnIfCurrentValueIsFunction = q{
            {
                auto asRuntimeFunctionType = cast(RuntimeFunctionType)call.currentFunctionValue.type;
                if (asRuntimeFunctionType)
                {
                    call.function_ = cast()asRuntimeFunctionType.get(call.currentFunctionValue.value);
                    call.function_.addUsedCall(call);
                    mixin(setDoneAnalyzeStateAndReturn(RuntimeCallAnalyzeState.done));
                }
            }
        };
    }

    final switch(call.analyzeState)
    {
    case RuntimeCallAnalyzeState.analyzeArguments:
        enum analyzeArgumentValueCode = q{
            analyzeValue!reportErrors(scope_, &call.arguments[call.analyzeArgumentsIndex], Yes.resolveSymbols/*, used*/)
        };
        static if (reportErrors)
        {
            //from!"std.stdio".writefln("[DEBUG] ReportErrors: analyzeRuntimeCall argument %s not analyzed", call.analyzeArgumentsIndex);
            assert(call.analyzeArgumentsIndex < call.arguments.length, "code bug");
            auto result = mixin(analyzeArgumentValueCode);
            assert(result, "code bug: runtime argument could not be analyzed but did not report any errors");
            return result;
        }
        else
        {
            //from!"std.stdio".writefln("[DEBUG] analyzeRuntimeCall.runtimeCall analyzeArguments...", call.syntaxNode.functionName);
            // analyze arguments
            for (; call.analyzeArgumentsIndex < call.arguments.length;
                call.analyzeArgumentsIndex++)
            {
                if (!mixin(analyzeArgumentValueCode))
                {
                    return; // not analyzed because call.analyzeState is not done
                }
            }
            if (false/*!used*/)
            {
                mixin(setDoneAnalyzeStateAndReturn(RuntimeCallAnalyzeState.done));
            }
            // There are cases where the function is already resolved (i.e. call(function(...))))
            if (!call.currentFunctionValue.isNull)
            {
                mixin(changeAndGotoRuntimeCallAnalyzeState(RuntimeCallAnalyzeState.analyzeFunctionValue));
            }
            mixin(changeAndGotoRuntimeCallAnalyzeState(RuntimeCallAnalyzeState.resolveFunctionSymbol));
        }
    case RuntimeCallAnalyzeState.resolveFunctionSymbol:
        //assert(used);
        //from!"std.stdio".writefln("[DEBUG] resolving function '%s'", call.syntaxNode.functionName);
        auto symbolResult = tryResolveQualified!reportErrors(scope_, call.syntaxNode.functionName);
        static if (reportErrors)
        {
            //from!"std.stdio".writefln("[DEBUG] ReportErrors: tryResolveQualified '%s' result = %s", call.syntaxNode.functionName, symbolResult);
            return symbolResult;
        }
        else
        {
            //from!"std.stdio".writefln("[DEBUG] analyzeRuntimeCall.runtimeCall resolveFunctionSymbol...", call.syntaxNode.functionName);
            if (symbolResult.state != ResolveResultEnum.haveEntry)
            {
                if (symbolResult.state == ResolveResultEnum.noEntryAndAllSymbolsAdded)
                {
                    from!"std.stdio".writefln("Error: could not resolve function '%s'", call.syntaxNode.functionName);
                    throw quit;
                }
                verbose(1, "could not resolve function '%s' yet", call.syntaxNode.functionName);
                return; // not analyzed because call.analyzeState is not done
            }
            call.currentFunctionValue = symbolResult.entry;

            mixin(finishAndReturnIfCurrentValueIsFunction);
            mixin(changeAndGotoRuntimeCallAnalyzeState(RuntimeCallAnalyzeState.analyzeFunctionValue));
        }
    case RuntimeCallAnalyzeState.analyzeFunctionValue:
        static if (reportErrors)
        {
            assert("not implemented or invalid code path");
        }
        else
        {
            //from!"std.stdio".writefln("[DEBUG] analyzeRuntimeCall.runtimeCall analyzeFunctionValue...", call.syntaxNode.functionName);
            for (;;)
            {
                //auto result = tryAnalyzeReturnValue(scope_, call.currentFunctionValue);
                //if (!result.analyzed)
                //{
                //    return; // not analyzed because call.analyzeState is not done
                //}
                mixin(finishAndReturnIfCurrentValueIsFunction);
                from!"std.stdio".writefln("Error: symbol '%s' is not a function, it's type is %s",
                    call.syntaxNode.functionName, call.currentFunctionValue.type.formatName);
                throw quit;
            }
        }
        assert(0);
    case RuntimeCallAnalyzeState.done:
        assert(0, CodeBugInvalidStateMessage);
    }
}

// Note: after a successful call, make sure to analyze the returnValue (call.returnValue)
private ReportReturn!(reportErrors, AnalyzeState) analyzeSemanticCall(ReportErrors reportErrors = NotReporting)
    (IReadonlyScope scope_, SemanticCall* call/*, Flag!"used" used*/)
{
    auto argumentNodesToAnalyzeCount = call.function_.semanticNodeAnalyzeCountFor(call.syntaxNode.arguments.length);
    if (call.analyzeArgumentsIndex < argumentNodesToAnalyzeCount)
    {
        bool argumentCouldAddSymbols = false;
        for (; call.analyzeArgumentsIndex < call.arguments.length; call.analyzeArgumentsIndex++)
        {
            auto result = analyzeValue!reportErrors(scope_, &call.arguments[call.analyzeArgumentsIndex], No.resolveSymbols/*, used*/);
            static if (reportErrors)
            {
                assert(result != 0, "code bug");
                return result;
            }
            else
            {
                // TODO: check if the argument could add symbols
                if (!result)
                {
                    // TODO: this can make the logic for configurable by replacing
                    //     call.function_.canAddSymbols
                    //WITH
                    //     call.function_.checkCallCanAddSymbols(call)
                    return (argumentCouldAddSymbols || call.function_.canAddSymbols) ?
                        AnalyzeState.notAnalyzed : AnalyzeState.addedSymbols;
                }
            }
        }
    }

    if (call.returnValue is null)
    {
        auto result = call.function_.interpret(scope_, call/*, used*/);
        final switch(result.state)
        {
        case ResolveResultEnum.haveEntry:
            call.returnValue = result.entry;
            break;
        case ResolveResultEnum.noEntryAndAllSymbolsAdded:
            from!"std.stdio".writefln("Error: analyze-time-semantic function '%s' is referencing undefined symbol(s)",
                call.syntaxNode.functionName);
            throw quit;
        case ResolveResultEnum.noEntryButMoreSymbolsCouldBeAdded:
            // TODO: this can make the logic for configurable by replacing
            //     call.function_.canAddSymbols
            //WITH
            //     call.function_.checkCallCanAddSymbols(call)
            return call.function_.canAddSymbols ? AnalyzeState.notAnalyzed : AnalyzeState.addedSymbols;
        }
    }
    static if (reportErrors)
        return 0; // no error
    else
        return AnalyzeState.analyzed;
}

// A special "pre-analysis" of a function argument, does not
// perform variable resolution since the actual call may take raw symbols.
ReportReturn!(reportErrors, Flag!"analyzed") analyzeValue(ReportErrors reportErrors = NotReporting)
    (IReadonlyScope scope_, SemanticNode* semanticNode, Flag!"resolveSymbols" resolveSymbols/*, Flag!"used" used*/)
{
    final switch(semanticNode.nodeType)
    {
    case SemanticNodeType.typedValue:
        static if (reportErrors)
            return 0; // no error
        else
            return Yes.analyzed; // do nothing for now
    case SemanticNodeType.tuple:
        static if (reportErrors)
            return analyzeValue!Reporting(scope_, &semanticNode.tuple.elements[semanticNode.tuple.analyzeElementsIndex], resolveSymbols/*, used*/);
        else
        {
            // analyze arguments
            for (; semanticNode.tuple.analyzeElementsIndex < semanticNode.tuple.elements.length;
                semanticNode.tuple.analyzeElementsIndex++)
            {
                if (!analyzeValue(scope_, &semanticNode.tuple.elements[semanticNode.tuple.analyzeElementsIndex], resolveSymbols/*, used*/))
                {
                    return No.analyzed;
                }
            }
            return Yes.analyzed;
        }
    case SemanticNodeType.symbol:

        if (!resolveSymbols)
        {
            static if (reportErrors)
                return 0;
            else
                return Yes.analyzed;
        }
        {
            auto symbol = semanticNode.symbol.symbolString;
            auto result = tryResolveQualifiedSymbol!reportErrors(scope_, symbol);
            static if (reportErrors)
            {
                return result;
            }
            else
            {
                final switch(result.state)
                {
                case ResolveResultEnum.haveEntry:
                    semanticNode.symbol.resolved = result.entry;
                    return Yes.analyzed;
                case ResolveResultEnum.noEntryAndAllSymbolsAdded:
                    from!"std.stdio".writefln("Error: undefined symbol '%s'", symbol);
                    throw quit;
                case ResolveResultEnum.noEntryButMoreSymbolsCouldBeAdded:
                    return No.analyzed;
                }
            }
        }
    case SemanticNodeType.semanticCall:
        {
            auto result = analyzeSemanticCall!reportErrors(scope_, &semanticNode.semanticCall/*, used*/);
            static if (reportErrors)
            {
                if (result)
                    return result;
            }
            else
            {
                if (result != AnalyzeState.analyzed)
                    return No.analyzed;
            }
            return analyzeValue!reportErrors(scope_, semanticNode.semanticCall.returnValue, resolveSymbols/*, used*/);
        }
    case SemanticNodeType.runtimeCall:
        enum analyzeCallCode = q{
            analyzeRuntimeCall!reportErrors(scope_, &semanticNode.runtimeCall/*, used*/)
        };
        static if (reportErrors)
        {
            return mixin(analyzeCallCode);
        }
        else
        {
            mixin(analyzeCallCode ~ ";");
            if (semanticNode.runtimeCall.analyzeState.isDone)
            {
                return Yes.analyzed;
            }
            return No.analyzed;
        }
    case SemanticNodeType.statementBlock:
        assert(0, "analyzeValue statementBlock not implemented");
    case SemanticNodeType.jump:
        assert(0, "analyzeValue jump not implemented");
    }
}

// Analyze a semantic node as a "statement".  This differs from analyzeValue which would typically
// be analyzing a node for its return value for something like a function argument.  This
// funcion analyzes a node to perform some operation.
ReportReturn!(reportErrors, AnalyzeState) analyzeStatementNode(ReportErrors reportErrors = NotReporting)
    (IScope scope_, SemanticNode* semanticNode)
{
    final switch(semanticNode.nodeType)
    {
    case SemanticNodeType.typedValue:
        static if (reportErrors)
            assert(0, "code bug");
        else
        {
            // TODO: throw semantic error if this node doesn't "do anything", i.e.
            //       if it's just a number or string or other kind of value function
            if (!semanticNode.typedValue.asTypedValue.isVoid)
            {
                from!"std.stdio".writefln("Error: return value of '%s' is ignored", *semanticNode);
                throw quit;
            }
            return AnalyzeState.analyzed;
        }
    case SemanticNodeType.tuple:
        assert(0, "analyzeStatementNode for tuples not implemented");
    case SemanticNodeType.symbol:
        from!"std.stdio".writefln("Error: lone symbol '%s' is not a valid statement", semanticNode.syntaxNode.source);
        throw quit;
    case SemanticNodeType.semanticCall:
        static if (reportErrors)
        {
            //from!"std.stdio".writefln("function '%s'", semanticNode.semanticCall.syntaxNode.functionName);
            assert(0, "analyzeStatementNode!Reporting for semanticCall not implemented");
        }
        else
        {
            auto analyzeState = analyzeSemanticCall(scope_, &semanticNode.semanticCall/*, module_.rootCodeIsUsed ? Yes.used : No.used*/);
            if (analyzeState == AnalyzeState.analyzed)
            {
                return analyzeStatementNode(scope_, semanticNode.semanticCall.returnValue);
            }
            return analyzeState;
        }
    case SemanticNodeType.runtimeCall:
        {
            enum analyzeCallCode = q{
                analyzeRuntimeCall!reportErrors(scope_, &semanticNode.runtimeCall/*, module_.rootCodeIsUsed ? Yes.used : No.used*/)
            };
            static if (reportErrors)
            {
                return mixin(analyzeCallCode);
            }
            else
            {
                mixin(analyzeCallCode ~ ";");
                if (semanticNode.runtimeCall.analyzeState.isDone)
                {
                    return AnalyzeState.analyzed;
                }
                return AnalyzeState.addedSymbols; // runtime functions DO NOT add symbols
            }
        }
    case SemanticNodeType.statementBlock:
        static if (reportErrors)
        {
            return analyzeStatementNode!reportErrors(semanticNode.statementBlock.scope_,
                &semanticNode.statementBlock.statements[semanticNode.statementBlock.analyzeStatementsIndex]);
        }
        else
        {
            for (; semanticNode.statementBlock.analyzeStatementsIndex < semanticNode.statementBlock.statements.length;
                semanticNode.statementBlock.analyzeStatementsIndex++)
            {
                auto result = analyzeStatementNode(semanticNode.statementBlock.scope_,
                    &semanticNode.statementBlock.statements[semanticNode.statementBlock.analyzeStatementsIndex]);
                if (result != AnalyzeState.analyzed)
                {
                    return result;
                }
            }
            return AnalyzeState.analyzed;
        }
    case SemanticNodeType.jump:
        if (null is scope_.tryGetJumpBlock())
        {
            from!"std.stdio".writefln("%sError: jumps must be inside jump blocks",
                scope_.getModule().formatLocation(semanticNode.syntaxNode.source));
            return AnalyzeState.error;
        }
        static if (reportErrors)
        {
            return analyzeStatementNode!reportErrors(scope_, semanticNode.jump.condition);
        }
        else
        {
            auto result = analyzeStatementNode!reportErrors(scope_, semanticNode.jump.condition);
            if (result == AnalyzeState.analyzed)
            {
                auto condition = semanticNode.jump.condition.getAnalyzedTypedValue(Yes.resolveSymbols);
                auto asConditionalType = cast(IConditionalType)condition.type;
                if (asConditionalType is null)
                {
                    from!"std.stdio".writefln("%sError: jumps require conditional value but got %s",
                        scope_.getModule().formatLocation(semanticNode.syntaxNode.source), condition.type.formatName);
                    return AnalyzeState.error;
                }
            }
            return result;
        }
    }
}


Flag!"analyzed" analyzeFunctionBody(UserDefinedFunction function_)
{
    for (; function_.codeAnalyzedCount < function_.analyzedCode.length;
          function_.codeAnalyzedCount++)
    {
        if (!analyzeStatementNode(function_.containingScope,
            &function_.analyzedCode[function_.codeAnalyzedCount]))
        {
            return No.analyzed;
        }
    }
    return Yes.analyzed;
}

/+
void analyzeStatementBlock(IScope scope_, SemanticNode[] semanticNodes,
    AnalyzeState[] semanticNodeAnalyzeStates, ref size_t nodesAddedSymbols, ref size_t nodesAnalyzed)
{
ANALYZE_GLOBAL_NODES_LOOP:
    while (nodesAnalyzed < semanticNodes.length)
    {
        auto nodesAnalyzedBeforePass = nodesAnalyzed;
        foreach (i, ref node; semanticNodes)
        {
            if (semanticNodeAnalyzeStates[i] != AnalyzeState.analyzed)
            {
                auto oldState = semanticNodeAnalyzeStates[i];
                auto newState = analyzeStatementNode(scope_, &node);
                // double check that nothing else modified the current state
                assert(oldState == semanticNodeAnalyzeStates[i], "code bug");
                if (newState != oldState)
                {
                    assert(newState.greaterThan(oldState), "code bug");
                    semanticNodeAnalyzeStates[i] = newState;
                    // NOTE: this logic is currently based on AnalyzeState having
                    //       3 values: notAnalyzed, addedSymbols and analyzed
                    if (oldState < AnalyzeState.addedSymbols)
                    {
                        assert(newState >= AnalyzeState.addedSymbols, "code bug");
                        nodesAddedSymbols++;
                    }
                    if (newState == AnalyzeState.analyzed)
                    {
                        nodesAnalyzed++;
                        if (nodesAnalyzed >= semanticNodes.length)
                        {
                            break ANALYZE_GLOBAL_NODES_LOOP;
                        }
                    }
                }
            }
        }

        // if nothing new was analyzed
        if (nodesAnalyzed == nodesAnalyzedBeforePass)
        {
            foreach (i, ref node; semanticNodes)
            {
                if (semanticNodeAnalyzeStates[i] != AnalyzeState.analyzed)
                {
                    auto result = analyzer.analyzeStatementNode!Reporting(scope_, &node);
                    assert(result > 0, "code bug: analyzeStatementNode failed but did not report any errors");
                }
            }
            throw quit;
        }
    }
}
+/