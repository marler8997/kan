module analyzer.pass1;

import std.typecons : Flag, Yes, No;

import common : from;
import log;
import syntax : SyntaxNodeType, SyntaxNode;
import semantics;
import builtin : tryGetSemanticFunctionFor;

uint inTreeOrderAnalyzeExpression(IScope scope_, SemanticNode semanticNode)
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
                errorCount += inTreeOrderAnalyzeExpression(scope_, part);
            }
        }
        void visit(SetReturnNode node) { assert(0, "not implemented"); }
        //void visit(Void node) { }
        void visit(Symbol node) { }
        void visit(RegularCall node) { errorCount = inTreeOrderAnalyzeRegularCall(scope_, node); }
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

uint inTreeOrderAnalyzeRegularCall(IScope scope_, RegularCall call)
{
    verbose(2, "analyzeRegularCallPass1 '%s'", call.formatLocation);
    uint errorCount = 0;
    foreach (arg; call.semanticArgs)
    {
        errorCount += inTreeOrderAnalyzeExpression(scope_, arg);
    }
    return errorCount;
}
