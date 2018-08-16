module interpreter;

import std.array : Appender;
import std.format : format;

import more.alloc : GCDoubler;
import more.builder : Builder;

import common : from, uarray, toUarray, quit;
import log;
import syntax : SyntaxNodeType, SyntaxNode, CallSyntaxNode;
import semantics/* : IHighLevelVisitor, IScope, SemanticNodeType, SemanticNode, RegularCall,
                   BuiltinRegularFunction, UserDefinedFunction,
                   BlockFlags*/;
import types : IType, VoidType;

struct RuntimeBlock
{
    RuntimeBlock *parentBlock;
    uarray!SemanticNode args; // the arguments if it is a function
    uarray!SemanticNode statements;
    size_t nextStatement;
    SemanticNode returnValue;
}

struct Interpreter
{
    RuntimeBlock *currentBlock;
    SemanticNode interpretFunctionOrModule(const(IType) returnType, uarray!SemanticNode args, uarray!SemanticNode statements)
    {
        auto newBlock = RuntimeBlock(currentBlock, args, statements, 0, null);
        this.currentBlock = &newBlock;
        scope(exit) this.currentBlock = newBlock.parentBlock;

        for(;;)
        {
            if (newBlock.returnValue)
                return newBlock.returnValue;
            auto statementIndex = newBlock.nextStatement;
            if (statementIndex >= newBlock.statements.length)
            {
                if (returnType is VoidType.instance)
                    return null;
                assert(0, "code bug? reached end of function or module without a return value being set, maybe this is OK in some cases?");
            }
            newBlock.nextStatement++;
            interpretStatement(statements[statementIndex]);
        }
    }
    private SemanticNode interpretStatement(SemanticNode node)
    {
        verbose(3, "interpretStatement '%s'", node);
        static class Visitor : HighLevelVisitorNotImplementedByDefault
        {
            Interpreter* interpreter;
            SemanticNode returnValue;
            this(Interpreter* interpreter) { this.interpreter = interpreter; }
            final override void visit(SymbolTableEntry node) { node.currentNode.accept(this); }
            final override void visit(SetReturnNode node) { }
            final override void visit(RegularCall node)
            {
                this.returnValue = interpreter.interpretRegularCall(node);
            }
            final override void visit(Value) { /* just ignore value*/ }
        }
        scope visitor = new Visitor(&this);
        node.accept(visitor);
        if (visitor.returnValue)
        {
            // TODO: verify that the return value is ignorable!!!
            return visitor.returnValue;
        }
        return null;
    }
    private SemanticNode interpretExpression(SemanticNode node)
    {
        verbose(3, "interpretExpression '%s'", node);
        static class Visitor : HighLevelVisitorNotImplementedByDefault
        {
            Interpreter* interpreter;
            SemanticNode returnValue;
            this(Interpreter* interpreter) { this.interpreter = interpreter; }
            final override void visit(SymbolTableEntry node)
            {
                this.returnValue = node.currentNode;
                node.currentNode.accept(this);
            }
            final override void visit(Value node) { this.returnValue = node; }
            final override void visit(RegularCall node)
            {
                this.returnValue = interpreter.interpretRegularCall(node);
            }
        }
        scope visitor = new Visitor(&this);
        node.accept(visitor);
        return visitor.returnValue;
    }
    private SemanticNode interpretRegularCall(RegularCall call)
    {
        // interpret arguments
        auto runtimeArgs = new SemanticNode[call.semanticArgs.length];
        foreach (argIndex; 0 .. call.semanticArgs.length)
        {
            auto arg = interpretExpression(call.semanticArgs[argIndex]);
            assert(arg, format("function argument expression returned no value"));
            runtimeArgs[argIndex] = arg;
        }

        //from!"std.stdio".writefln("[DEBUG] interpret runtime funtion '%s'", call.runtimeCall.syntaxNode.functionName);
        auto asBuiltin = cast(BuiltinRegularFunction)call.function_;
        if (asBuiltin)
        {
            return asBuiltin.interpret(&this, call, runtimeArgs.toUarray);
        }
        else
        {
            auto userDefined = cast(UserDefinedFunction)call.function_;
            assert(userDefined, "codebug: expected BuiltinRegularFunction or UserDefinedFunction");

            // Setup the stack!
            // Need to map the call arguments to the function parameters
            if (runtimeArgs.length != userDefined.params.length)
            {
                from!"std.stdio".writefln("call argument count %s != function argument count %s, not implemented",
                    runtimeArgs.length, userDefined.params.length);
                throw quit;
            }
            auto returnType = cast(IType)userDefined.returnType;
            if (!returnType)
            {
                assert(0, format(
                    "a user-defined return type is not a type, it is '%s', this should have been caught during semantic analysis",
                    userDefined.returnType));
            }

            if (!userDefined.isExternFunction)
                return interpretFunctionOrModule(returnType, runtimeArgs.toUarray, userDefined.bodyNodes);

            auto result = from!"funcload".loadExternFunc(userDefined);
            // TODO: I have a function pointer, now how do I call it?
            //       I think I'll need libffi
            assert(0, "I have the function pointer...now how do I call it?");
        }
    }
}
