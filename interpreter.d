module interpreter;

import std.array : Appender;
import std.format : format;

import more.alloc : GCDoubler;
import more.builder : Builder;

import common : from, uarray, toUarray, quit;

import syntax : SyntaxNodeType, SyntaxNode, CallSyntaxNode;
import semantics/* : IHighLevelVisitor, IScope, SemanticNodeType, SemanticNode, RegularCall,
                   BuiltinRegularFunction, UserDefinedFunction,
                   BlockFlags*/;

/*
union InterpretTimeParamValue
{
    SemanticNode node;
}
*/

struct CodeBlockPosition
{
    static CodeBlockPosition nullValue() { return CodeBlockPosition(uarray!SemanticNode.init); }

    uarray!SemanticNode nodes;
    uint nextStatementIndex = void;
    uarray!SemanticNode args;
    BlockFlags flags;
    //Builder!(ubyte, GCDoubler!64) stack;
    pragma(inline) bool isNull() { return nodes.ptr == null; }
}

struct Stack(T)
{
    T current = T.nullValue;
    private Appender!(T[]) stack;
    bool empty() { return current.isNull; }
    void put(T newValue)
    {
        if (!current.isNull)
            stack.put(current);
        current = newValue;
    }
    void pop()
    {
        if (stack.data.length == 0)
        {
            current = T.nullValue;
        }
        else
        {
            current = stack.data[$-1];
            stack.shrinkTo(stack.data.length - 1);
        }
    }
}

struct Interpreter
{
    Stack!CodeBlockPosition blockStack;
    void run()
    {
    BLOCK_STACK_LOOP:
        for (; !blockStack.empty; blockStack.pop)
        {
            for (;;)
            {
                if (blockStack.current.nextStatementIndex >= blockStack.current.nodes.length)
                {
                    assert(blockStack.current.nextStatementIndex == blockStack.current.nodes.length, "code bug");
                    break;
                }
                //from!"std.stdio".writefln("intepret statement %s: %s", blockStack.current.nextStatementIndex,
                //    blockStack.current.nodes[blockStack.current.nextStatementIndex]);
                handle(blockStack.current.nodes[blockStack.current.nextStatementIndex++]);
                if (blockStack.empty)
                    break BLOCK_STACK_LOOP;
            }
        }
    }

    private void handle(SemanticNode node)
    {
        static class Visitor : IHighLevelVisitor
        {
            Interpreter* interpreter;
            this(Interpreter* interpreter) { this.interpreter = interpreter; }
            void visit(Tuple) { assert(0, "interpreter.handle(Tuple) not implemented"); }
            //void visit(Void) { }
            void visit(Symbol) { assert(0, "Symbol not implemented"); }
            void visit(RegularCall node)
            {

                //from!"std.stdio".writefln("[DEBUG] interpret runtime funtion '%s'", node.runtimeCall.syntaxNode.functionName);
                auto asBuiltin = cast(BuiltinRegularFunction)node.function_;
                if (asBuiltin)
                {
                    asBuiltin.interpret(interpreter, node);
                }
                else
                {
                    auto userDefined = cast(UserDefinedFunction)node.function_;
                    assert(userDefined, "codebug: expected BuiltinRegularFunction or UserDefinedFunction");

                    // Setup the stack!
                    // Need to map the call arguments to the function parameters
                    if (node.arguments.length != userDefined.params.length)
                    {
                        from!"std.stdio".writefln("call argument count %s != function argument count %s, not implemented",
                            node.arguments.length, userDefined.params.length);
                        throw quit;
                    }
                    auto args = new SemanticNode[userDefined.params.length];
                    foreach (argIndex; 0 .. node.arguments.length)
                    {
                        //node.arguments[argIndex].makeValue(&paramValues[argIndex]);
                        args[argIndex] = node.arguments[argIndex];
                    }

                    interpreter.blockStack.put(CodeBlockPosition(userDefined.bodyNodes, 0, args.toUarray));
                    //interpretRegularCall(&node.runtimeCall, asUserDefined);
                }
            }
            void visit(SemanticCall) { assert(0, "SemanticCall not implemented"); }
            void visit(BuiltinType) { assert(0, "BuiltinType not implemented"); }
            void visit(SemanticFunction) { assert(0, "SemanticFunction not implemented"); }
            void visit(RegularFunction) { assert(0, "RegularCall not implemented"); }
            void visit(Value) { /* just ignore value*/ }
            void visit(FunctionParameter) { assert(0, "FunctionParameter not implemented"); }
            void visit(LazyNode) { assert(0, "LazyNode not implemented"); }
        }
        scope visitor = new Visitor(&this);
        node.accept(visitor);
        /+
        final switch(node.nodeType)
        {
        case SemanticNodeType.typedValue:
            import types : VoidType;
            if (node.typedValue.asTypedValue.type.val !is VoidType.instance)
            {
                import types; from!"std.stdio".writefln("[DEBUG] typed value %s", node.typedValue.asTypedValue.type.formatName);
                assert(0, "not implemented: interpreter handle semantc node typed value");
            }
            break;
        case SemanticNodeType.tuple:
            assert(0, "not implemented");
        case SemanticNodeType.symbol:
            assert(0, "not implemented");
        case SemanticNodeType.semanticCall:
            handle(node.semanticCall.returnValue);
            break;
        case SemanticNodeType.statementBlock:
            blockStack.put(CodeBlockPosition(
                node.statementBlock.statements, 0, node.statementBlock.flags
            ));
            break;
        case SemanticNodeType.jump:
            assert(0, "jump not implemented");
            break;
        }
        +/
    }
}
