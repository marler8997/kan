module interpreter;

import std.array : Appender;
import std.format : format;

import common : from, uarray;

import syntax : SyntaxNodeType, SyntaxNode, CallSyntaxNode;
import semantics : IScope, SemanticNodeType, SemanticNode, RuntimeCall, BuiltinRuntimeFunction, UserDefinedFunction, TypedValue;

struct CodeBlockPosition
{
    static CodeBlockPosition nullValue() { return CodeBlockPosition(uarray!SemanticNode.init); }
    uarray!SemanticNode nodes;
    uint nextStatementIndex = void;
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
                handle(&blockStack.current.nodes[blockStack.current.nextStatementIndex++]);
                if (blockStack.empty)
                    break BLOCK_STACK_LOOP;
            }
        }
    }

    private void handle(SemanticNode* node)
    {
        final switch(node.nodeType)
        {
        case SemanticNodeType.typedValue:
            import types : VoidType;
            if (node.typedValue.asTypedValue.type !is VoidType.instance)
            {
                import types; from!"std.stdio".writefln("[DEBUG] typed value %s", node.typedValue.asTypedValue.type.formatType);
                assert(0, "not implemented: interpreter handle semantc node typed value");
            }
            break;
        case SemanticNodeType.tuple:
            assert(0, "not implemented");
        case SemanticNodeType.symbol:
            assert(0, "not implemented");
        case SemanticNodeType.syntaxCall:
            // do nothing
            break;
        case SemanticNodeType.semanticCall:
            handle(node.semanticCall.returnValue);
            break;
        case SemanticNodeType.runtimeCall:
            //from!"std.stdio".writefln("[DEBUG] interpret runtime funtion '%s'", node.runtimeCall.syntaxNode.functionName);
            {
                auto asBuiltin = cast(BuiltinRuntimeFunction)node.runtimeCall.function_;
                if (asBuiltin)
                {
                    asBuiltin.interpret(&this, &node.runtimeCall);
                }
                else
                {
                    auto userDefined = cast(UserDefinedFunction)node.runtimeCall.function_;
                    assert(userDefined, "code bug");
                    blockStack.put(CodeBlockPosition(userDefined.analyzedCode, 0));
                    //interpretRuntimeCall(&node.runtimeCall, asUserDefined);
                }
            }
            break;
        }
    }
}
