module syntax;

import std.format : formattedWrite;
import more.format : StringSink;

import common : uarray;

struct CallSyntaxNode
{
    string source;
    string functionName;
    uarray!SyntaxNode arguments;
    auto base() inout { return cast(SyntaxNode*)&this; }
}

struct TupleSyntaxNode
{
    string source;
    uarray!SyntaxNode nodes;
    auto base() inout { return cast(SyntaxNode*)&this; }
}
struct StringSyntaxNode
{
    string source;
    string str;
    auto base() inout { return cast(SyntaxNode*)&this; }
}

enum KeywordType : ubyte
{
    void_,
    false_,
    true_,
}
struct KeywordSyntaxNode
{
    string source;
    KeywordType type;
    auto base() inout { return cast(SyntaxNode*)&this; }
}

enum SyntaxNodeType
{
    number,
    string_,
    keyword,
    symbol,
    // Note that the 'tuple' syntax is not necessary since it can be implemented
    // via an analyze-time function (i.e. tuple(...)), but since it's used so often
    // it's nice to have a simple syntax for it.
    tuple,
    call,
}
struct SyntaxNode
{
    union
    {
        string source = void;
        CallSyntaxNode call = void;
        TupleSyntaxNode tuple = void;
        StringSyntaxNode str = void;
        KeywordSyntaxNode keyword = void;
    }

    static assert(SyntaxNode.source.offsetof == CallSyntaxNode.source.offsetof);
    static assert(SyntaxNode.source.offsetof == TupleSyntaxNode.source.offsetof);
    static assert(SyntaxNode.source.offsetof == StringSyntaxNode.source.offsetof);
    static assert(SyntaxNode.source.offsetof == KeywordSyntaxNode.source.offsetof);

    SyntaxNodeType type;
    @disable this();
    private this(SyntaxNodeType type, string source)
        in { assert(type == SyntaxNodeType.number || type == SyntaxNodeType.symbol); } do
    {
        this.source = source;
        this.type = type;
    }
    private this(string source, KeywordType keywordType)
    {
        this.keyword = KeywordSyntaxNode(source, keywordType);
        this.type = SyntaxNodeType.keyword;
    }
    private this(string source, string str)
    {
        this.str = StringSyntaxNode(source, str);
        this.type = SyntaxNodeType.string_;
    }
    private this(string source, uarray!SyntaxNode arguments)
    {
        this.tuple = TupleSyntaxNode(source, arguments);
        this.type = SyntaxNodeType.tuple;
    }
    this(string source, string functionName, uarray!SyntaxNode arguments)
    {
        this.call = CallSyntaxNode(source, functionName, arguments);
        this.type = SyntaxNodeType.call;
    }

    pragma(inline) static SyntaxNode makeNumber(string source)
    {
        return SyntaxNode(SyntaxNodeType.number, source);
    }
    pragma(inline) static SyntaxNode makeKeyword(string source, KeywordType keywordType)
    {
        return SyntaxNode(source, keywordType);
    }
    pragma(inline) static SyntaxNode makeSymbol(string source)
    {
        return SyntaxNode(SyntaxNodeType.symbol, source);
    }
    pragma(inline) static SyntaxNode makeString(string source, string str)
    {
        return SyntaxNode(source, str);
    }
    pragma(inline) static SyntaxNode makeTuple(string source, uarray!SyntaxNode setNodes)
    {
        return SyntaxNode(source, setNodes);
    }
    pragma(inline) static SyntaxNode makeCall(string source, string functionName, uarray!SyntaxNode arguments)
    {
        return SyntaxNode(source, functionName, arguments);
    }

    void toString(StringSink sink)
    {
        final switch(type)
        {
        case SyntaxNodeType.number:
            sink(source);
            break;
        case SyntaxNodeType.string_:
            sink(source);
            break;
        case SyntaxNodeType.keyword:
            sink(source);
            break;
        case SyntaxNodeType.symbol:
            sink(source);
            break;
        case SyntaxNodeType.tuple:
            sink("{");
            string prefix = "";
            foreach (ref node; tuple.nodes)
            {
                sink(prefix);
                prefix = " ";
                node.toString(sink);
            }
            sink("}");
            break;
        case SyntaxNodeType.call:
            sink(call.functionName);
            sink("(");
            string prefix = "";
            foreach (i, ref node; call.arguments)
            {
                sink(prefix);
                prefix = ",";
                node.toString(sink);
            }
            sink(")");
            break;
        }
    }

    version(unittest)
    {
        bool opEqualsForTest(SyntaxNode other)
        {
            if (type != other.type || source != other.source)
            {
                return false;
            }
            final switch(type)
            {
            case SyntaxNodeType.number:
                return true;
            case SyntaxNodeType.string_:
                return str.str == other.str.str;
            case SyntaxNodeType.keyword:
                return true;
            case SyntaxNodeType.symbol:
                return true;
            case SyntaxNodeType.tuple:
                if (tuple.nodes.length != other.tuple.nodes.length)
                    return false;
                foreach (i; 0 .. tuple.nodes.length)
                {
                    if (!tuple.nodes[i].opEqualsForTest(other.tuple.nodes[i]))
                        return false;
                }
                return true;
            case SyntaxNodeType.call:
                if (call.functionName != other.call.functionName)
                    return false;
                if (call.arguments.length != other.call.arguments.length)
                    return false;
                foreach (i; 0 .. call.arguments.length)
                {
                    if (!call.arguments[i].opEqualsForTest(other.call.arguments[i]))
                        return false;
                }
                return true;
            }
        }
    }
}