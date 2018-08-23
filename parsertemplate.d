module parsertemplate;

import std.typecons : Flag, Yes, No, Nullable, nullable;
static import std.format;

import more.utf8;
import more.format;

import common : toUarray;
import syntax : SyntaxNode, KeywordType, NumberType;

// TODO:
// Maybe support a special character that allows you to use keywords as symbols
// i.e. @void, @false, ...
// Note that the other way to do this would be to have a syntax-function i.e.
// symbol(void), symbol(false)
//
class ParseException : Exception
{
    this(string msg, string filename, uint lineNumber)
    {
        super(msg, filename, lineNumber);
    }
}

union Char
{
    char codeUnit;
    dchar fullChar;
}

struct ParserTemplate(Policy)
{
    immutable(char)* nextPtr;
    string filenameForErrors;
    uint lineNumber;
    this(immutable(char)* nextPtr, string filenameForErrors = null, uint lineNumber = 1)
    {
        this.nextPtr = nextPtr;
        this.filenameForErrors = filenameForErrors;
        this.lineNumber = lineNumber;
    }
    private auto parseException(T...)(string fmt, T args)
    {
        return new ParseException(std.format.format(fmt, args), filenameForErrors, lineNumber);
    }
    void parse(Policy.NodeBuilderRef builder)
    {
        return parseBlock(builder, '\0');
    }

    // Assumption
    void parseBlock(Policy.NodeBuilderRef builder, char endChar)
    {
        for (;;)
        {
            Char nextChar = void;
            nextChar.codeUnit = skipWhitespaceAndComments();

            if (nextChar.codeUnit == '{')
            {
                auto sourceStart = nextPtr;
                nextPtr++;
                auto nodeBuilder = Policy.NodeBuilderDirect();
                parseBlock(Policy.nodeBuilderRef(&nodeBuilder), '}');
                nextPtr++;
                auto sourceLimit = nextPtr;
                builder.put(SyntaxNode.makeTuple(
                    sourceStart[0 .. sourceLimit - sourceStart], nodeBuilder.data.toUarray
                ));
                continue;
            }

            if (nextChar.codeUnit == '"')
            {
                builder.put(peelString());
                continue;
            }

            if (nextChar.codeUnit == endChar)
                return;

            if (validNumberFirstChar(nextChar.codeUnit))
            {
                builder.put(peelNumber());
                continue;
            }

            if (endChar == '\0')
            {
                if (nextChar.codeUnit == ')' || nextChar.codeUnit == '}')
                    throw parseException("extra closing '%s'", nextChar.codeUnit);
            }
            else if (nextChar.codeUnit == '\0')
                throw parseException("missing closing '%s'", endChar);

            // peel a name
            auto sourceStart = nextPtr;
            auto symbolResult = verifyAtSymbolAndPeelIt("a symbol to start a statement");
            {
                auto keywordType = tryGetKeywordType(symbolResult.str);
                if (!keywordType.isNull)
                {
                    builder.put(SyntaxNode.makeKeyword(symbolResult.str, keywordType.get));
                    continue;
                }
            }

            // could be at a variable or a function call, go to the next thing to see what it is
            nextChar.codeUnit = skipWhitespaceAndComments();
            if (nextChar.codeUnit == '(')
            {
                nextPtr++;
                auto setBuilder = Policy.NodeBuilderDirect();
                parseBlock(Policy.nodeBuilderRef(&setBuilder), ')');
                nextPtr++;
                auto sourceLimit = nextPtr;
                builder.put(SyntaxNode.makeCall(sourceStart[0 .. sourceLimit - sourceStart],
                    symbolResult.str, setBuilder.data.toUarray
                ));
            }
            else
            {
                // must just be a variable
                builder.put(SyntaxNode.makeSymbol(symbolResult.str));
            }
        }
    }


    static struct AfterSymbolResult
    {
        string str;
        dchar c;
        immutable(char)* afterC;
    }
    AfterSymbolResult verifyAtSymbolAndPeelIt(lazy string expectedErrorDescription)
    {
        auto start = nextPtr;
        auto ptr = start;
        auto firstChar = decodeUtf8(&ptr);
        if (!validNameFirstChar(firstChar))
            throw parseException("expected %s but got %s", expectedErrorDescription, formatToken(start));

        for (;;)
        {
            auto nextCharPtr = ptr;
            auto nextChar = decodeUtf8(&ptr);
            if (!validNameChar(nextChar))
            {
                nextPtr = nextCharPtr;
                return AfterSymbolResult(start[0 .. nextCharPtr - start], nextChar, nextCharPtr);
            }
        }
    }

    // next points to the first character to start checking
    // returns: the next "code-unit", note that the next "code-unit"
    // may or may not be the next full character
    char skipWhitespaceAndComments()
    {
        for (;;)
        {
            auto nextChar = nextPtr[0];
            if (nextChar == ' ' || nextChar == '\t' || nextChar == '\r')
            {
                //do nothing
            }
            else if (nextChar == '\n')
            {
                lineNumber++;
            }
            else if (nextChar == '/')
            {
                if (nextPtr[1] == '/')
                {
                    nextPtr += 2;
                    if (toNewline() == Yes.atEof)
                        return '\0';
                }
                else if (nextPtr[1] == '*')
                {
                    nextPtr += 2;
                    skipMultilineComment();
                }
                else
                {
                    return nextChar;
                }
            }
            else
            {
                return nextChar;
            }
            nextPtr++;
        }
    }
    void skipMultilineComment()
    {
        uint depth = 1;
        auto ptr = nextPtr;
        for (;;)
        {
            auto nextChar = ptr[0];
            if (nextChar == '/')
            {
                if (ptr[1] == '*')
                {
                    ptr++;
                    depth++;
                }
            }
            else if (nextChar == '*')
            {
                if (ptr[1] == '/')
                {
                    ptr++;
                    depth--;
                    if (depth == 0)
                    {
                        nextPtr = ptr;
                        return;
                    }
                }
            }
            else if (nextChar == '\0')
            {
                throw parseException("unterminted /* */ comment");
            }
            ptr++;
        }
    }

    Flag!"atEof" toNewline()
    {
        for (;; nextPtr++)
        {
            auto current = nextPtr[0];
            if (current == '\n')
                return No.atEof;
            if (current == '\0')
                return Yes.atEof;
        }
    }
    // Assumption: current is at the opening quote
    SyntaxNode peelString()
    {
        auto start = nextPtr;
        auto ptr = nextPtr;
        immutable(char)* firstEscape = null;
        for (;;)
        {
            ptr++;
            auto current = ptr[0];
            if (current == '"')
            {
                break;
            }
            if (current == '\\')
            {
                if (!firstEscape)
                {
                    firstEscape = nextPtr;
                }
                assert(0, "escapes not implemented");
            }
            else if (current == '\n')
            {
                // TODO: maybe provide a way to allow this
                throw parseException("double-quoted strings cannot contain newlines");
            }
            else if (current == '\0')
            {
                throw parseException("file ended inside double-quoted string");
            }
        }
        if (!firstEscape)
        {
            ptr++;
            nextPtr = ptr;
            auto source = start[0 .. ptr - start];
            auto str = source[1..$-1];
            return SyntaxNode.makeString(source, str);
        }
        assert(0, "escapes not implemented");
    }
    // Assumption: next is at the first digit
    SyntaxNode peelNumber()
    {
        auto start = nextPtr;
        auto ptr = start;
        NumberType type;
        if (ptr[0] == '0' && ptr[1] == 'x')
        {
            type = NumberType.hex;
            ptr += 2;
            for (;; ptr++)
            {
                auto nextChar = ptr[0];
                if (nextChar < '0' ||
                    (nextChar > '9' && nextChar < 'A') ||
                    (nextChar > 'F' && nextChar < 'a') ||
                    nextChar > 'f')
                    break;
            }
        }
        else
        {
            type = NumberType.decimal;
            for (;;)
            {
                ptr++;
                auto nextChar = ptr[0];
                if (nextChar < '0' || nextChar > '9')
                    break;
            }
        }
        nextPtr = ptr;
        auto source = start[0 .. ptr - start];
        return SyntaxNode.makeNumber(source, type);
    }
}

private bool validNumberFirstChar(char c)
{
    return c >= '0' && c <= '9';
}
private bool validNameFirstChar(dchar c)
{
    return
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c == '_');
}
private bool validNameChar(dchar c)
{
    return
        validNameFirstChar(c) ||
        (c >= '0' && c <= '9') ||
        c == '.';
}

Nullable!KeywordType tryGetKeywordType(const(char)[] str)
{
    final switch(str.length & 0x7)
    {
        case 0: return Nullable!KeywordType.init;
        case 1: return Nullable!KeywordType.init;
        case 2: return Nullable!KeywordType.init;
        case 3: return Nullable!KeywordType.init;
        case 4:
            if (str == "void") return nullable(KeywordType.void_);
            if (str == "true") return nullable(KeywordType.true_);
            return Nullable!KeywordType.init;
        case 5:
            if (str == "false") return nullable(KeywordType.false_);
            return Nullable!KeywordType.init;
        case 6: return Nullable!KeywordType.init;
        case 7: return Nullable!KeywordType.init;
    }
}

struct PeekedChar
{
    dchar nextChar;
    const(char)* nextNextPtr;
}

auto guessEndOfToken(const(char)* ptr)
{
    // TODO: should use the first char to determine the kind of token and
    //       then find the end using that information
    for (;;)
    {
        auto c = *ptr;
        if (c == '\0' || c == ' ' || c == '\t' || c == '\r' || c == '\n')
        {
            return ptr;
        }
        decodeUtf8(&ptr);
    }
}
auto formatToken(const(char)* token)
{
    static struct Formatter
    {
        const(char)* token;
        void toString(scope void delegate(const(char)[]) sink) const
        {
            if (*token == '\0')
            {
                sink("EOF");
            }
            else
            {
                sink("\"");
                sink.utf8WriteEscaped(token, guessEndOfToken(token));
                sink("\"");
            }
        }
    }
    return Formatter(token);
}

unittest
{
    import std.array : Appender;
    import std.conv : to;
    static struct ParserPolicy
    {
        import std.array : Appender;
        alias NodeBuilderDirect = Appender!(SyntaxNode[]);
        alias NodeBuilderRef =  Appender!(SyntaxNode[])*;
        static NodeBuilderRef nodeBuilderRef(NodeBuilderDirect* builder)
        {
            return builder;
        }
    }
    alias Parser = ParserTemplate!ParserPolicy;

    {
        auto parser = Parser(null);
    }
    static void test(string text, SyntaxNode[] expected, size_t line = __LINE__)
    {
        import std.stdio; writefln("testing line %s: %s", line, text);
        auto builder = Appender!(SyntaxNode[])();
        auto parser = Parser(text.ptr, "unittest_line_" ~ line.to!string);
        parser.parse(&builder);
        //import std.stdio;
        //writefln("expected: %s", expected);
        //writefln("actual  : %s", builder.data);
        assert(expected.length == builder.data.length);
        foreach (i; 0 .. expected.length)
        {
            assert(expected[i].opEqualsForTest(builder.data[i]));
        }
    }
    static void testError(string text, string expectedError, size_t line = __LINE__)
    {
        import std.stdio; writefln("testing line %s: %s", line, text);
        auto builder = Appender!(SyntaxNode[])();
        auto parser = Parser(text.ptr, "unittest_line_" ~ line.to!string);
        try
        {
            parser.parse(&builder);
            assert(0, "expected parse expection but did not get one");
        }
        catch(ParseException e)
        {
            import std.stdio;
            writefln("got expected exception: %s", e.msg);
            assert(e.msg == expectedError);
        }
    }
    static SyntaxNode sym(string source)
    {
        return SyntaxNode.makeSymbol(source);
    }
    static SyntaxNode tuple(string source, SyntaxNode[] nodes...)
    {
        return SyntaxNode.makeTuple(source, nodes.dup.toUarray);
    }
    static SyntaxNode str(string source, string escaped)
    {
        return SyntaxNode.makeString(source, escaped);
    }
    static SyntaxNode num(string source)
    {
        return SyntaxNode.makeNumber(source);
    }
    test("", []);

    testError("{", `missing closing '}'`);
    test("{}", [tuple("{}")]);
    testError("{)", `expected a symbol to start a statement but got ")"`);
    test("{{}}", [tuple("{{}}", tuple("{}"))]);
    testError("{{}", `missing closing '}'`);

    testError(`"`, `file ended inside double-quoted string`);
    test(`""`, [str(`""`, "")]);
    test(`"a"`, [str(`"a"`, "a")]);
    test(`"123a_asdf;/1j01j02"`, [str(`"123a_asdf;/1j01j02"`, "123a_asdf;/1j01j02")]);

    test("0", [num("0")]);
    test("1234", [num("1234")]);
    test("0x1234", [num("0x1234")]);

    test("//", []);
    testError("/", `expected a symbol to start a statement but got "/"`);
    testError("/*", `unterminted /* */ comment`);
    testError("/*/", `unterminted /* */ comment`);
    test("/**/", []);
    testError("/*/*/", `unterminted /* */ comment`);
    testError("/*/**/", `unterminted /* */ comment`);
    testError("/*/*/*/", `unterminted /* */ comment`);
    test("/*/**/*/", []);

    test("a", [sym("a")]);
    test("foo1", [sym("foo1")]);
}