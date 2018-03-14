module parser;

import parsertemplate;

import syntax;

struct ParserPolicy
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