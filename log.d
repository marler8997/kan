module log;

import more.format : StringSink;

static import global;
import common : passfail;
import semantics : ResultOrError, OptionalResultOrError;

struct LocationFormatter
{
    string filename;
    size_t lineNumber;
    this(string filename, size_t lineNumber)
    {
        this.filename = filename;
        this.lineNumber = lineNumber;
    }
    void toString(StringSink sink) const
    {
        import std.format : formattedWrite;
        if (lineNumber > 0)
        {
            formattedWrite(sink, "%s(%s) ", filename, lineNumber);
        }
        else
        {
            formattedWrite(sink, "%s: ", filename);
        }
    }
}

//
// TODO: I will probably add verbose categories at some point.
//       Each category can have their own verbosity "level" as well.
//
pragma(inline) void verbose(Args...)(ubyte level, string format, Args args)
{
    if (global.verbose > level)
    {
        verboseFunction(level, format, args);
    }
}

private void verboseFunction(Args...)(ubyte level, string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("[verbose%s] ", level);
    writefln(format, args);
}

void errorfNoLocation(Args...)(string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("Error: ");
    writefln(format, args);
}
void errorf(Location, Args...)(Location location, string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("%sError: ", location);
    writefln(format, args);
}
uint errorfUint(Location, Args...)(Location location, string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("%sError: ", location);
    writefln(format, args);
    return 1; // 1 error
}
passfail errorfPassfail(Location, Args...)(Location location, string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("%sError: ", location);
    writefln(format, args);
    return passfail.fail;
}


ResultOrError!T errorfResultOrError(T, Location, Args...)(Location location, string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("%sError: ", location);
    writefln(format, args);
    return ResultOrError!T(1);
}
OptionalResultOrError!T errorfOptionalResultOrError(T, Location, Args...)(Location location, string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("%sError: ", location);
    writefln(format, args);
    return OptionalResultOrError!T(1);
}
auto errorfNodeResult(Location, Args...)(Location location, string format, Args args)
{
    import semantics : SemanticNode;
    return errorfResultOrError!(SemanticNode, Location, Args)(location, format, args);
}
auto errorfOptionalNodeResult(Location, Args...)(Location location, string format, Args args)
{
    import semantics : SemanticNode;
    return errorfOptionalResultOrError!(SemanticNode, Location, Args)(location, format, args);
}
auto errorfSymbolResult(Location, Args...)(Location location, string format, Args args)
{
    import semantics : Symbol;
    return errorfResultOrError!(Symbol, Location, Args)(location, format, args);
}
auto errorfNullable(T, Location, Args...)(Location location, string format, Args args)
{
    import std.stdio : writef, writefln;
    writef("%sError: ", location);
    writefln(format, args);
    return null;
}
alias errorfString(Location, Args...) = errorfNullable!(string, Location, Args);
