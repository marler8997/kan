module log;

static import global;


//
// TODO: I will probably add verbose categories at some point.
//       Each category can have their own verbosity "level" as well.
//
pragma(inline) void verbose(T...)(ubyte level, string format, T args)
{
    if (global.verbose > level)
    {
        verboseFunction(level, format, args);
    }
}

private void verboseFunction(T...)(ubyte level, string format, T args)
{
    import std.stdio;
    writef("[verbose%s] ", level);
    writefln(format, args);
}