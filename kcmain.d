import std.algorithm : canFind;
import std.typecons : Flag, Yes, No;
import std.array : Appender;
import std.string : startsWith;
import std.path : buildPath, dirName;
import std.file : exists, thisExePath;
import std.stdio;

import more.file : readFile;

import common : QuitException, formatDir;
static import global;
import log;
import parser;
import mod : Module, loadModuleFromFilename;

void usage()
{
    writefln("Usage: kc [compile] <files>...");
    writeln("Options:");
    writeln("  -I<path>    add import search path");
    writeln("  -v          verbose output (use multiple for more verbose)");
}
int main(string[] args)
{
    try { return tryMain(args); }
    catch(QuitException e) { return 1; }
}
int tryMain(string[] args)
{
    args = args[1..$];
    {
        size_t newArgsLength = 0;
        scope(exit) args = args[0..newArgsLength];
        for (size_t i = 0; i < args.length; i++)
        {
            auto arg = args[i];
            if (arg.length > 0 && arg[0] != '-')
            {
                args[newArgsLength++] = arg;
            }
            else if (arg.startsWith("-I"))
            {
                // TODO: support -I<path>, -I=<path> and -I <path>
                auto path = arg[2..$];
                if (path[0] == '=')
                    path = path[1..$];
                if (global.importPaths.data.canFind(path))
                {
                    writefln("Error: import path %s has been given more than once", path.formatDir);
                    return 1;
                }
                // TODO: should we check if this path exists?
                if (!exists(path))
                {
                    writefln("Error: import path %s does not exist", path.formatDir);
                    return 1;
                }
                global.importPaths.put(path);
            }
            else if (arg == "-v" || arg == "-verbose")
            {
                global.verbose++;
            }
            else
            {
                writefln("Unknown option '%s'", arg);
                return 1;
            }
        }
    }

    if (args.length == 0)
    {
        usage();
        return 0;
    }
    enum Action
    {
        run, compile,
    }
    auto action = Action.run;
    {
        auto first = args[0];
        if (first == "compile")
        {
            action = Action.compile;
            args = args[1..$];
            global.willOptimize = true;
        }
    }

    // add include path based on exe location
    {
        auto stdLibrary = buildPath(dirName(thisExePath()), "std");
        if (exists(stdLibrary))
        {
            verbose(0, "added std library import path %s", stdLibrary.formatDir);
            global.importPaths.put(stdLibrary);
        }
    }

    auto filenames = args;
    auto commandLineModules = new Module[filenames.length];
    foreach (index, filename; filenames)
    {
        commandLineModules[index] = loadModuleFromFilename(filename, Yes.rootCodeIsUsed, null);
    }
    foreach (module_; commandLineModules)
    {
        module_.analyze();
    }
    //
    // Analyze all used functions
    //
    for (;;)
    {
        uint totalFunctionsLeftToAnalyze = 0;
        uint totalFunctionsAnalyzedOnLastPass = 0;
        // NOTE: do not use foreach so the module count is not cached
        for (size_t i = 0; i < global.modules.data.length; i++)
        {
            auto module_ = global.modules.data[i];
            auto moduleFunctionsToAnalyzeCount = module_.functionsToAnalyzeCount;
            if (moduleFunctionsToAnalyzeCount > 0)
            {
                module_.tryAnalyzeFunctions();
                totalFunctionsLeftToAnalyze += module_.functionsToAnalyzeCount;
                auto moduleFunctionsAnalyzed = moduleFunctionsToAnalyzeCount - module_.functionsToAnalyzeCount;
                totalFunctionsAnalyzedOnLastPass += moduleFunctionsAnalyzed;
            }
        }

        if (totalFunctionsLeftToAnalyze == 0)
        {
            break;
        }

        if (totalFunctionsAnalyzedOnLastPass == 0)
        {
            // TODO: print errors
            writefln("There are still %s functions let to analyze", totalFunctionsLeftToAnalyze);
            return 1;
        }
    }

    if (action == Action.run)
    {
        foreach (module_; commandLineModules)
        {
            verbose(0, "running %s", module_);
            module_.run();
        }
    }
    if (action == Action.compile)
    {
        /*
        foreach (module_; commandLineModules)
        {
            module_.optimize();
        }
        */
        foreach (module_; commandLineModules)
        {
            writefln("TODO: add module '%s' to compilation unit", module_);
        }
    }

    return 0;
}
