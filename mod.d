module mod;

import std.stdio;
import std.typecons : Flag, Yes, No, scoped;
import std.bitmanip : bitfields;
import std.array  : Appender;
import std.format : format, formattedWrite;
import std.string : lastIndexOf;
import std.path   : buildPath;
import std.file   : exists;
import std.algorithm: count;

import more.alloc : GCDoubler;
import more.builder : Builder;
import more.format : StringSink;
import more.file : readFile;

static import global;
import common : uarray, toUarray, toUint, from, singleton, quit,
                DirSeparatorChar, isDirSeparator, replaceAll, formatDir;
import log;
import id : Id;
import syntax : SyntaxNode;
import parser : Parser;
import semantics;
import types : IType, ModuleType;
import symtab : SymbolTable;
import interpreter : CodeBlockPosition, Interpreter;
import analyzer : Reporting, AnalyzeState, greaterThan;
static import analyzer;

class Module : IScope//, ICodeBlock
{
    string filename;
    const Flag!"rootCodeIsUsed" rootCodeIsUsed;
    Id.Value baseID;
    string importName; // the first import name that caused the module to be loaded.
    private string contents;
    private uarray!SyntaxNode syntaxNodes;
    private uarray!SemanticNode semanticNodes;
    private uarray!AnalyzeState semanticNodeAnalyzeStates;

    private size_t nodesAddedSymbols;
    private size_t nodesAnalyzed;
    mixin(bitfields!(
        bool, "parsed", 1,
        bool, "analyzeStarted", 1,
        void, null, 6
    ));

    SymbolTable symbolTable;

    static immutable builtin = new immutable Module();
    private this() immutable
    {
        this.filename = "<builtin>";
        this.rootCodeIsUsed = No.rootCodeIsUsed;
    }
    this(string filename, Flag!"rootCodeIsUsed" rootCodeIsUsed, string importName)
    {
        this.filename = filename;
        // Note: this should be determined when creating the module and should not change
        //       this affects how top-level code of the module is analyzed
        this.rootCodeIsUsed = rootCodeIsUsed;
        this.importName = importName;
        if (importName !is null)
        {
            this.baseID = Id.pool(sliceModuleBaseName(importName));
        }
        else
        {
            //assert(0, "not implemented");
        }
    }
    void read()
    {
        if (contents is null)
        {
            verbose(0, "reading '%s'", filename);
            contents = cast(string)readFile(filename, Yes.addNull);
            assert(contents !is null);
        }
    }
    void parse()
    {
        read();
        if (!parsed)
        {
            auto parser = Parser(contents.ptr, filename);
            auto nodesBuilder = Appender!(SyntaxNode[])();
            parser.parse(&nodesBuilder);
            this.syntaxNodes = nodesBuilder.data.toUarray;
            this.parsed = true;
        }
    }

    bool haveAllTopLevelSymbols()
    {
        // Need a way of knowing all the ways symbols can be added so that when
        // we are looking for symbols, we know whether or not that symbol could be added
        // to a "lower scope".
        return analyzeStarted && nodesAddedSymbols == semanticNodes.length;
    }

    void analyze()
    {
        if (analyzeStarted)
        {
            return;
        }

        parse();
        semanticNodes = new SemanticNode[syntaxNodes.length].toUarray;
        semanticNodeAnalyzeStates = new AnalyzeState[syntaxNodes.length].toUarray;
        foreach (i; 0..syntaxNodes.length)
        {
            semanticNodes[i].initialize(&syntaxNodes[i]);
            assert(semanticNodeAnalyzeStates[i] == AnalyzeState.notAnalyzed, "code bug");
        }
        analyzeStarted = true;

        // TODO: need a way to detect recursive calls to this

        verbose(0, "analyzing '%s'", filename);

    ANALYZE_GLOBAL_NODES_LOOP:
        while (nodesAnalyzed < semanticNodes.length)
        {
            auto nodesAnalyzedBeforePass = nodesAnalyzed;
            foreach (i, ref node; semanticNodes)
            {
                if (semanticNodeAnalyzeStates[i] != AnalyzeState.analyzed)
                {
                    auto oldState = semanticNodeAnalyzeStates[i];
                    auto newState = analyzer.analyzeModuleNode(this, &node);
                    // double check that nothing else modified the current state
                    assert(oldState == semanticNodeAnalyzeStates[i], "code bug");
                    if (newState != oldState)
                    {
                        assert(newState.greaterThan(oldState), "code bug");
                        semanticNodeAnalyzeStates[i] = newState;
                        // NOTE: this logic is currently based on AnalyzeState having
                        //       3 values: notAnalyzed, addedSymbols and analyzed
                        if (oldState < AnalyzeState.addedSymbols)
                        {
                            assert(newState >= AnalyzeState.addedSymbols, "code bug");
                            nodesAddedSymbols++;
                        }
                        if (newState == AnalyzeState.analyzed)
                        {
                            nodesAnalyzed++;
                            if (nodesAnalyzed >= semanticNodes.length)
                            {
                                break ANALYZE_GLOBAL_NODES_LOOP;
                            }
                        }
                    }
                }
            }

            // if nothing new was analyzed
            if (nodesAnalyzed == nodesAnalyzedBeforePass)
            {
                foreach (i, ref node; semanticNodes)
                {
                    if (semanticNodeAnalyzeStates[i] != AnalyzeState.analyzed)
                    {
                        auto result = analyzer.analyzeModuleNode!Reporting(this, &node);
                        assert(result > 0, "code bug: analyzeModuleNode failed but did not report any errors");
                    }
                }
                throw quit;
            }
        }
    }


    private Builder!(UserDefinedFunction, GCDoubler!32) functionsToAnalyze;
    uint functionsToAnalyzeCount()
    {
        return functionsToAnalyze.dataLength.toUint;
    }
    void addFunctionToAnalyze(UserDefinedFunction function_)
    {
        functionsToAnalyze.append(function_);
    }
    void tryAnalyzeFunctions()
    {
        // TODO: this loop isn't really a good way to do this
        uint nextFunctionIndex = 0;
        for (;nextFunctionIndex < functionsToAnalyze.dataLength;)
        {
            if (analyzer.analyzeFunctionBody(functionsToAnalyze.data[nextFunctionIndex]))
            {
                functionsToAnalyze.removeAt(nextFunctionIndex);
            }
            else
            {
                nextFunctionIndex++;
            }
        }
    }
    /+
    void printSemanticErrorsForMatching()
    {
        foreach (i, ref node; semanticNodes)
        {
            if (!semanticNodeAnalyzeStates[i].analyzedForMatching)
            {
                analyzer.printAnalyzeForMatchingErrors(this, &node);
            }
        }
    }
    +/

    void run()
    {
        assert(semanticNodes.ptr != null && nodesAnalyzed == semanticNodes.length, "code bug");
        auto interpreter = Interpreter();
        interpreter.blockStack.put(CodeBlockPosition(semanticNodes, 0));
        interpreter.run();
    }
    auto formatLocation(size_t lineNumber)
    {
        return LocationFormatter(this, lineNumber);
    }
    pragma(inline) auto formatLocation(string source) { return formatLocation(source.ptr); }
    auto formatLocation(immutable(char)* source)
    {
        if (source < contents.ptr || source > contents.ptr + contents.length)
        {
            assert(0, format("[CODEBUG] !!!!!!!!! formatLocation for file '%s' was given a source pointer that was not in the file", filename));
        }
        return LocationFormatter(this, 1 + count(contents[0 .. source - contents.ptr], '\n'));
    }
    override string toString() const
    {
        return filename;
    }
    //
    // Interface not defined yet functions
    //
    inout(TypedValue) asTypedValue() inout
    {
        return inout TypedValue(ModuleType.instance, inout Value(this));
    }
    //
    // IDotQualifiable Functions
    //
    ResolveResult tryGetUnqualified(string symbol)
    {
        analyze();

        {
            auto result = symbolTable.get(symbol);
            if (!result.isNull)
            {
                return ResolveResult(result);
            }
        }

        return haveAllTopLevelSymbols() ? ResolveResult.noEntryAndAllSymbolsAdded :
            ResolveResult.noEntryButMoreSymbolsCouldBeAdded;
    }
    //
    // IScope Functions
    //
    IScope getParent() { return cast()UniversalScope.instance; }
    Module getModule() { return this; }
    void add(const(string) symbol, const(TypedValue) typedValue)
    {
        auto existing = symbolTable.checkedAdd(symbol, typedValue);
        if (!existing.isNull)
        {
            writefln("Error: %s already has a definition for %s",
                /*formatLocation(assignment.symbol), */filename, symbol);
            throw quit;
        }
    }
}

struct LocationFormatter
{
    Module module_;
    size_t lineNumber;
    this(Module module_, size_t lineNumber)
    {
        this.module_ = module_;
        this.lineNumber = lineNumber;
    }
    void toString(StringSink sink) const
    {
        if (lineNumber > 0)
        {
            formattedWrite(sink, "%s(%s) ", module_.filename, lineNumber);
        }
        else
        {
            formattedWrite(sink, "%s: ", module_.filename);
        }
    }
}

auto moduleBaseNameIdFromFileName(inout(char)[] filename)
{
    size_t baseNameStart = filename.length;
    for (; baseNameStart > 0;)
    {
        if (isDirSeparator(filename[baseNameStart]))
        {
            baseNameStart++;
            assert(baseNameStart < filename.length);
            break;
        }
    }
    return filename[baseNameStart .. $];
}
auto sliceModuleBaseName(inout(char)[] fullModuleName)
{
    auto lastDotIndex = fullModuleName.lastIndexOf('.');
    if (-1 == lastDotIndex)
    {
        return fullModuleName;
    }
    return fullModuleName[lastDotIndex + 1 .. $];
}


Module loadImport(string importName)
{
    // first check if the module is already loaded
    foreach (module_; global.modules.data)
    {
        if (module_.importName == importName)
        {
            return module_;
        }
    }

    // find the module file
    auto moduleRelativeImportFileName = importName.replaceAll('.', DirSeparatorChar) ~ ".kan";
    string moduleFileName = null;
    foreach (importPath; global.importPaths.data)
    {
        auto filename = buildPath(importPath, moduleRelativeImportFileName);
        if (exists(filename))
        {
            if (moduleFileName !is null)
            {
                writefln("Error: module '%s' exists at '%s' and '%s'", importName, moduleFileName, filename);
                throw quit;
            }
            moduleFileName = filename;
            // don't break, keep going to make sure there is no module name conflicts
        }
    }
    if (moduleFileName is null)
    {
        if (global.importPaths.data.length == 0)
        {
            writefln("Error: cannot import '%s' because there are no include paths", importName);
            throw quit;
        }
        writefln("Error: import \"%s\" is not found in any of the following include paths:", importName);
        foreach (i, importPath; global.importPaths.data)
        {
            writefln("[%s] %s", i, importPath.formatDir);
        }
        throw quit;
    }
    return loadModuleFromFileCommon(moduleFileName, No.rootCodeIsUsed, importName);
}

// Assumption: the caller has ALREADY checked if filename exists
Module loadModuleFromFilename(string filename, Flag!"rootCodeIsUsed" rootCodeIsUsed, string importName)
{
    return loadModuleFromFileCommon(filename, rootCodeIsUsed, importName);
}

private Module loadModuleFromFileCommon(string filename, Flag!"rootCodeIsUsed" rootCodeIsUsed, string importName)
{
    // make sure that this file has not already been loaded
    foreach (module_; global.modules.data)
    {
        if (module_.filename == filename)
        {
            // make sure the import name is correct
            if (module_.importName is null)
            {
                module_.importName = importName;
            }
            else if (module_.importName != importName)
            {
                writefln("Error: file '%s' was imported with 2 different names '%s' and '%s'",
                    filename, module_.importName, importName);
                throw quit;
            }
            return module_;
        }
    }
    auto newModule = new Module(filename, rootCodeIsUsed, importName);
    global.modules.put(newModule);
    return newModule;
}

class UniversalScope : IScope
{
    mixin singleton;
    //
    // IDotQualifiable Functions
    //
    ResolveResult tryGetUnqualified(string symbol)
    {
        static import builtin;
        import types : RuntimeFunctionType;
        if (symbol == "leftIsLess")
        {
            return ResolveResult(RuntimeFunctionType.instance.createTypedValue(builtin.leftIsLessFunction.instance));
        }
        // TODO: add builtin stuff here
        return ResolveResult.noEntryAndAllSymbolsAdded;
    }
    //
    // IScope Functions
    //
    IScope getParent() { return null; }
    // TODO: probably return a pseudo universal-module
    Module getModule() { assert(0, "not implemented"); }
    void add(const(string) symbol, const(TypedValue) typedValue)
    {
        assert(0, "not sure if adding to the universal scope should be allowed");
    }
}