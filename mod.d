module mod;

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

import common : uarray, toUarray, toUint, unconst, toImmutable, from, singleton, quit,
                DirSeparatorChar, isDirSeparator, replaceAll, formatDir;
import log;
import id : Id;
import syntax : SyntaxNode;
import parser : Parser;
import semantics;
import types : IType, ModuleType;
import symtab : SymbolTable;
import interpreter : CodeBlockPosition, Interpreter;
import analyzer : AnalyzeOptions;
static import analyzer;

class Module : IScope
{
    string filename;
    const Flag!"rootCodeIsUsed" rootCodeIsUsed;
    Id.Value baseID;
    string importName; // the first import name that caused the module to be loaded.
    string content;
    private uarray!SyntaxNode syntaxNodes;
    private uarray!SemanticNode semanticNodes;

    enum State : ubyte
    {
        initial,
        parsed,
        pass1Started,
        pass1Done,
        pass2Started,
        pass2Done,
    }
    State state;
    bool parsed() const { return state >= State.parsed; }

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
        if (content is null)
        {
            verbose(0, "reading '%s'", filename);
            content = cast(string)readFile(filename, Yes.addNull);
            assert(content !is null);
        }
    }
    void parse()
    {
        if (state >= State.parsed)
            return;

        read();
        auto parser = Parser(content.ptr, filename);
        auto nodesBuilder = Appender!(SyntaxNode[])();
        parser.parse(&nodesBuilder);
        this.syntaxNodes = nodesBuilder.data.toUarray;
        state = State.parsed;
    }

    /+
    bool haveAllTopLevelSymbols()
    {
        // Need a way of knowing all the ways symbols can be added so that when
        // we are looking for symbols, we know whether or not that symbol could be added
        // to a "lower scope".
        return analyzeStarted && nodesAddedSymbols == semanticNodes.length;
    }
    +/

    uint analyzePass1()
    {
        if (state >= State.pass1Started)
            return 0;

        parse();
        assert(state == State.parsed);
        state = State.pass1Started;
        verbose(0, "analyzePass1 '%s'", filename);

        semanticNodes = newSemanticNodes(syntaxNodes);
        uint errorCount = 0;
        foreach (node; semanticNodes)
        {
            errorCount += analyzer.analyzeExpressionPass1(this, node);
        }
        assert(state == State.pass1Started, "codebug");
        state = State.pass1Done;
        return errorCount;
    }

    uint analyzePass2(Flag!"runContext" runContext)
    {
        if (state >= State.pass2Started)
            return 0;

        {
            auto errorCount = analyzePass1();
            if (state == State.pass1Started)
            {
                from!"std.stdio".writefln("PossibleError: circular reference(1)?");
                return 1;
                //throw quit;
            }
            if (errorCount > 0)
                return errorCount;
        }

        assert(state == State.pass1Done, "codebug");
        state = State.pass2Started;

        verbose(0, "analyzePass2 '%s'", filename);

        {
            uint totalErrorCount = 0;
            foreach (i; 0 .. semanticNodes.length)
            {
                const errorCount = analyzer.analyzeExpressionPass2(this, &semanticNodes[i], AnalyzeOptions.none);
                if (errorCount == 0)
                    totalErrorCount += analyzer.enforceValidStatement(this, semanticNodes[i]);
                else
                    totalErrorCount += errorCount;
            }
            if (totalErrorCount > 0)
                return totalErrorCount;
        }

        assert(state == State.pass2Started, "codebug");
        state = State.pass2Done;
        return 0;
    }

    /+
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
    +/

    void run()
    {
        assert(state == State.pass2Done, "codebug");
        auto interpreter = Interpreter();
        interpreter.blockStack.put(CodeBlockPosition(semanticNodes, 0));
        interpreter.run();
    }
    auto formatLocation(size_t lineNumber)
    {
        return LocationFormatter(this.filename, lineNumber);
    }
    pragma(inline) auto formatLocation(string source) { return formatLocation(source.ptr); }
    auto formatLocation(immutable(char)* source)
    {
        if (source < content.ptr || source > content.ptr + content.length)
        {
            assert(0, format("[CODEBUG] !!!!!!!!! formatLocation for file '%s' was given a source pointer that was not in the file", filename));
        }
        return LocationFormatter(this.filename, 1 + count(content[0 .. source - content.ptr], '\n'));
    }
    override string toString() const
    {
        return filename;
    }
    //
    // IDotQualifiable Functions
    //
    SemanticNode tryGetUnqualified(string symbol)
    {
        auto errorCount = analyzePass1();
        if (errorCount > 0)
        {
            // ???
        }
        if (state < State.pass1Done)
        {
            from!"std.stdio".writefln("PossibleError: circular reference?");
            throw quit;
        }

        return symbolTable.tryGet(symbol);
    }
    void scopeDescriptionFormatter(StringSink sink) const
    {
        sink("module ");
        sink(importName);
    }
    void dumpSymbols() const
    {
        symbolTable.dump();
    }
    //
    // IReadonlyScope Functions
    //
    @property final inout(IReadonlyScope) getParent() inout { return cast(inout(IReadonlyScope)) UniversalScope.instance; }
    @property final inout(Module) asModule() inout { return this; }
    @property final inout(IScope) asWriteable() inout { return this; }
    @property final inout(JumpBlock) asJumpBlock() inout { return null; }
    //
    // IScope Functions
    //
    void add(const(string) symbol, SemanticNode node)
    {
        auto existing = symbolTable.checkedAdd(symbol, node);
        if (existing)
        {
            from!"std.stdio".writefln("Error: %s already has a definition for symbol '%s'",
                /*formatLocation(assignment.symbol), */filename, symbol);
            throw quit;
        }
    }
    void evaluated(const(string) symbol, SemanticNode node)
    {
        if (symbolTable.update(symbol, node).failed)
        {
            from!"std.stdio".writefln("Error: CodeBug: attempted to update symbol '%s' but it does not exist!", symbol);
            throw quit;
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
                from!"std.stdio".writefln("Error: module '%s' exists at '%s' and '%s'", importName, moduleFileName, filename);
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
            from!"std.stdio".writefln("Error: cannot import '%s' because there are no include paths", importName);
            throw quit;
        }
        from!"std.stdio".writefln("Error: import \"%s\" is not found in any of the following include paths:", importName);
        foreach (i, importPath; global.importPaths.data)
        {
            from!"std.stdio".writefln("[%s] %s", i, importPath.formatDir);
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
                from!"std.stdio".writefln("Error: file '%s' was imported with 2 different names '%s' and '%s'",
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

struct BuiltinSymbol
{
    string symbol;
    SemanticNode node;

    static import builtin;
    import types;
    __gshared static immutable values = [
        immutable BuiltinSymbol("u32", UnsignedFixedWidthType!32.instance),
        immutable BuiltinSymbol("leftIsLess", builtin.leftIsLessFunction.instance),
        immutable BuiltinSymbol("length", builtin.lengthFunction.instance),
    ];
}

class UniversalScope : IReadonlyScope
{
    mixin singleton;
    //
    // IDotQualifiable Functions
    //
    SemanticNode tryGetUnqualified(string symbol)
    {
        //from!"std.stdio".writefln("Universal.tryGetUnqualified(\"%s\")", symbol);
        foreach (ref builtinSymbol; BuiltinSymbol.values)
        {
            if (builtinSymbol.symbol == symbol)
                return builtinSymbol.node.unconst;
        }
        return null;
    }
    void scopeDescriptionFormatter(StringSink sink) const { sink("global scope"); }
    void dumpSymbols() const
    {
        assert(0, "UniversalScope.dumpSymbols not impelemented");
    }
    //
    // IReadonlyScope Functions
    //
    @property final inout(IReadonlyScope) getParent() inout { return null; }
    // TODO: probably return a pseudo universal-module
    @property final inout(Module) asModule() inout { assert(0, "not implemented"); }
    @property final inout(IScope) asWriteable() inout { return null; }
    @property final inout(JumpBlock) asJumpBlock() inout { return null; }
}