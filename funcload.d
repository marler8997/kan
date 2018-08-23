module funcload;

import std.internal.cstring : tempCString;
import std.format : format;
import std.file : exists;

import more.alloc : GCDoubler;
import more.builder : Builder;

import common : from, uarray,  quit;
static import global;
import log;
import syntax : SyntaxNode;
import semantics : formatLocation, SemanticNode, FunctionParameter, UserDefinedFunction;
import types : IType, IFixedStorageSizeType;
static import ffi;
import interpreter : Interpreter;

version (Windows)
{
    alias HMODULE = uint;
    extern (Windows) uint GetLastError() nothrow @nogc;
    extern (Windows) HMODULE LoadLibraryA(const(char)* fileName) nothrow @nogc;
    extern (Windows) void* GetProcAddress(HMODULE mod, const(char)* name) nothrow @nogc;
}

struct LibraryHandle
{
    version (Windows)
    {
        HMODULE value;
    }
    else static assert(0, "LibraryHandle not implemented on this platform");
}

// Prints error on error
LibraryHandle loadLibrary(const(char)[] name)
{
    auto nameCString = tempCString(name);
    return loadLibraryCString(nameCString);
}
LibraryHandle loadLibraryCString(const(char)* nameCString)
{
    version (Windows)
    {
        auto result = LoadLibraryA(nameCString);
        if (result == 0)
        {
            errorfNoLocation("LoadLibrary(\"%s\") failed (e=%s)",
                nameCString[0 .. from!"core.stdc.string".strlen(nameCString)], GetLastError());
            throw quit;
        }
        return LibraryHandle(result);
    }
    else static assert(0, "loadLibrary not implemented on this platform");
}

void* loadProc(LibraryHandle libHandle, const(char)[] name)
{
    auto nameCString = tempCString(name);
    return loadProcCString(libHandle, nameCString);
}
void* loadProcCString(LibraryHandle libHandle, const(char)* nameCString)
{
    version (Windows)
    {
        auto result = GetProcAddress(libHandle.value, nameCString);
        if (result is null)
        {
            errorfNoLocation("GetProcAddress(\"%s\") failed (e=%s)",
                nameCString[0 .. from!"core.stdc.string".strlen(nameCString)], GetLastError());
            throw quit;
        }
        return result;
    }
    else static assert(0, "loadLibrary not implemented on this platform");
}

struct Proc
{
    string name;
    void* addr;
}
struct Library
{
    string fileName;
    LibraryHandle handle;
    Builder!(Proc, GCDoubler!16) procs;
    Proc getProc(string name)
    {
        foreach (ref loadedProc; procs.data)
        {
            if (loadedProc.name == name)
                return loadedProc;
        }
        auto addr = loadProc(handle, name);
        procs.append(Proc(name, addr));
        return procs.data[$-1];
    }
}
private __gshared Builder!(Library, GCDoubler!8) libraries;


Library* getLibrary(string fileName)
{
    foreach (ref loadedLibrary; libraries.data)
    {
        if (loadedLibrary.fileName == fileName)
            return &loadedLibrary;
    }
    auto handle = loadLibrary(fileName);
    libraries.append(Library(fileName, handle));
    return &libraries.data[$-1];
}


auto loadExternFunc(UserDefinedFunction func)
in { assert(func.isExternFunction); } do
{
    version (Windows)
    {
        string libraryFileName = "kernel32.dll"; // for now just assume all functions come from kernel32.dll
        Library *library = getLibrary(libraryFileName);
        return library.getProc(func.externName);
    }
    else static assert(0, "loadFunc not implemented on this platform");
}


void callExternFunc(Interpreter* interpreter, Proc proc, const(SyntaxNode)* funcSyntaxNode,
    IType returnType, uarray!FunctionParameter params, uarray!SemanticNode runtimeArgs)
{
    import core.stdc.stdlib : alloca;
    ffi.init();

    auto ffiTypeBuffers = cast(ffi.Type*)alloca(ffi.Type.sizeof * (1 + params.length));
    auto ffiArgTypes = cast(ffi.Type**)alloca((ffi.Type*).sizeof * params.length);
    setFfiType(returnType, &ffiTypeBuffers[0], funcSyntaxNode);
    size_t totalArgSize = 0;
    foreach (i; 0 .. params.length)
    {
        auto ffiTypeBuffer = &ffiTypeBuffers[i + 1];
        ffiArgTypes[i] = ffiTypeBuffer;
        totalArgSize += setFfiType(params[i].type, ffiTypeBuffer, params[i].getSyntaxNode);
    }

    ffi.Cif cif;
    {
        auto result = ffi.prep_cif(&cif, ffi.Abi.defaultAbi, params.length, &ffiTypeBuffers[0], ffiArgTypes);
        if (result != ffi.Status.ok)
        {
            errorf("failed to prepare ffi call to '%s', result=%s", proc.name, result);
            throw quit;
        }
    }

    auto returnValuePtr = cast(void*)alloca(30);
    auto ffiArgsBuffer = alloca(totalArgSize);
    auto ffiArgPtrs = cast(void**)alloca((void*).sizeof * params.length);
    size_t argOffset = 0;
    foreach (i; 0 .. params.length)
    {
        auto ffiArgBuffer = cast(ubyte*)ffiArgsBuffer + argOffset;
        ffiArgPtrs[i] = ffiArgBuffer;
        auto argSize = ffiArgTypes[i].size;
        setFfiValue(interpreter, params[i].type, runtimeArgs[i], ffiArgBuffer[0 .. argSize], params[i].getSyntaxNode);
        argOffset += argSize;
    }

    ffi.call(&cif, proc.addr, returnValuePtr, ffiArgPtrs);

    from!"std.stdio".writefln("[DEBUG] return value from extern function not implemented");
}

private auto setFfiType(IType kanType, ffi.Type* ffiType, const(SyntaxNode)* errorLocationNode)
{
    auto storeType = cast(IFixedStorageSizeType)kanType;
    if (!storeType)
    {
        // TODO: this should be checked during semantic analysis
        errorf(errorLocationNode.formatLocation,
            "all types for extern functions must implement IFixedStorageSizeType, but '%s' does not", kanType);
        throw quit;
    }

    auto size = storeType.getStorageSize();
    ffiType.size = size;
    if (size > size_t.sizeof)
        ffiType.alignment = size_t.sizeof;
    else
        ffiType.alignment = cast(ushort)size;

    if (size == 1)
        ffiType.type = ffi.TypeId.u8;
    else if (size == 2)
        ffiType.type = ffi.TypeId.u16;
    else if (size == 4)
        ffiType.type = ffi.TypeId.u32;
    else if (size == 8)
        ffiType.type = ffi.TypeId.u64;
    else
        assert(0, format("extern parameter of size %s is not implemented", size));

    ffiType.elements = null;
    return size;
}
private auto setFfiValue(Interpreter* interpreter, IType kanType,
    SemanticNode node, ubyte[] storage, const(SyntaxNode)* errorLocationNode)
{
    interpreter.serialize(node, kanType, storage);
}