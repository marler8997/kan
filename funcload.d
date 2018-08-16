module funcload;

import more.alloc : GCDoubler;
import more.builder : Builder;

import common : from;
import semantics : UserDefinedFunction;

version (Windows)
{
    alias HMODULE = uint;
    extern (Windows) uint GetLastError() nothrow @nogc;
    extern (Windows) HMODULE LoadLibraryA(const(char)* fileName) nothrow @nogc;
    extern (Windows) void* GetProcAddress(HMODULE mod, const(char)* name) nothrow @nogc;

    struct Proc
    {
        string name;
        void* addr;
    }
    struct Library
    {
        string fileName;
        HMODULE handle;
        Builder!(Proc, GCDoubler!16) procs;
        Proc* getProc(string name)
        {
            foreach (ref loadedProc; procs.data)
            {
                if (loadedProc.name == name)
                    return &loadedProc;
            }
            import std.internal.cstring : tempCString;
            auto cstr = tempCString(name);
            auto addr = GetProcAddress(handle, cstr);
            if (addr is null)
            {
                assert(0, from!"std.format".format("GetProcAddress(<%s>, \"%s\") failed (e=%s)",
                    fileName, name, GetLastError()));
            }
            procs.append(Proc(name, addr));
            return &procs.data[$-1];
        }
    }
    __gshared Builder!(Library, GCDoubler!8) libraries;

    Library* getLibrary(string fileName)
    {
        foreach (ref loadedLibrary; libraries.data)
        {
            if (loadedLibrary.fileName == fileName)
                return &loadedLibrary;
        }
        import std.internal.cstring : tempCString;
        auto cstr = tempCString(fileName);
        auto handle = LoadLibraryA(cstr);
        if (handle == 0)
        {
            assert(0, from!"std.format".format("LoadLibrary(\"%s\") failed (e=%s)", fileName, GetLastError()));
        }
        libraries.append(Library(fileName, handle));
        return &libraries.data[$-1];
    }

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