module ffi.i686_pc_mingw32;

import ctypes : c_int;

private enum ffi_abi_mixin = function() {
    string src = "enum Abi : c_int {\n";
    src ~= "    first = 0,\n";
    string defaultAbi;
    version (X86) // #ifdef X86_WIN32
    {
        src ~= "    sysv,\n";
        src ~= "    stdcall,\n";
        src ~= "    thiscall,\n";
        src ~= "    fastcall,\n";
        src ~= "    msCdecl,\n";
        src ~= "    pascal,\n";
        src ~= "    register,\n";
        version (_MSC_VER) // #ifdef _MSC_VER
            defaultAbi = "msCdecl";
        else
            defaultAbi = "sysv";
    }
    else version (X86_64) // #if defined(X86_WIN64)
    {
        src ~= "    win64,\n";
        src ~= "    stdcall,\n";
        defaultAbi = "win64";
    }
    /*
    else version (intel x86 and amd x64??)
    {
        src ~= "    sysv,\n";
        src ~= "    unix64,\n";
        src ~= "    thiscall,\n";
        src ~= "    fastcall,\n";
        src ~= "    stdcall,\n";
        src ~= "    pascal,\n";
        src ~= "    register,\n";
        #ifdef defined(__i386__) || defined(__i386)
            defaultAbi = "sysv";
        else
            defaultAbi = "unix64";
    }
    */
    else static assert(0, "not impl");
    src ~= "    lastAbi,\n";
    src ~= "    defaultAbi = " ~ defaultAbi ~ ",\n";
    src ~= "}\n";
    return src;
}();
//pragma(msg, ffi_abi_mixin);
mixin(ffi_abi_mixin);

/* ---- Definitions for closures ----------------------------------------- */

private enum FFI_CLOSURES = 1;
//private enum FFI_TYPE_SMALL_STRUCT_1B = (FFI_TYPE_LAST + 1);
//private enum FFI_TYPE_SMALL_STRUCT_2B = (FFI_TYPE_LAST + 2);
//private enum FFI_TYPE_SMALL_STRUCT_4B = (FFI_TYPE_LAST + 3);
//private enum FFI_TYPE_MS_STRUCT       = (FFI_TYPE_LAST + 4);

version (X86_64) // #if defined (X86_64) || (defined (__x86_64__) && defined (X86_DARWIN))
{
    private enum FFI_TRAMPOLINE_SIZE = 24;
    private enum FFI_NATIVE_RAW_API = 0;
}
else version (X86) // #ifdef X86_WIN32
{
    private enum FFI_TRAMPOLINE_SIZE = 52;
}
/*
else version (???) // #ifdef X86_WIN64
{
    private enum FFI_TRAMPOLINE_SIZE = 29;
    private enum FFI_NATIVE_RAW_API = 0;
    private enum FFI_NO_RAW_API = 1;
}
else
{
    private enum FFI_TRAMPOLINE_SIZE = 10;
}
*/

/*
#ifndef X86_WIN64
private enum FFI_NATIVE_RAW_API 1  // x86 has native raw api support
#endif
*/