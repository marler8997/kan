module ffi;

import std.format : format;
import std.file : exists;

import common : quit;
import log;
import ctypes;
import funcload : LibraryHandle, loadLibrary, loadProc;

/** Filename of the ffi runtime library */
private __gshared string filename;

/** Handle to the ffi library */
private __gshared LibraryHandle handle;

void setFilename(string filename)
{
    if (handle.value != 0)
        assert(0, "ffi.setFilename was called after ffi.init was called");
    ffi.filename = filename;
}

void init()
{
    if (handle.value != 0)
        assert(0, "ffi.init was called more than once!");

    if (filename.length == 0)
        assert(0, "ffi.filename has not been set");

    if (!exists(filename))
    {
        errorfNoLocation("libffi '%s' does not exist", filename);
        throw quit;
    }
    handle = loadLibrary(filename);

    // Load functions
    prep_cif = cast(typeof(prep_cif))loadProc(handle, "ffi_prep_cif");
    prep_cif_var = cast(typeof(prep_cif_var))loadProc(handle, "ffi_prep_cif_var");
    call = cast(typeof(call))loadProc(handle, "ffi_call");
}

// Specify which architecture libffi is configured for.
//#ifndef X86_WIN32
//#define X86_WIN32
//#endif

version (X86)
{
    public import ffi.i686_pc_mingw32;
}
else version (X86_64)
{
    static assert(0, "not implemented");
}
else static assert(0, "not implemented");


/* ---- System configuration information --------------------------------- */
/+
#ifndef LIBFFI_ASM

#if defined(_MSC_VER) && !defined(__clang__)
#define __attribute__(X)
#endif

#include <stddef.h>
#include <limits.h>

/* LONG_LONG_MAX is not always defined (not if STRICT_ANSI, for example).
   But we can find it either under the correct ANSI name, or under GNU
   C's internal name.  */

#define FFI_64_BIT_MAX 9223372036854775807

#ifdef LONG_LONG_MAX
# define FFI_LONG_LONG_MAX LONG_LONG_MAX
#else
# ifdef LLONG_MAX
#  define FFI_LONG_LONG_MAX LLONG_MAX
#  ifdef _AIX52 /* or newer has C99 LLONG_MAX */
#   undef FFI_64_BIT_MAX
#   define FFI_64_BIT_MAX 9223372036854775807LL
#  endif /* _AIX52 or newer */
# else
#  ifdef __GNUC__
#   define FFI_LONG_LONG_MAX __LONG_LONG_MAX__
#  endif
#  ifdef _AIX /* AIX 5.1 and earlier have LONGLONG_MAX */
#   ifndef __PPC64__
#    if defined (__IBMC__) || defined (__IBMCPP__)
#     define FFI_LONG_LONG_MAX LONGLONG_MAX
#    endif
#   endif /* __PPC64__ */
#   undef  FFI_64_BIT_MAX
#   define FFI_64_BIT_MAX 9223372036854775807LL
#  endif
# endif
#endif
+/
/* The closure code assumes that this works on pointers, i.e. a size_t	*/
/* can hold a pointer.							*/

extern (C) struct Type
{
    size_t size;
    c_ushort alignment;
    TypeId type;
    Type** elements;
}

/+
#ifndef LIBFFI_HIDE_BASIC_TYPES
#if SCHAR_MAX == 127
# define Type_uchar                Type_uint8
# define Type_schar                Type_sint8
#else
 #error "char size not supported"
#endif

#if SHRT_MAX == 32767
# define Type_ushort       Type_uint16
# define Type_sshort       Type_sint16
#elif SHRT_MAX == 2147483647
# define Type_ushort       Type_uint32
# define Type_sshort       Type_sint32
#else
 #error "short size not supported"
#endif

#if INT_MAX == 32767
# define Type_uint         Type_uint16
# define Type_sint         Type_sint16
#elif INT_MAX == 2147483647
# define Type_uint         Type_uint32
# define Type_sint         Type_sint32
#elif INT_MAX == 9223372036854775807
# define Type_uint         Type_uint64
# define Type_sint         Type_sint64
#else
 #error "int size not supported"
#endif

#if LONG_MAX == 2147483647
# if FFI_LONG_LONG_MAX != FFI_64_BIT_MAX
 #error "no 64-bit data type supported"
# endif
#elif LONG_MAX != FFI_64_BIT_MAX
 #error "long size not supported"
#endif

#if LONG_MAX == 2147483647
# define Type_ulong        Type_uint32
# define Type_slong        Type_sint32
#elif LONG_MAX == FFI_64_BIT_MAX
# define Type_ulong        Type_uint64
# define Type_slong        Type_sint64
#else
 #error "long size not supported"
#endif

/* Need minimal decorations for DLLs to works on Windows. */
/* GCC has autoimport and autoexport.  Rely on Libtool to */
/* help MSVC export from a DLL, but always declare data   */
/* to be imported for MSVC clients.  This costs an extra  */
/* indirection for MSVC clients using the static version  */
/* of the library, but don't worry about that.  Besides,  */
/* as a workaround, they can define FFI_BUILDING if they  */
/* *know* they are going to link with the static library. */
#if defined _MSC_VER && !defined FFI_BUILDING
#define FFI_EXTERN extern __declspec(dllimport)
#else
#define FFI_EXTERN extern
#endif

/* These are defined in types.c */
FFI_EXTERN Type Type_void;
FFI_EXTERN Type Type_uint8;
FFI_EXTERN Type Type_sint8;
FFI_EXTERN Type Type_uint16;
FFI_EXTERN Type Type_sint16;
FFI_EXTERN Type Type_uint32;
FFI_EXTERN Type Type_sint32;
FFI_EXTERN Type Type_uint64;
FFI_EXTERN Type Type_sint64;
FFI_EXTERN Type Type_float;
FFI_EXTERN Type Type_double;
FFI_EXTERN Type Type_pointer;

#if 1
FFI_EXTERN Type Type_longdouble;
#else
#define Type_longdouble Type_double
#endif

#ifdef FFI_TARGET_HAS_COMPLEX_TYPE
FFI_EXTERN Type Type_complex_float;
FFI_EXTERN Type Type_complex_double;
#if 1
FFI_EXTERN Type Type_complex_longdouble;
#else
#define Type_complex_longdouble Type_complex_double
#endif
#endif
#endif /* LIBFFI_HIDE_BASIC_TYPES */
+/
extern (C) enum Status
{
    ok = 0,
    badTypedef,
    badAbi,
}

//typedef unsigned FFI_TYPE;

extern (C) struct Cif
{
    Abi abi;
    c_unsigned nargs;
    Type **arg_types;
    Type *rtype;
    c_unsigned bytes;
    c_unsigned flags;
    /*
    static if (FFI_EXTRA_CIF_FIELDS)
    {
        mixin(FFI_EXTRA_CIF_FIELDS);
    }
    */
}

/+
#if 0
/* Used to adjust size/alignment of ffi types.  */
void ffi_prep_types (Abi abi);
#endif
+/


/+
/* Used internally, but overridden by some architectures */
Status ffi_prep_cif_core(Cif *cif,
			     Abi abi,
			     unsigned int isvariadic,
			     unsigned int nfixedargs,
			     unsigned int ntotalargs,
			     Type *rtype,
			     Type **atypes);
+/
// ---- Definitions for the raw API --------------------------------------
/+
private enum FFI_SIZEOF_ARG = c_long.sizeof;

extern (C) union ffi_raw
{
    ffi_sarg  sint;
    ffi_arg   uint_;
    float	    flt;
    char[FFI_SIZEOF_ARG] data;
    void*     ptr;
}

extern (C) void ffi_raw_call (Cif *cif,
		   void (*fn)(void),
		   void *rvalue,
		   ffi_raw *avalue);

extern (C) void ffi_ptrarray_to_raw (Cif *cif, void **args, ffi_raw *raw);
extern (C) void ffi_raw_to_ptrarray (Cif *cif, ffi_raw *raw, void **args);
extern (C) size_t ffi_raw_size (Cif *cif);

/* This is analogous to the raw API, except it uses Java parameter	*/
/* packing, even on 64-bit machines.  I.e. on 64-bit machines		*/
/* longs and doubles are followed by an empty 64-bit word.		*/

extern (C) void ffi_java_raw_call (Cif *cif,
			void (*fn)(void),
			void *rvalue,
			ffi_java_raw *avalue);

extern (C) void ffi_java_ptrarray_to_raw (Cif *cif, void **args, ffi_java_raw *raw);
extern (C) void ffi_java_raw_to_ptrarray (Cif *cif, ffi_java_raw *raw, void **args);
extern (C) size_t ffi_java_raw_size (Cif *cif);
+/
/* ---- Definitions for closures ----------------------------------------- */
/+
#if FFI_CLOSURES

#ifdef _MSC_VER
__declspec(align(8))
#endif
typedef struct {
#if 0
  void *trampoline_table;
  void *trampoline_table_entry;
#else
  char tramp[FFI_TRAMPOLINE_SIZE];
#endif
  Cif   *cif;
  void     (*fun)(Cif*,void*,void**,void*);
  void      *user_data;
#ifdef __GNUC__
} ffi_closure __attribute__((aligned (8)));
#else
} ffi_closure;
# ifdef __sgi
#  pragma pack 0
# endif
#endif

void *ffi_closure_alloc (size_t size, void **code);
void ffi_closure_free (void *);

Status
ffi_prep_closure (ffi_closure*,
		  Cif *,
		  void (*fun)(Cif*,void*,void**,void*),
		  void *user_data);

Status
ffi_prep_closure_loc (ffi_closure*,
		      Cif *,
		      void (*fun)(Cif*,void*,void**,void*),
		      void *user_data,
		      void*codeloc);

#ifdef __sgi
# pragma pack 8
#endif
typedef struct {
#if 0
  void *trampoline_table;
  void *trampoline_table_entry;
#else
  char tramp[FFI_TRAMPOLINE_SIZE];
#endif
  Cif   *cif;

#if !FFI_NATIVE_RAW_API

  /* if this is enabled, then a raw closure has the same layout
     as a regular closure.  We use this to install an intermediate
     handler to do the transaltion, void** -> ffi_raw*. */

  void     (*translate_args)(Cif*,void*,void**,void*);
  void      *this_closure;

#endif

  void     (*fun)(Cif*,void*,ffi_raw*,void*);
  void      *user_data;

} ffi_raw_closure;

typedef struct {
#if 0
  void *trampoline_table;
  void *trampoline_table_entry;
#else
  char tramp[FFI_TRAMPOLINE_SIZE];
#endif

  Cif   *cif;

#if !FFI_NATIVE_RAW_API

  /* if this is enabled, then a raw closure has the same layout
     as a regular closure.  We use this to install an intermediate
     handler to do the transaltion, void** -> ffi_raw*. */

  void     (*translate_args)(Cif*,void*,void**,void*);
  void      *this_closure;

#endif

  void     (*fun)(Cif*,void*,ffi_java_raw*,void*);
  void      *user_data;

} ffi_java_raw_closure;

Status
ffi_prep_raw_closure (ffi_raw_closure*,
		      Cif *cif,
		      void (*fun)(Cif*,void*,ffi_raw*,void*),
		      void *user_data);

Status
ffi_prep_raw_closure_loc (ffi_raw_closure*,
			  Cif *cif,
			  void (*fun)(Cif*,void*,ffi_raw*,void*),
			  void *user_data,
			  void *codeloc);

Status
ffi_prep_java_raw_closure (ffi_java_raw_closure*,
		           Cif *cif,
		           void (*fun)(Cif*,void*,ffi_java_raw*,void*),
		           void *user_data);

Status
ffi_prep_java_raw_closure_loc (ffi_java_raw_closure*,
			       Cif *cif,
			       void (*fun)(Cif*,void*,ffi_java_raw*,void*),
			       void *user_data,
			       void *codeloc);

#endif /* FFI_CLOSURES */
+/
/* ---- Public interface definition -------------------------------------- */

__gshared extern (C) Status function(
    Cif *cif,
    Abi abi,
    c_unsigned nargs,
    Type *rtype,
    Type **atypes) prep_cif;

__gshared extern (C) Status function(
    Cif *cif,
    Abi abi,
    c_unsigned nfixedargs,
    c_unsigned ntotalargs,
    Type *rtype,
    Type **atypes) prep_cif_var;

__gshared extern (C) void function(
    Cif *cif,
    void* fn, //void function(void) fn,
    void *rvalue,
    void **avalue) call;

// ---- Definitions shared with assembly code ----------------------------

/* If these change, update src/mips/ffitarget.h. */
enum TypeId : c_ushort
{
    void_     = 0,
    int_      = 1,
    float_    = 2,
    double_   = 3,
    /*
#if 1
    longDouble 4
#else
    longDouble DOUBLE
#endif
    */
    u8        =  5,
    s8        =  6,
    u16       =  7,
    s16       =  8,
    u32       =  9,
    s32       = 10,
    u64       = 11,
    s64       = 12,
    struct_   = 13,
    ptr       = 14,
    complex   = 15,
}