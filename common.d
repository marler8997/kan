module common;

import std.typecons : Flag, Yes, No;
import std.traits : Unqual;
import std.format : format;
import std.string : indexOf;

pragma(inline) final bool isNull(T)(const(T) class_)
    if ( is(T == class) || is(T == interface) )
{
    return class_ is null;
}

template from(string mod)
{
    mixin("import from = " ~ mod ~ ";");
}

struct ArrayTemplate(T, SizeType)
{
    @property static uarray!T nullValue() { return uarray!T(null, 0); }

    T* ptr;
    SizeType length;

    pragma(inline) @property auto ref last() const
        in { assert(length > 0); } do { return ptr[length - 1]; }

    pragma(inline) auto ref opIndex(SizeType index) inout
        in { assert(index < length, format("range violation %s >= %s", index, length)); } do
    {
        return ptr[index];
    }
    static if (size_t.sizeof != SizeType.sizeof)
    {
        pragma(inline) auto ref opIndex(size_t index) inout
            in { assert(index < length, format("range violation %s >= %s", index, length)); } do
        {
            return ptr[index];
        }
    }
    pragma(inline) SizeType opDollar() const
    {
        return length;
    }
    pragma(inline) auto ref opSlice(SizeType start, SizeType limit) inout
        in { assert(limit >= start, "slice range violation"); } do
    {
        return inout ArrayTemplate!(T,SizeType)(ptr + start, cast(SizeType)(limit - start));
    }

    pragma(inline) int opApply(scope int delegate(ref T element) dg) const
    {
        int result = 0;
        for (SizeType i = 0; i < length; i++)
        {
            result = dg(*cast(T*)&ptr[i]);
            if (result)
                break;
        }
        return result;
    }
    pragma(inline) int opApply(scope int delegate(SizeType index, ref T element) dg) const
    {
        int result = 0;
        for (SizeType i = 0; i < length; i++)
        {
            result = dg(i, *cast(T*)&ptr[i]);
            if (result)
                break;
        }
        return result;
    }

    //@property auto toArray() { return ptr[0..length]; }
    //alias toArray this;

    /*
    // range functions
    @property bool empty() { return length == 0; }
    @property auto front() { return *ptr; }
    void popFront() {
        ptr++;
        length--;
    }
    */
}
pragma(inline) @property auto toUarray(T)(T[] array)
    in {
        static if (size_t.sizeof != uint.sizeof)
        {
            assert(array.length <= uint.max,
                format("array length %s exceeded uint.max %s", array.length, uint.max));
        }
     } do
{
    return uarray!T(array.ptr, cast(uint)array.length);
}

alias uarray(T) = ArrayTemplate!(T, uint);
alias ushortarray(T) = ArrayTemplate!(T, ushort);

pragma(inline) uint toUint(size_t size)
{
    static if (size_t.sizeof != uint.sizeof)
    {
        assert(size <= uint.max, from!"std.format".
            format("size %s is too large for uint (max=%s)", size, uint.max));
    }
    return cast(uint)size;
}

pragma(inline) immutable(T) toImmutable(T)(T obj)
{
    return cast(immutable(T))obj;
}

mixin template singleton(Flag!"ctor" ctor = Yes.ctor)
{
    __gshared immutable static instance = new immutable typeof(this)();
    static if (ctor)
    {
        private this() immutable {}
    }
}

class QuitException : Throwable { this() { super(null); }}
@property auto quit() { return new QuitException(); }

version(Windows)
{
    auto DirSeparatorChar = '\\';
    bool isDirSeparator(char c)
    {
        return c == '\\' || c == '/';
    }
}
else
{
    auto DirSeparatorChar = '/';
    bool isDirSeparator(char c)
    {
        return c == '/';
    }
}

inout(T)[] replaceAll(T)(inout(T)[] array, const(T) from, const(T) to)
{
    size_t i = 0;
    for (; ;)
    {
        if (i >= array.length)
        {
            return array;
        }
        if (array[i] == from)
        {
            break;
        }
        i++;
    }
    auto newArray = new Unqual!T[array.length];
    newArray[0..i] = array[0..i];
    newArray[i] = to;
    for (;;)
    {
        i++;
        if (i >= array.length)
        {
            return cast(inout(T)[])newArray;
        }
        newArray[i] = (array[i] == from) ? to : array[i];
    }
}

@property auto formatDir(const(char)[] dir)
{
    if (dir.length == 0)
    {
        dir = ".";
    }
    return formatQuotedIfSpaces(dir);
}

// returns a formatter that will print the given string.  it will print
// it surrounded with quotes if the string contains any spaces.
@property auto formatQuotedIfSpaces(T...)(T args) if (T.length > 0)
{
    struct Formatter
    {
        T args;
        void toString(scope void delegate(const(char)[]) sink) const
        {
            bool useQuotes = false;
            foreach (arg; args)
            {
                if (arg.indexOf(' ') >= 0)
                {
                    useQuotes = true;
                    break;
                }
            }

            if (useQuotes)
            {
                sink("\"");
            }
            foreach (arg; args)
            {
                sink(arg);
            }
            if (useQuotes)
            {
                sink("\"");
            }
        }
    }
    return Formatter(args);
}