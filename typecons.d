module typecons;

import std.traits : isDynamicArray, Unqual;

struct Rebindable(T)
{
    union
    {
        private T rebinder;
        const(T) const_;
        immutable(T) immutable_;
    }
    alias const_ this;
    @trusted pure nothrow @nogc
    {
        this(T initializer)
        {
            rebinder = initializer;
        }
        this(const(T) initializer)
        {
            const_ = initializer;
        }
        this(immutable(T) initializer)
        {
            immutable_ = initializer;
        }
        this(const(T) initializer) inout
        {
            const_ = cast(inout(const(T)))initializer;
        }
        void opAssign(T another)
        {
            rebinder = another;
        }
        void opAssign(const(T) another)
        {
            rebinder = cast(T)another;
        }
        void opAssign(Rebindable!T another)
        {
            rebinder = another.rebinder;
        }
    }
}
auto rebindable(T)(T value)
{
    return Rebindable!(Unqual!T)(value);
}