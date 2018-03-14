module symtab;

import more.alloc : GCDoubler;
import more.builder;

import common : from;
import typecons : Rebindable, rebindable;
import semantics : TypedValue;

struct SymbolTable
{
    private struct Entry
    {
        string name;
        Rebindable!TypedValue obj;
    }
    Builder!(Entry,GCDoubler!16) table;
    const(TypedValue) get(string name)
    {
        //from!"std.stdio".writefln("get '%s'", name);
        foreach (entry; table.data)
        {
            if (entry.name == name)
            {
                return entry.obj;
            }
        }
        return TypedValue.nullValue;
    }
    const(TypedValue) checkedAdd(string name, const(TypedValue) obj)
    {
        auto existing = get(name);
        if (existing.isNull)
        {
            table.append(Entry(name, rebindable(obj)));
            return TypedValue.nullValue; // successfully added
        }
        return existing; // did not add, already an entry with this name
    }
    /*
    void dump()
    {
        foreach (pair; table.data)
        {
            from!"std.stdio".writefln("%s", pair.name);
        }
    }
    */
}
