module symtab;

import more.alloc : GCDoubler;
import more.builder;

import common : from, passfail;
import typecons : Rebindable, rebindable;
import semantics : TypedValue;

struct SymbolTable
{
    private struct Entry
    {
        string name;
        TypedValue obj;
    }
    Builder!(Entry,GCDoubler!16) table;

    private struct EntryReference
    {
        static EntryReference nullValue() { return EntryReference(index.max); }
        private size_t index;
        bool isNull() const { return index == index.max; }
        TypedValue getObj(SymbolTable* symtab) const
        {
            return symtab.table.data[index].obj;
        }
        void update(SymbolTable* symtab, TypedValue obj) const
        {
            symtab.table.data[index].obj = obj;
        }
    }

    EntryReference tryGetEntryRef(string name)
    {
        foreach (i, entry; table.data)
        {
            if (entry.name == name)
            {
                //from!"std.stdio".writefln("get '%s' => exists", name);
                return EntryReference(i);
            }
        }
        //from!"std.stdio".writefln("get '%s' => NULL", name);
        return EntryReference.nullValue;
    }

    const(TypedValue) get(string name)
    {
        auto entryRef = tryGetEntryRef(name);
        if (entryRef.isNull)
            return TypedValue.nullValue;

        auto obj = entryRef.getObj(&this);
        auto unevaluated = obj.tryAsUnevaluatedSymbol;
        if (unevaluated)
        {
            auto evaluated = unevaluated.tryEvaluate();
            entryRef.update(&this, evaluated);
            return evaluated;
        }
        return obj;
    }
    passfail update(string name, TypedValue obj)
    {
        auto entryRef = tryGetEntryRef(name);
        if (entryRef.isNull)
            return passfail.fail;
        entryRef.update(&this, obj);
        return passfail.fail;
    }
    const(TypedValue) checkedAdd(string name, TypedValue obj)
    {
        auto entryRef = tryGetEntryRef(name);
        if (entryRef.isNull)
        {
            table.append(Entry(name, obj));
            return TypedValue.nullValue; // successfully added
        }
        return entryRef.getObj(&this); // did not add, already an entry with this name
    }
    void dump() const
    {
        from!"std.stdio".writeln("SymbolTable:");
        foreach (pair; table.data)
        {
            from!"std.stdio".writefln("  %s", pair.name);
        }
    }
}
