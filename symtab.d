module symtab;

import more.alloc : GCDoubler;
import more.builder;

import common : from, passfail;
import typecons : Rebindable, rebindable;
import semantics : SemanticNode;

struct SymbolTable
{
    private struct Entry
    {
        string name;
        SemanticNode node;
    }
    Builder!(Entry,GCDoubler!16) table;

    private struct EntryReference
    {
        static EntryReference nullValue() { return EntryReference(index.max); }
        private size_t index;
        bool isNull() const { return index == index.max; }
        SemanticNode getNode(SymbolTable* symtab) const
        {
            return symtab.table.data[index].node;
        }
        void update(SymbolTable* symtab, SemanticNode node) const
        {
            symtab.table.data[index].node = node;
        }
    }

    EntryReference tryGetRef(string name)
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

    SemanticNode tryGet(string name)
    {
        auto entryRef = tryGetRef(name);
        return entryRef.isNull ? null : entryRef.getNode(&this);
    }
    passfail update(string name, SemanticNode node)
    {
        auto entryRef = tryGetRef(name);
        if (entryRef.isNull)
            return passfail.fail;
        entryRef.update(&this, node);
        return passfail.fail;
    }
    /**
    Returns: null if successfully added, otherwise, the current entry.
    */
    SemanticNode checkedAdd(string name, SemanticNode obj)
    {
        auto entryRef = tryGetRef(name);
        if (entryRef.isNull)
        {
            table.append(Entry(name, obj));
            return null; // successfully added
        }
        return entryRef.getNode(&this); // did not add, already an entry with this name
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
