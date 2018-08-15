module symtab;

import std.typecons : Flag, Yes, No;

import more.alloc : GCDoubler;
import more.builder;

import common : from, passfail;
import typecons : Rebindable, rebindable;
import semantics : IScope, SemanticNode, SymbolTableEntry;

struct SymbolAddResult
{
    SymbolTableEntry entry;
    Flag!"newEntryAdded" newEntryAdded;
}

struct SymbolTable
{
    Builder!(SymbolTableEntry,GCDoubler!16) table;
    SymbolTableEntry tryGet(string name)
    {
        foreach (entry; table.data)
        {
            if (entry.name == name)
            {
                //from!"std.stdio".writefln("get '%s' => exists", name);
                return entry;
            }
        }
        return null;
    }
    /**
    Returns: null if successfully added, otherwise, the current entry.
    */
    SymbolAddResult tryAdd(/*IScope setScope, */string name, SemanticNode obj)
    {
        auto existing = tryGet(name);
        if (existing !is null)
            return SymbolAddResult(existing, No.newEntryAdded);

        auto newEntry = new SymbolTableEntry(/*setScope,*/ name, obj);
        table.append(newEntry);
        return SymbolAddResult(newEntry, Yes.newEntryAdded);
    }
    void dump() const
    {
        from!"std.stdio".writeln("SymbolTable:");
        foreach (entry; table.data)
        {
            from!"std.stdio".writefln("  %s", entry.name);
        }
    }
}
