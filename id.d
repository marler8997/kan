module id;

import core.stdc.stdlib : malloc;
import std.array : Appender;

template IdTemplate(TableIndex, TableOffset, Length)
{
    struct Ptr
    {
        @property static Ptr nullValue() { return Ptr(TableIndex.max); }

        TableIndex tableIndex;
        TableOffset tableOffset = void;
        private this(TableIndex tableIndex) { this.tableIndex = tableIndex; }
        private this(TableIndex tableIndex, TableOffset tableOffset)
        {
            this.tableIndex = tableIndex;
            this.tableOffset = tableOffset;
        }
        @property bool isNull() { return tableIndex == TableIndex.max; }

    }
    struct Value
    {
        static Value nullValue() { return Value(TableIndex.max); }

        Ptr ptr;
        alias ptr this;
        Length length = void;

        private this(TableIndex tableIndex) { this.ptr.tableIndex = tableIndex; }
        private this(Ptr ptr, Length length)
        {
            this.ptr = ptr;
            this.length = length;
        }
        @property string toString() const
        {
            return tables.data[ptr.tableIndex].get(ptr.tableOffset)[0..length];
        }
    }

    private struct Table
    {
        char* buffer;
        TableOffset used;
        immutable(char)* get(TableOffset offset) { return cast(immutable(char)*)buffer + offset; }
        TableOffset tryAppend(const(char)[] str)
        {
            if (used + str.length + 1 <= TableOffset.max)
            {
                auto tableOffset = used;
                buffer[used .. used + str.length] = str[];
                used += str.length;
                buffer[used++] = '\0';
                return tableOffset;
            }
            return TableOffset.max;
        }
    }
    private Value[const(char)[]] lookupTable;
    private Appender!(Table[]) tables;

    Value tryGetExisting(const(char)[] idString)
    {
        return lookupTable.get(idString, Value.nullValue);
    }
    Value pool(const(char)[] idString)
    {
        auto existing = lookupTable.get(idString, Value.nullValue);
        if (!existing.isNull)
        {
            return existing;
        }

        // TODO: small optimization, have an index that points to the first
        //       table past all the completely full tables
        TableIndex tableIndex = 0;
        TableOffset tableOffset;
        for (;;)
        {
            if (tableIndex >= tables.data.length)
            {
                // string was NOT added
                assert(tableIndex < TableIndex.max, "table index type not large enough");

                // allocate table
                auto table = Table(cast(char*)malloc(TableOffset.max), 0);
                if (table.buffer is null)
                {
                    assert(0, "out of memory for string tables");
                }
                tables.put(table);
                tableOffset = table.tryAppend(idString);
                if (tableOffset == TableOffset.max)
                {
                    assert(0, "id string it too large for table!");
                }
                break;
            }
            tableOffset = tables.data[tableIndex].tryAppend(idString);
            if (tableOffset != TableOffset.max)
            {
                break;
            }
        }
        auto value = Value(Ptr(tableIndex, tableOffset), cast(TableIndex)idString.length);
        lookupTable[idString] = value;
        return value;

    }
}
alias Id = IdTemplate!(ushort, ushort, ushort);
unittest
{
    auto helloID = Id.pool("hello");
    import std.stdio;
    writefln("helloID is %s", helloID);
    auto anotherID = Id.pool("another");
    writefln("anotherID is %s", anotherID);
}
