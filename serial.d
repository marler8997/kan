module serial;

string trySerializeNumber(const(char)[] num, ubyte[] storage)
{
    if (num.length == 0)
        return "an empty string is not a valid number";
    if (num[0] == '0')
    {
        if (num.length == 1)
        {
            storage[] = 0;
            return null; // success
        }
        if (num[1] == 'x')
            return trySerializeHex(num[2 .. $], storage);
    }
    return "not implemented";
}

ubyte tryHexToByte(char c)
{
    if (c <= '9')
    {
        if (c >= '0')
            return cast(ubyte)(c - '0');
    }
    else if (c >= 'a')
    {
        if (c <= 'f')
            return cast(ubyte)(c + 10 - 'a');
    }
    else if (c >= 'A')
    {
        if (c <= 'F')
            return cast(ubyte)(c + 10 - 'A');
    }
    return ubyte.max;
}

string trySerializeHex(const(char)[] hex, ubyte[] storage)
{
    if (storage.length * 2 < hex.length)
        return "hex value does not fit";

    version (BigEndian)
        size_t storageOffset = storage.length - 1;
    else
        size_t storageOffset = 0;

    size_t strOffset = hex.length;
    for (;;)
    {
        if (strOffset < 2)
            break;
        strOffset -= 2;
        auto higher = tryHexToByte(hex[strOffset + 0]);
        auto lower  = tryHexToByte(hex[strOffset + 1]);
        if (higher == higher.max || lower == lower.max)
            return "contains invalid hex characters";
        storage[storageOffset] = cast(ubyte)(higher << 4 | lower);
        version (BigEndian)
            storageOffset--;
        else
            storageOffset++;
    }
    if (strOffset == 1)
    {
        auto lower = tryHexToByte(hex[0]);
        if (lower == lower.max)
            return "contains invalid hex characters";
        storage[storageOffset] = lower;
        version (BigEndian)
            storageOffset--;
        else
            storageOffset++;
    }
    version (BigEndian)
        storage[0 .. storageOffset] = 0;
    else
        storage[storageOffset .. $] = 0;
    return null; // success
}

unittest
{
    ubyte[100] buffer;

    assert(null is trySerializeHex(null, null));
    assert(null is trySerializeHex("", buffer[0..0]));

    assert("hex value does not fit" == trySerializeHex("0", buffer[0..0]));
    assert(null == trySerializeHex("0", buffer[0..1]));
    assert(buffer[0] == 0);
    assert(null == trySerializeHex("00", buffer[0..1]));
    assert(buffer[0] == 0);
    assert("hex value does not fit" == trySerializeHex("000", buffer[0..1]));
    assert(null == trySerializeHex("1aF", buffer[0..2]));
    version (BigEndian)
        assert(buffer[0] == 0x1 && buffer[1] == 0xaF);
    else
        assert(buffer[1] == 0x1 && buffer[0] == 0xaF);
    assert(null == trySerializeHex("3eC6", buffer[0..2]));
    version (BigEndian)
        assert(buffer[0] == 0x3e && buffer[1] == 0xC6);
    else
        assert(buffer[1] == 0x3e && buffer[0] == 0xC6);

    static void test(T)(const(char)[] hex, T value)
    {
        ubyte[T.sizeof] buffer;
        assert(null is trySerializeHex(hex, buffer));
        assert(*cast(T*)buffer.ptr == value);
    }
    test("0", 0);
    test("00", 0);
    test("000", 0);
    test("123", 0x123);
    test("1234ABcdE9", 0x1234ABcdE9);

    assert("contains invalid hex characters" == trySerializeHex("/", buffer));
    assert("contains invalid hex characters" == trySerializeHex(":", buffer));
    assert("contains invalid hex characters" == trySerializeHex("@", buffer));
    assert("contains invalid hex characters" == trySerializeHex("G", buffer));
    assert("contains invalid hex characters" == trySerializeHex("`", buffer));
    assert("contains invalid hex characters" == trySerializeHex("g", buffer));
    assert("contains invalid hex characters" == trySerializeHex("0/", buffer));
    assert("contains invalid hex characters" == trySerializeHex("00/", buffer));
    assert("contains invalid hex characters" == trySerializeHex("000/", buffer));
    assert("contains invalid hex characters" == trySerializeHex("0000/", buffer));
    assert("contains invalid hex characters" == trySerializeHex("00000/", buffer));
}