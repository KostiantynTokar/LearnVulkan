module util;

int strcmp(in string str, in char* cStr) pure nothrow @nogc @trusted
{
    import core.stdc.string : strncmp;

    auto res = strncmp(&str[0], cStr, str.length);
    if(res != 0) return res;
    return cStr[str.length];
}
