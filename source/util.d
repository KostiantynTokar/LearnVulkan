module util;

import from : from;

bool implies(in bool antecedent, in bool consequent) pure nothrow @nogc @safe
{
    return (!antecedent) || consequent;
}

struct StaticAlignedMallocator(uint alignment_)
{
    import std.experimental.allocator : forwardToMember;
    import std.experimental.allocator.mallocator : Mallocator;

    enum uint alignment = alignment_;
    mixin(forwardToMember("Mallocator.instance",
        "goodAllocSize",
        "allocate",
        "alignedAllocate",
        "allocateAll",
        "expand",
        "reallocate",
        "alignedReallocate",
        "owns",
        "resolveInternalPointer",
        "deallocate",
        "deallocateAll",
        "empty"
        ));
    static StaticAlignedMallocator instance;
}

template TupleCat(Ts...)
    if(from!"std.meta".allSatisfy!(from!"std.typecons".isTuple, Ts))
{
    import std.meta : AliasSeq;
    import std.typecons : Tuple;
    alias A = AliasSeq!();
    static foreach(T; Ts)
    {
        static foreach(i; 0 .. T.Types.length)
        {
            static if(T.fieldNames[i] == "")
            {
                A = AliasSeq!(A, T.Types[i]);
            }
            else
            {
        		A = AliasSeq!(A, T.Types[i], T.fieldNames[i]);
            }
        }
    }
    alias TupleCat = Tuple!A;
}

template TupleErase(T, memberNames...)
{
    enum remaining = calcRemaining!(T, memberNames)();
    alias TupleErase = TupleEraseImpl!(T, remaining);
}

template erase(memberNames...)
    if(from!"std.meta".allSatisfy!(from!"std.traits".isSomeString, typeof(memberNames)))
{
    import core.lifetime : forward;

    auto ref erase(T)(auto ref T t)
        if(from!"std.typecons".isTuple!T)
    {
        static auto genCode(in bool[T.Types.length] remaining)
        {
            import std.conv : to;
            string[] args;
            foreach(i, remains; remaining)
            {
                if(remains) args ~= ["forward!t[" ~ i.to!string ~ "]"];
            }
            auto argsStr = "";
            if(args.length != 0)
            {
                argsStr ~= args[0];
                args = args[1 .. $];
            }
            foreach(arg; args)
                argsStr ~= ", " ~ arg;
            return q{return TupleEraseImpl!(T, remaining)(} ~ argsStr ~ ");";
        }
        enum remaining = calcRemaining!(T, memberNames)();
        mixin(genCode(remaining));
    }
}

void println(in string str) nothrow @nogc @safe
{
    import std.experimental.allocator : makeArray, dispose;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.stdio : printf;

    auto cStr = Mallocator.instance.makeArray!char(str.length + 2);
    scope(exit) () @trusted { Mallocator.instance.dispose(cStr); }();
    cStr[0 .. str.length] = str[];
    cStr[str.length] = '\n';
    cStr[str.length + 1] = '\0';
    () @trusted { printf(cStr.ptr); }();
}

int strcmp(in string str, in char* cStr) pure nothrow @nogc @trusted
{
    import core.stdc.string : strncmp;

    auto res = strncmp(&str[0], cStr, str.length);
    if(res != 0) return res;
    return cStr[str.length];
}

private template calcRemaining(T, memberNames...)
    if(from!"std.typecons".isTuple!T)
{
    bool[T.Types.length] calcRemaining()
    {
        bool[T.Types.length] res;
        static foreach(i; 0 .. T.Types.length)
        {
            res[i] = true;
            foreach(name; memberNames)
            {
                if(T.fieldNames[i] == name)
                {
                    res[i] = false;
                    break;
                }
            }
        }
        return res;
    }
}

private template TupleEraseImpl(T, alias remaining)
{
    import std.typecons : Tuple;
    import std.meta : AliasSeq;

    alias A = AliasSeq!();
    static foreach(i; 0 .. T.Types.length)
    {
        static if(remaining[i])
        {
            static if(T.fieldNames[i] == "")
            {
                A = AliasSeq!(A, T.Types[i]);
            }
            else
            {
                A = AliasSeq!(A, T.Types[i], T.fieldNames[i]);
            }
        }
    }
    alias TupleEraseImpl = Tuple!A;
}
