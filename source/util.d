module util;

import from : from;

public import core.lifetime : forward, move;
public import std.typecons : Tuple;

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
                static assert(false, "Empty field names are not supported.");
            }
            else
            {
        		A = AliasSeq!(A, T.Types[i], T.fieldNames[i]);
            }
        }
    }
    auto genSortedIndices()
    {
        import std.algorithm : makeIndex;
        string[] fieldNames;
        static foreach(T; Ts)
        {
            static foreach(i; 0 .. T.Types.length)
            {
                fieldNames ~= T.fieldNames[i];
            }
        }
        auto indices = new size_t[fieldNames.length];
        fieldNames.makeIndex(indices);
        return indices;
    }
    alias B = AliasSeq!();
    static foreach(i; genSortedIndices())
    {
        B = AliasSeq!(B, A[2 * i], A[2 * i + 1]);
    }
    alias TupleCat = Tuple!B;
}

ToTuple partialConstruct(ToTuple, FromTuple)(auto ref FromTuple arg)
{
    ToTuple res;
    static foreach(name; FromTuple.fieldNames)
    {
        __traits(getMember, res, name) = forwardMember!(arg, name);
    }
    return res;
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
            string argsStr;
            foreach(i, remains; remaining)
            {
                // TODO: forwarding args one by one is ill-formed; after first forward t is init.
                if(remains) argsStr ~= "t[" ~ i.to!string ~ "], ";
            }
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

template forwardMember(alias arg, string member)
{
    import core.lifetime : move;
    // lvalue arg or non-moveable member (rvalue or const/immutable)
    static if (__traits(isRef,  arg) ||
               __traits(isOut,  arg) ||
               __traits(isLazy, arg) ||
               !is(typeof(move(__traits(getMember, arg, member)))))
        @property auto ref forwardMember(){ return __traits(getMember, arg, member); }
    // rvalue arg and moveable member (mutable lvalue)
    else
        @property auto forwardMember(){ return move(__traits(getMember, arg, member)); }
}

@("forwardMember")
@safe unittest
{
    struct A
    {
        int i;
        ~this() {}
    }
    struct S
    {
        A a;
        A rvalue() { return a; }
        ref A lvalue() { return a; }
    }

    bool foo(T)(auto ref T val)
    {
        return __traits(isRef, val);
    }
    bool bar(string member, T)(auto ref T val, out int after)
    {
        auto res = foo(forwardMember!(val, member));
        after = val.a.i;
        return res;
    }

    int after;

    // rvalue arg, member -> foo gets rvalue by move
    assert(bar!"a"(S(A(1729)), after) == false);
    assert(after == 0);

    // rvalue arg, rvalue method -> foo gets rvalue as return value of `rvalue` method, no moves
    assert(bar!"rvalue"(S(A(1729)), after) == false);
    assert(after == 1729);

    // rvalue arg, lvalue method -> foo gets rvalue by move
    assert(bar!"lvalue"(S(A(1729)), after) == false);
    assert(after == 0);

    auto s = S(A(42));

    // lvalue arg, member -> foo gets lvalue
    assert(bar!"a"(s, after) == true);
    assert(after == 42);
    assert(s.a.i == 42);

    // lvalue arg, rvalue method -> foo gets rvalue as return value of `rvalue` method, no moves
    assert(bar!"rvalue"(s, after) == false);
    assert(after == 42);
    assert(s.a.i == 42);

    // lvalue arg, lvalue method -> foo gets lvalue
    assert(bar!"lvalue"(s, after) == true);
    assert(after == 42);
    assert(s.a.i == 42);
}
