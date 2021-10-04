module from;

/** 
 * Import as expression.
 * Params:
 *   moduleName = Name of a module to import from.
 *
 * See_Also: $(LINK2 https://dlang.org/blog/2017/02/13/a-new-import-idiom/, A New Import Idiom)
 */
template from(string moduleName)
{
    mixin("import from = " ~ moduleName ~ ";");
}
///
unittest
{
    void f(from!"std.typecons".Tuple!(int, double) arg)
    {
        from!"std.stdio".writeln(arg);
    }
}
