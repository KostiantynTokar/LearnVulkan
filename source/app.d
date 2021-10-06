int main() nothrow @nogc @safe
{
    import app_run : run;
    import util : println;
    import core.stdc.stdlib : EXIT_SUCCESS, EXIT_FAILURE;
    import expected : mapOrElse;

    return run()
        .mapOrElse!(
            (t) { static assert(t.Types.length == 0); return EXIT_SUCCESS; },
            (e) { println(e); return EXIT_FAILURE; });
}
