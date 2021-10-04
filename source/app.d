import from : from;

struct HelloTriangleApplication
{
    auto run() nothrow @nogc @safe
    {
        import expected : andThen;
        return initWindow()
            .andThen!(t => t.initVulkan())(this)
            .andThen!(t => t.mainLoop())(this)
            .andThen!(t => t.cleanup())(this);
    }

private:

    auto initWindow() nothrow @nogc @trusted
    {
        import bindbc.glfw : glfwInit, glfwWindowHint, glfwCreateWindow,
            GLFW_CLIENT_API, GLFW_NO_API, GLFW_RESIZABLE, GLFW_FALSE;
        import expected : ok;

        glfwInit();

        glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
        glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
        window = glfwCreateWindow(windowWidth, windowHeight, "LearnVulkan", null, null);
        return ok;
    }

    auto initVulkan() nothrow @nogc @trusted
    {
        import erupted.vulkan_lib_loader : loadGlobalLevelFunctions;
        import expected : ok, err;

        if(!loadGlobalLevelFunctions())
        {
            return err("Failed to load Vulkan global level functions");
        }

        return ok;
    }

    auto mainLoop() nothrow @nogc @trusted
    {
        import bindbc.glfw : glfwWindowShouldClose, glfwPollEvents;
        import expected : ok;

        while(!glfwWindowShouldClose(window))
        {
            glfwPollEvents();
        }

        return ok;
    }

    auto cleanup() nothrow @nogc @trusted
    {
        import bindbc.glfw : glfwDestroyWindow, glfwTerminate;
        import expected : ok;

        glfwDestroyWindow(window);
        glfwTerminate();

        return ok;
    }

    from!"bindbc.glfw".GLFWwindow* window;
    enum windowWidth = 800;
    enum windowHeight = 600;
}

void println(string str) nothrow @nogc @safe
{
    import std.experimental.allocator : makeArray, dispose;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.stdio : printf;

    auto cStr = Mallocator.instance.makeArray!char(str.length + 2);
    scope(exit) () @trusted {Mallocator.instance.dispose(cStr);}();
    cStr[0 .. str.length] = str[];
    cStr[str.length] = '\n';
    cStr[str.length + 1] = '\0';
    () @trusted {printf(cStr.ptr);}();
}

extern(C) int main() nothrow @nogc
{
    import core.stdc.stdlib : EXIT_SUCCESS, EXIT_FAILURE;
    import expected : mapOrElse;

    HelloTriangleApplication app;

    return app.run()
        .mapOrElse!(
            () => EXIT_SUCCESS,
            (e) {println(e); return EXIT_FAILURE;});
}
