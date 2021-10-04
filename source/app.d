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
        import glfw_vulkan : glfwInit, glfwWindowHint, glfwCreateWindow,
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

        createInstance();

        return ok;
    }

    auto createInstance() nothrow @nogc @trusted
    {
        import erupted : VkApplicationInfo, VkInstanceCreateInfo, vkCreateInstance,
            VK_MAKE_API_VERSION, VK_API_VERSION_1_0, VK_SUCCESS;
        import glfw_vulkan : glfwGetRequiredInstanceExtensions;
        import expected : ok, err;

        VkApplicationInfo appInfo;
        appInfo.pApplicationName = "Hello Triangle";
        appInfo.applicationVersion = VK_MAKE_API_VERSION(0, 1, 0, 0);
        appInfo.pEngineName = "No engine";
        appInfo.engineVersion = VK_MAKE_API_VERSION(0, 1, 0, 0);
        appInfo.apiVersion = VK_API_VERSION_1_0;

        VkInstanceCreateInfo createInfo;
        createInfo.pApplicationInfo = &appInfo;

        uint glfwExtensionCount;
        const(char)** glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);

        createInfo.enabledExtensionCount = glfwExtensionCount;
        createInfo.ppEnabledExtensionNames = glfwExtensions;

        createInfo.enabledLayerCount = 0;

        return vkCreateInstance(&createInfo, null, &instance) == VK_SUCCESS ? ok : err("Failed to create VkInstance");
    }

    auto mainLoop() nothrow @nogc @trusted
    {
        import glfw_vulkan : glfwWindowShouldClose, glfwPollEvents;
        import expected : ok;

        while(!glfwWindowShouldClose(window))
        {
            glfwPollEvents();
        }

        return ok;
    }

    auto cleanup() nothrow @nogc @trusted
    {
        import glfw_vulkan : glfwDestroyWindow, glfwTerminate;
        import expected : ok;

        glfwDestroyWindow(window);
        glfwTerminate();

        return ok;
    }

    from!"glfw_vulkan".GLFWwindow* window;
    enum windowWidth = 800;
    enum windowHeight = 600;

    from!"erupted".VkInstance instance;
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

int main() nothrow @nogc
{
    import core.stdc.stdlib : EXIT_SUCCESS, EXIT_FAILURE;
    import expected : mapOrElse;

    HelloTriangleApplication app;

    return app.run()
        .mapOrElse!(
            () => EXIT_SUCCESS,
            (e) {println(e); return EXIT_FAILURE;});
}
