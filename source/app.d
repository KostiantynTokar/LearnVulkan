import from : from;

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

auto run() nothrow @nogc @safe
{
    import expected : andThen;
    return initWindow
        .andThen!initVulkan
        .andThen!mainLoop
        .andThen!cleanup;
}

enum WindowWidth = 800;
enum WindowHeight = 600;

auto initWindow() nothrow @nogc @trusted
{
    import glfw_vulkan : glfwInit, glfwWindowHint, glfwCreateWindow,
            GLFW_CLIENT_API, GLFW_NO_API, GLFW_RESIZABLE, GLFW_FALSE;
    import expected : ok;
    import std.typecons : tuple;

    glfwInit();

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    auto window = glfwCreateWindow(WindowWidth, WindowHeight, "LearnVulkan", null, null);
    return ok(tuple!"window"(window));
}

auto initVulkan(T)(auto ref T arg) nothrow @nogc @trusted
{
    import core.lifetime : forward;
    import erupted.vulkan_lib_loader : loadGlobalLevelFunctions;
    import erupted : loadInstanceLevelFunctions;
    import expected : ok, err, andThen;

    return (loadGlobalLevelFunctions() ? ok(forward!arg) : err!T("Failed to load Vulkan global level functions"))
        .andThen!createInstance
        .andThen!((t) @trusted { loadInstanceLevelFunctions(t.instance); return ok(forward!t); });
}

auto createInstance(T)(auto ref T arg) nothrow @nogc @trusted
{
    import core.lifetime : forward;
    import std.typecons : tuple;
    import erupted : VkApplicationInfo, VkInstanceCreateInfo, vkCreateInstance, VkInstance,
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

    version(LearnVulkan_PrintExtensions)
    () @trusted {
        import erupted : vkEnumerateInstanceExtensionProperties, VkExtensionProperties;
        import std.experimental.allocator.mallocator : Mallocator;
        import std.experimental.allocator : makeArray, dispose;
        import std.stdio : printf;
        uint extensionCount;
        vkEnumerateInstanceExtensionProperties(null, &extensionCount, null);
        auto extensions = Mallocator.instance.makeArray!VkExtensionProperties(extensionCount);
        scope(exit) () @trusted { Mallocator.instance.dispose(extensions); }();
        vkEnumerateInstanceExtensionProperties(null, &extensionCount, extensions.ptr);
        printf("Available extensions:\n");
        foreach(ref const extension; extensions)
        {
            printf("\t");
            printf(extension.extensionName.ptr);
            printf("\n");
        }
    }();

    import std.typecons : Tuple;
    alias Res = TupleCat!(T, Tuple!(VkInstance, "instance"));
    VkInstance instance;

    return vkCreateInstance(&createInfo, null, &instance) == VK_SUCCESS
        ? ok(Res(forward!arg.expand, instance))
        : err!Res("Failed to create VkInstance");
}

auto mainLoop(T)(auto ref T arg) nothrow @nogc @trusted
{
    import core.lifetime : forward;
    import glfw_vulkan : glfwWindowShouldClose, glfwPollEvents;
    import expected : ok;

    while(!glfwWindowShouldClose(arg.window))
    {
        glfwPollEvents();
    }

    return ok(forward!arg);
}

auto cleanup(T)(auto ref T arg) nothrow @nogc @trusted
{
    import erupted : vkDestroyInstance;
    import erupted.vulkan_lib_loader : freeVulkanLib;
    import glfw_vulkan : glfwDestroyWindow, glfwTerminate;
    import expected : ok;

    vkDestroyInstance(arg.instance, null);

    glfwDestroyWindow(arg.window);
    glfwTerminate();

    freeVulkanLib();

    return ok;
}

void println(string str) nothrow @nogc @safe
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

int main() nothrow @nogc @safe
{
    import core.stdc.stdlib : EXIT_SUCCESS, EXIT_FAILURE;
    import expected : mapOrElse;

    return run()
        .mapOrElse!(
            () => EXIT_SUCCESS,
            (e) { println(e); return EXIT_FAILURE; });
}
