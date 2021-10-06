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

auto ref run() nothrow @nogc @safe
{
    import expected : andThen;
    return initWindow
        .andThen!initVulkan
        .andThen!mainLoop
        .andThen!cleanup;
}

enum WindowWidth = 800;
enum WindowHeight = 600;

auto ref initWindow() nothrow @nogc @trusted
{
    import core.lifetime : move;
    import std.typecons : tuple;
    import glfw_vulkan : glfwInit, glfwWindowHint, glfwCreateWindow,
            GLFW_CLIENT_API, GLFW_NO_API, GLFW_RESIZABLE, GLFW_FALSE;
    import expected : ok;

    glfwInit();

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    auto window = glfwCreateWindow(WindowWidth, WindowHeight, "LearnVulkan", null, null);
    return ok(tuple!"window"(window.move));
}

auto ref initVulkan(T)(auto ref T arg) nothrow @nogc @trusted
{
    import core.lifetime : forward;
    import erupted.vulkan_lib_loader : loadGlobalLevelFunctions;
    import erupted : loadInstanceLevelFunctions;
    import expected : ok, err, andThen;

    return (loadGlobalLevelFunctions() ? ok(forward!arg) : err!T("Failed to load Vulkan global level functions."))
        .andThen!createInstance
        .andThen!((auto ref t) @trusted { loadInstanceLevelFunctions(t.instance); return ok(forward!t); });
}

auto ref createInstance(T)(auto ref T arg) nothrow @nogc @trusted
{
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkApplicationInfo, VkInstanceCreateInfo, vkCreateInstance, VkInstance,
        VK_MAKE_API_VERSION, VK_API_VERSION_1_0, VK_SUCCESS;
    import glfw_vulkan : glfwGetRequiredInstanceExtensions;
    import expected : ok, err;
    
    alias Res = TupleCat!(T, Tuple!(VkInstance, "instance"));

    debug(LearnVulkan_ValidationLayers)
    {
        if(!checkValidationLayerSupport())
        {
            return err!Res("Requested validation layers are not available.");
        }
    }

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

    VkInstance instance;

    return vkCreateInstance(&createInfo, null, &instance) == VK_SUCCESS
        ? ok(Res(forward!arg.expand, instance.move))
        : err!Res("Failed to create VkInstance.");
}

debug(LearnVulkan_ValidationLayers)
{
    bool checkValidationLayerSupport() nothrow @nogc @trusted
    {
        import core.stdc.string : strncmp;
        import erupted : vkEnumerateInstanceLayerProperties, VkLayerProperties;
        import std.experimental.allocator.mallocator : Mallocator;
        import std.experimental.allocator : makeArray, dispose;
        import util : strcmp;

        uint layerCount;
        vkEnumerateInstanceLayerProperties(&layerCount, null);
        auto availableLayers = Mallocator.instance.makeArray!VkLayerProperties(layerCount);
        scope(exit) () @trusted { Mallocator.instance.dispose(availableLayers); }();
        vkEnumerateInstanceLayerProperties(&layerCount, availableLayers.ptr);

        static immutable validationLayers = ["VK_LAYER_KHRONOS_validation"];

        foreach (ref const layerName; validationLayers)
        {
            auto layerFound = false;

            foreach (ref const layerProperties; availableLayers)
            {
                if(strcmp(layerName, layerProperties.layerName.ptr) == 0)
                {
                    layerFound = true;
                    break;
                }
            }

            if(!layerFound)
            {
                return false;
            }
        }

        return true;
    }
}

auto ref mainLoop(T)(auto ref T arg) nothrow @nogc @trusted
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

auto ref cleanup(T)(auto ref T arg) nothrow @nogc @trusted
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

int main() nothrow @nogc @safe
{
    import core.stdc.stdlib : EXIT_SUCCESS, EXIT_FAILURE;
    import expected : mapOrElse;

    return run()
        .mapOrElse!(
            () => EXIT_SUCCESS,
            (e) { println(e); return EXIT_FAILURE; });
}
