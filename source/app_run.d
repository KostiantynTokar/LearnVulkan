module app_run;

import from : from;

auto ref run() nothrow @nogc @safe
{
    import std.typecons : tuple;
    import expected : andThen;
    return initWindow(tuple())
        .andThen!initVulkan
        .andThen!mainLoop
        .andThen!cleanup;
}

private:

debug(LearnVulkan_ValidationLayers)
{
    enum ValidationLayersEnabled = true;
}
else
{
    enum ValidationLayersEnabled = false;
}

enum WindowWidth = 800;
enum WindowHeight = 600;

auto ref initWindow(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T)
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import glfw_vulkan : glfwInit, glfwWindowHint, glfwCreateWindow, GLFWwindow,
            GLFW_CLIENT_API, GLFW_NO_API, GLFW_RESIZABLE, GLFW_FALSE;
    import expected : ok;

    alias Res = TupleCat!(T, Tuple!(GLFWwindow*, "window"));

    glfwInit();

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    auto window = glfwCreateWindow(WindowWidth, WindowHeight, "LearnVulkan", null, null);
    return ok(Res(forward!arg.expand, window.move));
}

auto ref initVulkan(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T)
{
    import core.lifetime : forward, move;
    import std.meta : AliasSeq;
    import erupted.vulkan_lib_loader : loadGlobalLevelFunctions;
    import erupted : loadInstanceLevelFunctions;
    import expected : ok, err, andThen;

    auto res = (loadGlobalLevelFunctions() ? ok(forward!arg) : err!T("Failed to load Vulkan global level functions."))
        .andThen!createInstance
        .andThen!((auto ref t) @trusted { loadInstanceLevelFunctions(t.instance); return ok(forward!t); });
    
    debug(LearnVulkan_ValidationLayers)
    {
        return res.move
            .andThen!setupDebugMessanger;
    }
    else
    {
        return res;
    }
}

auto ref createInstance(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T)
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.meta : AliasSeq;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : makeArray, dispose;
    import erupted : VkApplicationInfo, VkInstanceCreateInfo, vkCreateInstance, VkInstance,
        VK_MAKE_API_VERSION, VK_API_VERSION_1_0, VK_SUCCESS;
    import glfw_vulkan : glfwGetRequiredInstanceExtensions;
    import expected : ok, err;
    
    alias Res = TupleCat!(T, Tuple!(VkInstance, "instance"));

    VkApplicationInfo appInfo;
    appInfo.pApplicationName = "Hello Triangle";
    appInfo.applicationVersion = VK_MAKE_API_VERSION(0, 1, 0, 0);
    appInfo.pEngineName = "No engine";
    appInfo.engineVersion = VK_MAKE_API_VERSION(0, 1, 0, 0);
    appInfo.apiVersion = VK_API_VERSION_1_0;

    VkInstanceCreateInfo createInfo;
    createInfo.pApplicationInfo = &appInfo;

    auto extensionNamesRange = getRequiredExtensions();
    auto extensions = Mallocator.instance.makeArray!(const(char)*)(extensionNamesRange);
    scope(exit) () @trusted { Mallocator.instance.dispose(extensions); }();

    createInfo.enabledExtensionCount = cast(uint) extensions.length;
    createInfo.ppEnabledExtensionNames = extensions.ptr;

    debug(LearnVulkan_ValidationLayers)
    {
        if(!checkValidationLayerSupport())
        {
            return err!Res("Requested validation layers are not available.");
        }
        createInfo.enabledLayerCount = ValidationLayers.length;
        createInfo.ppEnabledLayerNames = ValidationLayers.ptr;

        auto debugCreateInfo = defaultDebugMessengerCreateInfo();
        createInfo.pNext = &debugCreateInfo;
    }
    else
    {
        createInfo.enabledLayerCount = 0;
        createInfo.pNext = null;
    }

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

auto getRequiredExtensions() nothrow @nogc @trusted
{
    import std.range : only, chain;
    import erupted : VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
    import glfw_vulkan : glfwGetRequiredInstanceExtensions;

    uint glfwExtensionCount;
    const(char)** glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);
    debug(LearnVulkan_ValidationLayers)
    {
        static immutable name = VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
        return chain(glfwExtensions[0 .. glfwExtensionCount], only(name.ptr));
    }
    else
    {
        return glfwExtensions[0 .. glfwExtensionCount];
    }
}

debug(LearnVulkan_ValidationLayers)
{
    static immutable char*[] ValidationLayers = ["VK_LAYER_KHRONOS_validation"];

    bool checkValidationLayerSupport() nothrow @nogc @trusted
    {
        import core.stdc.string : strcmp;
        import erupted : vkEnumerateInstanceLayerProperties, VkLayerProperties;
        import std.experimental.allocator.mallocator : Mallocator;
        import std.experimental.allocator : makeArray, dispose;

        uint layerCount;
        vkEnumerateInstanceLayerProperties(&layerCount, null);
        auto availableLayers = Mallocator.instance.makeArray!VkLayerProperties(layerCount);
        scope(exit) () @trusted { Mallocator.instance.dispose(availableLayers); }();
        vkEnumerateInstanceLayerProperties(&layerCount, availableLayers.ptr);

        foreach (ref const layerName; ValidationLayers)
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

    auto defaultDebugMessengerCreateInfo() pure nothrow @nogc @trusted
    {
        from!"erupted".VkDebugUtilsMessengerCreateInfoEXT createInfo;
        createInfo.messageSeverity = 0
            // | from!"erupted".VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
            // | from!"erupted".VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
            | from!"erupted".VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
            ;
        createInfo.messageType = 0
            | from!"erupted".VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
            | from!"erupted".VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
            | from!"erupted".VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
            ;
        createInfo.pfnUserCallback = &debugCallback;
        createInfo.pUserData = null;
        return createInfo;
    }

    auto ref setupDebugMessanger(T)(auto ref T arg) nothrow @nogc @trusted
        if(from!"std.typecons".isTuple!T
            && is(typeof(arg.instance) : from!"erupted".VkInstance))
    {
        import util : TupleCat;
        import core.lifetime : forward, move;
        import std.typecons : Tuple;
        import erupted : vkCreateDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT, VK_SUCCESS;
        import expected : ok, err;
        
        alias Res = TupleCat!(T, Tuple!(VkDebugUtilsMessengerEXT, "debugMessenger"));

        auto createInfo = defaultDebugMessengerCreateInfo();

        VkDebugUtilsMessengerEXT debugMessenger;

        return vkCreateDebugUtilsMessengerEXT(arg.instance, &createInfo, null, &debugMessenger) == VK_SUCCESS
            ? ok(Res(forward!arg.expand, debugMessenger.move))
            : err!Res("Failed to create debug messenger.");
    }
}

extern(System) from!"erupted".VkBool32 debugCallback(
    from!"erupted".VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
    from!"erupted".VkDebugUtilsMessageTypeFlagsEXT messageType,
    const(from!"erupted".VkDebugUtilsMessengerCallbackDataEXT)* pCallbackData,
    void* pUserData) nothrow @nogc
{
    import core.stdc.stdio : fprintf, stderr;
    import erupted : VK_FALSE;

    fprintf(stderr, "Validation layer: ");
    fprintf(stderr, pCallbackData.pMessage);
    fprintf(stderr, "\n");

    return VK_FALSE;
}

auto ref mainLoop(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.window) : from!"bindbc.glfw".GLFWwindow*))
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
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.window) : from!"bindbc.glfw".GLFWwindow*)
        && is(typeof(arg.instance) : from!"erupted".VkInstance)
        && from!"util".implies(ValidationLayersEnabled,
            is(typeof(arg.debugMessenger) : from!"erupted".VkDebugUtilsMessengerEXT)))
{
    import util : erase;
    import core.lifetime : forward;
    import erupted : vkDestroyInstance;
    import erupted.vulkan_lib_loader : freeVulkanLib;
    import glfw_vulkan : glfwDestroyWindow, glfwTerminate;
    import expected : ok;

    debug(LearnVulkan_ValidationLayers)
    {
        import erupted : vkDestroyDebugUtilsMessengerEXT;
        vkDestroyDebugUtilsMessengerEXT(arg.instance, arg.debugMessenger, null);
    }

    vkDestroyInstance(arg.instance, null);

    glfwDestroyWindow(arg.window);
    glfwTerminate();

    freeVulkanLib();

    debug(LearnVulkan_ValidationLayers)
    {
        return ok(forward!arg.erase!("window", "instance", "debugMessenger"));
    }
    else
    {
        return ok(forward!arg.erase!("window", "instance"));
    }
}
