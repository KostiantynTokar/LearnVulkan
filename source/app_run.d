module app_run;

import from : from;

auto ref run() nothrow @nogc @safe
{
    import std.typecons : tuple;
    import expected : andThen;
    return initWindow(tuple())
        .andThen!initVulkan
        .andThen!mainLoop
        .andThen!cleanup
        ;
}

private:

debug(LearnVulkan_ValidationLayers)
{
    enum validationLayersEnabled = true;
    static immutable char*[] validationLayers = ["VK_LAYER_KHRONOS_validation"];
}
else
{
    enum validationLayersEnabled = false;
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
    import erupted : loadInstanceLevelFunctions, loadDeviceLevelFunctions;
    import expected : ok, err, andThen;

    return (loadGlobalLevelFunctions() ? ok(forward!arg) : err!T("Failed to load Vulkan global level functions."))
        .andThen!createInstance
        .andThen!((auto ref t) @trusted { loadInstanceLevelFunctions(t.instance); return ok(forward!t); })
        .andThenSetupDebugMessenger
        .andThen!createSurface
        .andThen!pickPhysicalDevice
        .andThen!createLogicalDevice
        .andThen!((auto ref t) @trusted { loadDeviceLevelFunctions(t.device); return ok(forward!t); })
        .andThen!getDeviceQueues
        ;
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
        createInfo.enabledLayerCount = validationLayers.length;
        createInfo.ppEnabledLayerNames = validationLayers.ptr;

        immutable debugCreateInfo = defaultDebugMessengerCreateInfo();
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

    auto ref setupDebugMessenger(T)(auto ref T arg) nothrow @nogc @trusted
        if(from!"std.typecons".isTuple!T
            && is(typeof(arg.instance) : from!"erupted".VkInstance)
            )
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

auto ref andThenSetupDebugMessenger(Exp)(auto ref Exp exp) nothrow @nogc @safe
{
    debug(LearnVulkan_ValidationLayers)
    {
        import core.lifetime : forward;
        import expected : andThen;
        return forward!exp.andThen!setupDebugMessenger;
    }
    else
    {
        return exp;
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

auto ref createSurface(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.window) : from!"bindbc.glfw".GLFWwindow*)
        && is(typeof(arg.instance) : from!"erupted".VkInstance)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import glfw_vulkan : glfwCreateWindowSurface;
    import erupted : VkSurfaceKHR, VK_SUCCESS;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(VkSurfaceKHR, "surface"));

    VkSurfaceKHR surface;
    return glfwCreateWindowSurface(arg.instance, arg.window, null, &surface) == VK_SUCCESS
        ? ok(Res(forward!arg.expand, surface.move))
        : err!Res("Failed to create window surface.");
}

// bool isDeviceSuitable(from!"erupted".VkPhysicalDevice device) nothrow @nogc @trusted
// {
//     // from!"erupted".VkPhysicalDeviceProperties deviceProperties;
//     // from!"erupted".vkGetPhysicalDeviceProperties(device, &deviceProperties);

//     // from!"erupted".VkPhysicalDeviceFeatures deviceFeatures;
//     // from!"erupted".vkGetPhysicalDeviceFeatures(device, &deviceFeatures);

//     import optional : match;

//     immutable indices = findQueueFamilies(device);

//     return indices.match!(v => true, () => false);
// }

struct QueueFamilyIndices
{
    uint graphicsFamily;
    uint presentFamily;
}

from!"optional".Optional!QueueFamilyIndices findQueueFamilies
    (
        from!"erupted".VkPhysicalDevice device,
        from!"erupted".VkSurfaceKHR surface,
    ) nothrow @nogc @trusted
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : makeArray, dispose;
    import erupted : VkQueueFamilyProperties, vkGetPhysicalDeviceQueueFamilyProperties,
        vkGetPhysicalDeviceSurfaceSupportKHR, VkBool32, VK_QUEUE_GRAPHICS_BIT;
    import optional : Optional, match, some, none;
    
    uint queueFamilyCount;
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, null);
    auto queueFamilies = Mallocator.instance.makeArray!VkQueueFamilyProperties(queueFamilyCount);
    scope(exit) () @trusted { Mallocator.instance.dispose(queueFamilies); }();
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queueFamilyCount, queueFamilies.ptr);

    Optional!QueueFamilyIndices indices;
    Optional!uint optGraphicsFamily;
    Optional!uint optPresentFamily;

    foreach(i, const ref queueFamily; queueFamilies)
    {
        auto ind = cast(uint) i;

        if(queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT)
        {
           optGraphicsFamily = ind;
        }

        VkBool32 presentSupport = false;
        vkGetPhysicalDeviceSurfaceSupportKHR(device, ind, surface, &presentSupport);
        if(presentSupport)
        {
            optPresentFamily = ind;
        }

        indices = optGraphicsFamily.match!(
            graphicsFamily => 
                optPresentFamily.match!(
                    presentFamily => some(QueueFamilyIndices(graphicsFamily, presentFamily)),
                    () => Optional!QueueFamilyIndices()
                ),
            () => Optional!QueueFamilyIndices()
        );
        if(!indices.empty) break;
    }

    return indices;
}

auto ref pickPhysicalDevice(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.instance) : from!"erupted".VkInstance)
        )
{
    import util : TupleCat;
    import core.lifetime : forward;
    import std.typecons : Tuple;
    import std.range : zip, repeat, takeOne;
    import std.algorithm : map, fold;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : makeArray, dispose;
    import erupted : VkPhysicalDevice, vkEnumeratePhysicalDevices, VK_NULL_HANDLE;
    import expected : ok, err, orElse;
    import optional : Optional, match;

    alias Res = TupleCat!(T, Tuple!(VkPhysicalDevice, "physicalDevice", QueueFamilyIndices, "queueFamilyIndices"));
    
    uint deviceCount;
    vkEnumeratePhysicalDevices(arg.instance, &deviceCount, null);

    if(deviceCount == 0)
    {
        return err!Res("Failed to find GPU with Vulkan support.");
    }

    auto devices = Mallocator.instance.makeArray!VkPhysicalDevice(deviceCount);
    scope(exit) () @trusted { Mallocator.instance.dispose(devices); }();

    vkEnumeratePhysicalDevices(arg.instance, &deviceCount, devices.ptr);

    return devices.zip(arg.repeat)
        .map!((elem)
        {
            immutable indices = findQueueFamilies(elem[0], elem[1].surface);
            return indices.match!(
                q => ok(Res(elem[1].expand, elem[0], q)),
                () => err!Res("Failed to find suitable GPU."));
        })
        .fold!((prev, exp) => prev.orElse!(e => e)(exp))
            (err!Res("Failed to find GPU with Vulkan support."));
}

auto ref createLogicalDevice(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.physicalDevice) : from!"erupted".VkPhysicalDevice)
        && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.algorithm : sort, uniq;
    import erupted : VkDevice;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(VkDevice, "device"));

    enum queuesCount = arg.queueFamilyIndices.tupleof.length;

    from!"erupted".VkDeviceQueueCreateInfo[queuesCount] queueCreateInfos;
    uint[queuesCount] queueFamilies = [arg.queueFamilyIndices.tupleof];
    immutable queuePriority = 1.0f;
    uint uniqueFamiliesCount = 0;
    // TODO: sort requieres expected ModuleInfo? Breaks betterC.
    foreach(queueFamily; queueFamilies[].sort.uniq)
    {
        queueCreateInfos[uniqueFamiliesCount].queueFamilyIndex = queueFamily;
        queueCreateInfos[uniqueFamiliesCount].queueCount = 1;
        queueCreateInfos[uniqueFamiliesCount].pQueuePriorities = &queuePriority;
        ++uniqueFamiliesCount;
    }

    from!"erupted".VkPhysicalDeviceFeatures deviceFeatures;

    from!"erupted".VkDeviceCreateInfo createInfo;

    createInfo.pQueueCreateInfos = queueCreateInfos.ptr;
    createInfo.queueCreateInfoCount = uniqueFamiliesCount;

    createInfo.pEnabledFeatures = &deviceFeatures;

    createInfo.enabledExtensionCount = 0;

    static if(validationLayersEnabled)
    {
        createInfo.enabledLayerCount = validationLayers.length;
        createInfo.ppEnabledLayerNames = validationLayers.ptr;
    }
    else
    {
        createInfo.enabledLayerCount = 0;
    }

    VkDevice device;

    return from!"erupted".vkCreateDevice(arg.physicalDevice, &createInfo, null, &device) == from!"erupted".VK_SUCCESS
        ? ok(Res(forward!arg.expand, device.move))
        : err!Res("Failed to create logical device.");
}

auto ref getDeviceQueues(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkQueue, vkGetDeviceQueue;

    alias Res = TupleCat!(T, Tuple!(VkQueue, "graphicsQueue", VkQueue, "presentQueue"));

    VkQueue graphicsQueue;
    vkGetDeviceQueue(arg.device, arg.queueFamilyIndices.graphicsFamily, 0, &graphicsQueue);

    VkQueue presentQueue;
    vkGetDeviceQueue(arg.device, arg.queueFamilyIndices.presentFamily, 0, &presentQueue);

    return from!"expected".ok(Res(forward!arg.expand, graphicsQueue.move, presentQueue.move));
}

auto ref mainLoop(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.window) : from!"bindbc.glfw".GLFWwindow*)
        )
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
        && is(typeof(arg.surface) : from!"erupted".VkSurfaceKHR)
        && from!"util".implies(validationLayersEnabled,
            is(typeof(arg.debugMessenger) : from!"erupted".VkDebugUtilsMessengerEXT))
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        )
{
    import util : erase;
    import core.lifetime : forward;
    import std.meta : AliasSeq;
    import erupted.vulkan_lib_loader : freeVulkanLib;
    import glfw_vulkan : glfwDestroyWindow, glfwTerminate;
    import expected : ok;

    from!"erupted".vkDestroyDevice(arg.device, null);

    debug(LearnVulkan_ValidationLayers)
    {
        import erupted : vkDestroyDebugUtilsMessengerEXT;
        vkDestroyDebugUtilsMessengerEXT(arg.instance, arg.debugMessenger, null);
    }

    from!"erupted".vkDestroySurfaceKHR(arg.instance, arg.surface, null);
    from!"erupted".vkDestroyInstance(arg.instance, null);

    glfwDestroyWindow(arg.window);
    glfwTerminate();

    freeVulkanLib();

    static if(validationLayersEnabled)
    {
        alias validationLayersErasedNames = AliasSeq!("debugMessenger");
    }
    else
    {
        alias validationLayersErasedNames = AliasSeq!();
    }

    alias erasedNames = AliasSeq!("window", "instance", "surface", "device", validationLayersErasedNames);
    return ok(forward!arg.erase!erasedNames);
}
