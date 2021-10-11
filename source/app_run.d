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

static immutable char*[] deviceExtensions = [from!"erupted".VK_KHR_SWAPCHAIN_EXTENSION_NAME];

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
        .andThen!createSwapChain
        .andThen!getSwapChainImages
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

struct SwapChainSupportDetails
{
    import std.experimental.allocator.mallocator : Mallocator;
    import automem : Vector;

    from!"erupted".VkSurfaceCapabilitiesKHR capabilities;
    Vector!(from!"erupted".VkSurfaceFormatKHR, Mallocator) formats;
    Vector!(from!"erupted".VkPresentModeKHR, Mallocator) presentModes;
}

SwapChainSupportDetails querySwapChainSupport(
    from!"erupted".VkPhysicalDevice device,
    from!"erupted".VkSurfaceKHR surface,
    ) nothrow @nogc @trusted
{
    import erupted : vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        vkGetPhysicalDeviceSurfaceFormatsKHR,
        vkGetPhysicalDeviceSurfacePresentModesKHR;
    
    SwapChainSupportDetails details;

    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &details.capabilities);

    uint formatCount;
    vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &formatCount, null);
    if(formatCount != 0)
    {
        details.formats.length = formatCount;
        vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &formatCount, details.formats.ptr);
    }

    uint presentModeCount;
    vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &presentModeCount, null);
    if(presentModeCount != 0)
    {
        details.presentModes.length = presentModeCount;
        vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &presentModeCount, details.presentModes.ptr);
    }

    return details;
}

struct ChosenSwapChainSupport
{
    from!"erupted".VkSurfaceCapabilitiesKHR capabilities;
    from!"erupted".VkSurfaceFormatKHR surfaceFormat;
    from!"erupted".VkPresentModeKHR presentMode;
    from!"erupted".VkExtent2D extent;
    uint imageCount;
}

from!"erupted".VkSurfaceFormatKHR chooseSwapSurfaceFormat(VkSurfaceFormatKHRArray)(
    const auto ref VkSurfaceFormatKHRArray availableFormats
    ) nothrow @nogc @trusted
{
    foreach(const ref availableFormat; availableFormats[])
    {
        if(availableFormat.format == from!"erupted".VK_FORMAT_B8G8R8A8_SRGB
            && availableFormat.colorSpace == from!"erupted".VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
        {
            return availableFormat;
        }
    }
    // TODO: statically verify that availableFormats is not empty.
    return availableFormats.ptr[0];
}

from!"erupted".VkPresentModeKHR chooseSwapPresentMode(VkPresentModeKHRArray)(
    const auto ref VkPresentModeKHRArray availablePresentModes
    ) nothrow @nogc @trusted
{
    foreach(const ref availablePresentMode; availablePresentModes[])
    {
        // Search for mode that can be used for triple buffering.
        if(availablePresentMode == from!"erupted".VK_PRESENT_MODE_MAILBOX_KHR)
        {
            return availablePresentMode;
        }
    }
    // This mode is guaranteed to be available. Used for double buffering.
    return from!"erupted".VK_PRESENT_MODE_FIFO_KHR;
}

from!"erupted".VkExtent2D chooseSwapExtent(
    const ref from!"erupted".VkSurfaceCapabilitiesKHR capabilities
    ) nothrow @nogc @trusted
{
    import std.algorithm : clamp;

    if(capabilities.currentExtent.width != uint.max)
    {
        // Window manager allows non-fixed extent.
        return capabilities.currentExtent;
    }

    from!"erupted".VkExtent2D actualExtent;

    actualExtent.width = WindowWidth
        .clamp(
            capabilities.minImageExtent.width,
            capabilities.maxImageExtent.width
            );
    actualExtent.height = WindowHeight
        .clamp(
            capabilities.minImageExtent.height,
            capabilities.maxImageExtent.height
            );
    
    return actualExtent;
}

uint chooseImageCount(
    const ref from!"erupted".VkSurfaceCapabilitiesKHR capabilities
    ) nothrow @nogc @trusted
{
    import std.algorithm : min;

    if(capabilities.maxImageCount == 0)
    {
        // There is no maximum.
        return capabilities.minImageCount + 1;
    }
    return min(capabilities.minImageCount + 1, capabilities.maxImageCount);
}

bool checkDeviceExtensionSupport(from!"erupted".VkPhysicalDevice device) nothrow @nogc @trusted
{
    import core.stdc.string : strcmp;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : makeArray, dispose;
    import erupted : vkEnumerateDeviceExtensionProperties, VkExtensionProperties;

    uint extensionCount;
    vkEnumerateDeviceExtensionProperties(device, null, &extensionCount, null);
    auto availableExtensions = Mallocator.instance.makeArray!VkExtensionProperties(extensionCount);
    scope(exit) () @trusted { Mallocator.instance.dispose(availableExtensions); }();
    vkEnumerateDeviceExtensionProperties(device, null, &extensionCount, availableExtensions.ptr);

    foreach (ref const extensionName; deviceExtensions)
    {
        auto extensionFound = false;

        foreach (ref const extensionProperties; availableExtensions)
        {
            if(strcmp(extensionName, extensionProperties.extensionName.ptr) == 0)
            {
                extensionFound = true;
                break;
            }
        }

        if(!extensionFound)
        {
            return false;
        }
    }

    return true;
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

    alias Res = TupleCat!(T, Tuple!(
        VkPhysicalDevice, "physicalDevice",
        QueueFamilyIndices, "queueFamilyIndices",
        ChosenSwapChainSupport, "chosenSwapChainSupport",
        ));
    
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
            if(!checkDeviceExtensionSupport(elem[0]))
            {
                return err!Res("Failed to find suitable GPU.");
            }
            auto swapChainSupport = querySwapChainSupport(elem[0], elem[1].surface);
            if(swapChainSupport.formats.empty || swapChainSupport.presentModes.empty)
            {
                return err!Res("Failed to find suitable GPU.");
            }
            immutable chosenSwapChainSupport = ChosenSwapChainSupport(
                swapChainSupport.capabilities,
                chooseSwapSurfaceFormat(swapChainSupport.formats),
                chooseSwapPresentMode(swapChainSupport.presentModes),
                chooseSwapExtent(swapChainSupport.capabilities),
                chooseImageCount(swapChainSupport.capabilities)
            );
            immutable indices = findQueueFamilies(elem[0], elem[1].surface);
            return indices.match!(
                queueFamilyIndices => ok(Res(elem[1].expand, elem[0], queueFamilyIndices, chosenSwapChainSupport)),
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

    createInfo.enabledExtensionCount = deviceExtensions.length;
    createInfo.ppEnabledExtensionNames = deviceExtensions.ptr;

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

auto ref createSwapChain(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.surface) : from!"erupted".VkSurfaceKHR)
        && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
        && is(typeof(arg.chosenSwapChainSupport) : ChosenSwapChainSupport)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkSwapchainKHR, VkFormat, VkExtent2D;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkSwapchainKHR, "swapChain",
        VkFormat, "swapChainImageFormat",
        VkExtent2D, "swapChainExtent",
        ));

    from!"erupted".VkSwapchainCreateInfoKHR createInfo;

    createInfo.surface = arg.surface;
    createInfo.minImageCount = arg.chosenSwapChainSupport.imageCount;
    createInfo.imageFormat = arg.chosenSwapChainSupport.surfaceFormat.format;
    createInfo.imageColorSpace = arg.chosenSwapChainSupport.surfaceFormat.colorSpace;
    createInfo.imageExtent = arg.chosenSwapChainSupport.extent;
    // Always 1 unless for stereoscopic 3D application.
    createInfo.imageArrayLayers = 1;
    // Render directly to imega. Use VK_IMAGE_USAGE_TRANSFER_DST_BIT for off-screen rendering.
    createInfo.imageUsage = from!"erupted".VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;

    const uint[2] queueFamilyIndicesArr = [arg.queueFamilyIndices.graphicsFamily, arg.queueFamilyIndices.presentFamily];
    if(arg.queueFamilyIndices.graphicsFamily != arg.queueFamilyIndices.presentFamily)
    {
        // Can be exclusive (more performant), but it requires explicit ownership control.
        createInfo.imageSharingMode = from!"erupted".VK_SHARING_MODE_CONCURRENT;
        createInfo.queueFamilyIndexCount = 2;
        createInfo.pQueueFamilyIndices = queueFamilyIndicesArr.ptr;
    }
    else
    {
        createInfo.imageSharingMode = from!"erupted".VK_SHARING_MODE_EXCLUSIVE;
        createInfo.queueFamilyIndexCount = 0; // Optional
        createInfo.pQueueFamilyIndices = null; // Optional
    }

    // Do not want any transforms applied to swap chain images.
    createInfo.preTransform = arg.chosenSwapChainSupport.capabilities.currentTransform;

    // Blending with other windows in the window system.
    createInfo.compositeAlpha = from!"erupted".VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
    
    createInfo.presentMode = arg.chosenSwapChainSupport.presentMode;
    // Do not render obscured pixels.
    createInfo.clipped = from!"erupted".VK_TRUE;

    createInfo.oldSwapchain = from!"erupted".VK_NULL_HANDLE;

    VkSwapchainKHR swapChain;
    return from!"erupted".vkCreateSwapchainKHR(arg.device, &createInfo, null, &swapChain) == from!"erupted".VK_SUCCESS
        ? ok(Res(forward!arg.expand, swapChain.move, createInfo.imageFormat.move, createInfo.imageExtent.move))
        : err!Res("Failed to create swap chain.");
}

auto ref getSwapChainImages(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.swapChain) : from!"erupted".VkSwapchainKHR)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted : vkGetSwapchainImagesKHR;
    import expected : ok;
    import automem : Vector;
    
    alias VectorType = Vector!(from!"erupted".VkImage, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainImages"));

    VectorType swapChainImages;
    uint imageCount;
    vkGetSwapchainImagesKHR(arg.device, arg.swapChain, &imageCount, null);
    swapChainImages.length = imageCount;
    vkGetSwapchainImagesKHR(arg.device, arg.swapChain, &imageCount, swapChainImages.ptr);
    return ok(Res(forward!arg.expand, swapChainImages.move));
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
        && is(typeof(arg.swapChain) : from!"erupted".VkSwapchainKHR)
        )
{
    import util : erase;
    import core.lifetime : forward;
    import std.meta : AliasSeq;
    import erupted.vulkan_lib_loader : freeVulkanLib;
    import glfw_vulkan : glfwDestroyWindow, glfwTerminate;
    import expected : ok;

    from!"erupted".vkDestroySwapchainKHR(arg.device, arg.swapChain, null);
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

    alias erasedNames = AliasSeq!(
        "window",
        "instance",
        "surface",
        "device",
        "swapChain",
        validationLayersErasedNames,
        );
    return ok(forward!arg.erase!erasedNames);
}
