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

enum StartWindowWidth = 800;
enum StartWindowHeight = 600;

enum MaxFramesInFlight = 3;

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
    auto window = glfwCreateWindow(StartWindowWidth, StartWindowHeight, "LearnVulkan", null, null);
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
        .andThen!createSwapChainAndRelatedObjects
        .andThen!createSyncObjects
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

    const VkApplicationInfo appInfo =
    {
        pApplicationName : "Hello Triangle",
        applicationVersion : VK_MAKE_API_VERSION(0, 1, 0, 0),
        pEngineName : "No engine",
        engineVersion : VK_MAKE_API_VERSION(0, 1, 0, 0),
        apiVersion : VK_API_VERSION_1_0,
    };

    auto extensionNamesRange = getRequiredExtensions();
    auto extensions = Mallocator.instance.makeArray!(const(char)*)(extensionNamesRange);
    scope(exit) () @trusted { Mallocator.instance.dispose(extensions); }();

    VkInstanceCreateInfo createInfo =
    {
        pApplicationInfo : &appInfo,
        enabledExtensionCount : cast(uint) extensions.length,
        ppEnabledExtensionNames : extensions.ptr,
    };

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
        const from!"erupted".VkDebugUtilsMessengerCreateInfoEXT createInfo=
        {
            messageSeverity : 0
                // | from!"erupted".VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                // | from!"erupted".VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                | from!"erupted".VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
                ,
            messageType : 0
                | from!"erupted".VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                | from!"erupted".VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                | from!"erupted".VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
                ,
            pfnUserCallback : &debugCallback,
            pUserData : null,
        };
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

        const createInfo = defaultDebugMessengerCreateInfo();

        VkDebugUtilsMessengerEXT debugMessenger;

        return vkCreateDebugUtilsMessengerEXT(arg.instance, &createInfo, null, &debugMessenger) == VK_SUCCESS
            ? ok(Res(forward!arg.expand, debugMessenger.move))
            : err!Res("Failed to create debug messenger.");
    }

    extern(System) from!"erupted".VkBool32 debugCallback(
        from!"erupted".VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
        from!"erupted".VkDebugUtilsMessageTypeFlagsEXT messageType,
        const(from!"erupted".VkDebugUtilsMessengerCallbackDataEXT)* pCallbackData,
        void* pUserData) nothrow @nogc
    {
        import core.stdc.stdio : fprintf, stderr;
        import erupted : VK_FALSE;

        fprintf(stderr, pCallbackData.pMessage);
        fprintf(stderr, "\n");

        return VK_FALSE;
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
    from!"glfw_vulkan".GLFWwindow* window,
    const ref from!"erupted".VkSurfaceCapabilitiesKHR capabilities
    ) nothrow @nogc @trusted
{
    import std.algorithm : clamp;
    import glfw_vulkan : glfwGetFramebufferSize;

    if(capabilities.currentExtent.width != uint.max)
    {
        // Window manager allows non-fixed extent.
        return capabilities.currentExtent;
    }

    int width;
    int height;
    glfwGetFramebufferSize(window, &width, &height);

    from!"erupted".VkExtent2D actualExtent;

    actualExtent.width = width
        .clamp(
            capabilities.minImageExtent.width,
            capabilities.maxImageExtent.width
            );
    actualExtent.height = height
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
                chooseSwapExtent(elem[1].window, swapChainSupport.capabilities),
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

    const from!"erupted".VkPhysicalDeviceFeatures deviceFeatures;

    from!"erupted".VkDeviceCreateInfo createInfo =
    {
        pQueueCreateInfos : queueCreateInfos.ptr,
        queueCreateInfoCount : uniqueFamiliesCount,

        pEnabledFeatures : &deviceFeatures,

        enabledExtensionCount : deviceExtensions.length,
        ppEnabledExtensionNames : deviceExtensions.ptr,
    };


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

auto ref recreateSwapChain(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.surface) : from!"erupted".VkSurfaceKHR)
        && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
        && is(typeof(arg.chosenSwapChainSupport) : ChosenSwapChainSupport)
        )
{
    import core.lifetime : forward;
    import expected : andThen;

    from!"erupted".vkDeviceWaitIdle(arg.device);

    return cleanupSwapChain(forward!arg)
        .andThen!createSwapChainAndRelatedObjects
        ;
}

auto ref createSwapChainAndRelatedObjects(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.surface) : from!"erupted".VkSurfaceKHR)
        && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
        && is(typeof(arg.chosenSwapChainSupport) : ChosenSwapChainSupport)
        )
{
    import core.lifetime : forward;
    import expected : andThen;

    return createSwapChain(forward!arg)
        .andThen!getSwapChainImages
        .andThen!createImageViews
        .andThen!createRenderPass
        .andThen!createGraphicsPipeline
        .andThen!createFramebuffers
        .andThen!createCommandPool
        .andThen!createCommandBuffers
        ;
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

    from!"erupted".VkSwapchainCreateInfoKHR createInfo =
    {
        surface : arg.surface,
        minImageCount : arg.chosenSwapChainSupport.imageCount,
        imageFormat : arg.chosenSwapChainSupport.surfaceFormat.format,
        imageColorSpace : arg.chosenSwapChainSupport.surfaceFormat.colorSpace,
        imageExtent : arg.chosenSwapChainSupport.extent,
        // Always 1 unless for stereoscopic 3D application.
        imageArrayLayers : 1,
        // Render directly to imega. Use VK_IMAGE_USAGE_TRANSFER_DST_BIT for off-screen rendering.
        imageUsage : from!"erupted".VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        // Do not want any transforms applied to swap chain images.
        preTransform : arg.chosenSwapChainSupport.capabilities.currentTransform,
        // Blending with other windows in the window system.
        compositeAlpha : from!"erupted".VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        presentMode : arg.chosenSwapChainSupport.presentMode,
        // Do not render obscured pixels.
        clipped : from!"erupted".VK_TRUE,
        oldSwapchain : from!"erupted".VK_NULL_HANDLE,
    };

    if(arg.queueFamilyIndices.graphicsFamily != arg.queueFamilyIndices.presentFamily)
    {
        const uint[2] queueFamilyIndicesArr =
        [
            arg.queueFamilyIndices.graphicsFamily,
            arg.queueFamilyIndices.presentFamily,
        ];
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

auto ref createImageViews(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.swapChainImageFormat) : from!"erupted".VkFormat)
        && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.range : enumerate;
    import std.experimental.allocator.mallocator : Mallocator;
    import expected : ok, err;
    import automem : Vector;

    alias VectorType = Vector!(from!"erupted".VkImageView, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainImageViews"));

    VectorType swapChainImageViews;
    swapChainImageViews.length = arg.swapChainImages.length;

    foreach(i, ref image; arg.swapChainImages[].enumerate)
    {
        const from!"erupted".VkImageViewCreateInfo createInfo =
        {
            image : image,
            viewType : from!"erupted".VK_IMAGE_VIEW_TYPE_2D,
            format : arg.swapChainImageFormat,

            components : 
            {
                r : from!"erupted".VK_COMPONENT_SWIZZLE_IDENTITY,
                g : from!"erupted".VK_COMPONENT_SWIZZLE_IDENTITY,
                b : from!"erupted".VK_COMPONENT_SWIZZLE_IDENTITY,
                a : from!"erupted".VK_COMPONENT_SWIZZLE_IDENTITY,
            },

            subresourceRange :
            {
                aspectMask : from!"erupted".VK_IMAGE_ASPECT_COLOR_BIT,
                baseMipLevel : 0,
                levelCount : 1,
                baseArrayLayer : 0,
                // More then 1 for stereographic 3D application.
                layerCount : 1,
            },
        };

        if(from!"erupted".vkCreateImageView(arg.device, &createInfo, null, &swapChainImageViews.ptr[i])
            != from!"erupted".VK_SUCCESS)
        {
            foreach(ref imageView; swapChainImageViews.ptr[0 .. i])
            {
                from!"erupted".vkDestroyImageView(arg.device, imageView, null);
            }
            return err!Res("Failed to create image view.");
        }
    }

    return ok(Res(forward!arg.expand, swapChainImageViews.move));
}

auto ref createRenderPass(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.swapChainImageFormat) : from!"erupted".VkFormat)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkRenderPass, VK_SUCCESS;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkRenderPass, "renderPass"
    ));

    const from!"erupted".VkAttachmentDescription colorAttachment =
    {
        format : arg.swapChainImageFormat,
        samples : from!"erupted".VK_SAMPLE_COUNT_1_BIT,
        loadOp : from!"erupted".VK_ATTACHMENT_LOAD_OP_CLEAR,
        storeOp : from!"erupted".VK_ATTACHMENT_STORE_OP_STORE,
        stencilLoadOp : from!"erupted".VK_ATTACHMENT_LOAD_OP_DONT_CARE,
        stencilStoreOp : from!"erupted".VK_ATTACHMENT_STORE_OP_DONT_CARE,
        initialLayout : from!"erupted".VK_IMAGE_LAYOUT_UNDEFINED,
        finalLayout : from!"erupted".VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, // Image to be presented in the swap chain.
    };

    const from!"erupted".VkAttachmentReference colorAttachmentRef =
    {
        attachment : 0, // Index in attachment array.
        layout : from!"erupted".VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
    };

    const from!"erupted".VkSubpassDescription subpass =
    {
        // Use for graphics.
        pipelineBindPoint : from!"erupted".VK_PIPELINE_BIND_POINT_GRAPHICS,
        colorAttachmentCount : 1,
        // Index in this array referenced from the fragment shader layout(location=0) out vec4 outColor directive.
        pColorAttachments : &colorAttachmentRef,
    };

    const from!"erupted".VkSubpassDependency dependency =
    {
        srcSubpass : from!"erupted".VK_SUBPASS_EXTERNAL, // Refers to implicit subpass before and after render pass.
        dstSubpass : 0, // This subpass's index.

        // What operations to wait.
        srcStageMask : from!"erupted".VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        srcAccessMask : 0,

        // What operations are delayed.
        dstStageMask : from!"erupted".VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        dstAccessMask : from!"erupted".VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
    };

    const from!"erupted".VkRenderPassCreateInfo renderPassInfo =
    {
        attachmentCount : 1,
        pAttachments : &colorAttachment,
        subpassCount : 1,
        pSubpasses : &subpass,

        dependencyCount : 1,
        pDependencies : &dependency,
    };

    VkRenderPass renderPass;
    return from!"erupted".vkCreateRenderPass(arg.device, &renderPassInfo, null, &renderPass) == VK_SUCCESS
        ? ok(Res(forward!arg.expand, renderPass.move))
        : err!Res("Failed to create render pass.");
}

auto ref createShaderModule(from!"erupted".VkDevice device, const(ubyte)[] code) nothrow @nogc @trusted
    in(cast(ptrdiff_t) code.ptr % 4 == 0)
{
    import core.lifetime : move;
    import expected : ok, err;
    
    alias Res = from!"erupted".VkShaderModule;

    const from!"erupted".VkShaderModuleCreateInfo createInfo =
    {
        codeSize : code.length,
        pCode : cast(const(uint)*) code.ptr,
    };

    from!"erupted".VkShaderModule shaderModule;
    return from!"erupted".vkCreateShaderModule(device, &createInfo, null, &shaderModule) == from!"erupted".VK_SUCCESS
        ? ok(shaderModule.move)
        : err!Res("Failed to create shader module");
}

auto ref createShaderModule(from!"erupted".VkDevice device, immutable(char)* compiledShaderFileName) nothrow @nogc @trusted
{
    import core.stdc.stdio : fopen, fclose, fread, fseek, ftell, SEEK_SET, SEEK_END;
    import std.experimental.allocator : makeArray, dispose;
    import util : StaticAlignedMallocator;

    auto shaderFile = fopen(compiledShaderFileName, "rb");
    scope(exit) fclose(shaderFile);
    fseek(shaderFile, 0, SEEK_END);
    auto shaderSize = ftell(shaderFile);
    fseek(shaderFile, 0, SEEK_SET);
    auto shaderCode = StaticAlignedMallocator!4.instance.makeArray!(ubyte)(shaderSize);
    scope(exit) () @trusted { StaticAlignedMallocator!4.instance.dispose(shaderCode); }();
    fread(shaderCode.ptr, 1, shaderSize, shaderFile);

    return createShaderModule(device, shaderCode);
}

auto ref createGraphicsPipeline(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
        && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkPipelineLayout, VkPipeline, vkDestroyShaderModule, VK_FALSE, VK_TRUE, VK_SUCCESS;
    import expected : ok, err, andThen, orElse;

    alias Res = TupleCat!(T, Tuple!
        (
            VkPipelineLayout, "pipelineLayout",
            VkPipeline, "graphicsPipeline",
        ));

    return createShaderModule(arg.device, "shaders_bin/vert.spv")
        .andThen!((vertShaderModule) @trusted
        {
            return createShaderModule(arg.device, "shaders_bin/frag.spv")
                .andThen!((fragShaderModule) @trusted
                {
                    const from!"erupted".VkPipelineShaderStageCreateInfo[2] shaderStageInfos =
                    [
                        {
                            stage : from!"erupted".VK_SHADER_STAGE_VERTEX_BIT,
                            module_ : vertShaderModule,
                            pName : "main", // Entry point.
                            pSpecializationInfo : null, // Specifying shader constants.
                        },
                        {
                            stage : from!"erupted".VK_SHADER_STAGE_FRAGMENT_BIT,
                            module_ : fragShaderModule,
                            pName : "main", // Entry point.
                            pSpecializationInfo : null, // Specifying shader constants.
                        },
                    ];

                    const from!"erupted".VkPipelineVertexInputStateCreateInfo vertexInput =
                    {
                        // Data is per-vertex or per-instance?
                        vertexBindingDescriptionCount : 0,
                        pVertexBindingDescriptions : null,
                        // Binding and offset of attributes.
                        vertexAttributeDescriptionCount : 0,
                        pVertexAttributeDescriptions : null,
                    };

                    const from!"erupted".VkPipelineInputAssemblyStateCreateInfo inputAssembly =
                    {
                        topology : from!"erupted".VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                        primitiveRestartEnable : VK_FALSE,
                    };

                    const from!"erupted".VkViewport viewport =
                    {
                        x : 0.0f,
                        y : 0.0f,
                        width : arg.swapChainExtent.width,
                        height : arg.swapChainExtent.height,
                        minDepth : 0.0f,
                        maxDepth : 1.0f,
                    };

                    const from!"erupted".VkRect2D scissor =
                    {
                        offset : {0, 0},
                        extent : arg.swapChainExtent,
                    };

                    const from!"erupted".VkPipelineViewportStateCreateInfo viewportState =
                    {
                        viewportCount : 1, // Need extension for more then 1.
                        pViewports : &viewport,
                        scissorCount : 1, // Need extension for more then 1.
                        pScissors : &scissor,
                    };

                    const from!"erupted".VkPipelineRasterizationStateCreateInfo rasterizer =
                    {
                        depthClampEnable : VK_FALSE,
                        rasterizerDiscardEnable : VK_FALSE,
                        // Other modes require GPU feature.
                        polygonMode : from!"erupted".VK_POLYGON_MODE_FILL,
                        // 1 pixel wide. Thicker requires wideLines GPU feature.
                        lineWidth : 1.0f,
                        cullMode : from!"erupted".VK_CULL_MODE_BACK_BIT,
                        frontFace : from!"erupted".VK_FRONT_FACE_CLOCKWISE,

                        // Modify fragment's depth depending on its slope.
                        depthBiasEnable : VK_FALSE,
                        depthBiasConstantFactor : 0.0f, // Optional
                        depthBiasClamp : 0.0f, // Optional
                        depthBiasSlopeFactor : 0.0f, // Optional
                    };

                    // Multisampling requires a GPU feature.
                    const from!"erupted".VkPipelineMultisampleStateCreateInfo multisampling =
                    {
                        sampleShadingEnable : VK_FALSE,
                        rasterizationSamples : from!"erupted".VK_SAMPLE_COUNT_1_BIT,
                        minSampleShading : 1.0f, // Optional
                        pSampleMask : null, // Optional
                        alphaToCoverageEnable : VK_FALSE, // Optional
                        alphaToOneEnable : VK_FALSE, // Optional
                    };

                    // Optional
                    // from!"erupted".VkPipelineDepthStencilStateCreateInfo depthStencil;
                    
                    // Framebuffer-specific blending options.
                    const from!"erupted".VkPipelineColorBlendAttachmentState colorBlendAttachment =
                    {
                        colorWriteMask :
                            from!"erupted".VK_COLOR_COMPONENT_R_BIT |
                            from!"erupted".VK_COLOR_COMPONENT_G_BIT |
                            from!"erupted".VK_COLOR_COMPONENT_B_BIT |
                            from!"erupted".VK_COLOR_COMPONENT_A_BIT ,
                        blendEnable : VK_FALSE,
                        srcColorBlendFactor : from!"erupted".VK_BLEND_FACTOR_ONE, // Optional
                        dstColorBlendFactor : from!"erupted".VK_BLEND_FACTOR_ZERO, // Optional
                        colorBlendOp : from!"erupted".VK_BLEND_OP_ADD, // Optional
                        srcAlphaBlendFactor : from!"erupted".VK_BLEND_FACTOR_ONE, // Optional
                        dstAlphaBlendFactor : from!"erupted".VK_BLEND_FACTOR_ZERO, // Optional
                        alphaBlendOp : from!"erupted".VK_BLEND_OP_ADD, // Optional
                    };

                    // Global blending options
                    const from!"erupted".VkPipelineColorBlendStateCreateInfo colorBlending =
                    {
                        // Bitwise blending disables framebuffer-specific blending.
                        logicOpEnable : VK_FALSE,
                        logicOp : from!"erupted".VK_LOGIC_OP_COPY, // Optional
                        attachmentCount : 1,
                        pAttachments : &colorBlendAttachment,
                        blendConstants : [ 0.0f, 0.0f, 0.0f, 0.0f ] // Optional
                    };

                    // const from!"erupted".VkDynamicState[2] dynamicStates =
                    // [
                        // from!"erupted".VK_DYNAMIC_STATE_VIEWPORT,
                        // from!"erupted".VK_DYNAMIC_STATE_LINE_WIDTH,
                    // ];

                    // const from!"erupted".VkPipelineDynamicStateCreateInfo dynamicState =
                    // {
                    //     dynamicStateCount : 2,
                    //     pDynamicStates : dynamicStates.ptr,
                    // };

                    // Uniform variables and push constants
                    const from!"erupted".VkPipelineLayoutCreateInfo pipelineLayoutInfo =
                    {
                        setLayoutCount : 0, // Optional
                        pSetLayouts : null, // Optional
                        pushConstantRangeCount : 0, // Optional
                        pPushConstantRanges : null, // Optional
                    };

                    VkPipelineLayout pipelineLayout;
                    if(from!"erupted".vkCreatePipelineLayout(
                        arg.device, &pipelineLayoutInfo, null, &pipelineLayout) != VK_SUCCESS)
                    {
                        return err!Res("Failed to create pipeline layout.");
                    }

                    const from!"erupted".VkGraphicsPipelineCreateInfo pipelineInfo =
                    {
                        stageCount : shaderStageInfos.length,
                        pStages : shaderStageInfos.ptr,
                        pVertexInputState : &vertexInput,
                        pInputAssemblyState : &inputAssembly,
                        pViewportState : &viewportState,
                        pRasterizationState : &rasterizer,
                        pMultisampleState : &multisampling,
                        pDepthStencilState : null, // Optional
                        pColorBlendState : &colorBlending,
                        pDynamicState : null, // Optional
                        layout : pipelineLayout,
                        renderPass : arg.renderPass,
                        subpass : 0, // Subpass index of this pipeline.
                        // Specify parent pipelint with common settings.
                        // VK_PIPELINE_CREATE_DERIVATIVE_BIT in flag should be set.
                        basePipelineHandle : from!"erupted".VK_NULL_HANDLE,
                        basePipelineIndex: -1,
                    };

                    VkPipeline graphicsPipeline;
                    if(from!"erupted".vkCreateGraphicsPipelines(arg.device,
                        from!"erupted".VK_NULL_HANDLE, // VkPipelineCache of data relevant to pipeline creation.
                        1, &pipelineInfo, // Possible to create multiple pipelines at once.
                        null, &graphicsPipeline) != VK_SUCCESS)
                    {
                        return err!Res("Failed to create graphics pipeline.");
                    }

                    vkDestroyShaderModule(arg.device, vertShaderModule, null);
                    vkDestroyShaderModule(arg.device, fragShaderModule, null);
                    
                    return ok(Res(forward!arg.expand, pipelineLayout.move, graphicsPipeline.move));
                })
                .orElse!((msg)
                {
                    vkDestroyShaderModule(arg.device, vertShaderModule, null);
                    return err!Res(msg);
                })
                ;
        });
}

auto ref createFramebuffers(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
        && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
        && is(from!"std.range".ElementType!(typeof(arg.swapChainImageViews[])) : from!"erupted".VkImageView)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.range : enumerate;
    import erupted : VK_SUCCESS, vkCreateFramebuffer, vkDestroyFramebuffer;
    import expected : ok, err;
    import automem : Vector;
    
    alias VectorType = Vector!(from!"erupted".VkFramebuffer, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainFramebuffers"));

    VectorType swapChainFramebuffers;
    swapChainFramebuffers.length = arg.swapChainImageViews.length;

    foreach (i, ref imageView; arg.swapChainImageViews[].enumerate)
    {
        const from!"erupted".VkImageView[1] attachments =
        [
            imageView,
        ];

        const from!"erupted".VkFramebufferCreateInfo framebufferInfo =
        {
            renderPass : arg.renderPass,
            attachmentCount : 1,
            pAttachments : attachments.ptr,
            width : arg.swapChainExtent.width,
            height : arg.swapChainExtent.height,
            layers : 1, // Number of layers in image arrays.
        };

        if(vkCreateFramebuffer(arg.device, &framebufferInfo, null, &swapChainFramebuffers.ptr[i]) != VK_SUCCESS)
        {
            foreach(ref framebuffer; swapChainFramebuffers.ptr[0 .. i])
            {
                vkDestroyFramebuffer(arg.device, framebuffer, null);
            }
            return err!Res("Failed to create framebuffer.");
        }
    }

    return ok(Res(forward!arg.expand, swapChainFramebuffers.move));
}

auto ref createCommandPool(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkCommandPool, vkCreateCommandPool, VK_SUCCESS;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkCommandPool, "commandPool",
    ));

    const from!"erupted".VkCommandPoolCreateInfo poolInfo =
    {
        queueFamilyIndex : arg.queueFamilyIndices.graphicsFamily,
        flags : 0
            // Hint that command buffers are rerecorded with new commands very often.
            // | from!"erupted".VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
            // Allow command buffers to be rerecorded individually, by default they are reset together.
            // | from!"erupted".VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            ,
    };

    VkCommandPool commandPool;
    return vkCreateCommandPool(arg.device, &poolInfo, null, &commandPool) == VK_SUCCESS
        ? ok(Res(forward!arg.expand, commandPool.move))
        : err!Res("Failed to create command pool.");
}

auto ref createCommandBuffers(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
        && is(from!"std.range".ElementType!(typeof(arg.swapChainFramebuffers[])) : from!"erupted".VkFramebuffer)
        && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
        && is(typeof(arg.graphicsPipeline) : from!"erupted".VkPipeline)
        && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.range : enumerate;
    import erupted : VK_SUCCESS, vkAllocateCommandBuffers, vkBeginCommandBuffer;
    import expected : ok, err;
    import automem : Vector;
    
    alias VectorType = Vector!(from!"erupted".VkCommandBuffer, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "commandBuffers"));

    VectorType commandBuffers;
    commandBuffers.length = arg.swapChainFramebuffers.length;

    const from!"erupted".VkCommandBufferAllocateInfo allocInfo =
    {
        commandPool : arg.commandPool,
        // Primary - can be submitted to a queue, but cannot be called from other command buffers.
        // Secondary - cannot be submitted directly, but can be called from primary command buffers.
        level : from!"erupted".VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        commandBufferCount : cast(uint) commandBuffers.length,
    };

    if(vkAllocateCommandBuffers(arg.device, &allocInfo, commandBuffers.ptr) != VK_SUCCESS)
    {
        return err!Res("Failed to allocate command buffers.");
    }

    foreach(i, ref commandBuffer; commandBuffers[].enumerate)
    {
        const from!"erupted".VkCommandBufferBeginInfo beginInfo =
        {
            flags : 0
                // Buffer will be rerecorded right after executing it once.
                // | from!"erupted".VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                // This is a secondary command buffer that will be entirely within a single render pass.
                // | from!"erupted".VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
                // Command buffer can be resubmitted while it is pending execution.
                // | from!"erupted".VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
                , // Optional
            pInheritanceInfo : null, // Optional, for secondary buffer specifies what state to inherit from primary buffer.
        };

        if(vkBeginCommandBuffer(commandBuffer, &beginInfo) != VK_SUCCESS)
        {
            return err!Res("Failed to begin recording command buffer.");
        }

        const from!"erupted".VkClearValue clearColor =
        {
            color :
            {
                float32 : [ 0.0f, 0.0f, 0.0f, 1.0f, ],
            },
        };
        
        const from!"erupted".VkRenderPassBeginInfo renderPassInfo =
        {
            renderPass : arg.renderPass,
            framebuffer : arg.swapChainFramebuffers.ptr[i],
            renderArea :
            {
                offset : {0, 0},
                extent : arg.swapChainExtent,
            },
            clearValueCount : 1,
            pClearValues : &clearColor,
        };

        // Inline - render pass commands embedded in the primary command buffer, no secondary buffers will be executed.
        // Secondary command buffers - render pass commands will be executed from secondary buffers.
        from!"erupted".vkCmdBeginRenderPass(
            commandBuffer,
            &renderPassInfo,
            from!"erupted".VK_SUBPASS_CONTENTS_INLINE);

        from!"erupted".vkCmdBindPipeline(
            commandBuffer,
            from!"erupted".VK_PIPELINE_BIND_POINT_GRAPHICS,
            arg.graphicsPipeline);
        
        from!"erupted".vkCmdDraw(
            commandBuffer,
            3, // vertexCount.
            1, // instanceCount
            0, // firstVertex - lowest value of gl_VertexIndex
            0, // firstInstance - lowest value of gl_InstanceIndex
            );
        
        from!"erupted".vkCmdEndRenderPass(commandBuffer);

        if(from!"erupted".vkEndCommandBuffer(commandBuffer) != VK_SUCCESS)
        {
            return err!Res("Failed to record command buffer.");
        }
    }

    return ok(Res(forward!arg.expand, commandBuffers.move));
}

auto ref createSyncObjects(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
        )
{
    import util : TupleCat;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted : VkSemaphore, VkFence, VK_SUCCESS;
    import automem : Vector;
    import expected : ok, err;

    alias VectorType = Vector!(VkFence, Mallocator);

    alias Res = TupleCat!(T, Tuple!(
        VkSemaphore[MaxFramesInFlight], "imageAvailableSemaphores",
        VkSemaphore[MaxFramesInFlight], "renderFinishedSemaphores",
        VkFence[MaxFramesInFlight], "inFlightFences",
        VectorType, "imagesInFlight",
    ));

    VectorType imagesInFlight;
    imagesInFlight.length = arg.swapChainImages.length;

    VkSemaphore[MaxFramesInFlight][2] semaphores;
    VkFence[MaxFramesInFlight] inFlightFences;

    void destroyCreatedObjects(immutable size_t i, immutable size_t j)
    {
        foreach(k; 0 .. i)
        {
            foreach(l; 0 .. semaphores.length)
            {
                from!"erupted".vkDestroySemaphore(arg.device, semaphores[l][k], null);
            }
            from!"erupted".vkDestroyFence(arg.device, inFlightFences[k], null);
        }
        foreach(l; 0 .. j)
        {
            from!"erupted".vkDestroySemaphore(arg.device, semaphores[l][i], null);
        }
    }

    foreach(i; 0 .. MaxFramesInFlight)
    {
        foreach(j; 0 .. semaphores.length)
        {
            const from!"erupted".VkSemaphoreCreateInfo semaphoreInfo;
            if(from!"erupted".vkCreateSemaphore(arg.device, &semaphoreInfo, null, &semaphores[j][i]) != VK_SUCCESS)
            {
                destroyCreatedObjects(i, j);
                return err!Res("Failed to create semaphore.");
            }
        }

        const from!"erupted".VkFenceCreateInfo fenceInfo =
        {
            flags : from!"erupted".VK_FENCE_CREATE_SIGNALED_BIT,
        };
        if(from!"erupted".vkCreateFence(arg.device, &fenceInfo, null, &inFlightFences[i]) != VK_SUCCESS)
        {
            destroyCreatedObjects(i, semaphores.length);
            return err!Res("Failed to create fence.");
        }
    }

    return ok(Res(
        forward!arg.expand,
        semaphores[0].move, semaphores[1].move,
        inFlightFences.move, imagesInFlight.move
        ));
}

auto drawFrame(T)(auto ref T arg, immutable size_t currentFrame) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.graphicsQueue) : from!"erupted".VkQueue)
        && is(typeof(arg.presentQueue) : from!"erupted".VkQueue)
        && is(typeof(arg.swapChain) : from!"erupted".VkSwapchainKHR)
        && is(from!"std.range".ElementType!(typeof(arg.imageAvailableSemaphores[])) : from!"erupted".VkSemaphore)
        && is(from!"std.range".ElementType!(typeof(arg.renderFinishedSemaphores[])) : from!"erupted".VkSemaphore)
        && is(from!"std.range".ElementType!(typeof(arg.inFlightFences[])) : from!"erupted".VkFence)
        && is(from!"std.range".ElementType!(typeof(arg.imagesInFlight[])) : from!"erupted".VkFence)
        && is(from!"std.range".ElementType!(typeof(arg.commandBuffers[])) : from!"erupted".VkCommandBuffer)
    )
{
    import core.lifetime : forward;
    import erupted : vkQueueSubmit, VK_NULL_HANDLE, VK_SUCCESS, VK_TRUE;
    import expected : ok, err;

    from!"erupted".vkWaitForFences(arg.device, 1, &arg.inFlightFences[currentFrame],
        VK_TRUE, // Whether to wait all fences in the array or any of the fences.
        ulong.max
        );
    
    uint imageIndex;
    from!"erupted".vkAcquireNextImageKHR(
        arg.device,
        arg.swapChain,
        ulong.max, // Timeout in nanoseconds. 64 unsigned max disables timeout.
        arg.imageAvailableSemaphores[currentFrame],
        VK_NULL_HANDLE, // Fence.
        &imageIndex,
        );
    
    if(arg.imagesInFlight.ptr[imageIndex] != VK_NULL_HANDLE)
    {
        from!"erupted".vkWaitForFences(arg.device, 1, &arg.imagesInFlight.ptr[imageIndex],
            VK_TRUE, // Whether to wait all fences in the array or any of the fences.
            ulong.max
            );
    }

    arg.imagesInFlight.ptr[imageIndex] = arg.inFlightFences[currentFrame];
    
    const from!"erupted".VkPipelineStageFlags[1] waitStages =
    [
        from!"erupted".VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
    ];

    const from!"erupted".VkSubmitInfo submitInfo =
    {
        waitSemaphoreCount : 1,
        pWaitSemaphores : &arg.imageAvailableSemaphores[currentFrame],
        pWaitDstStageMask : waitStages.ptr,

        commandBufferCount : 1,
        pCommandBuffers : &arg.commandBuffers.ptr[imageIndex], // TODO: get rid of ptr.

        signalSemaphoreCount : 1,
        pSignalSemaphores : &arg.renderFinishedSemaphores[currentFrame],
    };

    from!"erupted".vkResetFences(arg.device, 1, &arg.inFlightFences[currentFrame]);
    if(vkQueueSubmit(arg.graphicsQueue, 1, &submitInfo, arg.inFlightFences[currentFrame]) != VK_SUCCESS)
    {
        return err!T("Failed to submit draw command buffer.");
    }

    const from!"erupted".VkPresentInfoKHR presentInfo =
    {
        waitSemaphoreCount : 1,
        pWaitSemaphores : &arg.renderFinishedSemaphores[currentFrame],

        swapchainCount : 1,
        pSwapchains : &arg.swapChain,
        pImageIndices : &imageIndex,

        // Array of VkResult, usefull when there are multiple swapchains. Otherwise just check the return value.
        pResults : null,
    };

    from!"erupted".vkQueuePresentKHR(arg.presentQueue, &presentInfo);

    return ok(forward!arg);
}

auto ref mainLoop(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.window) : from!"bindbc.glfw".GLFWwindow*)
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.graphicsQueue) : from!"erupted".VkQueue)
        && is(typeof(arg.presentQueue) : from!"erupted".VkQueue)
        && is(from!"std.range".ElementType!(typeof(arg.imageAvailableSemaphores[])) : from!"erupted".VkSemaphore)
        && is(from!"std.range".ElementType!(typeof(arg.renderFinishedSemaphores[])) : from!"erupted".VkSemaphore)
        && is(from!"std.range".ElementType!(typeof(arg.commandBuffers[])) : from!"erupted".VkCommandBuffer)
        && is(from!"std.range".ElementType!(typeof(arg.inFlightFences[])) : from!"erupted".VkFence)
        && is(from!"std.range".ElementType!(typeof(arg.imagesInFlight[])) : from!"erupted".VkFence)
        )
{
    import core.lifetime : move, forward;
    import glfw_vulkan : glfwWindowShouldClose, glfwPollEvents;
    import expected : ok, err;

    size_t currentFrame = 0;
    auto exp = ok(forward!arg);

    while(!glfwWindowShouldClose(exp.value.window))
    {
        glfwPollEvents();
        exp = drawFrame(move(exp.value), currentFrame); // TODO: fix bug in cumulativeFold and use algorithms.
        if(!exp)
        {
            return exp;
        }

        currentFrame = (currentFrame + 1) % MaxFramesInFlight;
    }

    from!"erupted".vkDeviceWaitIdle(arg.device);
    return exp;
}

auto ref cleanupSwapChain(T)(auto ref T arg) nothrow @nogc @trusted
    if(from!"std.typecons".isTuple!T
        && is(typeof(arg.device) : from!"erupted".VkDevice)
        && is(typeof(arg.swapChain) : from!"erupted".VkSwapchainKHR)
        && is(from!"std.range".ElementType!(typeof(arg.swapChainImageViews[])) : from!"erupted".VkImageView)
        && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
        && is(typeof(arg.pipelineLayout) : from!"erupted".VkPipelineLayout)
        && is(typeof(arg.graphicsPipeline) : from!"erupted".VkPipeline)
        && is(from!"std.range".ElementType!(typeof(arg.swapChainFramebuffers[])) : from!"erupted".VkFramebuffer)
        && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
        && is(from!"std.range".ElementType!(typeof(arg.commandBuffers[])) : from!"erupted".VkCommandBuffer)
    )
{
    import util : erase;
    import core.lifetime : forward;
    import std.meta : AliasSeq;
    import expected : ok;

    foreach(ref framebuffer; arg.swapChainFramebuffers[])
    {
        from!"erupted".vkDestroyFramebuffer(arg.device, framebuffer, null);
    }

    from!"erupted".vkFreeCommandBuffers(arg.device, arg.commandPool,
        cast(uint) arg.commandBuffers.length, arg.commandBuffers.ptr);

    from!"erupted".vkDestroyPipeline(arg.device, arg.graphicsPipeline, null);
    from!"erupted".vkDestroyPipelineLayout(arg.device, arg.pipelineLayout, null);
    from!"erupted".vkDestroyRenderPass(arg.device, arg.renderPass, null);

    foreach(ref imageView; arg.swapChainImageViews[])
    {
        from!"erupted".vkDestroyImageView(arg.device, imageView, null);
    }

    from!"erupted".vkDestroySwapchainKHR(arg.device, arg.swapChain, null);

    alias toErase = AliasSeq!(
        "swapChain",
        "swapChainImageViews",
        "renderPass",
        "pipelineLayout",
        "graphicsPipeline",
        "swapChainFramebuffers",
        );
    return ok(forward!arg.erase!toErase);
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
        && is(from!"std.range".ElementType!(typeof(arg.swapChainImageViews[])) : from!"erupted".VkImageView)
        && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
        && is(typeof(arg.pipelineLayout) : from!"erupted".VkPipelineLayout)
        && is(typeof(arg.graphicsPipeline) : from!"erupted".VkPipeline)
        && is(from!"std.range".ElementType!(typeof(arg.swapChainFramebuffers[])) : from!"erupted".VkFramebuffer)
        && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
        && is(from!"std.range".ElementType!(typeof(arg.commandBuffers[])) : from!"erupted".VkCommandBuffer)
        && is(from!"std.range".ElementType!(typeof(arg.imageAvailableSemaphores[])) : from!"erupted".VkSemaphore)
        && is(from!"std.range".ElementType!(typeof(arg.renderFinishedSemaphores[])) : from!"erupted".VkSemaphore)
        && is(from!"std.range".ElementType!(typeof(arg.inFlightFences[])) : from!"erupted".VkFence)
    )
{
    import util : erase;
    import core.lifetime : forward;
    import std.meta : AliasSeq;
    import erupted.vulkan_lib_loader : freeVulkanLib;
    import glfw_vulkan : glfwDestroyWindow, glfwTerminate;
    import expected : ok, andThen;

    return cleanupSwapChain(forward!arg)
        .andThen!((auto ref arg)
        {
            foreach(i; 0 .. MaxFramesInFlight)
            {
                from!"erupted".vkDestroyFence(arg.device, arg.inFlightFences[i], null);
                from!"erupted".vkDestroySemaphore(arg.device, arg.renderFinishedSemaphores[i], null);
                from!"erupted".vkDestroySemaphore(arg.device, arg.imageAvailableSemaphores[i], null);
            }

            from!"erupted".vkDestroyCommandPool(arg.device, arg.commandPool, null);

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
                alias validationLayersToErase = AliasSeq!("debugMessenger");
            }
            else
            {
                alias validationLayersToErase = AliasSeq!();
            }

            alias toErase = AliasSeq!(
                "window",
                "instance",
                "surface",
                "device",
                "commandPool",
                "imageAvailableSemaphores",
                "renderFinishedSemaphores",
                "inFlightFences",
                validationLayersToErase,
                );
            return ok(forward!arg.erase!toErase);
        });
}
