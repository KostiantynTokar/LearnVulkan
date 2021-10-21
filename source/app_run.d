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

struct Vertex
{
    from!"gfm.math".vec2f pos;
    from!"gfm.math".vec3f color;

    static from!"erupted".VkVertexInputBindingDescription getBindingDescription() nothrow @nogc @trusted
    {
        const from!"erupted".VkVertexInputBindingDescription bindingDescription =
        {
            // Index of the binding in the array of bindings of VkPipelineVertexInputStateCreateInfo.
            binding : 0,
            // Number of bytes from one entry to next.
            stride : Vertex.sizeof,
            // VERTEX or INSTANCE: move to next entry after each vertex/instance.
            inputRate : from!"erupted".VK_VERTEX_INPUT_RATE_VERTEX,
        };
        return bindingDescription;
    }

    static from!"erupted".VkVertexInputAttributeDescription[2] getAttributeDescriptions() nothrow @nogc @trusted
    {
        const from!"erupted".VkVertexInputAttributeDescription[2] attributeDescriptions =
        [
            {
                // From which binding data comes.
                binding : 0,
                // Input location in the vertex shader.
                location : 0,
                // Formats: R32[G32[B32[A32]]]_<SFLOAT|SINT|UINT> etc.
                format : from!"erupted".VK_FORMAT_R32G32_SFLOAT,
                offset : Vertex.pos.offsetof,
            },
            {
                binding : 0,
                location : 1,
                format : from!"erupted".VK_FORMAT_R32G32B32_SFLOAT,
                offset : Vertex.color.offsetof,
            },
        ];
        return attributeDescriptions;
    }
}

immutable Vertex[3] vertices = ()
{
    import gfm.math : vec2f, vec3f;
    immutable Vertex[3] vertices = [
        { vec2f( 0.0f, -0.5f), vec3f(1.0f, 0.0f, 0.0f) },
        { vec2f( 0.5f,  0.5f), vec3f(0.0f, 1.0f, 0.0f) },
        { vec2f(-0.5f,  0.5f), vec3f(0.0f, 0.0f, 1.0f) },
    ];
    return vertices;
}();

auto ref initWindow(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator;
    import glfw_vulkan : glfwInit, glfwWindowHint, glfwCreateWindow,
            glfwSetWindowUserPointer, glfwSetFramebufferSizeCallback, GLFWwindow,
            GLFW_CLIENT_API, GLFW_NO_API, GLFW_RESIZABLE, GLFW_FALSE;
    import expected : ok;
    import automem : RefCounted;

    alias PtrType = RefCounted!(bool, Mallocator);
    alias Res = TupleCat!(T, Tuple!(GLFWwindow*, "window", PtrType, "framebufferResized"));
    auto res = partialConstruct!Res(forward!arg);

    res.framebufferResized = typeof(res.framebufferResized)(false);

    glfwInit();

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    // glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    res.window = glfwCreateWindow(StartWindowWidth, StartWindowHeight, "LearnVulkan", null, null);
    glfwSetWindowUserPointer(res.window, res.framebufferResized);
    glfwSetFramebufferSizeCallback(res.window, &framebufferResizeCallback);
    return ok(res.move);
}

extern(C) void framebufferResizeCallback(from!"glfw_vulkan".GLFWwindow* window, int width, int height) nothrow @nogc @trusted
{
    import glfw_vulkan : glfwGetWindowUserPointer;
    auto framebufferResized = cast(bool*) glfwGetWindowUserPointer(window);
    *framebufferResized = true;
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
        .andThen!createCommandPool
        .andThen!createVertexBuffer
        .andThen!createCommandBuffers
        .andThen!createSyncObjects
        ;
}

auto ref createInstance(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T)
{
    import util : TupleCat, partialConstruct;
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
    auto res = partialConstruct!Res(forward!arg);

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

    return vkCreateInstance(&createInfo, null, &res.instance) == VK_SUCCESS
        ? ok(res.move)
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
        import util : TupleCat, partialConstruct;
        import core.lifetime : forward, move;
        import std.typecons : Tuple;
        import erupted : vkCreateDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT, VK_SUCCESS;
        import expected : ok, err;
        
        alias Res = TupleCat!(T, Tuple!(VkDebugUtilsMessengerEXT, "debugMessenger"));
        auto res = partialConstruct!Res(forward!arg);

        const createInfo = defaultDebugMessengerCreateInfo();

        return vkCreateDebugUtilsMessengerEXT(res.instance, &createInfo, null, &res.debugMessenger) == VK_SUCCESS
            ? ok(res.move)
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
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import glfw_vulkan : glfwCreateWindowSurface;
    import erupted : VkSurfaceKHR, VK_SUCCESS;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(VkSurfaceKHR, "surface"));
    auto res = partialConstruct!Res(forward!arg);

    return glfwCreateWindowSurface(res.instance, res.window, null, &res.surface) == VK_SUCCESS
        ? ok(res.move)
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
    from!"erupted".VkSurfaceFormatKHR surfaceFormat;
    from!"erupted".VkPresentModeKHR presentMode;
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

from!"std.typecons".Tuple!(
    from!"erupted".VkExtent2D, "extent",
    from!"erupted".VkSurfaceCapabilitiesKHR, "capabilities")
    chooseSwapExtent(
        from!"glfw_vulkan".GLFWwindow* window,
        from!"erupted".VkPhysicalDevice device,
        from!"erupted".VkSurfaceKHR surface,
        in int framebufferWidth,
        in int framebufferHeight
    ) nothrow @nogc @trusted
{
    import std.algorithm : clamp;
    import erupted : vkGetPhysicalDeviceSurfaceCapabilitiesKHR;

    from!"erupted".VkSurfaceCapabilitiesKHR capabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &capabilities);

    if(capabilities.currentExtent.width != uint.max)
    {
        // Window manager allows non-fixed extent.
        return typeof(return)(capabilities.currentExtent, capabilities);
    }

    from!"erupted".VkExtent2D actualExtent;

    actualExtent.width = framebufferWidth
        .clamp(
            capabilities.minImageExtent.width,
            capabilities.maxImageExtent.width
            );
    actualExtent.height = framebufferHeight
        .clamp(
            capabilities.minImageExtent.height,
            capabilities.maxImageExtent.height
            );

    return typeof(return)(actualExtent, capabilities);
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
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
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
    auto res = partialConstruct!Res(forward!arg);
    
    uint deviceCount;
    vkEnumeratePhysicalDevices(res.instance, &deviceCount, null);

    if(deviceCount == 0)
    {
        return err!Res("Failed to find GPU with Vulkan support.");
    }

    auto devices = Mallocator.instance.makeArray!VkPhysicalDevice(deviceCount);
    scope(exit) () @trusted { Mallocator.instance.dispose(devices); }();

    vkEnumeratePhysicalDevices(res.instance, &deviceCount, devices.ptr);

    return devices.zip(res.repeat)
        .map!((elem)
        {
            elem[1].physicalDevice = elem[0];
            if(!checkDeviceExtensionSupport(elem[1].physicalDevice))
            {
                return err!Res("Failed to find suitable GPU.");
            }
            auto swapChainSupport = querySwapChainSupport(elem[1].physicalDevice, elem[1].surface);
            if(swapChainSupport.formats.empty || swapChainSupport.presentModes.empty)
            {
                return err!Res("Failed to find suitable GPU.");
            }
            elem[1].chosenSwapChainSupport = ChosenSwapChainSupport(
                chooseSwapSurfaceFormat(swapChainSupport.formats),
                chooseSwapPresentMode(swapChainSupport.presentModes),
                chooseImageCount(swapChainSupport.capabilities)
            );
            immutable indices = findQueueFamilies(elem[1].physicalDevice, elem[1].surface);
            return indices.match!(
                (queueFamilyIndices)
                {
                    elem[1].queueFamilyIndices = queueFamilyIndices;
                    return ok(elem[1].move);
                },
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
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.algorithm : sort, uniq;
    import erupted : VkDevice;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(VkDevice, "device"));
    auto res = partialConstruct!Res(forward!arg);

    enum queuesCount = res.queueFamilyIndices.tupleof.length;

    from!"erupted".VkDeviceQueueCreateInfo[queuesCount] queueCreateInfos;
    uint[queuesCount] queueFamilies = [res.queueFamilyIndices.tupleof];
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

    return from!"erupted".vkCreateDevice(res.physicalDevice, &createInfo, null, &res.device) == from!"erupted".VK_SUCCESS
        ? ok(res.move)
        : err!Res("Failed to create logical device.");
}

auto ref getDeviceQueues(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkQueue, vkGetDeviceQueue;

    alias Res = TupleCat!(T, Tuple!(VkQueue, "graphicsQueue", VkQueue, "presentQueue"));
    auto res = partialConstruct!Res(forward!arg);

    vkGetDeviceQueue(res.device, res.queueFamilyIndices.graphicsFamily, 0, &res.graphicsQueue);

    vkGetDeviceQueue(res.device, res.queueFamilyIndices.presentFamily, 0, &res.presentQueue);

    return from!"expected".ok(res.move);
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
        .andThen!recreateCommandBuffers
        ;
}

auto ref recreateCommandBuffers(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainFramebuffers[])) : from!"erupted".VkFramebuffer)
    && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
    && is(typeof(arg.graphicsPipeline) : from!"erupted".VkPipeline)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
)
{
    import util : erase;
    import core.lifetime : forward;
    return createCommandBuffers(forward!arg.erase!"commandBuffers");
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
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import glfw_vulkan : glfwGetFramebufferSize, glfwWaitEvents;
    import erupted : VkSwapchainKHR, VkFormat, VkExtent2D;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkSwapchainKHR, "swapChain",
        VkFormat, "swapChainImageFormat",
        VkExtent2D, "swapChainExtent",
        ));
    auto res = partialConstruct!Res(forward!arg);

    int width = 0;
    int height = 0;
    while(width == 0 || height == 0)
    {
        glfwGetFramebufferSize(res.window, &width, &height);
        glfwWaitEvents();
    }
    
    immutable extentAndCapabilites = chooseSwapExtent(res.window, res.physicalDevice, res.surface, width, height);
    res.swapChainExtent = extentAndCapabilites.extent;

    from!"erupted".VkSwapchainCreateInfoKHR createInfo =
    {
        surface : res.surface,
        minImageCount : res.chosenSwapChainSupport.imageCount,
        imageFormat : res.chosenSwapChainSupport.surfaceFormat.format,
        imageColorSpace : res.chosenSwapChainSupport.surfaceFormat.colorSpace,
        imageExtent : res.swapChainExtent,
        // Always 1 unless for stereoscopic 3D application.
        imageArrayLayers : 1,
        // Render directly to imega. Use VK_IMAGE_USAGE_TRANSFER_DST_BIT for off-screen rendering.
        imageUsage : from!"erupted".VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        // Do not want any transforms applied to swap chain images.
        preTransform : extentAndCapabilites.capabilities.currentTransform,
        // Blending with other windows in the window system.
        compositeAlpha : from!"erupted".VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        presentMode : res.chosenSwapChainSupport.presentMode,
        // Do not render obscured pixels.
        clipped : from!"erupted".VK_TRUE,
        oldSwapchain : from!"erupted".VK_NULL_HANDLE,
    };

    if(res.queueFamilyIndices.graphicsFamily != res.queueFamilyIndices.presentFamily)
    {
        const uint[2] queueFamilyIndicesArr =
        [
            res.queueFamilyIndices.graphicsFamily,
            res.queueFamilyIndices.presentFamily,
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

    res.swapChainImageFormat = createInfo.imageFormat;

    return from!"erupted".vkCreateSwapchainKHR(res.device, &createInfo, null, &res.swapChain) == from!"erupted".VK_SUCCESS
        ? ok(res.move)
        : err!Res("Failed to create swap chain.");
}

auto ref getSwapChainImages(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChain) : from!"erupted".VkSwapchainKHR)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted : vkGetSwapchainImagesKHR;
    import expected : ok;
    import automem : Vector;
    
    alias VectorType = Vector!(from!"erupted".VkImage, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainImages"));
    auto res = partialConstruct!Res(forward!arg);

    uint imageCount;
    vkGetSwapchainImagesKHR(res.device, res.swapChain, &imageCount, null);
    res.swapChainImages.length = imageCount;
    vkGetSwapchainImagesKHR(res.device, res.swapChain, &imageCount, res.swapChainImages.ptr);
    return ok(res.move);
}

auto ref createImageViews(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChainImageFormat) : from!"erupted".VkFormat)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.range : enumerate;
    import std.experimental.allocator.mallocator : Mallocator;
    import expected : ok, err;
    import automem : Vector;

    alias VectorType = Vector!(from!"erupted".VkImageView, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainImageViews"));
    auto res = partialConstruct!Res(forward!arg);

    res.swapChainImageViews.length = res.swapChainImages.length;

    foreach(i, ref image; res.swapChainImages[].enumerate)
    {
        const from!"erupted".VkImageViewCreateInfo createInfo =
        {
            image : image,
            viewType : from!"erupted".VK_IMAGE_VIEW_TYPE_2D,
            format : res.swapChainImageFormat,

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

        if(from!"erupted".vkCreateImageView(res.device, &createInfo, null, &res.swapChainImageViews.ptr[i])
            != from!"erupted".VK_SUCCESS)
        {
            foreach(ref imageView; res.swapChainImageViews.ptr[0 .. i])
            {
                from!"erupted".vkDestroyImageView(res.device, imageView, null);
            }
            return err!Res("Failed to create image view.");
        }
    }

    return ok(res.move);
}

auto ref createRenderPass(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChainImageFormat) : from!"erupted".VkFormat)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkRenderPass, VK_SUCCESS;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkRenderPass, "renderPass"
    ));
    auto res = partialConstruct!Res(forward!arg);

    const from!"erupted".VkAttachmentDescription colorAttachment =
    {
        format : res.swapChainImageFormat,
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

    return from!"erupted".vkCreateRenderPass(res.device, &renderPassInfo, null, &res.renderPass) == VK_SUCCESS
        ? ok(res.move)
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
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkPipelineLayout, VkPipeline, vkDestroyShaderModule, VK_FALSE, VK_TRUE, VK_SUCCESS;
    import expected : ok, err, andThen, orElse;

    alias Res = TupleCat!(T, Tuple!
        (
            VkPipelineLayout, "pipelineLayout",
            VkPipeline, "graphicsPipeline",
        ));
    auto res = partialConstruct!Res(forward!arg);

    return createShaderModule(res.device, "shaders_bin/vert.spv")
        .andThen!((vertShaderModule) @trusted
        {
            return createShaderModule(res.device, "shaders_bin/frag.spv")
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

                    const bindingDescription = Vertex.getBindingDescription();
                    const attributeDescriptions = Vertex.getAttributeDescriptions();

                    const from!"erupted".VkPipelineVertexInputStateCreateInfo vertexInputInfo =
                    {
                        // Data is per-vertex or per-instance?
                        vertexBindingDescriptionCount : 1,
                        pVertexBindingDescriptions : &bindingDescription,
                        // Binding and offset of attributes.
                        vertexAttributeDescriptionCount : attributeDescriptions.length,
                        pVertexAttributeDescriptions : attributeDescriptions.ptr,
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
                        width : res.swapChainExtent.width,
                        height : res.swapChainExtent.height,
                        minDepth : 0.0f,
                        maxDepth : 1.0f,
                    };

                    const from!"erupted".VkRect2D scissor =
                    {
                        offset : {0, 0},
                        extent : res.swapChainExtent,
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

                    if(from!"erupted".vkCreatePipelineLayout(
                        res.device, &pipelineLayoutInfo, null, &res.pipelineLayout) != VK_SUCCESS)
                    {
                        return err!Res("Failed to create pipeline layout.");
                    }

                    const from!"erupted".VkGraphicsPipelineCreateInfo pipelineInfo =
                    {
                        stageCount : shaderStageInfos.length,
                        pStages : shaderStageInfos.ptr,
                        pVertexInputState : &vertexInputInfo,
                        pInputAssemblyState : &inputAssembly,
                        pViewportState : &viewportState,
                        pRasterizationState : &rasterizer,
                        pMultisampleState : &multisampling,
                        pDepthStencilState : null, // Optional
                        pColorBlendState : &colorBlending,
                        pDynamicState : null, // Optional
                        layout : res.pipelineLayout,
                        renderPass : res.renderPass,
                        subpass : 0, // Subpass index of this pipeline.
                        // Specify parent pipelint with common settings.
                        // VK_PIPELINE_CREATE_DERIVATIVE_BIT in flag should be set.
                        basePipelineHandle : from!"erupted".VK_NULL_HANDLE,
                        basePipelineIndex: -1,
                    };

                    if(from!"erupted".vkCreateGraphicsPipelines(res.device,
                        from!"erupted".VK_NULL_HANDLE, // VkPipelineCache of data relevant to pipeline creation.
                        1, &pipelineInfo, // Possible to create multiple pipelines at once.
                        null, &res.graphicsPipeline) != VK_SUCCESS)
                    {
                        return err!Res("Failed to create graphics pipeline.");
                    }

                    vkDestroyShaderModule(res.device, vertShaderModule, null);
                    vkDestroyShaderModule(res.device, fragShaderModule, null);
                    
                    return ok(res.move);
                })
                .orElse!((msg)
                {
                    vkDestroyShaderModule(res.device, vertShaderModule, null);
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
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.range : enumerate;
    import erupted : VK_SUCCESS, vkCreateFramebuffer, vkDestroyFramebuffer;
    import expected : ok, err;
    import automem : Vector;
    
    alias VectorType = Vector!(from!"erupted".VkFramebuffer, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainFramebuffers"));
    auto res = partialConstruct!Res(forward!arg);

    res.swapChainFramebuffers.length = res.swapChainImageViews.length;

    foreach (i, ref imageView; res.swapChainImageViews[].enumerate)
    {
        const from!"erupted".VkImageView[1] attachments =
        [
            imageView,
        ];

        const from!"erupted".VkFramebufferCreateInfo framebufferInfo =
        {
            renderPass : res.renderPass,
            attachmentCount : 1,
            pAttachments : attachments.ptr,
            width : res.swapChainExtent.width,
            height : res.swapChainExtent.height,
            layers : 1, // Number of layers in image arrays.
        };

        if(vkCreateFramebuffer(res.device, &framebufferInfo, null, &res.swapChainFramebuffers.ptr[i]) != VK_SUCCESS)
        {
            foreach(ref framebuffer; res.swapChainFramebuffers.ptr[0 .. i])
            {
                vkDestroyFramebuffer(res.device, framebuffer, null);
            }
            return err!Res("Failed to create framebuffer.");
        }
    }

    return ok(res.move);
}

auto findMemoryType(T)(
    auto ref T arg,
    in from!"erupted".VkMemoryPropertyFlags properties
    ) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.physicalDevice) : from!"erupted".VkPhysicalDevice)
    && is(typeof(arg.memRequirements) : from!"erupted".VkMemoryRequirements)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(uint, "chosenMemoryTypeIndex"));
    auto res = partialConstruct!Res(forward!arg);
    
    from!"erupted".VkPhysicalDeviceMemoryProperties memProperties;
    from!"erupted".vkGetPhysicalDeviceMemoryProperties(res.physicalDevice, &memProperties);

    foreach(i; 0 .. memProperties.memoryTypeCount)
    {
        if((res.memRequirements.memoryTypeBits & (1 << i)) // Check memory type.
            // Check the heap and its memory types properties.
            && (memProperties.memoryTypes[i].propertyFlags & properties) == properties
        )
        {
            res.chosenMemoryTypeIndex = i;
            return ok(res.move);
        }
    }

    return err!Res("Falied to find suitable memory type.");
}

auto ref createVertexBuffer(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
)
{
    import util : TupleCat, partialConstruct, TupleErase, erase;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.meta : AliasSeq;
    import erupted : VK_SUCCESS;
    import expected : ok, err, andThen;

    return (auto ref arg)
    {
        alias Res = TupleCat!(T, Tuple!(
            from!"erupted".VkBuffer, "vertexBuffer",
            from!"erupted".VkMemoryRequirements, "memRequirements",
        ));
        auto res = partialConstruct!Res(forward!arg);

        const from!"erupted".VkBufferCreateInfo bufferInfo =
        {
            size : vertices.sizeof,
            // Possible to specify multiple purposes using a bitwase or.
            usage : from!"erupted".VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
            // Sharing between queue families.
            sharingMode : from!"erupted".VK_SHARING_MODE_EXCLUSIVE,
            // Configure sparse buffer memory.
            flags : 0,
        };

        if(from!"erupted".vkCreateBuffer(res.device, &bufferInfo, null, &res.vertexBuffer) != VK_SUCCESS)
        {
            return err!Res("Failed to create vertex buffer.");
        }

        from!"erupted".vkGetBufferMemoryRequirements(res.device, res.vertexBuffer, &res.memRequirements);

        return ok(res.move);
    }(forward!arg)
    .andThen!findMemoryType(
        from!"erupted".VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | // Host (CPU) can see the memory.
        from!"erupted".VK_MEMORY_PROPERTY_HOST_COHERENT_BIT // Data can be immediatly written to the memory by host.
    )
    .andThen!((auto ref arg)
    {
        import core.stdc.string : memcpy;
        
        alias TempRes = TupleCat!(typeof(arg), Tuple!(
            from!"erupted".VkDeviceMemory, "vertexBufferMemory",
        ));
        alias toErase = AliasSeq!("memRequirements", "chosenMemoryType");
        alias Res = TupleErase!(TempRes, toErase);
        auto res = partialConstruct!TempRes(forward!arg);

        from!"erupted".VkMemoryAllocateInfo allocInfo =
        {
            allocationSize : res.memRequirements.size,
            memoryTypeIndex : res.chosenMemoryTypeIndex,
        };

        if(from!"erupted".vkAllocateMemory(res.device, &allocInfo, null, &res.vertexBufferMemory) != VK_SUCCESS)
        {
            return err!Res("Failed to allocate vertex buffer memory.");
        }

        // Device, vertex buffer, device memory and offset within the device memory.
        from!"erupted".vkBindBufferMemory(res.device, res.vertexBuffer, res.vertexBufferMemory, 0);

        void* data;
        from!"erupted".vkMapMemory(res.device, res.vertexBufferMemory,
            0, // Offset.
            vertices.sizeof, // Size (or VK_WHOLE_SIZE).
            0, // Flags, not used in current API.
            &data
        );
        memcpy(data, vertices.ptr, vertices.sizeof);
        auto tmp = cast(void*)vertices.ptr;
        from!"erupted".vkUnmapMemory(res.device, res.vertexBufferMemory);
        
        return ok(res.move.erase!toErase);
    })
    ;
}

auto ref createCommandPool(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import erupted : VkCommandPool, vkCreateCommandPool, VK_SUCCESS;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkCommandPool, "commandPool",
    ));
    auto res = partialConstruct!Res(forward!arg);

    const from!"erupted".VkCommandPoolCreateInfo poolInfo =
    {
        queueFamilyIndex : res.queueFamilyIndices.graphicsFamily,
        flags : 0
            // Hint that command buffers are rerecorded with new commands very often.
            // | from!"erupted".VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
            // Allow command buffers to be rerecorded individually, by default they are reset together.
            // | from!"erupted".VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            ,
    };

    return vkCreateCommandPool(res.device, &poolInfo, null, &res.commandPool) == VK_SUCCESS
        ? ok(res.move)
        : err!Res("Failed to create command pool.");
}

auto ref createCommandBuffers(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainFramebuffers[])) : from!"erupted".VkFramebuffer)
    && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
    && is(typeof(arg.graphicsPipeline) : from!"erupted".VkPipeline)
    && is(typeof(arg.vertexBuffer) : from!"erupted".VkBuffer)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.range : enumerate;
    import erupted : VK_SUCCESS, vkAllocateCommandBuffers, vkBeginCommandBuffer;
    import expected : ok, err;
    import automem : Vector;
    
    alias VectorType = Vector!(from!"erupted".VkCommandBuffer, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "commandBuffers"));
    auto res = partialConstruct!Res(forward!arg);

    res.commandBuffers.length = res.swapChainFramebuffers.length;

    const from!"erupted".VkCommandBufferAllocateInfo allocInfo =
    {
        commandPool : res.commandPool,
        // Primary - can be submitted to a queue, but cannot be called from other command buffers.
        // Secondary - cannot be submitted directly, but can be called from primary command buffers.
        level : from!"erupted".VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        commandBufferCount : cast(uint) res.commandBuffers.length,
    };

    if(vkAllocateCommandBuffers(res.device, &allocInfo, res.commandBuffers.ptr) != VK_SUCCESS)
    {
        return err!Res("Failed to allocate command buffers.");
    }

    foreach(i, ref commandBuffer; res.commandBuffers[].enumerate)
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
            renderPass : res.renderPass,
            framebuffer : res.swapChainFramebuffers.ptr[i],
            renderArea :
            {
                offset : {0, 0},
                extent : res.swapChainExtent,
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
            res.graphicsPipeline);

        const from!"erupted".VkDeviceSize[1] offsets = [ 0 ];
        from!"erupted".vkCmdBindVertexBuffers(
            commandBuffer,
            0, // Offset.
            1, // Number of bindings.
            &res.vertexBuffer,
            offsets.ptr,
        );
        
        from!"erupted".vkCmdDraw(
            commandBuffer,
            vertices.length, // vertexCount.
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

    return ok(res.move);
}

auto ref createSyncObjects(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
)
{
    import util : TupleCat, partialConstruct;
    import core.lifetime : forward, move;
    import std.typecons : Tuple;
    import std.range : only;
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
    auto res = partialConstruct!Res(forward!arg);

    res.imagesInFlight.length = res.swapChainImages.length;

    VkSemaphore[MaxFramesInFlight]*[2] semaphores = [ &res.imageAvailableSemaphores, &res.renderFinishedSemaphores ];

    void destroyCreatedObjects(immutable size_t i, immutable size_t j)
    {
        foreach(k; 0 .. i)
        {
            foreach(l; 0 .. semaphores.length)
            {
                from!"erupted".vkDestroySemaphore(res.device, (*semaphores[l])[k], null);
            }
            from!"erupted".vkDestroyFence(res.device, res.inFlightFences[k], null);
        }
        foreach(l; 0 .. j)
        {
            from!"erupted".vkDestroySemaphore(res.device, (*semaphores[l])[i], null);
        }
    }

    foreach(i; 0 .. MaxFramesInFlight)
    {
        foreach(j; 0 .. semaphores.length)
        {
            const from!"erupted".VkSemaphoreCreateInfo semaphoreInfo;
            if(from!"erupted".vkCreateSemaphore(res.device, &semaphoreInfo, null, &(*semaphores[j])[i]) != VK_SUCCESS)
            {
                destroyCreatedObjects(i, j);
                return err!Res("Failed to create semaphore.");
            }
        }

        const from!"erupted".VkFenceCreateInfo fenceInfo =
        {
            flags : from!"erupted".VK_FENCE_CREATE_SIGNALED_BIT,
        };
        if(from!"erupted".vkCreateFence(res.device, &fenceInfo, null, &res.inFlightFences[i]) != VK_SUCCESS)
        {
            destroyCreatedObjects(i, semaphores.length);
            return err!Res("Failed to create fence.");
        }
    }

    return ok(res.move);
}

from!"expected".Expected!T drawFrame(T)(auto ref T arg, immutable size_t currentFrame) nothrow @nogc @trusted
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
    import expected : ok, err, andThen;

    from!"erupted".vkWaitForFences(arg.device, 1, &arg.inFlightFences[currentFrame],
        VK_TRUE, // Whether to wait all fences in the array or any of the fences.
        ulong.max
        );
    
    uint imageIndex;
    immutable result = from!"erupted".vkAcquireNextImageKHR(
        arg.device,
        arg.swapChain,
        ulong.max, // Timeout in nanoseconds. 64 unsigned max disables timeout.
        arg.imageAvailableSemaphores[currentFrame],
        VK_NULL_HANDLE, // Fence.
        &imageIndex,
        );
    
    if(result == from!"erupted".VK_ERROR_OUT_OF_DATE_KHR || *arg.framebufferResized)
    {
        *arg.framebufferResized = false;
        return recreateSwapChain(forward!arg)
            .andThen!drawFrame(currentFrame)
            ;
    }
    else if(result != VK_SUCCESS && result != from!"erupted".VK_SUBOPTIMAL_KHR)
    {
        return err!T("Failed to acquire swap chain image.");
    }
    
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

    from!"erupted".vkDeviceWaitIdle(exp.value.device);
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
        "swapChainImageFormat",
        "swapChainExtent",
        "swapChain",
        "swapChainImages",
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
    && is(typeof(arg.framebufferResized))
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
    && is(typeof(arg.vertexBufferMemory) : from!"erupted".VkDeviceMemory)
    && is(typeof(arg.vertexBuffer) : from!"erupted".VkBuffer)
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

            from!"erupted".vkDestroyBuffer(arg.device, arg.vertexBuffer, null);

            from!"erupted".vkFreeMemory(arg.device, arg.vertexBufferMemory, null);

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
                "framebufferResized",
                "instance",
                "surface",
                "device",
                "commandPool",
                "vertexBufferMemory",
                "vertexBuffer",
                "imageAvailableSemaphores",
                "renderFinishedSemaphores",
                "inFlightFences",
                validationLayersToErase,
                );
            return ok(forward!arg.erase!toErase);
        });
}
