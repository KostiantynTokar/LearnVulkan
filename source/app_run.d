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

enum ModelPath = "source/models/viking_room.obj";
enum TexturePath = "source/textures/viking_room.png";

enum MaxFramesInFlight = 3;

struct Vertex
{
    import gfm.math : vec2f, vec3f;
    import erupted;

    vec3f pos;
    vec3f color;
    vec2f texCoord;

    static VkVertexInputBindingDescription getBindingDescription() nothrow @nogc @trusted
    {
        const VkVertexInputBindingDescription bindingDescription =
        {
            // Index of the binding in the array of bindings of VkPipelineVertexInputStateCreateInfo.
            binding : 0,
            // Number of bytes from one entry to next.
            stride : Vertex.sizeof,
            // VERTEX or INSTANCE: move to next entry after each vertex/instance.
            inputRate : VK_VERTEX_INPUT_RATE_VERTEX,
        };
        return bindingDescription;
    }

    static VkVertexInputAttributeDescription[3] getAttributeDescriptions() nothrow @nogc @trusted
    {
        const VkVertexInputAttributeDescription[3] attributeDescriptions =
        [
            {
                // From which binding data comes.
                binding : 0,
                // Input location in the vertex shader.
                location : 0,
                // Formats: R32[G32[B32[A32]]]_<SFLOAT|SINT|UINT> etc.
                format : VK_FORMAT_R32G32B32_SFLOAT,
                offset : Vertex.pos.offsetof,
            },
            {
                binding : 0,
                location : 1,
                format : VK_FORMAT_R32G32B32_SFLOAT,
                offset : Vertex.color.offsetof,
            },
            {
                binding : 0,
                location : 2,
                format : VK_FORMAT_R32G32_SFLOAT,
                offset : Vertex.texCoord.offsetof,
            },
        ];
        return attributeDescriptions;
    }
}

// immutable vertices = ()
// {
//     import std.array : staticArray;
//     import gfm.math : vec2f, vec3f;
//     return
//         [
//             Vertex( vec3f(-0.5f, -0.5f,  0.0f), vec3f(1.0f, 0.0f, 0.0f), vec2f(0.0f, 0.0f) ),
//             Vertex( vec3f( 0.5f, -0.5f,  0.0f), vec3f(0.0f, 1.0f, 0.0f), vec2f(1.0f, 0.0f) ),
//             Vertex( vec3f( 0.5f,  0.5f,  0.0f), vec3f(0.0f, 0.0f, 1.0f), vec2f(1.0f, 1.0f) ),
//             Vertex( vec3f(-0.5f,  0.5f,  0.0f), vec3f(1.0f, 1.0f, 1.0f), vec2f(0.0f, 1.0f) ),

//             Vertex( vec3f(-0.5f, -0.5f, -0.5f), vec3f(1.0f, 0.0f, 0.0f), vec2f(0.0f, 0.0f) ),
//             Vertex( vec3f( 0.5f, -0.5f, -0.5f), vec3f(0.0f, 1.0f, 0.0f), vec2f(1.0f, 0.0f) ),
//             Vertex( vec3f( 0.5f,  0.5f, -0.5f), vec3f(0.0f, 0.0f, 1.0f), vec2f(1.0f, 1.0f) ),
//             Vertex( vec3f(-0.5f,  0.5f, -0.5f), vec3f(1.0f, 1.0f, 1.0f), vec2f(0.0f, 1.0f) ),
//         ]
//         .staticArray
//         ;
// }();
// immutable indices = ()
// {
//     import std.array : staticArray;
//     static ushort s(int n)
//     {
//         return cast(ushort) n;
//     }
//     return
//         [
//             s(0), s(1), s(2), s(2), s(3), s(0),
//             s(4), s(5), s(6), s(6), s(7), s(4),
//         ]
//         .staticArray
//         ;
// }();

struct UniformBufferObject
{
    import gfm.math : mat4f;

    align(16) mat4f model;
    align(16) mat4f view;
    align(16) mat4f proj;
}

auto ref initWindow(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T)
{
    import util;
    import std.experimental.allocator.mallocator : Mallocator;
    import glfw_vulkan;
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
        .andThen!createDescriptorSetLayout
        .andThen!createCommandPool
        .andThen!createTextureImage
        .andThen!createTextureImageView
        .andThen!createTextureSampler
        .andThen!loadModel
        .andThen!createVertexBuffer
        .andThen!createIndexBuffer
        .andThen!createSwapChainAndRelatedObjects
        .andThen!createSyncObjects
        ;
}

auto ref createInstance(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T)
{
    import util;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : makeArray, dispose;
    import erupted;
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
        import erupted;
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
        import util;
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
    import util;
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
    import erupted;
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
    import erupted;
    import automem : Vector;

    VkSurfaceCapabilitiesKHR capabilities;
    Vector!(VkSurfaceFormatKHR, Mallocator) formats;
    Vector!(VkPresentModeKHR, Mallocator) presentModes;
}

SwapChainSupportDetails querySwapChainSupport(
    from!"erupted".VkPhysicalDevice device,
    from!"erupted".VkSurfaceKHR surface,
    ) nothrow @nogc @trusted
{
    import erupted;
    
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
    import erupted;

    VkSurfaceFormatKHR surfaceFormat;
    VkPresentModeKHR presentMode;
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
    import erupted;

    VkSurfaceCapabilitiesKHR capabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &capabilities);

    if(capabilities.currentExtent.width != uint.max)
    {
        // Window manager allows non-fixed extent.
        return typeof(return)(capabilities.currentExtent, capabilities);
    }

    VkExtent2D actualExtent;

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

from!"erupted".VkSampleCountFlagBits getMaxUsableSampleCount(
    from!"erupted".VkPhysicalDevice physicalDevice,
) nothrow @nogc @trusted
{
    import erupted;

    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(physicalDevice, &properties);
    const VkSampleCountFlags counts =
        properties.limits.framebufferColorSampleCounts &
        properties.limits.framebufferDepthSampleCounts;
    if(counts & VK_SAMPLE_COUNT_64_BIT)
    {
        return VK_SAMPLE_COUNT_64_BIT;
    }
    if(counts & VK_SAMPLE_COUNT_32_BIT)
    {
        return VK_SAMPLE_COUNT_32_BIT;
    }
    if(counts & VK_SAMPLE_COUNT_16_BIT)
    {
        return VK_SAMPLE_COUNT_16_BIT;
    }
    if(counts & VK_SAMPLE_COUNT_4_BIT)
    {
        return VK_SAMPLE_COUNT_4_BIT;
    }
    if(counts & VK_SAMPLE_COUNT_2_BIT)
    {
        return VK_SAMPLE_COUNT_2_BIT;
    }
    return VK_SAMPLE_COUNT_1_BIT;
}

auto ref pickPhysicalDevice(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.instance) : from!"erupted".VkInstance)
)
{
    import util;
    import std.range : zip, repeat, takeOne;
    import std.algorithm : map, fold;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : makeArray, dispose;
    import erupted;
    import expected : ok, err, orElse;
    import optional : Optional, match;

    alias Res = TupleCat!(T, Tuple!(
        VkPhysicalDevice, "physicalDevice",
        QueueFamilyIndices, "queueFamilyIndices",
        ChosenSwapChainSupport, "chosenSwapChainSupport",
        VkSampleCountFlagBits, "msaaSamples",
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
            // TODO: get msaa samples at later stage (after checks).
            elem[1].msaaSamples = getMaxUsableSampleCount(elem[1].physicalDevice);
            VkPhysicalDeviceFeatures supportedFeatures;
            vkGetPhysicalDeviceFeatures(elem[1].physicalDevice, &supportedFeatures);
            if(!supportedFeatures.samplerAnisotropy)
            {
                return err!Res("Failed to find suitable GPU.");
            }
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
    import util;
    import std.algorithm : sort, uniq;
    import erupted;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(VkDevice, "device"));
    auto res = partialConstruct!Res(forward!arg);

    enum queuesCount = res.queueFamilyIndices.tupleof.length;

    VkDeviceQueueCreateInfo[queuesCount] queueCreateInfos;
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

    const VkPhysicalDeviceFeatures deviceFeatures =
    {
        samplerAnisotropy : VK_TRUE,
    };

    VkDeviceCreateInfo createInfo =
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

    return vkCreateDevice(res.physicalDevice, &createInfo, null, &res.device) == VK_SUCCESS
        ? ok(res.move)
        : err!Res("Failed to create logical device.");
}

auto ref getDeviceQueues(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
)
{
    import util;
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
        .andThen!createSwapChainImageViews
        .andThen!createRenderPass
        .andThen!createGraphicsPipeline
        .andThen!createColorResources
        .andThen!createDepthResources
        .andThen!createFramebuffers
        .andThen!createUniformBuffers
        .andThen!createDescriptorPool
        .andThen!createDescriptorSets
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
    import util;
    import glfw_vulkan : glfwGetFramebufferSize, glfwWaitEvents;
    import erupted;
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

    VkSwapchainCreateInfoKHR createInfo =
    {
        surface : res.surface,
        minImageCount : res.chosenSwapChainSupport.imageCount,
        imageFormat : res.chosenSwapChainSupport.surfaceFormat.format,
        imageColorSpace : res.chosenSwapChainSupport.surfaceFormat.colorSpace,
        imageExtent : res.swapChainExtent,
        // Always 1 unless for stereoscopic 3D application.
        imageArrayLayers : 1,
        // Render directly to imega. Use VK_IMAGE_USAGE_TRANSFER_DST_BIT for off-screen rendering.
        imageUsage : VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        // Do not want any transforms applied to swap chain images.
        preTransform : extentAndCapabilites.capabilities.currentTransform,
        // Blending with other windows in the window system.
        compositeAlpha : VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        presentMode : res.chosenSwapChainSupport.presentMode,
        // Do not render obscured pixels.
        clipped : VK_TRUE,
        oldSwapchain : VK_NULL_HANDLE,
    };

    if(res.queueFamilyIndices.graphicsFamily != res.queueFamilyIndices.presentFamily)
    {
        const uint[2] queueFamilyIndicesArr =
        [
            res.queueFamilyIndices.graphicsFamily,
            res.queueFamilyIndices.presentFamily,
        ];
        // Can be exclusive (more performant), but it requires explicit ownership control.
        createInfo.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
        createInfo.queueFamilyIndexCount = 2;
        createInfo.pQueueFamilyIndices = queueFamilyIndicesArr.ptr;
    }
    else
    {
        createInfo.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
        createInfo.queueFamilyIndexCount = 0; // Optional
        createInfo.pQueueFamilyIndices = null; // Optional
    }

    res.swapChainImageFormat = createInfo.imageFormat;

    return vkCreateSwapchainKHR(res.device, &createInfo, null, &res.swapChain) == VK_SUCCESS
        ? ok(res.move)
        : err!Res("Failed to create swap chain.");
}

auto ref getSwapChainImages(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChain) : from!"erupted".VkSwapchainKHR)
)
{
    import util;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted;
    import expected : ok;
    import automem : Vector;
    
    alias VectorType = Vector!(VkImage, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainImages"));
    auto res = partialConstruct!Res(forward!arg);

    uint imageCount;
    vkGetSwapchainImagesKHR(res.device, res.swapChain, &imageCount, null);
    res.swapChainImages.length = imageCount;
    vkGetSwapchainImagesKHR(res.device, res.swapChain, &imageCount, res.swapChainImages.ptr);
    return ok(res.move);
}

auto createImageViews(Range)(from!"erupted".VkDevice device, Range imagesAndFormatsAndAspectFlags)
if(from!"std.range".isInputRange!Range
    && is(typeof(from!"std.range".ElementType!Range.init[0]) : from!"erupted".VkImage)
    && is(typeof(from!"std.range".ElementType!Range.init[1]) : from!"erupted".VkFormat)
    && is(typeof(from!"std.range".ElementType!Range.init[2]) : from!"erupted".VkImageAspectFlags)
    && is(typeof(from!"std.range".ElementType!Range.init[3]) : uint) // mipLevels
)
{
    import std.range : zip, repeat;
    import std.algorithm : map;
    import erupted;
    import expected : ok, err;

    return device.repeat.zip(imagesAndFormatsAndAspectFlags).map!(
    (auto ref elem)
    {
        const VkImageViewCreateInfo viewInfo =
        {
            image : elem[1][0],
            viewType : VK_IMAGE_VIEW_TYPE_2D,
            format : elem[1][1],
            components : 
            {
                r : VK_COMPONENT_SWIZZLE_IDENTITY,
                g : VK_COMPONENT_SWIZZLE_IDENTITY,
                b : VK_COMPONENT_SWIZZLE_IDENTITY,
                a : VK_COMPONENT_SWIZZLE_IDENTITY,
            },
            subresourceRange :
            {
                aspectMask : elem[1][2],
                baseMipLevel : 0,
                levelCount : elem[1][3],
                baseArrayLayer : 0,
                // More then 1 for stereographic 3D application.
                layerCount : 1,
            },
        };

        VkImageView imageView;
        return vkCreateImageView(elem[0], &viewInfo, null, &imageView) == VK_SUCCESS
            ? ok(imageView)
            : err!VkImageView("Failed to create image view.");
    })
    ;
}

auto createImageView(
    from!"erupted".VkDevice device,
    from!"erupted".VkImage image,
    from!"erupted".VkFormat format,
    from!"erupted".VkImageAspectFlags aspectFlags,
    in uint mipLevels,
)
{
    import std.typecons : tuple;
    import std.range : only;
    return createImageViews(device, only(tuple(image, format, aspectFlags, mipLevels))).front;
}

auto ref createSwapChainImageViews(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChainImageFormat) : from!"erupted".VkFormat)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
)
{
    import util;
    import std.range : enumerate, zip, repeat;
    import std.algorithm : fold;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted;
    import expected : ok, err, andThen, mapOrElse;
    import automem : Vector;

    alias VkImageViewArray = Vector!(VkImageView, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VkImageViewArray, "swapChainImageViews"));
    auto res = partialConstruct!Res(forward!arg);

    res.swapChainImageViews.length = res.swapChainImages.length;
    
    return createImageViews(
        res.device,
        zip(
            res.swapChainImages[],
            res.swapChainImageFormat.repeat(res.swapChainImages.length),
            VK_IMAGE_ASPECT_COLOR_BIT.repeat(res.swapChainImages.length),
            1U.repeat(res.swapChainImages.length),
        )
    )
    .enumerate
    .fold!((accum, elem)
    {
        return accum.andThen!((auto ref arg)
        {
            return elem[1].mapOrElse!(
                (imageView)
                {
                    arg.swapChainImageViews.ptr[elem[0]] = imageView;
                    return ok(forward!arg);
                },
                e => err!Res(e)
            );
        })
        ;
    })(ok(res.move))
    ;
}

auto ref createRenderPass(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.physicalDevice) : from!"erupted".VkPhysicalDevice)
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChainImageFormat) : from!"erupted".VkFormat)
    && is(typeof(arg.msaaSamples) : from!"erupted".VkSampleCountFlagBits)
)
{
    import util;
    import erupted;
    import expected : ok, err, andThen;

    return findDepthFormat(arg.physicalDevice)
    .andThen!((auto ref depthFormat, auto ref arg)
    {
        alias Res = TupleCat!(T, Tuple!(
            VkRenderPass, "renderPass"
        ));
        auto res = partialConstruct!Res(forward!arg);

        const VkAttachmentDescription[3] attachments =
        [
            {
                // colorAttachment
                format : res.swapChainImageFormat,
                samples : res.msaaSamples,
                loadOp : VK_ATTACHMENT_LOAD_OP_CLEAR,
                storeOp : VK_ATTACHMENT_STORE_OP_STORE,
                stencilLoadOp : VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                stencilStoreOp : VK_ATTACHMENT_STORE_OP_DONT_CARE,
                initialLayout : VK_IMAGE_LAYOUT_UNDEFINED,
                finalLayout : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            },
            {
                // depthAttachment
                format : depthFormat,
                samples : res.msaaSamples,
                loadOp : VK_ATTACHMENT_LOAD_OP_CLEAR,
                storeOp : VK_ATTACHMENT_STORE_OP_DONT_CARE,
                stencilLoadOp : VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                stencilStoreOp : VK_ATTACHMENT_STORE_OP_DONT_CARE,
                initialLayout : VK_IMAGE_LAYOUT_UNDEFINED,
                finalLayout : VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
            },
            {
                // colorAttachmentResolve
                format : res.swapChainImageFormat,
                samples : VK_SAMPLE_COUNT_1_BIT,
                loadOp : VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                storeOp : VK_ATTACHMENT_STORE_OP_STORE,
                stencilLoadOp : VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                stencilStoreOp : VK_ATTACHMENT_STORE_OP_DONT_CARE,
                initialLayout : VK_IMAGE_LAYOUT_UNDEFINED,
                finalLayout : VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, // Image to be presented in the swap chain.
            }
        ];

        const VkAttachmentReference colorAttachmentRef =
        {
            attachment : 0, // Index in attachment array.
            layout : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        };

        const VkAttachmentReference depthAttachmentRef =
        {
            attachment : 1,
            layout : VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
        };

        const VkAttachmentReference colorAttachmentResolveRef =
        {
            attachment : 2,
            layout : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        };

        const VkSubpassDescription subpass =
        {
            // Use for graphics.
            pipelineBindPoint : VK_PIPELINE_BIND_POINT_GRAPHICS,
            colorAttachmentCount : 1,
            // Index in this array referenced from the fragment shader layout(location=0) out vec4 outColor directive.
            pColorAttachments : &colorAttachmentRef,
            pDepthStencilAttachment : &depthAttachmentRef,
            // It is enough to let the render pass define a multisample resolve.
            pResolveAttachments : &colorAttachmentResolveRef
        };

        const VkSubpassDependency dependency =
        {
            srcSubpass : VK_SUBPASS_EXTERNAL, // Refers to implicit subpass before and after render pass.
            dstSubpass : 0, // This subpass's index.

            // What operations to wait.
            srcStageMask : VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            srcAccessMask : 0,

            // What operations are delayed.
            dstStageMask : VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
            dstAccessMask : VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
        };

        const VkRenderPassCreateInfo renderPassInfo =
        {
            attachmentCount : attachments.length,
            pAttachments : attachments.ptr,
            subpassCount : 1,
            pSubpasses : &subpass,

            dependencyCount : 1,
            pDependencies : &dependency,
        };

        return vkCreateRenderPass(res.device, &renderPassInfo, null, &res.renderPass) == VK_SUCCESS
            ? ok(res.move)
            : err!Res("Failed to create render pass.");
    })(forward!arg)
    ;
}

auto ref createShaderModule(from!"erupted".VkDevice device, const(ubyte)[] code) nothrow @nogc @trusted
in(cast(ptrdiff_t) code.ptr % 4 == 0)
{
    import core.lifetime : move;
    import erupted;
    import expected : ok, err;
    
    alias Res = VkShaderModule;

    const VkShaderModuleCreateInfo createInfo =
    {
        codeSize : code.length,
        pCode : cast(const(uint)*) code.ptr,
    };

    VkShaderModule shaderModule;
    return vkCreateShaderModule(device, &createInfo, null, &shaderModule) == VK_SUCCESS
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

auto ref createDescriptorSetLayout(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
)
{
    import util;
    import erupted;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkDescriptorSetLayout, "descriptorSetLayout",
    ));
    auto res = partialConstruct!Res(forward!arg);

    const VkDescriptorSetLayoutBinding[2] bindings =
    [
        // uboLayoutBinding
        {
            // Binding used in the shader.
            binding : 0,
            descriptorType : VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            // More then 1 to specify an array of UBOs.
            descriptorCount : 1,
            // Specify from which shader this binding is referenced. Can be OR'ed or VK_SHADER_STAGE_ALL_GRAPHICS.
            stageFlags : VK_SHADER_STAGE_VERTEX_BIT,
            pImmutableSamplers : null, // Optional
        },
        // samplerLayoutBinding
        {
            binding : 1,
            descriptorType : VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            descriptorCount : 1,
            stageFlags : VK_SHADER_STAGE_FRAGMENT_BIT,
            pImmutableSamplers : null,
        },
    ];

    const VkDescriptorSetLayoutCreateInfo layoutInfo =
    {
        bindingCount : bindings.length,
        pBindings : bindings.ptr,
    };

    if(vkCreateDescriptorSetLayout(res.device, &layoutInfo, null, &res.descriptorSetLayout) != VK_SUCCESS)
    {
        return err!Res("Failed to create descriptor set layout.");
    }

    return ok(res.move);
}

auto ref createGraphicsPipeline(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
    && is(typeof(arg.msaaSamples) : from!"erupted".VkSampleCountFlagBits)
    && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
    && is(typeof(arg.descriptorSetLayout) : from!"erupted".VkDescriptorSetLayout)
)
{
    import util;
    import erupted;
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
                    const VkPipelineShaderStageCreateInfo[2] shaderStageInfos =
                    [
                        {
                            stage : VK_SHADER_STAGE_VERTEX_BIT,
                            module_ : vertShaderModule,
                            pName : "main", // Entry point.
                            pSpecializationInfo : null, // Specifying shader constants.
                        },
                        {
                            stage : VK_SHADER_STAGE_FRAGMENT_BIT,
                            module_ : fragShaderModule,
                            pName : "main", // Entry point.
                            pSpecializationInfo : null, // Specifying shader constants.
                        },
                    ];

                    const bindingDescription = Vertex.getBindingDescription();
                    const attributeDescriptions = Vertex.getAttributeDescriptions();

                    const VkPipelineVertexInputStateCreateInfo vertexInputInfo =
                    {
                        // Data is per-vertex or per-instance?
                        vertexBindingDescriptionCount : 1,
                        pVertexBindingDescriptions : &bindingDescription,
                        // Binding and offset of attributes.
                        vertexAttributeDescriptionCount : attributeDescriptions.length,
                        pVertexAttributeDescriptions : attributeDescriptions.ptr,
                    };

                    const VkPipelineInputAssemblyStateCreateInfo inputAssembly =
                    {
                        topology : VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                        primitiveRestartEnable : VK_FALSE,
                    };

                    const VkViewport viewport =
                    {
                        x : 0.0f,
                        y : 0.0f,
                        width : res.swapChainExtent.width,
                        height : res.swapChainExtent.height,
                        minDepth : 0.0f,
                        maxDepth : 1.0f,
                    };

                    const VkRect2D scissor =
                    {
                        offset : {0, 0},
                        extent : res.swapChainExtent,
                    };

                    const VkPipelineViewportStateCreateInfo viewportState =
                    {
                        viewportCount : 1, // Need extension for more then 1.
                        pViewports : &viewport,
                        scissorCount : 1, // Need extension for more then 1.
                        pScissors : &scissor,
                    };

                    const VkPipelineRasterizationStateCreateInfo rasterizer =
                    {
                        depthClampEnable : VK_FALSE,
                        rasterizerDiscardEnable : VK_FALSE,
                        // Other modes require GPU feature.
                        polygonMode : VK_POLYGON_MODE_FILL,
                        // 1 pixel wide. Thicker requires wideLines GPU feature.
                        lineWidth : 1.0f,
                        cullMode : VK_CULL_MODE_BACK_BIT,
                        frontFace : VK_FRONT_FACE_COUNTER_CLOCKWISE,

                        // Modify fragment's depth depending on its slope.
                        depthBiasEnable : VK_FALSE,
                        depthBiasConstantFactor : 0.0f, // Optional
                        depthBiasClamp : 0.0f, // Optional
                        depthBiasSlopeFactor : 0.0f, // Optional
                    };

                    // Multisampling requires a GPU feature.
                    const VkPipelineMultisampleStateCreateInfo multisampling =
                    {
                        sampleShadingEnable : VK_FALSE,
                        rasterizationSamples : res.msaaSamples,
                        minSampleShading : 1.0f, // Optional
                        pSampleMask : null, // Optional
                        alphaToCoverageEnable : VK_FALSE, // Optional
                        alphaToOneEnable : VK_FALSE, // Optional
                    };

                    // Optional
                    const VkPipelineDepthStencilStateCreateInfo depthStencil =
                    {
                        depthTestEnable : VK_TRUE,
                        // Disabling the writes useful for drawing transparent objects. 
                        depthWriteEnable : VK_TRUE,
                        // Less - lower depth means closer.
                        depthCompareOp : VK_COMPARE_OP_LESS,
                        // Discard fragments with depth that is outside the bounds.
                        depthBoundsTestEnable : VK_FALSE,
                        minDepthBounds : 0.0f, // Optional
                        maxDepthBounds : 1.0f, // Optional
                        stencilTestEnable : VK_FALSE,
                        front : {}, // Optional
                        back : {}, // Optional
                    };
                    
                    // Framebuffer-specific blending options.
                    const VkPipelineColorBlendAttachmentState colorBlendAttachment =
                    {
                        colorWriteMask :
                            VK_COLOR_COMPONENT_R_BIT |
                            VK_COLOR_COMPONENT_G_BIT |
                            VK_COLOR_COMPONENT_B_BIT |
                            VK_COLOR_COMPONENT_A_BIT ,
                        blendEnable : VK_FALSE,
                        srcColorBlendFactor : VK_BLEND_FACTOR_ONE, // Optional
                        dstColorBlendFactor : VK_BLEND_FACTOR_ZERO, // Optional
                        colorBlendOp : VK_BLEND_OP_ADD, // Optional
                        srcAlphaBlendFactor : VK_BLEND_FACTOR_ONE, // Optional
                        dstAlphaBlendFactor : VK_BLEND_FACTOR_ZERO, // Optional
                        alphaBlendOp : VK_BLEND_OP_ADD, // Optional
                    };

                    // Global blending options
                    const VkPipelineColorBlendStateCreateInfo colorBlending =
                    {
                        // Bitwise blending disables framebuffer-specific blending.
                        logicOpEnable : VK_FALSE,
                        logicOp : VK_LOGIC_OP_COPY, // Optional
                        attachmentCount : 1,
                        pAttachments : &colorBlendAttachment,
                        blendConstants : [ 0.0f, 0.0f, 0.0f, 0.0f ] // Optional
                    };

                    // const VkDynamicState[2] dynamicStates =
                    // [
                        // VK_DYNAMIC_STATE_VIEWPORT,
                        // VK_DYNAMIC_STATE_LINE_WIDTH,
                    // ];

                    // const VkPipelineDynamicStateCreateInfo dynamicState =
                    // {
                    //     dynamicStateCount : 2,
                    //     pDynamicStates : dynamicStates.ptr,
                    // };

                    // Uniform variables and push constants
                    const VkPipelineLayoutCreateInfo pipelineLayoutInfo =
                    {
                        setLayoutCount : 1,
                        pSetLayouts : &res.descriptorSetLayout,
                        pushConstantRangeCount : 0, // Optional
                        pPushConstantRanges : null, // Optional
                    };

                    if(vkCreatePipelineLayout(
                        res.device, &pipelineLayoutInfo, null, &res.pipelineLayout) != VK_SUCCESS)
                    {
                        return err!Res("Failed to create pipeline layout.");
                    }

                    const VkGraphicsPipelineCreateInfo pipelineInfo =
                    {
                        stageCount : shaderStageInfos.length,
                        pStages : shaderStageInfos.ptr,
                        pVertexInputState : &vertexInputInfo,
                        pInputAssemblyState : &inputAssembly,
                        pViewportState : &viewportState,
                        pRasterizationState : &rasterizer,
                        pMultisampleState : &multisampling,
                        pDepthStencilState : &depthStencil, // Optional, unless render pass contains a depth stencil attachment.
                        pColorBlendState : &colorBlending,
                        pDynamicState : null, // Optional
                        layout : res.pipelineLayout,
                        renderPass : res.renderPass,
                        subpass : 0, // Subpass index of this pipeline.
                        // Specify parent pipelint with common settings.
                        // VK_PIPELINE_CREATE_DERIVATIVE_BIT in flag should be set.
                        basePipelineHandle : VK_NULL_HANDLE,
                        basePipelineIndex: -1,
                    };

                    if(vkCreateGraphicsPipelines(res.device,
                        VK_NULL_HANDLE, // VkPipelineCache of data relevant to pipeline creation.
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

auto createColorResources(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.physicalDevice) : from!"erupted".VkPhysicalDevice)
    && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
    && is(typeof(arg.swapChainImageFormat) : from!"erupted".VkFormat)
    && is(typeof(arg.msaaSamples) : from!"erupted".VkSampleCountFlagBits)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
    && is(typeof(arg.graphicsQueue) : from!"erupted".VkQueue)
)
{
    import util;
    import erupted;
    import expected : ok, err, andThen;

    VkImage colorImage;
    VkDeviceMemory colorImageMemory;

    const VkFormat colorFormat = arg.swapChainImageFormat;
    const extent = arg.swapChainExtent;
    const VkSampleCountFlagBits msaaSamples = arg.msaaSamples;

    return createImage(
        forward!arg,
        extent.width, extent.height,
        1,
        msaaSamples,
        colorFormat,
        VK_IMAGE_TILING_OPTIMAL,
        VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        colorImage,
        colorImageMemory,
    )
    .andThen!((auto ref arg)
    {
        return createImageView(arg.device, colorImage, colorFormat, VK_IMAGE_ASPECT_COLOR_BIT, 1)
        .andThen!((colorImageView, auto ref arg)
        {
            alias Res = TupleCat!(T, Tuple!(
                VkImage, "colorImage",
                VkDeviceMemory, "colorImageMemory",
                VkImageView, "colorImageView",
            ));
            auto res = partialConstruct!Res(forward!arg);

            res.colorImage = colorImage.move;
            res.colorImageMemory = colorImageMemory.move;
            res.colorImageView = colorImageView.move;

            return ok(res.move);
        })(forward!arg)
        ;
    })
    ;
}

from!"expected".Expected!(from!"erupted".VkFormat) findSupportedFormat(VkFormatRange)(
    from!"erupted".VkPhysicalDevice physicalDevice,
    from!"erupted".VkImageTiling tiling,
    from!"erupted".VkFormatFeatureFlags features,
    VkFormatRange candidates,
) nothrow @nogc @trusted
if(from!"std.range".isInputRange!VkFormatRange
    && is(from!"std.range".ElementType!VkFormatRange : from!"erupted".VkFormat)
)
{
    import erupted;
    import expected : ok, err;

    foreach(format; candidates)
    {
        // props contains linearTilingFeatures, optimalTilingFeatures, bufferFeatures.
        VkFormatProperties props;
        vkGetPhysicalDeviceFormatProperties(physicalDevice, format, &props);
        if(tiling == VK_IMAGE_TILING_LINEAR && (props.linearTilingFeatures & features) == features)
        {
            return ok(format);
        }
        else if(tiling == VK_IMAGE_TILING_OPTIMAL && (props.optimalTilingFeatures & features) == features)
        {
            return ok(format);
        }
    }

    return err!VkFormat("Failed to find supported format.");
}

auto findDepthFormat(
    from!"erupted".VkPhysicalDevice physicalDevice,
) nothrow @nogc @trusted
{
    import std.range : only;
    import erupted;

    return findSupportedFormat(
        physicalDevice,
        VK_IMAGE_TILING_OPTIMAL,
        VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT,
        only
        (
            VK_FORMAT_D32_SFLOAT,
            VK_FORMAT_D32_SFLOAT_S8_UINT,
            VK_FORMAT_D24_UNORM_S8_UINT
        ),
    );
}

bool hasStencilComponent(from!"erupted".VkFormat format) pure nothrow @nogc @safe
{
    import erupted;
    return format == VK_FORMAT_D32_SFLOAT_S8_UINT || format == VK_FORMAT_D24_UNORM_S8_UINT;
}

auto ref createDepthResources(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.physicalDevice) : from!"erupted".VkPhysicalDevice)
    && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
    && is(typeof(arg.msaaSamples) : from!"erupted".VkSampleCountFlagBits)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
    && is(typeof(arg.graphicsQueue) : from!"erupted".VkQueue)
)
{
    import util;
    import erupted;
    import expected : ok, err, andThen;

    VkImage depthImage;
    VkDeviceMemory depthImageMemory;

    return findDepthFormat(arg.physicalDevice)
    .andThen!((auto ref depthFormat, auto ref arg)
    {
        auto extent = arg.swapChainExtent;
        auto msaaSamples = arg.msaaSamples;
        return createImage(
            forward!arg,
            extent.width, extent.height,
            1,
            msaaSamples,
            depthFormat,
            VK_IMAGE_TILING_OPTIMAL,
            VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
            depthImage,
            depthImageMemory,
        )
        .andThen!((auto ref arg)
        {
            return createImageView(arg.device, depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT, 1)
            .andThen!((auto ref depthImageView)
            {
                alias Res = TupleCat!(T, Tuple!(
                    VkImage, "depthImage",
                    VkDeviceMemory, "depthImageMemory",
                    VkImageView, "depthImageView",
                ));
                auto res = partialConstruct!Res(forward!arg);

                res.depthImage = depthImage;
                res.depthImageMemory = depthImageMemory;
                res.depthImageView = depthImageView;
                transitionImageLayout(
                    res.device, res.commandPool, res.graphicsQueue,
                    res.depthImage, depthFormat,
                    VK_IMAGE_LAYOUT_UNDEFINED,
                    VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                    1,
                );
                return ok(res);
            })
            ;
        })
        ;
    })(forward!arg)
    ;
}

auto ref createFramebuffers(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
    && is(typeof(arg.swapChainExtent) : from!"erupted".VkExtent2D)
    && is(typeof(arg.colorImageView) : from!"erupted".VkImageView)
    && is(typeof(arg.depthImageView) : from!"erupted".VkImageView)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImageViews[])) : from!"erupted".VkImageView)
)
{
    import util;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.range : enumerate;
    import erupted;
    import expected : ok, err;
    import automem : Vector;
    
    alias VectorType = Vector!(VkFramebuffer, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "swapChainFramebuffers"));
    auto res = partialConstruct!Res(forward!arg);

    res.swapChainFramebuffers.length = res.swapChainImageViews.length;

    foreach (i, ref imageView; res.swapChainImageViews[].enumerate)
    {
        const VkImageView[3] attachments =
        [
            res.colorImageView,
            res.depthImageView,
            imageView,
        ];

        const VkFramebufferCreateInfo framebufferInfo =
        {
            renderPass : res.renderPass,
            attachmentCount : attachments.length,
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
    in from!"erupted".VkMemoryRequirements memRequirements,
    in from!"erupted".VkMemoryPropertyFlags properties,
    out uint chosenMemoryTypeIndex,
) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.physicalDevice) : from!"erupted".VkPhysicalDevice)
)
{
    import util;
    import erupted;
    import expected : ok, err;
    
    VkPhysicalDeviceMemoryProperties memProperties;
    vkGetPhysicalDeviceMemoryProperties(arg.physicalDevice, &memProperties);

    foreach(i; 0 .. memProperties.memoryTypeCount)
    {
        if((memRequirements.memoryTypeBits & (1 << i)) // Check memory type.
            // Check the heap and its memory types properties.
            && (memProperties.memoryTypes[i].propertyFlags & properties) == properties
        )
        {
            chosenMemoryTypeIndex = i;
            return ok(forward!arg);
        }
    }

    return err!T("Falied to find suitable memory type.");
}

auto createBuffer(T)(
    auto ref T arg,
    in from!"erupted".VkDeviceSize size,
    in from!"erupted".VkBufferUsageFlags usage,
    in from!"erupted".VkMemoryPropertyFlags properties,
    out from!"erupted".VkBuffer buffer,
    out from!"erupted".VkDeviceMemory bufferMemory,
) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
)
{
    import util;
    import std.meta : AliasSeq;
    import erupted;
    import expected : ok, err, andThen;

    VkMemoryRequirements memRequirements;
    uint chosenMemoryTypeIndex;

    return (auto ref arg)
    {
        const VkBufferCreateInfo bufferInfo =
        {
            size : size,
            // Possible to specify multiple purposes using a bitwase or.
            usage : usage,
            // Sharing between queue families.
            sharingMode : VK_SHARING_MODE_EXCLUSIVE,
            // Configure sparse buffer memory.
            flags : 0,
        };

        if(vkCreateBuffer(arg.device, &bufferInfo, null, &buffer) != VK_SUCCESS)
        {
            return err!T("Failed to create buffer.");
        }

        vkGetBufferMemoryRequirements(arg.device, buffer, &memRequirements);

        return ok(forward!arg);
    }(forward!arg)
    .andThen!findMemoryType(memRequirements, properties, chosenMemoryTypeIndex)
    .andThen!((auto ref arg)
    {
        import core.stdc.string : memcpy;

        const VkMemoryAllocateInfo allocInfo =
        {
            allocationSize : memRequirements.size,
            memoryTypeIndex : chosenMemoryTypeIndex,
        };

        if(vkAllocateMemory(arg.device, &allocInfo, null, &bufferMemory) != VK_SUCCESS)
        {
            return err!T("Failed to allocate buffer memory.");
        }

        // Device, vertex buffer, device memory and offset within the device memory.
        vkBindBufferMemory(arg.device, buffer, bufferMemory, 0);
        
        return ok(forward!arg);
    })
    ;
}

auto createImage(T)(
    auto ref T arg,
    in uint width, in uint height,
    in uint mipLevels,
    in from!"erupted".VkSampleCountFlagBits numSamples,
    in from!"erupted".VkFormat format,
    in from!"erupted".VkImageTiling tiling,
    in from!"erupted".VkImageUsageFlags usage,
    in from!"erupted".VkMemoryPropertyFlags properties,
    out from!"erupted".VkImage image,
    out from!"erupted".VkDeviceMemory imageMemory,
) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
)
{
    import util;
    import erupted;
    import expected : ok, err, andThen;

    VkMemoryRequirements memRequirements;
    uint chosenMemoryTypeIndex;

    return (auto ref arg)
    {
        const VkImageCreateInfo imageInfo =
        {
            imageType : VK_IMAGE_TYPE_2D,
            extent :
            {
                width : width,
                height : height,
                depth : 1,
            },
            mipLevels : mipLevels,
            arrayLayers : 1,
            // I.e., VK_FORMAT_R8G8B8A8_SRGB.
            format : format,
            // Linear tiling - texels laid out in a row-major order (for direct access).
            // Optimal tiling - implementation defined order.
            tiling : tiling,
            // Undefined - first transition will discard the textels.
            // Preinitialized - first transition will preserve texels.
            initialLayout : VK_IMAGE_LAYOUT_UNDEFINED,
            usage : usage,
            // Image will be used by one queue family.
            sharingMode : VK_SHARING_MODE_EXCLUSIVE,
            // Multisampling relevant for attachment images.
            samples : numSamples,
            // Flags can specify that image is sparce.
            flags : 0,
        };

        if(vkCreateImage(arg.device, &imageInfo, null, &image) != VK_SUCCESS)
        {
            return err!T("Failed to create image.");
        }

        vkGetImageMemoryRequirements(arg.device, image, &memRequirements);
        
        return ok(forward!arg);
    }(forward!arg)
    .andThen!findMemoryType(memRequirements, properties, chosenMemoryTypeIndex)
    .andThen!((auto ref arg)
    {
        import core.stdc.string : memcpy;

        const VkMemoryAllocateInfo allocInfo =
        {
            allocationSize : memRequirements.size,
            memoryTypeIndex : chosenMemoryTypeIndex,
        };

        if(vkAllocateMemory(arg.device, &allocInfo, null, &imageMemory) != VK_SUCCESS)
        {
            return err!T("Failed to allocate image memory.");
        }

        // Device, vertex buffer, device memory and offset within the device memory.
        vkBindImageMemory(arg.device, image, imageMemory, 0);
        
        return ok(forward!arg);
    })
    ;
}

void transitionImageLayout(
    from!"erupted".VkDevice device,
    from!"erupted".VkCommandPool commandPool,
    from!"erupted".VkQueue transferQueue,
    from!"erupted".VkImage image,
    from!"erupted".VkFormat format,
    from!"erupted".VkImageLayout oldLayout,
    from!"erupted".VkImageLayout newLayout,
    in uint mipLevels,
) nothrow @nogc @trusted
{
    import erupted;

    auto commandBuffer = beginSingleTimeCommands(device, commandPool);

    VkImageMemoryBarrier barrier =
    {
        oldLayout : oldLayout, // Possible to use VK_IMAGELAYOUT_UNDEFINED.
        newLayout : newLayout,
        // Specify queue indices for queue ownership transfer.
        srcQueueFamilyIndex : VK_QUEUE_FAMILY_IGNORED,
        dstQueueFamilyIndex : VK_QUEUE_FAMILY_IGNORED,
        image : image,
        // Specify concrete image part to be affected (mip level and array indices for arrays of images).
        subresourceRange :
        {
            baseMipLevel : 0,
            levelCount : mipLevels,
            baseArrayLayer : 0,
            layerCount : 1,
        },
    };

    VkPipelineStageFlags sourceStage;
    VkPipelineStageFlags destinationStage;

    if(oldLayout == VK_IMAGE_LAYOUT_UNDEFINED && newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
    {
        barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        barrier.srcAccessMask = 0;
        barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }
    else if(oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL && newLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
    {
        barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
        sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        destinationStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    }
    else if(oldLayout == VK_IMAGE_LAYOUT_UNDEFINED && newLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
    {
        barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_DEPTH_BIT;
        if(hasStencilComponent(format))
        {
            barrier.subresourceRange.aspectMask |= VK_IMAGE_ASPECT_STENCIL_BIT;
        }
        barrier.srcAccessMask = 0;
        barrier.dstAccessMask =
            VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
            | VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        destinationStage = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    }
    else
    {
        assert(false, "Unsupported layout transition.");
    }

    vkCmdPipelineBarrier(
        commandBuffer,
        sourceStage,
        destinationStage,
        0, // 0 or VK_DEPENDENCY_BY_REGION_BIT. The latter turn the barrier into a per-region condition
        0, null,
        0, null,
        1, &barrier,
    );

    endSingleTimeCommands(device, commandPool, transferQueue, commandBuffer);
}

void copyBufferToImage(
    from!"erupted".VkDevice device,
    from!"erupted".VkCommandPool commandPool,
    from!"erupted".VkQueue transferQueue,
    from!"erupted".VkBuffer buffer,
    from!"erupted".VkImage image,
    in uint width, in uint height,
) nothrow @nogc @trusted
{
    import erupted;
    
    auto commandBuffer = beginSingleTimeCommands(device, commandPool);

    const VkBufferImageCopy region =
    {
        bufferOffset : 0,
        // 0 for bufferRowLength and bufferImageHeight means that pixels are tightly packed.
        bufferRowLength : 0,
        bufferImageHeight : 0,
        imageSubresource :
        {
            aspectMask : VK_IMAGE_ASPECT_COLOR_BIT,
            mipLevel : 0,
            baseArrayLayer : 0,
            layerCount : 1,
        },
        imageOffset :
        {
            x : 0,
            y : 0,
            z : 0,
        },
        imageExtent :
        {
            width : width,
            height : height,
            depth : 1,
        },
    };

    vkCmdCopyBufferToImage(
        commandBuffer,
        buffer,
        image,
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, // Assumes that the image is already optimal as destination.
        1, &region, // Array of regions.
    );

    endSingleTimeCommands(device, commandPool, transferQueue, commandBuffer);
}

from!"expected".Expected!void generateMipmaps(
    from!"erupted".VkPhysicalDevice physicalDevice,
    from!"erupted".VkDevice device,
    from!"erupted".VkCommandPool commandPool,
    from!"erupted".VkQueue transferQueue,
    from!"erupted".VkImage image,
    from!"erupted".VkFormat imageFormat,
    in uint textureWidth, in uint textureHeight,
    in uint mipLevels,
) nothrow @nogc @trusted 
{
    import erupted;
    import expected : ok, err;

    // TODO: vkGetPhysicalDeviceFormatProperties is already called somewhere previously. Do not repeat.
    VkFormatProperties formatProperties;
    vkGetPhysicalDeviceFormatProperties(physicalDevice, imageFormat, &formatProperties);
    if(!(formatProperties.optimalTilingFeatures & VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT))
    {
        // In this case it is possible to find image format that supports blitting,
        // or generate mipmaps on CPU and load them to GPU separately.
        // Usually all texture mipmaps are pregenerated and stored in one file.
        return err!void("Image format does not support linear blitting, unable to generate mipmaps.");
    }

    auto commandBuffer = beginSingleTimeCommands(device, commandPool);

    VkImageMemoryBarrier barrier =
    {
        image : image,
        srcQueueFamilyIndex : VK_QUEUE_FAMILY_IGNORED,
        dstQueueFamilyIndex : VK_QUEUE_FAMILY_IGNORED,
        subresourceRange :
        {
            aspectMask : VK_IMAGE_ASPECT_COLOR_BIT,
            baseArrayLayer : 0,
            layerCount : 1,
            levelCount : 1,
        },
    };

    uint mipWidth = textureWidth;
    uint mipHeight = textureHeight;

    foreach(uint i; 1 .. mipLevels)
    {
        barrier.subresourceRange.baseMipLevel = i - 1;
        barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        barrier.newLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        barrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;

        // Barrier to wait on previous transition and on vkCmdCopyBufferToImage.
        vkCmdPipelineBarrier(
            commandBuffer,
            VK_PIPELINE_STAGE_TRANSFER_BIT,
            VK_PIPELINE_STAGE_TRANSFER_BIT,
            0,
            0, null,
            0, null,
            1, &barrier,
        );

        const VkImageBlit blit =
        {
            srcOffsets :
            [
                { 0, 0, 0 },
                { mipWidth, mipHeight, 1 }
            ],
            srcSubresource :
            {
                aspectMask : VK_IMAGE_ASPECT_COLOR_BIT,
                mipLevel : i - 1,
                baseArrayLayer : 0,
                layerCount : 1,
            },
            dstOffsets :
            [
                { 0, 0, 0 },
                { mipWidth > 1 ? mipWidth / 2 : 1, mipHeight > 1 ? mipHeight / 2 : 1, 1 },
            ],
            dstSubresource :
            {
                aspectMask : VK_IMAGE_ASPECT_COLOR_BIT,
                mipLevel : i,
                baseArrayLayer : 0,
                layerCount : 1,
            },
        };

        vkCmdBlitImage(
            commandBuffer,
            image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, // Need to specify current leyout of the image.
            image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            1, &blit,
            VK_FILTER_LINEAR,
        );

        barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        barrier.newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;

        vkCmdPipelineBarrier(
            commandBuffer,
            VK_PIPELINE_STAGE_TRANSFER_BIT,
            VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
            0,
            0, null,
            0, null,
            1, &barrier,
        );

        if(mipWidth > 1)
        {
            mipWidth /= 2;
        }
        if(mipHeight > 1)
        {
            mipHeight /= 2;
        }
    }

    barrier.subresourceRange.baseMipLevel = mipLevels - 1;
    barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
    barrier.newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
    barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;

    vkCmdPipelineBarrier(
        commandBuffer,
        VK_PIPELINE_STAGE_TRANSFER_BIT,
        VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
        0,
        0, null,
        0, null,
        1, &barrier,
    );

    endSingleTimeCommands(device, commandPool, transferQueue, commandBuffer);

    return ok();
}

auto createTextureImage(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.physicalDevice) : from!"erupted".VkPhysicalDevice)
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
    && is(typeof(arg.graphicsQueue) : from!"erupted".VkQueue)
)
{
    import util;
    import erupted;
    import expected : ok, err, andThen;
    import imagefmt : IFImage, read_image;

    IFImage image;
    VkDeviceSize imageSize;
    uint textureWidth;
    uint textureHeight;
    uint mipLevels;
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    VkImage textureImage;
    VkDeviceMemory textureImageMemory;

    return (auto ref arg)
    {
        import std.math : log2, floor;
        import std.algorithm : max;
        image = read_image(TexturePath, 4);
        if(image.e)
        {
            return err!T("Failed to load texture image.");
        }
        textureWidth = image.w;
        textureHeight = image.h;
        imageSize = image.w * image.h * image.c;
        mipLevels = cast(uint) max(textureWidth, textureHeight).log2.floor;
        return ok(forward!arg);
        // TODO: free image on error.
    }(forward!arg)
    .andThen!createBuffer(
        imageSize,
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        stagingBuffer,
        stagingBufferMemory,
    )
    // TODO: free buffer on error.
    .andThen!((auto ref arg)
    {
        import core.stdc.string : memcpy;

        void* data;
        vkMapMemory(arg.device, stagingBufferMemory, 0, imageSize, 0, &data);
        memcpy(data, image.buf8.ptr, imageSize);
        vkUnmapMemory(arg.device, stagingBufferMemory);
        image.free();
        return ok(forward!arg);
    })
    .andThen!createImage(
        textureWidth, textureHeight,
        mipLevels,
        VK_SAMPLE_COUNT_1_BIT,
        VK_FORMAT_R8G8B8A8_SRGB,
        VK_IMAGE_TILING_OPTIMAL,
        VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT // Sampled - for usage in shaders.
        | VK_IMAGE_USAGE_TRANSFER_SRC_BIT, // For blitting in mipmap generation.
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        textureImage,
        textureImageMemory,
    )
    .andThen!((auto ref arg)
    {
        alias Res = TupleCat!(T, Tuple!(
            VkImage, "textureImage",
            VkDeviceMemory, "textureImageMemory",
            uint, "mipLevels"
        ));
        auto res = partialConstruct!Res(forward!arg);
        res.textureImage = textureImage.move;
        res.textureImageMemory = textureImageMemory.move;
        res.mipLevels = mipLevels.move;

        // TODO: create setupCommandBuffer and flushCommandBuffer functions instead of creating multiple independent command buffers.
        transitionImageLayout(
            res.device, res.commandPool, res.graphicsQueue,
            res.textureImage,
            VK_FORMAT_R8G8B8A8_SRGB,
            VK_IMAGE_LAYOUT_UNDEFINED, // Image was created with undefined layout.
            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            res.mipLevels,
        );

        copyBufferToImage(
            res.device, res.commandPool, res.graphicsQueue,
            stagingBuffer,
            res.textureImage,
            textureWidth, textureHeight,
        );

        vkDestroyBuffer(res.device, stagingBuffer, null);
        vkFreeMemory(res.device, stagingBufferMemory, null);

        // Image transfered to VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL in generateMipmaps.
        auto exp = generateMipmaps(
            res.physicalDevice,
            res.device, res.commandPool, res.graphicsQueue,
            res.textureImage,
            VK_FORMAT_R8G8B8A8_SRGB,
            textureWidth, textureHeight,
            res.mipLevels,
        );
        if(exp.hasError)
        {
            return err!Res(exp.error);
        }
        
        return ok(res.move);
    })
    ;
}

auto createTextureImageView(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.textureImage) : from!"erupted".VkImage)
    && is(typeof(arg.mipLevels) : uint)
)
{
    import util;
    import erupted;
    import expected : ok, err, mapOrElse;

    alias Res = TupleCat!(T, Tuple!(
        VkImageView, "textureImageView",
    ));
    auto res = partialConstruct!Res(forward!arg);

    return createImageView(res.device, res.textureImage, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_ASPECT_COLOR_BIT, res.mipLevels)
    .mapOrElse!(
        (imageView)
        {
            res.textureImageView = imageView;
            return ok(res.move);
        },
        e => err!Res(e),
    )
    ;
}

from!"erupted".VkCommandBuffer beginSingleTimeCommands(
    from!"erupted".VkDevice device,
    from!"erupted".VkCommandPool commandPool,
) nothrow @nogc @trusted
{
    import erupted;

    const VkCommandBufferAllocateInfo allocInfo =
    {
        level : VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        commandPool : commandPool,
        commandBufferCount : 1,
    };

    VkCommandBuffer commandBuffer;
    vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer);

    immutable VkCommandBufferBeginInfo beginInfo =
    {
        flags : VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
    };

    vkBeginCommandBuffer(commandBuffer, &beginInfo); // TODO: check result?

    return commandBuffer;
}

auto createTextureSampler(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.mipLevels) : uint)
)
{
    import util;
    import erupted;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkSampler, "textureSampler",
    ));
    auto res = partialConstruct!Res(forward!arg);

    const VkSamplerCreateInfo sampelrInfo =
    {
        // Magnification for oversampling - more fragments than texels.
        magFilter : VK_FILTER_LINEAR,
        // Minification for undersampling - more texels than fragments.
        minFilter : VK_FILTER_LINEAR,
        // Modes: repeat, mirrored repeat, clamp to edge, mirror clamp to edge, clamp to border.
        addressModeU : VK_SAMPLER_ADDRESS_MODE_REPEAT,
        addressModeV : VK_SAMPLER_ADDRESS_MODE_REPEAT,
        addressModeW : VK_SAMPLER_ADDRESS_MODE_REPEAT,
        // Anisotropic filtering is an optional device feature. Need to check VkPhysicalDeviceFeatures.samplerAnisotropy.
        anisotropyEnable : VK_TRUE,
        // Maximum amount of texel samples that can be used to calculate the final color.
        // Today 16 is maximum.
        maxAnisotropy : 16,
        // Black, white or transparent.
        borderColor : VK_BORDER_COLOR_INT_OPAQUE_BLACK,
        // Example for 2D textures:
        // True - [0, textureWidth) x [0, textureHeight)
        // False - [0, 1) x [0, 1)
        unnormalizedCoordinates : VK_FALSE,
        // If enabled, texels will first be compared to a value, and the result is used in filtering operations.
        // Mainly used for percentage-closer filtering on shadow maps.
        compareEnable : VK_FALSE,
        compareOp : VK_COMPARE_OP_ALWAYS,
        mipmapMode : VK_SAMPLER_MIPMAP_MODE_LINEAR,
        // Lod (level of detail) is calculated from screen size, then mipLodBias is added (result can be negative).
        // If lod is negative, then magnifying filter is used, and minifying filter otherwise.
        mipLodBias : 0.0f,
        // Then lod is clamped to [minLod, maxLod]. Mip level is a floot(lod), clamped to [0, texture.mipLevels].
        minLod : 0.0f,
        maxLod : cast(float) res.mipLevels,
    };

    return vkCreateSampler(res.device, &sampelrInfo, null, &res.textureSampler) == VK_SUCCESS
        ? ok(res.move)
        : err!Res("Failed to create texture sampler.");
}

void endSingleTimeCommands(
    from!"erupted".VkDevice device,
    from!"erupted".VkCommandPool commandPool,
    from!"erupted".VkQueue transferQueue,
    from!"erupted".VkCommandBuffer commandBuffer,
) nothrow @nogc @trusted
{
    import erupted;

    vkEndCommandBuffer(commandBuffer); // TODO: check result?

    const VkSubmitInfo submitInfo =
    {
        commandBufferCount : 1,
        pCommandBuffers: &commandBuffer,
    };

    vkQueueSubmit(transferQueue, 1, &submitInfo, VK_NULL_HANDLE);
    vkQueueWaitIdle(transferQueue);
    vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
}

void copyBuffer(
    from!"erupted".VkDevice device,
    from!"erupted".VkCommandPool commandPool,
    from!"erupted".VkQueue transferQueue,
    from!"erupted".VkBuffer srcBuffer,
    from!"erupted".VkBuffer dstBuffer,
    from!"erupted".VkDeviceSize size,
) nothrow @nogc @trusted
{
    import erupted;

    auto commandBuffer = beginSingleTimeCommands(device, commandPool);

    VkBufferCopy copyRegion =
    {
        srcOffset : 0, // Optional
        dstOffset : 0, // Optional
        // Not possible to specify VK_WHOLE_SIZE, unlike the vkMapMemory.
        size : size,
    };

    vkCmdCopyBuffer(commandBuffer, srcBuffer, dstBuffer, 1, &copyRegion);

    endSingleTimeCommands(device, commandPool, transferQueue, commandBuffer);
}

auto ref loadModel(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
)
{
    import util;
    import std.experimental.allocator.mallocator : Mallocator;
    import bindbc.assimp;
    import gfm.math : vec2f, vec3f;
    import expected : ok, err;
    import automem : Vector;

    alias Res = TupleCat!(T, Tuple!(
        Vector!(Vertex, Mallocator), "vertices",
        Vector!(uint, Mallocator), "indices",
    ));
    auto res = partialConstruct!Res(forward!arg);

    const scene = aiImportFile(ModelPath, aiPostProcessSteps.Triangulate | aiPostProcessSteps.FlipUVs);

    if(!scene || scene.mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene.mRootNode)
    {
        return err!Res("Failed to load model.");
    }
    scope(exit) aiReleaseImport(scene);

    void processMesh(const aiMesh* mesh, const aiScene* scene)
    {
        foreach(i; 0 .. mesh.mNumVertices)
        {
            Vertex v =
            {
                pos : vec3f(mesh.mVertices[i].x, mesh.mVertices[i].y, mesh.mVertices[i].z),
                color : vec3f(1.0f, 1.0f, 1.0f),
                texCoord : vec2f(mesh.mTextureCoords[0][i].x, mesh.mTextureCoords[0][i].y),
            };
            res.vertices ~= v;
            res.indices ~= cast(uint) res.indices.length;
        }

        // TODO: why mesh.mFaces is pointing to 0x1b1b1b1b1b1b1b1b? It should be a valid array.
        // foreach(const aiFace face; mesh.mFaces[0 .. mesh.mNumFaces])
        // {
        //     foreach(index; face.mIndices[0 .. face.mNumIndices])
        //     {
        //         res.indices ~= index;
        //     }
        // }
    }

    void processNode(const aiNode* node, const aiScene* scene)
    {
        foreach(i; 0 .. node.mNumMeshes)
        {
            const aiMesh* mesh = scene.mMeshes[node.mMeshes[i]];
            processMesh(mesh, scene);
        }

        foreach(i; 0 .. node.mNumChildren)
        {
            processNode(node.mChildren[i], scene);
        }
    }

    processNode(scene.mRootNode, scene);

    return ok(res.move);
}

auto ref createVertexBuffer(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.vertices[]))
)
{
    import util;
    import std.range : ElementType;
    import erupted;
    import expected : ok, err, andThen;

    immutable VkDeviceSize bufferSize = arg.vertices.length * ElementType!(typeof(arg.vertices[])).sizeof;

    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    VkBuffer vertexBuffer;
    VkDeviceMemory vertexBufferMemory;

    return forward!arg.createBuffer(
        bufferSize,
        // Possible to specify multiple purposes using a bitwase or.
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | // Host (CPU) can see the memory.
        VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, // Data can be immediatly written to the memory by host.
        stagingBuffer,
        stagingBufferMemory,
    )
    .andThen!((auto ref arg)
    {
        import core.stdc.string : memcpy;

        void* data;
        vkMapMemory(arg.device, stagingBufferMemory,
            0, // Offset.
            bufferSize, // Size (or VK_WHOLE_SIZE).
            0, // Flags, not used in current API.
            &data
        );
        memcpy(data, arg.vertices.ptr, bufferSize);
        vkUnmapMemory(arg.device, stagingBufferMemory);
        
        return ok(forward!arg);
    })
    .andThen!createBuffer(
        bufferSize,
        VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        vertexBuffer,
        vertexBufferMemory,
    )
    .andThen!((auto ref arg)
    {
        alias Res = TupleCat!(T, Tuple!(
            VkBuffer, "vertexBuffer",
            VkDeviceMemory, "vertexBufferMemory",
        ));
        auto res = partialConstruct!Res(forward!arg);
        res.vertexBuffer = vertexBuffer.move;
        res.vertexBufferMemory = vertexBufferMemory.move;

        copyBuffer(
            res.device, res.commandPool, res.graphicsQueue,
            stagingBuffer, res.vertexBuffer, bufferSize
        );
        vkDestroyBuffer(res.device, stagingBuffer, null);
        vkFreeMemory(res.device, stagingBufferMemory, null);
        return ok(res.move);
    })
    ;
}

auto ref createIndexBuffer(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.indices))
)
{
    import util;
    import std.range : ElementType;
    import erupted;
    import expected : ok, err, andThen;

    immutable VkDeviceSize bufferSize = arg.indices.length * ElementType!(typeof(arg.indices[])).sizeof;

    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    VkBuffer indexBuffer;
    VkDeviceMemory indexBufferMemory;

    return forward!arg.createBuffer(
        bufferSize,
        // Possible to specify multiple purposes using a bitwase or.
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | // Host (CPU) can see the memory.
        VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, // Data can be immediatly written to the memory by host.
        stagingBuffer,
        stagingBufferMemory,
    )
    .andThen!((auto ref arg)
    {
        import core.stdc.string : memcpy;

        void* data;
        vkMapMemory(arg.device, stagingBufferMemory,
            0, // Offset.
            bufferSize, // Size (or VK_WHOLE_SIZE).
            0, // Flags, not used in current API.
            &data
        );
        memcpy(data, arg.indices.ptr, bufferSize);
        vkUnmapMemory(arg.device, stagingBufferMemory);
        
        return ok(forward!arg);
    })
    .andThen!createBuffer(
        bufferSize,
        VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        indexBuffer,
        indexBufferMemory,
    )
    .andThen!((auto ref arg)
    {
        alias Res = TupleCat!(T, Tuple!(
            VkBuffer, "indexBuffer",
            VkDeviceMemory, "indexBufferMemory",
        ));
        auto res = partialConstruct!Res(forward!arg);
        res.indexBuffer = indexBuffer.move;
        res.indexBufferMemory = indexBufferMemory.move;

        copyBuffer(
            res.device, res.commandPool, res.graphicsQueue,
            stagingBuffer, res.indexBuffer, bufferSize
        );
        vkDestroyBuffer(res.device, stagingBuffer, null);
        vkFreeMemory(res.device, stagingBufferMemory, null);
        return ok(res.move);
    })
    ;
}

auto ref createUniformBuffers(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
)
{
    import util;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted;
    import expected : ok, andThen;
    import automem : Vector;
    
    immutable VkDeviceSize bufferSize = UniformBufferObject.sizeof;

    Vector!(VkBuffer, Mallocator) uniformBuffers;
    Vector!(VkDeviceMemory, Mallocator) uniformBuffersMemory;

    uniformBuffers.length = arg.swapChainImages.length;
    uniformBuffersMemory.length = arg.swapChainImages.length;

    auto exp = ok(forward!arg);
    foreach(i; 0 ..  uniformBuffers.length)
    {
        exp = exp.andThen!createBuffer(
            bufferSize,
            VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
            uniformBuffers.ptr[i],
            uniformBuffersMemory.ptr[i],
        );
    }

    return exp
    .andThen!((auto ref arg)
    {
        alias Res = TupleCat!(T, Tuple!(
            Vector!(VkBuffer, Mallocator), "uniformBuffers",
            Vector!(VkDeviceMemory, Mallocator), "uniformBuffersMemory",
        ));
        auto res = partialConstruct!Res(forward!arg);
        res.uniformBuffers = uniformBuffers.move;
        res.uniformBuffersMemory = uniformBuffersMemory.move;
        return ok(res.move);
    })
    ;
}

auto createDescriptorPool(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
)
{
    import util;
    import erupted;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkDescriptorPool, "descriptorPool",
    ));
    auto res = partialConstruct!Res(forward!arg);

    const VkDescriptorPoolSize[2] poolSizes =
    [
        {
            type : VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            descriptorCount : cast(uint) res.swapChainImages.length,
        },
        {
            type : VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            descriptorCount : cast(uint) res.swapChainImages.length,
        }
    ];

    const VkDescriptorPoolCreateInfo poolInfo =
    {
        poolSizeCount : poolSizes.length,
        pPoolSizes : poolSizes.ptr,
        // Maximum number of descriptor sets that can be allocated.
        maxSets : cast(uint) res.swapChainImages.length,
        flags : 0
            // If individual descriptor sets can be freed.
            // | VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
    };
    
    return vkCreateDescriptorPool(res.device, &poolInfo, null, &res.descriptorPool) == VK_SUCCESS
        ? ok(res.move)
        : err!Res("Failed to create descriptor pool");
}

auto createDescriptorSets(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.descriptorSetLayout) : from!"erupted".VkDescriptorSetLayout)
    && is(typeof(arg.descriptorPool) : from!"erupted".VkDescriptorPool)
    && is(typeof(arg.textureImageView) : from!"erupted".VkImageView)
    && is(typeof(arg.textureSampler) : from!"erupted".VkSampler)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImages[])) : from!"erupted".VkImage)
    && is(from!"std.range".ElementType!(typeof(arg.uniformBuffers[])) : from!"erupted".VkBuffer)
)
{
    import util;
    import std.range : repeat;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted;
    import expected : ok, err;
    import automem : Vector;

    alias Res = TupleCat!(T, Tuple!(
        Vector!(VkDescriptorSet, Mallocator), "descriptorSets",
    ));
    auto res = partialConstruct!Res(forward!arg);

    const Vector!(VkDescriptorSetLayout, Mallocator) layouts = res.descriptorSetLayout.repeat(res.swapChainImages.length);

    const VkDescriptorSetAllocateInfo allocInfo =
    {
        descriptorPool : res.descriptorPool,
        descriptorSetCount : cast(uint) res.swapChainImages.length,
        pSetLayouts : layouts.ptr,
    };

    res.descriptorSets.length = res.swapChainImages.length;
    if(vkAllocateDescriptorSets(res.device, &allocInfo, res.descriptorSets.ptr) != VK_SUCCESS)
    {
        return err!Res("Failed to allocate descriptor sets.");
    }

    foreach(i; 0 .. res.swapChainImages.length)
    {
        const VkDescriptorBufferInfo bufferInfo =
        {
            buffer : res.uniformBuffers.ptr[i],
            offset : 0,
            // Range can be set to VK_WHOLE_SIZE.
            range : UniformBufferObject.sizeof,
        };

        const VkDescriptorImageInfo imageInfo =
        {
            imageLayout : VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
            imageView : res.textureImageView,
            sampler : res.textureSampler,
        };

        const VkWriteDescriptorSet[2] descriptorWrites =
        [
            {
                // What set and binding of the set to update.
                dstSet : res.descriptorSets.ptr[i],
                dstBinding : 0,
                // Index of the first element, if descriptor is an array.
                dstArrayElement : 0,
                descriptorType : VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, // Need to specify it again?!
                descriptorCount : 1,
                // For descriptors that refer to buffer data.
                pBufferInfo : &bufferInfo,
                // For descriptors that refer to image data.
                pImageInfo : null, // Optional
                // For descriptors that refer to buffer views.
                pTexelBufferView : null, // Optional
            },
            {
                dstSet : res.descriptorSets.ptr[i],
                dstBinding : 1,
                dstArrayElement : 0,
                descriptorType : VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                descriptorCount : 1,
                pImageInfo : &imageInfo,
            }
        ];

        vkUpdateDescriptorSets(res.device,
            descriptorWrites.length, descriptorWrites.ptr, // Array of VkWriteDescriptorSet.
            0, null // Array of VkCopyDescriptorSet.
        );
    }

    return ok(res.move);
}

auto ref createCommandPool(T)(auto ref T arg) nothrow @nogc @trusted
if(from!"std.typecons".isTuple!T
    && is(typeof(arg.device) : from!"erupted".VkDevice)
    && is(typeof(arg.queueFamilyIndices) : QueueFamilyIndices)
)
{
    import util;
    import erupted;
    import expected : ok, err;

    alias Res = TupleCat!(T, Tuple!(
        VkCommandPool, "commandPool",
    ));
    auto res = partialConstruct!Res(forward!arg);

    const VkCommandPoolCreateInfo poolInfo =
    {
        queueFamilyIndex : res.queueFamilyIndices.graphicsFamily,
        flags : 0
            // Hint that command buffers are rerecorded with new commands very often.
            // | VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
            // Allow command buffers to be rerecorded individually, by default they are reset together.
            // | VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
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
    && is(from!"std.range".ElementType!(typeof(arg.descriptorSets[])) : from!"erupted".VkDescriptorSet)
    && is(typeof(arg.vertexBuffer) : from!"erupted".VkBuffer)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
)
{
    import util;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.range : enumerate;
    import erupted;
    import expected : ok, err;
    import automem : Vector;
    
    alias VectorType = Vector!(VkCommandBuffer, Mallocator);
    alias Res = TupleCat!(T, Tuple!(VectorType, "commandBuffers"));
    auto res = partialConstruct!Res(forward!arg);

    res.commandBuffers.length = res.swapChainFramebuffers.length;

    const VkCommandBufferAllocateInfo allocInfo =
    {
        commandPool : res.commandPool,
        // Primary - can be submitted to a queue, but cannot be called from other command buffers.
        // Secondary - cannot be submitted directly, but can be called from primary command buffers.
        level : VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        commandBufferCount : cast(uint) res.commandBuffers.length,
    };

    if(vkAllocateCommandBuffers(res.device, &allocInfo, res.commandBuffers.ptr) != VK_SUCCESS)
    {
        return err!Res("Failed to allocate command buffers.");
    }

    foreach(i, ref commandBuffer; res.commandBuffers[].enumerate)
    {
        const VkCommandBufferBeginInfo beginInfo =
        {
            flags : 0
                // Buffer will be rerecorded right after executing it once.
                // | VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                // This is a secondary command buffer that will be entirely within a single render pass.
                // | VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
                // Command buffer can be resubmitted while it is pending execution.
                // | VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
                , // Optional
            pInheritanceInfo : null, // Optional, for secondary buffer specifies what state to inherit from primary buffer.
        };

        if(vkBeginCommandBuffer(commandBuffer, &beginInfo) != VK_SUCCESS)
        {
            return err!Res("Failed to begin recording command buffer.");
        }

        const VkClearValue[2] clearValues =
        [
            {
                // clearColor
                color :
                {
                    float32 : [ 0.0f, 0.0f, 0.0f, 1.0f, ],
                },
            },
            {
                // clearDepth
                depthStencil :
                {
                    // Range from 0.0 (closest) to 1.0 (farthest).
                    depth : 1.0f,
                    stencil : 0,
                }
            }
        ];
        
        const VkRenderPassBeginInfo renderPassInfo =
        {
            renderPass : res.renderPass,
            framebuffer : res.swapChainFramebuffers.ptr[i],
            renderArea :
            {
                offset : {0, 0},
                extent : res.swapChainExtent,
            },
            clearValueCount : clearValues.length,
            pClearValues : clearValues.ptr,
        };

        // Inline - render pass commands embedded in the primary command buffer, no secondary buffers will be executed.
        // Secondary command buffers - render pass commands will be executed from secondary buffers.
        vkCmdBeginRenderPass(
            commandBuffer,
            &renderPassInfo,
            VK_SUBPASS_CONTENTS_INLINE);

        vkCmdBindPipeline(
            commandBuffer,
            VK_PIPELINE_BIND_POINT_GRAPHICS,
            res.graphicsPipeline);

        const VkDeviceSize[1] offsets = [ 0 ];
        vkCmdBindVertexBuffers(
            commandBuffer,
            0, // Offset.
            1, // Number of bindings.
            &res.vertexBuffer,
            offsets.ptr,
        );

        // Type is UINT16 or UINT32.
        vkCmdBindIndexBuffer(commandBuffer, res.indexBuffer, 0, VK_INDEX_TYPE_UINT32);

        vkCmdBindDescriptorSets(
            commandBuffer,
            // Graphics or compute pipeline.
            VK_PIPELINE_BIND_POINT_GRAPHICS,
            // Layout that the descriptor is based on.
            res.pipelineLayout,
            0, // Index of the first descriptor set.
            1, // Number of sets to bind.
            &res.descriptorSets.ptr[i], // Array of sets to bind.
            0, null, // Array of offsets for dynamic descriptors.
        );
        
        // vkCmdDraw(
        //     commandBuffer,
        //     cast(uint) res.vertices.length, // vertexCount.
        //     1, // instanceCount
        //     0, // firstVertex - lowest value of gl_VertexIndex
        //     0, // firstInstance - lowest value of gl_InstanceIndex
        //     );

        vkCmdDrawIndexed(
            commandBuffer,
            cast(uint) res.indices.length, // vertexCount.
            1, // instanceCount
            0, // Offset into the index buffer in indices, not bytes.
            0, // Offset to add to the indices in the index buffer.
            0, // Offset for instancing.
        );
        
        vkCmdEndRenderPass(commandBuffer);

        if(vkEndCommandBuffer(commandBuffer) != VK_SUCCESS)
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
    import util;
    import std.range : only;
    import std.experimental.allocator.mallocator : Mallocator;
    import erupted;
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
                vkDestroySemaphore(res.device, (*semaphores[l])[k], null);
            }
            vkDestroyFence(res.device, res.inFlightFences[k], null);
        }
        foreach(l; 0 .. j)
        {
            vkDestroySemaphore(res.device, (*semaphores[l])[i], null);
        }
    }

    foreach(i; 0 .. MaxFramesInFlight)
    {
        foreach(j; 0 .. semaphores.length)
        {
            const VkSemaphoreCreateInfo semaphoreInfo;
            if(vkCreateSemaphore(res.device, &semaphoreInfo, null, &(*semaphores[j])[i]) != VK_SUCCESS)
            {
                destroyCreatedObjects(i, j);
                return err!Res("Failed to create semaphore.");
            }
        }

        const VkFenceCreateInfo fenceInfo =
        {
            flags : VK_FENCE_CREATE_SIGNALED_BIT,
        };
        if(vkCreateFence(res.device, &fenceInfo, null, &res.inFlightFences[i]) != VK_SUCCESS)
        {
            destroyCreatedObjects(i, semaphores.length);
            return err!Res("Failed to create fence.");
        }
    }

    return ok(res.move);
}

void updateUniformBuffer(
    from!"erupted".VkDevice device,
    from!"erupted".VkExtent2D swapChainExtent,
    from!"erupted".VkDeviceMemory uniformBufferMemory,
) nothrow @nogc @trusted
{
    import core.stdc.string : memcpy;
    import glfw_vulkan : glfwGetTime;
    import erupted;
    import gfm.math : vec3f, mat4f, radians;

    immutable float time = glfwGetTime();

    UniformBufferObject ubo =
    {
        model : mat4f.rotation(time * radians(90.0f), vec3f(0.0f, 0.0f, 1.0f)).transposed,
        view : mat4f.lookAt(vec3f(2.0f, 2.0f, 2.0f), vec3f(0.0f, 0.0f, 0.0f), vec3f(0.0f, 0.0f, 1.0f)).transposed,
        proj : mat4f.perspective(
            radians(45.0f),
            swapChainExtent.width / cast(float) swapChainExtent.height,
            0.1f, 10.0f
            ).transposed,
    };
    ubo.proj.c[1][1] *= -1; // Vulkan, unlike OpenGL, have Y coordinate upside down.

    void* data;
    vkMapMemory(device, uniformBufferMemory, 0, ubo.sizeof, 0, &data);
    memcpy(data, &ubo, ubo.sizeof);
    vkUnmapMemory(device, uniformBufferMemory);
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
    import erupted;
    import expected : ok, err, andThen;

    vkWaitForFences(arg.device, 1, &arg.inFlightFences[currentFrame],
        VK_TRUE, // Whether to wait all fences in the array or any of the fences.
        ulong.max
        );
    
    uint imageIndex;
    immutable ackuireResult = vkAcquireNextImageKHR(
        arg.device,
        arg.swapChain,
        ulong.max, // Timeout in nanoseconds. 64 unsigned max disables timeout.
        arg.imageAvailableSemaphores[currentFrame],
        VK_NULL_HANDLE, // Fence.
        &imageIndex,
        );
    
    if(ackuireResult == VK_ERROR_OUT_OF_DATE_KHR || *arg.framebufferResized)
    {
        *arg.framebufferResized = false;
        return recreateSwapChain(forward!arg)
            .andThen!drawFrame(currentFrame)
            ;
    }
    else if(ackuireResult != VK_SUCCESS && ackuireResult != VK_SUBOPTIMAL_KHR)
    {
        return err!T("Failed to acquire swap chain image.");
    }

    updateUniformBuffer(arg.device, arg.swapChainExtent, arg.uniformBuffersMemory.ptr[imageIndex]);
    
    if(arg.imagesInFlight.ptr[imageIndex] != VK_NULL_HANDLE)
    {
        vkWaitForFences(arg.device, 1, &arg.imagesInFlight.ptr[imageIndex],
            VK_TRUE, // Whether to wait all fences in the array or any of the fences.
            ulong.max
            );
    }

    arg.imagesInFlight.ptr[imageIndex] = arg.inFlightFences[currentFrame];
    
    const VkPipelineStageFlags[1] waitStages =
    [
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
    ];

    const VkSubmitInfo submitInfo =
    {
        waitSemaphoreCount : 1,
        pWaitSemaphores : &arg.imageAvailableSemaphores[currentFrame],
        pWaitDstStageMask : waitStages.ptr,

        commandBufferCount : 1,
        pCommandBuffers : &arg.commandBuffers.ptr[imageIndex], // TODO: get rid of ptr.

        signalSemaphoreCount : 1,
        pSignalSemaphores : &arg.renderFinishedSemaphores[currentFrame],
    };

    vkResetFences(arg.device, 1, &arg.inFlightFences[currentFrame]);
    if(vkQueueSubmit(arg.graphicsQueue, 1, &submitInfo, arg.inFlightFences[currentFrame]) != VK_SUCCESS)
    {
        return err!T("Failed to submit draw command buffer.");
    }

    const VkPresentInfoKHR presentInfo =
    {
        waitSemaphoreCount : 1,
        pWaitSemaphores : &arg.renderFinishedSemaphores[currentFrame],

        swapchainCount : 1,
        pSwapchains : &arg.swapChain,
        pImageIndices : &imageIndex,

        // Array of VkResult, usefull when there are multiple swapchains. Otherwise just check the return value.
        pResults : null,
    };

    immutable presentResult = vkQueuePresentKHR(arg.presentQueue, &presentInfo);

    if(presentResult == VK_ERROR_OUT_OF_DATE_KHR ||
        presentResult == VK_SUBOPTIMAL_KHR ||
        *arg.framebufferResized
    )
    {
        *arg.framebufferResized = false;
        return recreateSwapChain(forward!arg)
            .andThen!drawFrame(currentFrame)
            ;
    }
    else if(presentResult != VK_SUCCESS)
    {
        return err!T("Failed to present swap chain image.");
    }

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
    import erupted : vkDeviceWaitIdle;
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

    vkDeviceWaitIdle(exp.value.device);
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
    && is(typeof(arg.colorImage) : from!"erupted".VkImage)
    && is(typeof(arg.colorImageMemory) : from!"erupted".VkDeviceMemory)
    && is(typeof(arg.colorImageView) : from!"erupted".VkImageView)
    && is(typeof(arg.depthImage) : from!"erupted".VkImage)
    && is(typeof(arg.depthImageMemory) : from!"erupted".VkDeviceMemory)
    && is(typeof(arg.depthImageView) : from!"erupted".VkImageView)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainFramebuffers[])) : from!"erupted".VkFramebuffer)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
    && is(from!"std.range".ElementType!(typeof(arg.commandBuffers[])) : from!"erupted".VkCommandBuffer)
    && is(from!"std.range".ElementType!(typeof(arg.uniformBuffers[])) : from!"erupted".VkBuffer)
    && is(from!"std.range".ElementType!(typeof(arg.uniformBuffersMemory[])) : from!"erupted".VkDeviceMemory)
    && is(typeof(arg.descriptorPool) : from!"erupted".VkDescriptorPool)
    && is(from!"std.range".ElementType!(typeof(arg.descriptorSets[])) : from!"erupted".VkDescriptorSet)
)
{
    import util : erase;
    import core.lifetime : forward;
    import std.meta : AliasSeq;
    import erupted;
    import expected : ok;

    vkDestroyDescriptorPool(arg.device, arg.descriptorPool, null);

    foreach(ref uniformBuffer; arg.uniformBuffers[])
    {
        vkDestroyBuffer(arg.device, uniformBuffer, null);
    }
    foreach(ref uniformBufferMemory; arg.uniformBuffersMemory[])
    {
        vkFreeMemory(arg.device, uniformBufferMemory, null);
    }

    foreach(ref framebuffer; arg.swapChainFramebuffers[])
    {
        vkDestroyFramebuffer(arg.device, framebuffer, null);
    }

    vkFreeCommandBuffers(arg.device, arg.commandPool,
        cast(uint) arg.commandBuffers.length, arg.commandBuffers.ptr);
    
    vkDestroyImageView(arg.device, arg.depthImageView, null);
    vkDestroyImage(arg.device, arg.depthImage, null);
    vkFreeMemory(arg.device, arg.depthImageMemory, null);
    
    vkDestroyImageView(arg.device, arg.colorImageView, null);
    vkDestroyImage(arg.device, arg.colorImage, null);
    vkFreeMemory(arg.device, arg.colorImageMemory, null);

    vkDestroyPipeline(arg.device, arg.graphicsPipeline, null);
    vkDestroyPipelineLayout(arg.device, arg.pipelineLayout, null);
    vkDestroyRenderPass(arg.device, arg.renderPass, null);

    foreach(ref imageView; arg.swapChainImageViews[])
    {
        vkDestroyImageView(arg.device, imageView, null);
    }

    vkDestroySwapchainKHR(arg.device, arg.swapChain, null);

    alias toErase = AliasSeq!(
        "swapChainImageFormat",
        "swapChainExtent",
        "swapChain",
        "swapChainImages",
        "swapChainImageViews",
        "renderPass",
        "pipelineLayout",
        "graphicsPipeline",
        "colorImage",
        "colorImageMemory",
        "colorImageView",
        "depthImage",
        "depthImageMemory",
        "depthImageView",
        "swapChainFramebuffers",
        "uniformBuffers",
        "uniformBuffersMemory",
        "descriptorPool",
        "descriptorSets",
        "commandBuffers",
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
    && is(typeof(arg.descriptorSetLayout) : from!"erupted".VkDescriptorSetLayout)
    && is(typeof(arg.swapChain) : from!"erupted".VkSwapchainKHR)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainImageViews[])) : from!"erupted".VkImageView)
    && is(typeof(arg.renderPass) : from!"erupted".VkRenderPass)
    && is(typeof(arg.pipelineLayout) : from!"erupted".VkPipelineLayout)
    && is(typeof(arg.graphicsPipeline) : from!"erupted".VkPipeline)
    && is(from!"std.range".ElementType!(typeof(arg.swapChainFramebuffers[])) : from!"erupted".VkFramebuffer)
    && is(typeof(arg.commandPool) : from!"erupted".VkCommandPool)
    && is(typeof(arg.textureImageMemory) : from!"erupted".VkDeviceMemory)
    && is(typeof(arg.textureImage) : from!"erupted".VkImage)
    && is(typeof(arg.textureImageView) : from!"erupted".VkImageView)
    && is(typeof(arg.textureSampler) : from!"erupted".VkSampler)
    && is(typeof(arg.vertexBufferMemory) : from!"erupted".VkDeviceMemory)
    && is(typeof(arg.vertexBuffer) : from!"erupted".VkBuffer)
    && is(typeof(arg.indexBufferMemory) : from!"erupted".VkDeviceMemory)
    && is(typeof(arg.indexBuffer) : from!"erupted".VkBuffer)
    && is(from!"std.range".ElementType!(typeof(arg.commandBuffers[])) : from!"erupted".VkCommandBuffer)
    && is(from!"std.range".ElementType!(typeof(arg.imageAvailableSemaphores[])) : from!"erupted".VkSemaphore)
    && is(from!"std.range".ElementType!(typeof(arg.renderFinishedSemaphores[])) : from!"erupted".VkSemaphore)
    && is(from!"std.range".ElementType!(typeof(arg.inFlightFences[])) : from!"erupted".VkFence)
)
{
    import util : erase;
    import core.lifetime : forward;
    import std.meta : AliasSeq;
    import erupted;
    import erupted.vulkan_lib_loader : freeVulkanLib;
    import glfw_vulkan : glfwDestroyWindow, glfwTerminate;
    import expected : ok, andThen;

    return cleanupSwapChain(forward!arg)
        .andThen!((auto ref arg)
        {
            foreach(i; 0 .. MaxFramesInFlight)
            {
                vkDestroyFence(arg.device, arg.inFlightFences[i], null);
                vkDestroySemaphore(arg.device, arg.renderFinishedSemaphores[i], null);
                vkDestroySemaphore(arg.device, arg.imageAvailableSemaphores[i], null);
            }

            vkDestroyBuffer(arg.device, arg.indexBuffer, null);
            vkFreeMemory(arg.device, arg.indexBufferMemory, null);

            vkDestroyBuffer(arg.device, arg.vertexBuffer, null);
            vkFreeMemory(arg.device, arg.vertexBufferMemory, null);

            vkDestroySampler(arg.device, arg.textureSampler, null);
            vkDestroyImageView(arg.device, arg.textureImageView, null);
            vkDestroyImage(arg.device, arg.textureImage, null);
            vkFreeMemory(arg.device, arg.textureImageMemory, null);

            vkDestroyCommandPool(arg.device, arg.commandPool, null);

            vkDestroyDescriptorSetLayout(arg.device, arg.descriptorSetLayout, null);

            vkDestroyDevice(arg.device, null);

            debug(LearnVulkan_ValidationLayers)
            {
                import erupted : vkDestroyDebugUtilsMessengerEXT;
                vkDestroyDebugUtilsMessengerEXT(arg.instance, arg.debugMessenger, null);
            }

            vkDestroySurfaceKHR(arg.instance, arg.surface, null);
            vkDestroyInstance(arg.instance, null);

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
                "descriptorSetLayout",
                "commandPool",
                "textureImageMemory",
                "textureImage",
                "textureImageView",
                "vertexBufferMemory",
                "vertexBuffer",
                "indexBufferMemory",
                "indexBuffer",
                "imageAvailableSemaphores",
                "renderFinishedSemaphores",
                "inFlightFences",
                validationLayersToErase,
                );
            return ok(forward!arg.erase!toErase);
        });
}
