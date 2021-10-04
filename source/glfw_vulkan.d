module glfw_vulkan;

import erupted : VkInstance, VkPhysicalDevice, VkAllocationCallbacks, VkSurfaceKHR, VkResult;
public import bindbc.glfw;
mixin(bindGLFW_Vulkan);
