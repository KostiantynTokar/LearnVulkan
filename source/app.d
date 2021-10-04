import std.stdio;
import erupted.vulkan_lib_loader;
import erupted;
import bindbc.glfw;

void main()
{
    "Hello, World!".writeln;

    if(!loadGlobalLevelFunctions())
    {
        "Oops".writeln;
        return;
    }

    glfwInit();

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    GLFWwindow* window = glfwCreateWindow(800, 600, "LearnVulkan", null, null);

    uint extensionCount = 0;
    vkEnumerateInstanceExtensionProperties(null, &extensionCount, null);

    writeln(extensionCount, " extensions supported");

    while(!glfwWindowShouldClose(window))
    {
        glfwPollEvents();
    }

    glfwDestroyWindow(window);
    glfwTerminate();
}
