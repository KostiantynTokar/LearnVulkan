import std.stdio;
import erupted.vulkan_lib_loader;
import erupted;
import bindbc.glfw;
import core.stdc.stdlib;

struct HelloTriangleApplication
{
    void run()
    {
        initWindow();
        initVulkan();
        mainLoop();
        cleanup();
    }

private:

    void initWindow()
    {
        glfwInit();

        glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
        glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
        window = glfwCreateWindow(windowWidth, windowHeight, "LearnVulkan", null, null);
    }

    void initVulkan()
    {
        if(!loadGlobalLevelFunctions())
        {
            "Oops".writeln;
            return;
        }
    }

    void mainLoop()
    {
        while(!glfwWindowShouldClose(window))
        {
            glfwPollEvents();
        }
    }

    void cleanup()
    {
        glfwDestroyWindow(window);
        glfwTerminate();
    }

    GLFWwindow* window;
    enum windowWidth = 800;
    enum windowHeight = 600;
}

int main()
{
    HelloTriangleApplication app;

    try
    {
        app.run();
    }
    catch(Exception e)
    {
        e.message.writeln();
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
