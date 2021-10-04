cmake -S ./dependencies/glfw -B ./dependencies/glfw/build -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF
cmake --build ./dependencies/glfw/build
cmake --build ./dependencies/glfw/build --config Release
