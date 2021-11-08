cmake -S ./dependencies/glfw -B ./dependencies/glfw/build -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF
cmake --build ./dependencies/glfw/build
cmake --build ./dependencies/glfw/build --config Release
cmake -S ./dependencies/assimp -B ./dependencies/assimp/build -DASSIMP_BUILD_TESTS=OFF
cmake --build ./dependencies/assimp/build
cmake --build ./dependencies/assimp/build --config Release
