mkdir -p ./shaders_bin
glslc ./source/shaders/shader.vert -o ./shaders_bin/vert.spv
glslc ./source/shaders/shader.frag -o ./shaders_bin/frag.spv
