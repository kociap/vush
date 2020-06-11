#include <vush/vush.hpp>

#include <iostream>

int main() {
    char const* const import_directories[] = {"C:/Users/lapinozz/Documents/vush2/build"};
    vush::Constant_Define defines[1] = {{u8"_HAS_TEXTURE", 1}};
    vush::Configuration config;
    config.defines = defines;
    config.defines_count = 1;
    config.import_directories = import_directories;
    config.import_directories_count = 1;
    config.source_path = u8"C:/Users/lapinozz/documents/vush2/build/shader.vush";
    vush::Expected<anton::Array<vush::GLSL_File>, anton::String> result = vush ::compile_to_glsl(config);
    if(result) {
        for(vush::GLSL_File& file: result.value()) {
            std::cout << file.data.data() << "\n\n";
        }
        return 0;
    } else {
        std::cout << result.error().data() << '\n';
        return -1;
    }
}
