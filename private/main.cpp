#include <iostream>
#include <vush/vush.hpp>

int main() {
    char const* const include_paths[] = {"C:/Users/lapinozz/Documents/vush2/build"};
    vush::Constant_Define defines[1] = {{u8"_HAS_TEXTURE", 1}};
    vush::Expected<vush::Compiled_File, vush::String> result =
        vush ::compile_to_glsl(u8"C:/Users/lapinozz/documents/vush2/build/shader.vush", include_paths, 1, defines, 1);
    if(!result) {
        std::cout << result.error().data() << '\n';
        return -1;
    } else {
        return 0;
    }
}
