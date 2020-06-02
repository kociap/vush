#include <iostream>
#include <vush/vush.hpp>

int main() {
    vush::Expected<vush::Compiled_File, vush::String> result = vush ::compile_to_glsl(u8"C:/Users/lapinozz/documents/vush2/build/shader.vush", nullptr, 0);
    if(!result) {
        std::cout << result.error().data() << '\n';
        return -1;
    } else {
        return 0;
    }
}
