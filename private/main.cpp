#include <vush/vush.hpp>

#include <iostream>

int main() {
    anton::Array<anton::String> import_directories{anton::variadic_construct, anton::String{u8"C:/Users/an0num0us/Documents/vush2/build/shaders"}};
    anton::Array<vush::Constant_Define> constant_defines{anton::variadic_construct, vush::Constant_Define{anton::String{u8"_HAS_TEXTURE"}, 1}};
    vush::Configuration config;
    config.defines = anton::move(constant_defines);
    config.import_directories = anton::move(import_directories);
    config.source_name = u8"C:/Users/an0num0us/documents/vush2/build/shaders/skin.vush";
    anton::Expected<vush::Build_Result, anton::String> result = vush ::compile_to_glsl(config);
    if(result) {
        vush::Build_Result const& res = result.value();
        for(vush::Pass_Settings const& pass_settings: res.settings) {
            std::cout << pass_settings.pass_name.data() << " settings:\n";
            for(vush::Setting_Key_Value const& kv: pass_settings.settings) {
                std::cout << "    " << kv.key.data() << ": " << kv.value.data() << "\n";
            }
        }
        std::cout << "\n\n";

        for(vush::Pass_Data const& pass: res.passes) {
            std::cout << "\n\nPASS " << pass.name.data() << "\n\n";
            for(vush::GLSL_File const& file: pass.files) {
                std::cout << file.data.data() << "\n\n";
            }
        }
        return 0;
    } else {
        std::cout << result.error().data() << '\n';
        return -1;
    }
}
