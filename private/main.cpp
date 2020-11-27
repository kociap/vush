#include <vush/vush.hpp>

#include <chrono>
#include <iostream>

static anton::Expected<vush::Source_Definition, anton::String> source_definition_cb(anton::String_View, anton::Slice<vush::Pass_Settings const>,
                                                                                    anton::Slice<vush::Sourced_Variable const>,
                                                                                    anton::Slice<vush::Sourced_Variable const>,
                                                                                    anton::Slice<vush::Sourced_Variable const>, void*) {
    return {anton::expected_value, anton::String{}, anton::String{}};
}

int main() {
    anton::Array<anton::String> import_directories{anton::variadic_construct, anton::String{u8"C:/Users/An0num0us/Documents/vush2/build/shaders"}};
    anton::Array<vush::Constant_Define> constant_defines{anton::variadic_construct, vush::Constant_Define{anton::String{u8"_HAS_TEXTURE"}, 0},
                                                         vush::Constant_Define{anton::String{u8"_CASTS_SHADOWS"}, 1},
                                                         vush::Constant_Define{anton::String{u8"_EMISSIVE"}, 0}};
    anton::Array<vush::Extension> extensions{anton::variadic_construct,
                                             vush::Extension{anton::String{u8"GL_EXT_nonuniform_qualifier"}, vush::Extension_Behaviour::require}};
    vush::Configuration config;
    config.defines = ANTON_MOV(constant_defines);
    config.import_directories = ANTON_MOV(import_directories);
    config.extensions = ANTON_MOV(extensions);
    config.source_name = u8"C:/Users/An0num0us/Documents/vush2/build/shaders/diffuse.vush";
    config.source_definition_cb = source_definition_cb;
    auto t1 = std::chrono::high_resolution_clock::now();
    anton::Expected<vush::Build_Result, anton::String> result = vush::compile_to_glsl(config);
    auto t2 = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(t2 - t1).count();
    std::cout << "execution time " << duration << "ns\n";
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
