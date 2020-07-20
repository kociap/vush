#include <vush/vush.hpp>

#include <iostream>

int main() {
    anton::String_View const import_directories[] = {"C:/Users/an0num0us/Documents/vush2/build"};
    vush::Constant_Define defines[1] = {{u8"_HAS_TEXTURE", 1}};
    vush::Configuration config;
    config.defines = defines;
    config.import_directories = import_directories;
    config.source_path = u8"C:/Users/an0num0us/documents/vush2/build/shader.vush";
    anton::Expected<vush::Build_Result, anton::String> result = vush ::compile_to_glsl(config);
    if(result) {
        vush::Build_Result const& res = result.value();
        for(vush::Pass_Settings const& pass_settings: res.settings) {
            std::cout << pass_settings.pass_name.data() << " settings:\n";
            for(vush::Settings_Group const& group: pass_settings.settings_groups) {
                std::cout << "  " << group.group_name.data() << " group:\n";
                for(vush::Setting_Key_Value const& kv: group.settings) {
                    std::cout << "    " << kv.key.data() << ": " << kv.value.data() << "\n";
                }
            }
        }
        std::cout << "\n\n";

        for(vush::GLSL_File const& file: res.files) {
            std::cout << file.data.data() << "\n\n";
        }
        return 0;
    } else {
        std::cout << result.error().data() << '\n';
        return -1;
    }
}
