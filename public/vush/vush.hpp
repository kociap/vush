#pragma once

#include <anton/array.hpp>
#include <anton/expected.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>
#include <vush/types.hpp>

namespace vush {
    struct Constant_Define {
        anton::String_View name;
        i64 value;
    };

    struct Format_Options {};

    struct Configuration {
        anton::String_View source_path;
        anton::Slice<anton::String_View const> import_directories;
        anton::Slice<Constant_Define const> defines;
        Format_Options format;
    };

    enum struct Stage_Type {
        vertex,
        fragment,
        compute,
    };

    struct Setting_Key_Value {
        anton::String key;
        anton::String value;
    };

    struct Settings_Group {
        anton::String group_name;
        anton::Array<Setting_Key_Value> settings;
    };

    struct Pass_Settings {
        anton::String pass_name;
        anton::Array<Settings_Group> settings_groups;
    };

    struct GLSL_File {
        anton::String data;
        anton::String pass_name;
        Stage_Type shader_type;
    };

    struct Source_Request_Result {
        anton::String source_name;
        anton::String data;
    };

    struct Build_Result {
        anton::Array<Pass_Settings> settings;
        anton::Array<GLSL_File> files;
    };

    using source_request_callback = anton::Expected<Source_Request_Result, anton::String> (*)(anton::String const& path, void* user_data);

    // compile_to_glsl
    // Compiles given vush shader to glsl shader.
    // Uses the callback to request source.
    // Does not use import_directories.
    //
    // Returns compiled glsl files or error message.
    //
    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config, source_request_callback callback, void* user_data);

    // compile_to_glsl
    // Compiles given vush shader to glsl shader.
    // Reads the source files from the disk.
    // Uses the import paths provided in import_directories to resolve import directives.
    //
    // Returns compiled glsl files or error message.
    //
    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config);
} // namespace vush
