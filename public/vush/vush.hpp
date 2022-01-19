#pragma once

#include <anton/array.hpp>
#include <anton/expected.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>
#include <vush/allocator.hpp>
#include <vush/types.hpp>

namespace vush {
    struct Constant_Define {
        anton::String name;
        i32 value;
    };

    enum struct Extension_Behaviour { require, enable, warn, disable };

    struct Extension {
        anton::String name;
        Extension_Behaviour behaviour;
    };

    struct Sourced_Variable {
        anton::String name;
        anton::String type;
        bool unsized;
    };

    struct Sourced_Opaque_Variable {
        anton::String name;
        anton::String type;
        anton::String image_qualifier;
        bool unsized;
    };

    struct Source_Definition {
        anton::String declaration;
        anton::String bind;
    };

    struct Setting_Key_Value {
        anton::String key;
        anton::String value;
    };

    struct Pass_Settings {
        anton::String pass_name;
        Array<Setting_Key_Value> settings;
    };

    struct Stage_Sourced_Data {
        anton::Slice<Sourced_Variable const> variables;
        anton::Slice<Sourced_Opaque_Variable const> opaque_variables;
    };

    struct Source_Definition_Context {
        anton::String_View pass_name;
        anton::String_View source_name;
        anton::Slice<Setting_Key_Value const> settings;
        // Per stage sourced data. Index by casting Stage_Type to i64.
        anton::Slice<Stage_Sourced_Data const> sourced_data;
        void* user_data;
    };

    // source_definition_callback
    //
    // Parameters:
    //     context - the definition context of a source.
    // definitions - output parameter for per-stage source definitions.
    //               The slice is always presized to stage_type_count.
    //
    using source_definition_callback = anton::Expected<void, anton::String> (*)(Source_Definition_Context const& context,
                                                                                anton::Slice<Source_Definition> definitions);

    struct Diagnostics_Options {
        // Whether to provide extended diagnostic messages that include more thorough explanation
        // of the error and source code snippets with the exact location highlighted
        bool extended = true;
        // Whether to display line numbers on the left side of code snippets
        bool display_line_numbers = true;
    };

    struct Configuration {
        anton::String source_name;
        Array<Constant_Define> defines;
        Array<Extension> extensions;
        source_definition_callback source_definition_cb = nullptr;
        void* source_definition_user_data = nullptr;
        Diagnostics_Options diagnostics;
    };

    enum struct Stage_Type {
        vertex,
        fragment,
        compute,
    };

    // stage_type_count
    // The number of enumerations in Stage_Type.
    constexpr i64 stage_type_count = 3;

    struct GLSL_File {
        anton::String data;
        Stage_Type stage_type;
    };

    struct Pass_Data {
        anton::String name;
        Array<GLSL_File> files;
    };

    struct Build_Result {
        Array<Pass_Settings> settings;
        Array<Pass_Data> passes;
    };

    struct Source_Request_Result {
        anton::String source_name;
        anton::String data;
    };

    using source_request_callback = anton::Expected<Source_Request_Result, anton::String> (*)(anton::String const& path, void* user_data);

    // compile_to_glsl
    // Compiles given vush shader to glsl shader.
    // Uses the callback to request sources.
    //
    // Returns compiled glsl files or error message.
    //
    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config, Allocator& allocator, source_request_callback callback,
                                                                 void* user_data);

    // compile_to_glsl
    // Compiles given vush shader to glsl shader.
    // Reads the source files from the disk.
    // Uses the import paths provided in import_directories to resolve import directives.
    //
    // Returns compiled glsl files or error message.
    //
    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config, Allocator& allocator,
                                                                 anton::Slice<anton::String const> const& import_directories);
} // namespace vush
