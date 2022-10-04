#pragma once

#include <anton/array.hpp>
#include <anton/expected.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>
#include <vush/allocator.hpp>
#include <vush/types.hpp>

namespace vush {
    struct Error {
        // Name of the source where the error was generated.
        anton::String source;
        // The diagnostic message.
        anton::String diagnostic;
        // A more thorough explanation of the error that contains
        // source code snippets with the exact locations highlighted.
        anton::String extended_diagnostic;
        i64 line;
        i64 column;
        i64 end_line;
        i64 end_column;

        // format
        // Format diagnostic message with source, line and column.
        //   <source>:<line>:<column>: error: <diagnostic>
        // The extended diagnostic will be included if include_extended_diagnostic is true.
        //   <source>:<line>:<column>: error: <diagnostic>
        //   <extended_diagnostic>
        //
        // Parameters:
        //                   allocator - allocator to use for allocating the string.
        // include_extended_diagnostic - whether to include the extended diagnostic message.
        //
        // Returns:
        // Formatted diagnostic message.
        //
        [[nodiscard]] anton::String format(Allocator* allocator, bool include_extended_diagnostic) const;
    };

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
        // Per stage sourced data. Index by casting Stage_Kind to i64.
        anton::Slice<Stage_Sourced_Data const> sourced_data;
        void* user_data;
    };

    // source_definition_callback
    //
    // Parameters:
    //     context - the definition context of a source.
    // definitions - output parameter for per-stage source definitions.
    //               The slice is always presized to stage_kind_count.
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

    enum struct Stage_Kind : u8 {
        vertex,
        fragment,
        compute,
    };

    // stage_kind_count
    // The number of enumerations in Stage_Kind.
    constexpr i64 stage_kind_count = 3;

    struct Source_Import_Result {
        anton::String source_name;
        anton::String data;
    };

    using source_import_callback = anton::Expected<Source_Import_Result, anton::String> (*)(Allocator& allocator, anton::String_View path, void* user_data);

    namespace spirv {
        struct Shader {
            anton::String data;
            Stage_Kind stage_kind;
        };

        struct Pass_Data {
            anton::String name;
            Array<Shader> shaders;
        };

        struct Build_Result {
            Array<Pass_Settings> settings;
            Array<Pass_Data> passes;
        };

        // compile
        // Compiles given vush shader to a spirv shader.
        // Uses the callback to import sources.
        //
        // Returns compiled spirv files or an error.
        //
        anton::Expected<Build_Result, Error> compile(Configuration const& config, Allocator& allocator, source_import_callback callback, void* user_data);

        // compile
        // Compiles given vush shader to a spirv shader.
        // Reads the source files from the disk.
        // Uses the import paths provided in import_directories to resolve import directives.
        //
        // Returns compiled spirv files or an error.
        //
        anton::Expected<Build_Result, Error> compile(Configuration const& config, Allocator& allocator,
                                                     anton::Slice<anton::String const> const& import_directories);
    } // namespace spirv
} // namespace vush
