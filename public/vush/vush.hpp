#pragma once

#include <anton/array.hpp>
#include <anton/string.hpp>
#include <vush/expected.hpp>
#include <vush/types.hpp>

namespace vush {
    struct Constant_Define {
        char const* name;
        i64 value;
    };

    struct Format_Options {};

    struct Configuration {
        char const* source_path;
        char const* const* import_directories;
        i64 import_directories_count;
        Constant_Define const* defines;
        i64 defines_count;
        Format_Options format;
    };

    struct GLSL_File {
        anton::String data;
    };

    // compile_to_glsl
    // Compiles given vush shader to glsl shader.
    // Uses the import paths provided in import_paths to resolve import directives.
    //
    // Returns compiled glsl file or error message.
    //
    Expected<anton::Array<GLSL_File>, anton::String> compile_to_glsl(Configuration const& config);
} // namespace vush
