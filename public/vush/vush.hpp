#pragma once

#include <vush/expected.hpp>
#include <vush/types.hpp>

namespace vush {
    struct Constant_Define {
        char const* name;
        i64 value;
    };

    struct Compiled_File {};

    // compile_to_glsl
    // Compiles given vush shader to glsl shader.
    // Uses the import paths provided in import_paths to resolve import directives.
    //
    // Returns compiled glsl file or error message.
    //
    Expected<Compiled_File, String> compile_to_glsl(char const* source_path, char const* const* import_paths, i64 import_paths_count,
                                                    Constant_Define const* defines, i64 defines_count);
} // namespace vush
