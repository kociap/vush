#pragma once

#include <vush/expected.hpp>
#include <vush/types.hpp>

namespace vush {
    struct Compiled_File {};

    // compile_to_glsl
    // Compiles given vush shader to glsl shader.
    // Uses the include paths provided in include_paths to resolve #include directives.
    //
    // Returns compiled glsl file or error message.
    //
    Expected<Compiled_File, String> compile_to_glsl(char const* source_path, char const* const* include_paths, i64 include_paths_count);
} // namespace vush
