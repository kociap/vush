#pragma once

#include <context.hpp>
#include <vush/expected.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Declaration_List;

    Expected<anton::Array<GLSL_File>, anton::String> generate_glsl(Context const& ctx, Declaration_List& node, Format_Options const& format);
} // namespace vush
