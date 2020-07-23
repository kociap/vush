#pragma once

#include <anton/expected.hpp>
#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Declaration_List;

    anton::Expected<anton::Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Declaration_List& node, Format_Options const& format);
} // namespace vush
