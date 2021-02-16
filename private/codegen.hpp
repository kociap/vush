#pragma once

#include <anton/expected.hpp>
#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
    anton::Expected<anton::Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Declaration_List& node, Format_Options const& format,
                                                                          anton::Slice<Extension const> extensions, anton::Slice<Pass_Settings const> settings);
} // namespace vush
