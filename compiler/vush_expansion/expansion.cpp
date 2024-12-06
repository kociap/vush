#include <vush_expansion/expansion.hpp>

#include <vush_core/context.hpp>

namespace vush {
  anton::Expected<Array<SNOT>, Error> full_expand(Context& ctx,
                                                  Array<SNOT> snots)
  {
    Array<anton::String> import_paths{ctx.allocator};
    // TODO: For now this is just pass-through.
    return {anton::expected_value, ANTON_MOV(snots)};
  }
} // namespace vush
