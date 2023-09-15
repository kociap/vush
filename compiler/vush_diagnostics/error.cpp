#include <vush_diagnostics/error.hpp>

#include <vush_diagnostics/utility.hpp>

namespace vush {
  using namespace anton::literals;

  anton::String Error::format(Allocator* const allocator,
                              bool const include_extended_diagnostic) const
  {
    // Add 32 bytes for line and column numbers, colons, spaces and newlines.
    i64 const size =
      source.size_bytes() + diagnostic.size_bytes() + extended_diagnostic.size_bytes() + 32;
    anton::String error_message{anton::reserve, size, allocator};
    error_message += format_diagnostic_location(allocator, source, line, column);
    error_message += diagnostic;
    error_message += "\n"_sv;
    if(include_extended_diagnostic && extended_diagnostic.size_bytes() > 0) {
      error_message += extended_diagnostic;
      error_message += "\n"_sv;
    }
    return error_message;
  }
} // namespace vush
