#include <vush_core/context.hpp>

#include <vush_core/memory.hpp>
#include <vush_core/source_info.hpp>
#include <vush_diagnostics/diagnostics.hpp>

namespace vush {
  anton::Expected<Source_Data const*, Error>
  import_main_source(Context& ctx, anton::String_View const source_name)
  {
    anton::Expected<anton::String, anton::String> query_result =
      ctx.query_source_cb(ctx.bump_allocator, source_name,
                          ctx.query_main_source_user_data);
    if(!query_result) {
      return {anton::expected_error,
              err_source_import_failed_no_location(ctx, query_result.error())};
    }

    // Ensure we're not importing the same source multiple times.
    anton::String& source_identifier = query_result.value();
    if(ctx.source_registry->find_source(source_identifier) != nullptr) {
      return {anton::expected_value, nullptr};
    }

    anton::Expected<anton::String, anton::String> import_result =
      ctx.import_source_cb(ctx.bump_allocator, source_name,
                           ctx.import_main_source_user_data);

    if(!import_result) {
      return {anton::expected_error,
              err_source_import_failed_no_location(ctx, import_result.error())};
    }

    anton::String& source_data = import_result.value();
    isize const source_size = source_data.size_bytes();
    if(source_size > anton::limits::maximum_i32) {
      return {anton::expected_error,
              err_source_too_large_no_location(ctx, source_name, source_size)};
    }

    auto const source = VUSH_ALLOCATE(Source_Data, ctx.bump_allocator,
                                      ANTON_MOV(source_identifier),
                                      ANTON_MOV(import_result.value()));
    ctx.source_registry->add_source(source);
    return {anton::expected_value, source};
  }

  anton::Expected<Source_Data const*, Error>
  import_source(Context& ctx, anton::String_View const source_name,
                Source_Info const& source_info)
  {
    anton::Expected<anton::String, anton::String> query_result =
      ctx.query_source_cb(ctx.bump_allocator, source_name,
                          ctx.query_source_user_data);
    if(!query_result) {
      return {anton::expected_error,
              err_source_import_failed(ctx, source_info, query_result.error())};
    }

    // Ensure we're not importing the same source multiple times.
    anton::String& source_identifier = query_result.value();
    if(ctx.source_registry->find_source(source_identifier) != nullptr) {
      return {anton::expected_value, nullptr};
    }

    anton::Expected<anton::String, anton::String> import_result =
      ctx.import_source_cb(ctx.bump_allocator, source_name,
                           ctx.import_source_user_data);

    if(!import_result) {
      return {
        anton::expected_error,
        err_source_import_failed(ctx, source_info, import_result.error())};
    }

    anton::String& source_data = import_result.value();
    isize const source_size = source_data.size_bytes();
    if(source_size > anton::limits::maximum_i32) {
      return {anton::expected_error,
              err_source_too_large(ctx, source_info, source_name, source_size)};
    }

    auto const source = VUSH_ALLOCATE(Source_Data, ctx.bump_allocator,
                                      ANTON_MOV(source_identifier),
                                      ANTON_MOV(import_result.value()));
    ctx.source_registry->add_source(source);
    return {anton::expected_value, source};
  }
} // namespace vush
