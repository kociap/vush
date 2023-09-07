#include <vush_ast_passes/diagnostics.hpp>

#include <anton/format.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/context.hpp>
#include <vush_diagnostics/utility.hpp>

namespace vush {
  using namespace anton::literals;

  Error err_init_invalid_matrix_initializer_kind(Context const& ctx,
                                                 ast::Initializer const* const initializer)
  {
    Source_Info const source_info = initializer->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    error.diagnostic = "error: matrix initializer is not a field or range initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic += " matrix initializers must be field or range initializers"_sv;
    return error;
  }

  Error err_init_invalid_struct_initializer_kind(Context const& ctx,
                                                 ast::Initializer const* const initializer)
  {
    Source_Info const source_info = initializer->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    error.diagnostic = "error: initializer is not field initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic += " struct initializers must be field initializers"_sv;
    return error;
  }
} // namespace vush
