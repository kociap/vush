#include <vush_expansion/expansion.hpp>

#include <anton/ilist.hpp>

#include <vush_autogen/syntax_accessors.hpp>
#include <vush_core/context.hpp>
#include <vush_lexer/lexer.hpp>
#include <vush_parser/parser.hpp>

namespace vush {
#define RETURN_ON_FAIL(variable, fn, ...)                        \
  auto variable = fn(__VA_ARGS__);                               \
  if(!variable) {                                                \
    return {anton::expected_error, ANTON_MOV(variable.error())}; \
  }

  anton::Expected<SNOT*, Error> full_expand(Context& ctx, SNOT* snots)
  {
    while(true) {
      if(snots->kind == SNOT_Kind::decl_import) {
        SNOT const* const path_snot = get_decl_import_path(snots);
        ANTON_ASSERT(path_snot->is_token(), "import path is not syntax token");
        auto const import_path =
          anton::shrink_bytes(path_snot->get_value(), 1, 1);
        RETURN_ON_FAIL(import_result, import_main_source, ctx, import_path);

        Source_Data const* const source = import_result.value();
        if(source == nullptr) {
          continue;
        }

        RETURN_ON_FAIL(lex_result, lex_source, ctx, source->path,
                       anton::String7_View{source->data.bytes_begin(),
                                           source->data.bytes_end()});

        Parse_Syntax_Options parse_options{.include_whitespace_and_comments =
                                             false};
        RETURN_ON_FAIL(parse_result, parse_tokens, ctx, source,
                       lex_result.value(), parse_options);

        anton::ilist_splice_after(snots, parse_result.value());
        anton::ilist_erase(snots);
        snots = parse_result.value();
        continue;
      }

      SNOT* const next = anton::ilist_next(snots);
      if(next == nullptr) {
        break;
      }

      snots = next;
    }

    snots = anton::ilist_begin(snots);

    return {anton::expected_value, snots};
  }
} // namespace vush
