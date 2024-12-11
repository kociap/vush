#include <vush_parser/parser.hpp>

#include <anton/assert.hpp>
#include <anton/ilist.hpp>
#include <anton/intrinsics.hpp>
#include <anton/memory.hpp>
#include <anton/optional.hpp>
#include <anton/ranges.hpp>
#include <anton/string.hpp>
#include <anton/string7_stream.hpp>
#include <anton/string7_view.hpp>
#include <anton/type_traits/utility.hpp>

#include <vush_core/context.hpp>
#include <vush_core/memory.hpp>

namespace vush {
#define ANNOTATE_FUNCTION()

  using anton::Optional;
  using namespace anton::literals;

  class Lexer_State {
  public:
    Token const* current;
    i64 offset;
  };

  // TODO: Place this comment somewhere
  //
  // The source string is ASCII only, so String7 will be the exact same size as
  // String, but String7 will avoid all Unicode function calls and thus
  // accelerate parsing.

  struct Lexer {
  public:
    Lexer(Token const* begin, Token const* end)
      : current(begin), end(end), begin(begin)
    {
    }

    void advance_token()
    {
      if(current != end) {
        ++current;
      }
    }

    [[nodiscard]] anton::Optional<Token> peek_token()
    {
      if(current != end) {
        return *current;
      } else {
        return anton::null_optional;
      }
    }

    [[nodiscard]] anton::Optional<Token> next_token()
    {
      if(current != end) {
        Token const& token = *current;
        ++current;
        return token;
      } else {
        return anton::null_optional;
      }
    }

    [[nodiscard]] bool match_eof()
    {
      ignore_whitespace_and_comments();
      return current == end;
    }

    void ignore_whitespace_and_comments()
    {
      while(current != end) {
        Token_Kind const kind = current->kind;
        if(kind == Token_Kind::comment | kind == Token_Kind::whitespace) {
          ++current;
        } else {
          break;
        }
      }
    }

    [[nodiscard]] Lexer_State get_current_state()
    {
      ignore_whitespace_and_comments();
      return {current, current - begin};
    }

    [[nodiscard]] Lexer_State get_current_state_noskip()
    {
      return {current, current - begin};
    }

    [[nodiscard]] bool is_state_end(Lexer_State const& state) const
    {
      return state.current == end;
    }

    void restore_state(Lexer_State const& state)
    {
      current = state.current;
    }

  private:
    Token const* current;
    Token const* end;
    Token const* begin;
  };

#define EXPECT_TOKEN(token, message, snots) \
  if(auto _expect_token = match(token)) {   \
    snots.insert_back(_expect_token);       \
  } else {                                  \
    set_error(message);                     \
    _lexer.restore_state(begin_state);      \
    return nullptr;                         \
  }

#define EXPECT_TOKEN_SKIP(token, message, snots) \
  if(auto _expect_token = skipmatch(token)) {    \
    snots.insert_back(_expect_token);            \
  } else {                                       \
    set_error(message);                          \
    _lexer.restore_state(begin_state);           \
    return nullptr;                              \
  }

#define EXPECT_TOKEN2(result, token1, token2, message, snots) \
  if(auto _expect_token = match(result, token1, token2)) {    \
    snots.insert_back(_expect_token);                         \
  } else {                                                    \
    set_error(message);                                       \
    _lexer.restore_state(begin_state);                        \
    return nullptr;                                           \
  }

#define EXPECT_TOKEN2_SKIP(result, token1, token2, message, snots) \
  if(auto _expect_token = skipmatch(result, token1, token2)) {     \
    snots.insert_back(_expect_token);                              \
  } else {                                                         \
    set_error(message);                                            \
    _lexer.restore_state(begin_state);                             \
    return nullptr;                                                \
  }

#define EXPECT_NODE(fn, snots)         \
  if(auto _expect_node = fn()) {       \
    snots.insert_back(_expect_node);   \
  } else {                             \
    _lexer.restore_state(begin_state); \
    return nullptr;                    \
  }

#define ALLOCATE_SNOT(...) VUSH_ALLOCATE(SNOT, _allocator, __VA_ARGS__)

  class Parser {
  public:
    Parser(Allocator* allocator, Source_Data const* source, Lexer&& lexer)
      : _allocator(allocator), _source(source), _lexer(ANTON_MOV(lexer))
    {
      _source_data = _source->data.bytes_begin();
    }

    anton::Expected<SNOT*, Error> build_syntax_tree()
    {
      ANNOTATE_FUNCTION()
      anton::IList<SNOT> snots;
      while(true) {
        if(_lexer.match_eof()) {
          return {anton::expected_value, snots.unlink()};
        }

        if(SNOT* declaration = try_declaration()) {
          snots.insert_back(declaration);
        } else {
          return {anton::expected_error, _last_error.to_error(_source->path)};
        }
      }
    }

  private:
    struct Parse_Error {
      anton::String message;
      i64 line = 0;
      i64 column = 0;
      i64 stream_offset = 0;

      Error to_error(anton::String_View source) const
      {
        return Error{.source = anton::String(source),
                     .diagnostic = message,
                     .extended_diagnostic = ""_s,
                     .line = line,
                     .column = column};
      }
    };

    Allocator* _allocator;
    Source_Data const* _source;
    char8 const* _source_data;
    Lexer _lexer;
    Parse_Error _last_error;

  private:
    void set_error(anton::String_View const message, Lexer_State const& state)
    {
      if(_lexer.is_state_end(state)) {
        Token const& token = *(state.current - 1);
        _last_error.message = message;
        // _last_error.line = token.end_line;
        // _last_error.column = token.end_column;
        _last_error.stream_offset = token.end_offset;
      }

      Token const& token = *state.current;
      if(token.offset >= _last_error.stream_offset) {
        _last_error.message = message;
        _last_error.line = token.line;
        _last_error.column = token.column;
        _last_error.stream_offset = token.offset;
      }
    }

    void set_error(anton::String_View const message)
    {
      Lexer_State const state = _lexer.get_current_state_noskip();
      if(_lexer.is_state_end(state)) {
        Token const& token = *(state.current - 1);
        _last_error.message = message;
        // _last_error.line = token.end_line;
        // _last_error.column = token.end_column;
        _last_error.stream_offset = token.end_offset;
      }

      Token const& token = *state.current;
      if(token.offset >= _last_error.stream_offset) {
        _last_error.message = message;
        _last_error.line = token.line;
        _last_error.column = token.column;
        _last_error.stream_offset = token.offset;
      }
    }

    [[nodiscard]] Source_Info src_info(Lexer_State const& start,
                                       Lexer_State const& end)
    {
      Token const& start_token = *start.current;
      Token const& end_token = *(end.current - 1);
      return Source_Info{.source = _source,
                         .line = start_token.line,
                         .column = start_token.column,
                         .offset = start_token.offset,
                         // .end_line = end_token.end_line,
                         // .end_column = end_token.end_column,
                         .end_offset = end_token.end_offset};
    }

    [[nodiscard]] SNOT* token_to_SNOT(Token const& token)
    {
      // Token_Kind and SNOT_Kind have overlapping values, therefore we
      // convert one to the other via a cast.
      auto const node_kind = static_cast<SNOT_Kind>(token.kind);
      Source_Info source{
        .source = _source,
        .line = token.line,
        .column = token.column,
        .offset = token.offset,
        .end_offset = token.end_offset,
      };
      return ALLOCATE_SNOT(node_kind, source);
    }

    // match
    //
    // Matches the next token with a specified type in the token stream.
    //
    // Parameters:
    // type - the type of the token to match.
    //
    // Returns:
    // Syntax token of the specified type or nullptr if the next token's type
    // is not type.
    //
    [[nodiscard]] SNOT* match(Token_Kind const type)
    {
      anton::Optional<Token> const token = _lexer.peek_token();
      if(!token) {
        return nullptr;
      }

      Token_Kind const token_kind = token->kind;
      if(token_kind == type) {
        _lexer.advance_token();
        return token_to_SNOT(*token);
      } else {
        return nullptr;
      }
    }

    // skipmatch
    //
    // Matches the next token with a specified type in the token stream skipping
    // whitespace and comments. Does not match comments or whitespaces.
    //
    // Parameters:
    // type - the type of the token to match. Must not be Token_Kind::comment or
    //        Token_Kind::whitespace.
    //
    // Returns:
    // Syntax token of the specified type or nullptr if the next token's type
    // is not type.
    //
    [[nodiscard]] SNOT* skipmatch(Token_Kind const type)
    {
      _lexer.ignore_whitespace_and_comments();
      return match(type);
    }

    // match
    //
    // Matches identifier token with a specified content.
    //
    // Parameters:
    // value - the required identifier value.
    //
    // Returns:
    // An identifier syntax token if the value of the token is the same as the
    // parameter. nullptr otherwise.
    //
    [[nodiscard]] SNOT* match(anton::String7_View const value)
    {
      anton::Optional<Token> const token = _lexer.peek_token();
      if(!token) {
        return nullptr;
      }

      Token_Kind const token_kind = token->kind;
      anton::String7_View const token_value = token->get_value(_source_data);
      if(token_kind == Token_Kind::identifier && token_value == value) {
        _lexer.advance_token();
        return token_to_SNOT(*token);
      } else {
        return nullptr;
      }
    }

    // skipmatch
    //
    // Matches identifier token with a specified content.
    //
    // Parameters:
    // value - the required identifier value.
    //
    // Returns:
    // An identifier syntax token if the value of the token is the same as the
    // parameter. nullptr otherwise.
    //
    [[nodiscard]] SNOT* skipmatch(anton::String7_View const value)
    {
      _lexer.ignore_whitespace_and_comments();
      return match(value);
    }

    // match
    //
    [[nodiscard]] SNOT* match(SNOT_Kind const result_kind,
                              Token_Kind const tk_type1,
                              Token_Kind const tk_type2)
    {
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      auto const token1 = _lexer.peek_token();
      _lexer.advance_token();
      auto const token2 = _lexer.peek_token();
      _lexer.advance_token();
      if(!token1 || !token2) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      if(token1->kind != tk_type1 || token2->kind != tk_type2) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      Source_Info source{
        .source = _source,
        .line = token1->line,
        .column = token1->column,
        .offset = token1->offset,
        .end_offset = token2->end_offset,
      };
      return ALLOCATE_SNOT(result_kind, source);
    }

    // match
    //
    [[nodiscard]] SNOT* match(SNOT_Kind const result_kind,
                              Token_Kind const tk_type1,
                              Token_Kind const tk_type2,
                              Token_Kind const tk_type3)
    {
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      auto const token1 = _lexer.peek_token();
      _lexer.advance_token();
      auto const token2 = _lexer.peek_token();
      _lexer.advance_token();
      auto const token3 = _lexer.peek_token();
      _lexer.advance_token();
      if(!token1 || !token2 || !token3) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      if(token1->kind != tk_type1 || token2->kind != tk_type2 ||
         token2->kind != tk_type3) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      Source_Info source{
        .source = _source,
        .line = token1->line,
        .column = token1->column,
        .offset = token1->offset,
        .end_offset = token3->end_offset,
      };
      return ALLOCATE_SNOT(result_kind, source);
    }

    // skipmatch
    //
    [[nodiscard]] SNOT* skipmatch(SNOT_Kind const result_type,
                                  Token_Kind const tk_type1,
                                  Token_Kind const tk_type2)
    {
      _lexer.ignore_whitespace_and_comments();
      return match(result_type, tk_type1, tk_type2);
    }

    // skipmatch
    //
    [[nodiscard]] SNOT* skipmatch(SNOT_Kind const result_type,
                                  Token_Kind const tk_type1,
                                  Token_Kind const tk_type2,
                                  Token_Kind const tk_type3)
    {
      _lexer.ignore_whitespace_and_comments();
      return match(result_type, tk_type1, tk_type2, tk_type3);
    }

    // match_setting_string
    // Matches and combines a series of tokens in the stream that are allowed
    // within tk_setting_string.
    //
    // Returns:
    // tk_setting_string or null_optional.
    //
    [[nodiscard]] SNOT* match_setting_string()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      // We require at least one token to be present.
      if(!_lexer.peek_token()) {
        return nullptr;
      }

      while(true) {
        anton::Optional<Token> const token = _lexer.peek_token();
        if(!token) {
          break;
        }

        Token_Kind const token_kind = token->kind;
        if(token_kind == Token_Kind::whitespace ||
           token_kind == Token_Kind::comment ||
           token_kind == Token_Kind::tk_colon ||
           token_kind == Token_Kind::tk_rbrace ||
           token_kind == Token_Kind::tk_lbrace) {
          break;
        }

        _lexer.advance_token();
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::tk_setting_string, source);
    }

    SNOT* match_attribute_parameter_key()
    {
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      Optional<Token> lookahead1 = _lexer.peek_token();
      if(!lookahead1) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      _lexer.advance_token();
      _lexer.ignore_whitespace_and_comments();
      Optional<Token> lookahead2 = _lexer.peek_token();
      if(!lookahead2) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      if(lookahead1->kind == Token_Kind::identifier &&
         lookahead2->kind == Token_Kind::tk_equals) {
        snots.insert_back(token_to_SNOT(*lookahead1));
        snots.insert_back(token_to_SNOT(*lookahead2));
        return snots.unlink();
      } else {
        _lexer.restore_state(begin_state);
        return nullptr;
      }
    }

    SNOT* match_attribute_parameter()
    {
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      bool keyed = false;
      // Look ahead to check whether the parameter is keyed.
      //   identifier '='
      if(auto const key = match_attribute_parameter_key()) {
        snots.splice(key);
        keyed = true;
      }
      EXPECT_NODE(try_expression_without_init, snots);

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      if(keyed) {
        return ALLOCATE_SNOT(SNOT_Kind::attribute_parameter_keyed, source,
                             snots.unlink());
      } else {
        return ALLOCATE_SNOT(SNOT_Kind::attribute_parameter_positional, source,
                             snots.unlink());
      }
    }

    // TODO: Error inside try_attribute does not bubble upward.
    SNOT* try_attribute()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_at, "expected '@'"_sv, snots);
      // No whitespace between '@' and identifier are allowed.
      EXPECT_TOKEN(Token_Kind::identifier, "expected identifier"_sv, snots);

      Lexer_State const parameter_list_begin_state = _lexer.get_current_state();
      anton::IList<SNOT> parameter_list_snots;
      if(auto tk_lparen = skipmatch(Token_Kind::tk_lparen)) {
        parameter_list_snots.insert_back(tk_lparen);

        if(auto tk_rparen = skipmatch(Token_Kind::tk_rparen)) {
          parameter_list_snots.insert_back(tk_rparen);
        } else {
          while(true) {
            EXPECT_NODE(match_attribute_parameter, parameter_list_snots);
            if(auto const tk_comma = skipmatch(Token_Kind::tk_comma)) {
              parameter_list_snots.insert_back(tk_comma);
            } else {
              break;
            }
          }
        }

        EXPECT_TOKEN(Token_Kind::tk_rparen, "expected ')'",
                     parameter_list_snots);
        Lexer_State const parameter_list_end_state =
          _lexer.get_current_state_noskip();
        Source_Info const parameter_list_source =
          src_info(parameter_list_begin_state, parameter_list_end_state);
        snots.insert_back(ALLOCATE_SNOT(SNOT_Kind::attribute_parameter_list,
                                        parameter_list_source,
                                        parameter_list_snots.unlink()));
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::attribute, source, snots.unlink());
    }

    SNOT* try_attribute_list()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      while(auto attribute = try_attribute()) {
        snots.insert_back(attribute);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::attribute_list, source, snots.unlink());
    }

    SNOT* try_declaration()
    {
      ANNOTATE_FUNCTION()

      _lexer.ignore_whitespace_and_comments();
      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return nullptr;
      }

      // Match declarations that do not permit attribute lists first.
      switch(lookahead->kind) {
      case Token_Kind::kw_if:
        return try_decl_if();

      case Token_Kind::kw_import:
        return try_decl_import();

      case Token_Kind::kw_settings:
        return try_decl_settings();

      default:
        break;
      }

      // Match the attribute list. It will be moved into the matched construct.
      // Doing so saves us having to match it over and over again.
      auto attribute_list = try_attribute_list();

      _lexer.ignore_whitespace_and_comments();

      // Match declarations that do allow attribute lists.
      switch(lookahead->kind) {
      case Token_Kind::kw_struct:
        return try_decl_struct(attribute_list);

      case Token_Kind::kw_buffer:
        return try_decl_buffer(attribute_list);

      case Token_Kind::kw_fn: {
        // TODO: Merge these two functions.
        if(auto decl_stage_function = try_decl_stage_function(attribute_list)) {
          return decl_stage_function;
        }

        if(auto decl_function = try_decl_function(attribute_list)) {
          return decl_function;
        }
      } break;

      default:
        break;
      }

      // TODO: Constants
      // if(Optional decl_constant = try_variable(attribute_list)) {
      //   return decl_constant;
      // }

      set_error(u8"expected declaration");
      return nullptr;
    }

    // try_decl_block
    // Match declaration list enclosed in braces
    //   '{' declaration_list '}'
    //
    SNOT* try_decl_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);

      while(true) {
        if(auto tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.insert_back(tk_rbrace);
          break;
        }

        EXPECT_NODE(try_declaration, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_block, source, snots.unlink());
    }

    SNOT* try_decl_import()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN_SKIP(Token_Kind::kw_import, "expected 'import'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::lt_string, "expected string"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_import, source, snots.unlink());
    }

    SNOT* try_decl_if()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_if, "expected 'if'"_sv, snots);
      // condition
      EXPECT_NODE(try_expression_without_init, snots);
      // then branch
      EXPECT_NODE(try_decl_block, snots);

      if(auto kw_else = skipmatch(Token_Kind::kw_else)) {
        snots.insert_back(kw_else);
        if(auto decl_if = try_decl_if()) {
          snots.insert_back(decl_if);
        } else if(auto decl_block = try_decl_block()) {
          snots.insert_back(decl_block);
        } else {
          _lexer.restore_state(begin_state);
          return nullptr;
        }
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_if, source, snots.unlink());
    }

    SNOT* try_variable()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Source_Info const source = src_info(begin_state, begin_state);
      auto dummy_attribute_list =
        ALLOCATE_SNOT(SNOT_Kind::attribute_list, source, (SNOT*)nullptr);
      return try_variable(dummy_attribute_list);
    }

    SNOT* try_variable(SNOT* attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_var, "expected 'var'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);
      EXPECT_NODE(try_type, snots);

      if(auto tk_equals = skipmatch(Token_Kind::tk_equals)) {
        snots.insert_back(tk_equals);
        EXPECT_NODE(try_expression, snots);
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);

      snots.insert_front(attribute_list);

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::variable, source, snots.unlink());
    }

    SNOT* try_struct_field()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      // TODO: Attribute list does not propagate errors correctly.
      {
        auto attribute_list = try_attribute_list();
        snots.insert_back(attribute_list);
      }

      // identifier ':' type
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);
      EXPECT_NODE(try_type, snots);

      if(auto tk_equals = skipmatch(Token_Kind::tk_equals)) {
        snots.insert_back(tk_equals);
        EXPECT_NODE(try_expression, snots);
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::struct_field, source, snots.unlink());
    }

    SNOT* try_struct_field_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(auto tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.insert_back(tk_rbrace);
          break;
        }

        EXPECT_NODE(try_struct_field, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::struct_field_block, source,
                           snots.unlink());
    }

    SNOT* try_decl_struct(SNOT* attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_struct, "expected 'struct'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_struct_field_block, snots);

      snots.insert_front(attribute_list);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_struct, source, snots.unlink());
    }

    SNOT* try_buffer_field()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      // TODO: Attribute list does not propagate errors correctly.
      {
        auto attribute_list = try_attribute_list();
        snots.insert_back(attribute_list);
      }

      // identifier ':' type
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);
      EXPECT_NODE(try_type, snots);

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::buffer_field, source, snots.unlink());
    }

    SNOT* try_buffer_field_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(auto tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.insert_back(tk_rbrace);
          break;
        }

        EXPECT_NODE(try_buffer_field, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::buffer_field_block, source,
                           snots.unlink());
    }

    SNOT* try_decl_buffer(SNOT* attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_buffer, "expected 'buffer'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN2_SKIP(SNOT_Kind::tk_colon2, Token_Kind::tk_colon,
                         Token_Kind::tk_colon, "expected '::'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_buffer_field_block, snots);

      snots.insert_front(attribute_list);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_buffer, source, snots.unlink());
    }

    SNOT* try_setting()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_NODE(match_setting_string, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);

      // TODO: Flatten setting keys.

      if(auto block = try_setting_block()) {
        snots.insert_back(block);
      } else if(auto value = match_setting_string()) {
        snots.insert_back(value);
      } else {
        set_error("expected value string after ':'"_sv);
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::setting_keyval, source, snots.unlink());
    }

    SNOT* try_setting_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(auto tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.insert_back(tk_rbrace);
          break;
        }

        EXPECT_NODE(try_setting, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::setting_block, source, snots.unlink());
    }

    SNOT* try_decl_settings()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_settings, "expected 'settings'"_sv, snots);
      // pass
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_setting_block, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_settings, source, snots.unlink());
    }

    SNOT* try_fn_parameter()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;

      {
        auto attribute_list = try_attribute_list();
        snots.insert_back(attribute_list);
      }

      // identifier ':' type
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);
      EXPECT_NODE(try_type, snots);

      if(auto kw_from = skipmatch(Token_Kind::kw_from)) {
        snots.insert_back(kw_from);
        // source
        EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                          snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::fn_parameter, source, snots.unlink());
    }

    SNOT* try_fn_parameter_list()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lparen, "expected '('"_sv, snots);
      // Match rparen to allow empty parameter lists.
      if(auto tk_rparen = skipmatch(Token_Kind::tk_rparen)) {
        snots.insert_back(tk_rparen);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::fn_parameter_list, source,
                             snots.unlink());
      }

      while(true) {
        EXPECT_NODE(try_fn_parameter, snots);
        if(auto tk_comma = skipmatch(Token_Kind::tk_comma)) {
          snots.insert_back(tk_comma);
        } else {
          break;
        }
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::fn_parameter_list, source,
                           snots.unlink());
    }

    SNOT* try_decl_stage_function(SNOT* attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;

      EXPECT_TOKEN(Token_Kind::kw_fn, "expected 'fn'"_sv, snots);

      // pass
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN2_SKIP(SNOT_Kind::tk_colon2, Token_Kind::tk_colon,
                         Token_Kind::tk_colon, "expected '::'"_sv, snots);

      // shader stage
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);

      EXPECT_NODE(try_fn_parameter_list, snots);

      EXPECT_NODE(try_stmt_block, snots);

      snots.insert_front(attribute_list);

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_stage_function, source,
                           snots.unlink());
    }

    SNOT* try_decl_function(SNOT* attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_fn, "expected 'fn'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_fn_parameter_list, snots);

      EXPECT_TOKEN2_SKIP(SNOT_Kind::tk_thin_arrow, Token_Kind::tk_minus,
                         Token_Kind::tk_rangle, "expected '->'"_sv, snots);

      EXPECT_NODE(try_type, snots);

      EXPECT_NODE(try_stmt_block, snots);

      snots.insert_front(attribute_list);

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::decl_function, source, snots.unlink());
    }

    SNOT* try_statement()
    {
      ANNOTATE_FUNCTION()

      _lexer.ignore_whitespace_and_comments();
      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return nullptr;
      }

      switch(lookahead->kind) {
      case Token_Kind::tk_semicolon: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        anton::IList<SNOT> snots;
        EXPECT_TOKEN(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::stmt_empty, source, snots.unlink());
      }

      case Token_Kind::kw_var:
        return try_variable();

      case Token_Kind::tk_lbrace:
        return try_stmt_block();

      case Token_Kind::kw_if:
        return try_stmt_if();

      case Token_Kind::kw_switch:
        return try_stmt_switch();

      case Token_Kind::kw_for:
        return try_stmt_for();

      case Token_Kind::kw_while:
        return try_stmt_while();

      case Token_Kind::kw_do:
        return try_stmt_do_while();

      case Token_Kind::kw_return:
        return try_stmt_return();

      case Token_Kind::kw_break: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        anton::IList<SNOT> snots;
        EXPECT_TOKEN(Token_Kind::kw_break, "expected 'break'"_sv, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::stmt_break, source, snots.unlink());
      }

      case Token_Kind::kw_continue: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        anton::IList<SNOT> snots;
        EXPECT_TOKEN(Token_Kind::kw_continue, "expected 'continue'"_sv, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::stmt_continue, source, snots.unlink());
      }

      case Token_Kind::kw_discard: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        anton::IList<SNOT> snots;
        EXPECT_TOKEN(Token_Kind::kw_discard, "expected 'discard'"_sv, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::stmt_discard, source, snots.unlink());
      }

      default: {
        // TODO: These may be merged because they have common prefix.
        if(auto stmt_assignment = try_stmt_assignment()) {
          return stmt_assignment;
        }

        if(auto stmt_expression = try_stmt_expression()) {
          return stmt_expression;
        }
      }
      }

      set_error(u8"expected a statement");
      return nullptr;
    }

    // try_stmt_block
    // Match a list of statements enclosed in braces
    //   '{' statements '}'
    SNOT* try_stmt_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(auto tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.insert_back(tk_rbrace);
          break;
        }
        EXPECT_NODE(try_statement, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_block, source, snots.unlink());
    }

    SNOT* try_stmt_if()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_if, "expected 'if'"_sv, snots);
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(try_stmt_block, snots);

      if(auto kw_else = skipmatch(Token_Kind::kw_else)) {
        snots.insert_back(kw_else);
        if(auto stmt_if = try_stmt_if()) {
          snots.insert_back(stmt_if);
        } else {
          EXPECT_NODE(try_stmt_block, snots);
        }
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_if, source, snots.unlink());
    }

    SNOT* try_stmt_switch()
    {
      ANNOTATE_FUNCTION()
      auto match_switch_arm_list = [this]() -> SNOT* {
        auto match_switch_arm = [this]() -> SNOT* {
          Lexer_State const begin_state = _lexer.get_current_state();
          anton::IList<SNOT> snots;
          while(true) {
            if(auto kw_default = skipmatch(Token_Kind::kw_default)) {
              Source_Info const source_info = kw_default->source_info;
              auto expr_default =
                ALLOCATE_SNOT(SNOT_Kind::expr_default, source_info, kw_default);
              auto label = ALLOCATE_SNOT(SNOT_Kind::switch_arm_label,
                                         source_info, expr_default);
              snots.insert_back(label);
            } else if(auto literal = try_expr_lt_integer()) {
              Source_Info const source_info = literal->source_info;
              auto label = ALLOCATE_SNOT(SNOT_Kind::switch_arm_label,
                                         source_info, literal);
              snots.insert_back(label);
            } else {
              _lexer.restore_state(begin_state);
              return nullptr;
            }

            if(auto tk_comma = skipmatch(Token_Kind::tk_comma)) {
              snots.insert_back(tk_comma);
            } else {
              break;
            }
          }
          EXPECT_TOKEN2_SKIP(SNOT_Kind::tk_thick_arrow, Token_Kind::tk_equals,
                             Token_Kind::tk_rangle, "expected '=>'"_sv, snots);
          EXPECT_NODE(try_stmt_block, snots);
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          return ALLOCATE_SNOT(SNOT_Kind::switch_arm, source, snots.unlink());
        };

        Lexer_State const begin_state = _lexer.get_current_state();
        anton::IList<SNOT> snots;
        EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
        while(true) {
          if(auto tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
            snots.insert_back(tk_rbrace);
            break;
          }
          EXPECT_NODE(match_switch_arm, snots);
        }

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::switch_arm_list, source,
                             snots.unlink());
      };

      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_switch, "expected 'switch'"_sv, snots);
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(match_switch_arm_list, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_switch, source, snots.unlink());
    }

    SNOT* try_stmt_for()
    {
      ANNOTATE_FUNCTION()
      auto match_for_stmt_variable = [this]() -> SNOT* {
        Lexer_State const begin_state = _lexer.get_current_state();
        anton::IList<SNOT> snots;
        // TODO: consider changing to identifier ':' type
        EXPECT_NODE(try_type, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected 'identifier'"_sv,
                          snots);
        if(auto tk_equals = skipmatch(Token_Kind::tk_equals)) {
          snots.insert_back(tk_equals);
          EXPECT_NODE(try_expression, snots);
        }

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(
          SNOT_Kind::for_variable, source,
          ALLOCATE_SNOT(SNOT_Kind::variable, source, snots.unlink()));
      };

      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_for, "expected 'for'"_sv, snots);
      if(auto for_stmt_variable = match_for_stmt_variable()) {
        snots.insert_back(for_stmt_variable);
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);

      if(auto condition = try_expression()) {
        Source_Info const source_info = condition->source_info;
        snots.insert_back(
          ALLOCATE_SNOT(SNOT_Kind::for_condition, source_info, condition));
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      // TODO FIX: does not match assignment.
      if(auto expression = try_expression_without_init()) {
        Source_Info const source_info = expression->source_info;
        snots.insert_back(
          ALLOCATE_SNOT(SNOT_Kind::for_expression, source_info, expression));
      }

      EXPECT_NODE(try_stmt_block, snots);

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_for, source, snots.unlink());
    }

    SNOT* try_stmt_while()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_while, "expected 'while'"_sv, snots);
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(try_stmt_block, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_while, source, snots.unlink());
    }

    SNOT* try_stmt_do_while()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_do, "expected 'do'"_sv, snots);
      EXPECT_NODE(try_stmt_block, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::kw_while, "expected 'while'"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_do_while, source, snots.unlink());
    }

    SNOT* try_stmt_return()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_return, "expected 'return'"_sv, snots);
      if(auto return_expression = try_expression()) {
        Source_Info const source_info = return_expression->source_info;
        snots.insert_back(ALLOCATE_SNOT(SNOT_Kind::return_expression,
                                        source_info, return_expression));
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_return, source, snots.unlink());
    }

    SNOT* try_stmt_assignment()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_NODE(try_expression, snots);
      // Match any of the possible assignment tokens.
      if(auto op = match(Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_ampeq, Token_Kind::tk_amp,
                                Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_pipeeq, Token_Kind::tk_pipe,
                                Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_hateq, Token_Kind::tk_hat,
                                Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_shleq, Token_Kind::tk_langle,
                                Token_Kind::tk_langle, Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_shreq, Token_Kind::tk_rangle,
                                Token_Kind::tk_rangle, Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_percenteq, Token_Kind::tk_percent,
                                Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_slasheq, Token_Kind::tk_slash,
                                Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op =
                  match(SNOT_Kind::tk_asteriskeq, Token_Kind::tk_asterisk,
                        Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_pluseq, Token_Kind::tk_plus,
                                Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else if(auto op = match(SNOT_Kind::tk_minuseq, Token_Kind::tk_minus,
                                Token_Kind::tk_equals)) {
        snots.insert_back(op);
      } else {
        set_error("expected assignment"_sv);
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_assignment, source, snots.unlink());
    }

    SNOT* try_stmt_expression()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::stmt_expression, source, snots.unlink());
    }

    SNOT* try_expression()
    {
      ANNOTATE_FUNCTION()
      return try_expr_binary(false);
    }

    SNOT* try_expression_without_init()
    {
      ANNOTATE_FUNCTION()
      return try_expr_binary(true);
    }

    // try_expr_binary
    //
    // Parameters:
    // disable_init - whether we are matching the production for an expression
    //                without init or a production expression with init (false).
    //
    SNOT* try_expr_binary(bool const disable_init)
    {
      ANNOTATE_FUNCTION()
      // Parsing binary expressions consists primarily of repeatedly calling two
      // procedures to construct the expression trees - insert_operator and
      // insert_expression.
      //
      // insert_expression - inserts an expression into a "free slot" (if a
      //   binary expression does not have either right or left expression, we
      //   consider it a "free slot").
      //
      // insert_operator - does the actual work of building the binary
      //   expression tree. Depending on the associativity of an operator and
      //   its precedence, the following might happen:
      //   - an operator is left associative: if the operator has a **higher or
      //     equal** precedence than a node we are visiting, then we make the
      //     operator a new parent of the node. Otherwise we descend to the
      //     right subtree.
      //   - an operator is right associative: if the operator has a **higher**
      //     precedence than a node we are visiting, then we make the operator a
      //     new parent of the node. Otherwise we descend to the right subtree.
      //
      //   The difference between left and right associative operators is
      //   subtle, yet it results in vastly different expression trees being
      //   constructed.
      //
      // Precedence is ranked from 1 (highest).

      auto insert_operator = [_allocator = this->_allocator](SNOT* root,
                                                             SNOT* op) {
        enum Associativity { ASSOC_LEFT, ASSOC_RIGHT };

        auto get_associativity = [](SNOT_Kind type) -> Associativity {
          switch(type) {
          default:
            return ASSOC_LEFT;
          }
        };

        auto get_precedence = [](SNOT_Kind type) -> i32 {
          // Precedence values are exactly the same as in the GLSL Specification.
          switch(type) {
          case SNOT_Kind::tk_pipe2:
            return 14;
          case SNOT_Kind::tk_hat2:
            return 13;
          case SNOT_Kind::tk_amp2:
            return 12;
          case SNOT_Kind::tk_pipe:
            return 11;
          case SNOT_Kind::tk_hat:
            return 10;
          case SNOT_Kind::tk_amp:
            return 9;

          case SNOT_Kind::tk_eq2:
          case SNOT_Kind::tk_neq:
            return 8;

          case SNOT_Kind::tk_langle:
          case SNOT_Kind::tk_rangle:
          case SNOT_Kind::tk_gteq:
          case SNOT_Kind::tk_lteq:
            return 7;

          case SNOT_Kind::tk_shl:
          case SNOT_Kind::tk_shr:
            return 6;

          case SNOT_Kind::tk_plus:
          case SNOT_Kind::tk_minus:
            return 5;

          case SNOT_Kind::tk_asterisk:
          case SNOT_Kind::tk_slash:
          case SNOT_Kind::tk_percent:
            return 4;

          default:
            ANTON_UNREACHABLE("invalid operator type");
          }
        };

        i32 const op_prec = get_precedence(op->kind);
        i32 const op_assoc = get_associativity(op->kind);
        SNOT* dest_node = root;
        while(true) {
          // We will replace the node if it is not a binary expression.
          if(dest_node->kind != SNOT_Kind::expr_binary) {
            break;
          }

          ANTON_ASSERT(anton::ilist_size(dest_node->children) == 3,
                       "expr_binary does not have exactly 3 children");
          SNOT* const dest_op = anton::ilist_next(dest_node->children);
          ANTON_ASSERT(dest_op->is_token(),
                       "second child of expr_binary is not a syntax token");
          i32 const dest_prec = get_precedence(dest_op->kind);
          bool const descend = (op_assoc == ASSOC_LEFT) ? op_prec <= dest_prec
                                                        : op_prec < dest_prec;
          if(!descend) {
            break;
          }

          dest_node = anton::ilist_next(dest_op);
        }

        // Replace dest_node with a new expr_binary node. Since dest_node is
        // linked into a list, we replace its contents.
        auto const child = ALLOCATE_SNOT(
          dest_node->kind, dest_node->source_info, dest_node->children);
        anton::IList<SNOT> snots;
        snots.insert_back(child);
        snots.insert_back(op);
        dest_node->kind = SNOT_Kind::expr_binary;
        dest_node->source_info = Source_Info{};
        dest_node->children = snots.unlink();
      };

      auto insert_expression = [](SNOT* root, SNOT* expr) -> void {
        // We only ever have to descend down the right children because we are
        // inserting leaf nodes.
        while(anton::ilist_size(root->children) == 3) {
          ANTON_ASSERT(root->kind == SNOT_Kind::expr_binary,
                       "syntax node is not expr_binary");
          auto const right_child =
            anton::ilist_next(anton::ilist_next(root->children));
          ANTON_ASSERT(right_child->is_node(),
                       "third child of expr_binary is not a node");
          root = right_child;
        }

        if(root->children != nullptr) {
          auto const end = anton::ilist_end(root->children);
          anton::ilist_insert_after(end, expr);
        } else {
          root->children = expr;
        }
      };

      auto match_binary_operator = [this]() -> SNOT* {
        Optional<Token> lookahead = _lexer.peek_token();
        if(!lookahead) {
          return nullptr;
        }

        switch(lookahead->kind) {
        case Token_Kind::tk_amp:
          if(auto op = match(SNOT_Kind::tk_amp2, Token_Kind::tk_amp,
                             Token_Kind::tk_amp)) {
            return op;
          } else {
            _lexer.advance_token();
            return token_to_SNOT(*lookahead);
          }

        case Token_Kind::tk_pipe:
          if(auto op = match(SNOT_Kind::tk_pipe2, Token_Kind::tk_pipe,
                             Token_Kind::tk_pipe)) {
            return op;
          } else {
            _lexer.advance_token();
            return token_to_SNOT(*lookahead);
          }

        case Token_Kind::tk_hat:
          if(auto op = match(SNOT_Kind::tk_hat2, Token_Kind::tk_hat,
                             Token_Kind::tk_hat)) {
            return op;
          } else {
            _lexer.advance_token();
            return token_to_SNOT(*lookahead);
          }

        case Token_Kind::tk_equals:
          return match(SNOT_Kind::tk_eq2, Token_Kind::tk_equals,
                       Token_Kind::tk_equals);

        case Token_Kind::tk_bang:
          return match(SNOT_Kind::tk_neq, Token_Kind::tk_bang,
                       Token_Kind::tk_equals);

        case Token_Kind::tk_langle:
          if(auto op = match(SNOT_Kind::tk_lteq, Token_Kind::tk_langle,
                             Token_Kind::tk_equals)) {
            return op;
          } else if(auto op = match(SNOT_Kind::tk_shl, Token_Kind::tk_langle,
                                    Token_Kind::tk_langle)) {
            return op;
          } else {
            _lexer.advance_token();
            return token_to_SNOT(*lookahead);
          }

        case Token_Kind::tk_rangle:
          if(auto op = match(SNOT_Kind::tk_gteq, Token_Kind::tk_rangle,
                             Token_Kind::tk_equals)) {
            return op;
          } else if(auto op = match(SNOT_Kind::tk_shr, Token_Kind::tk_rangle,
                                    Token_Kind::tk_rangle)) {
            return op;
          } else {
            _lexer.advance_token();
            return token_to_SNOT(*lookahead);
          }

        case Token_Kind::tk_plus:
        case Token_Kind::tk_minus:
        case Token_Kind::tk_asterisk:
        case Token_Kind::tk_slash:
        case Token_Kind::tk_percent:
          _lexer.advance_token();
          return token_to_SNOT(*lookahead);

        default:
          return nullptr;
        }
      };

      Lexer_State const begin_state = _lexer.get_current_state();
      auto root_expr = try_expr_prefix(disable_init);
      if(!root_expr) {
        return nullptr;
      }

      while(true) {
        _lexer.ignore_whitespace_and_comments();
        if(auto op = match_binary_operator()) {
          insert_operator(root_expr, op);
        } else {
          break;
        }

        if(auto expr = try_expr_prefix(disable_init)) {
          insert_expression(root_expr, expr);
        } else {
          _lexer.restore_state(begin_state);
          return nullptr;
        }
      }

      auto recalculate_source_info = [](auto& recalculate_source_info,
                                        SNOT* node) -> void {
        if(node->kind != SNOT_Kind::expr_binary) {
          return;
        }

        ANTON_ASSERT(anton::ilist_size(node->children) == 3,
                     "expr_binary does not have exactly 3 children");
        auto const left_child = node->children;
        ANTON_ASSERT(left_child->is_node(),
                     "the left child of expr_binary is not a syntax node");
        auto const right_child =
          anton::ilist_next(anton::ilist_next(left_child));
        ANTON_ASSERT(right_child->is_node(),
                     "the right child of expr_binary is not a syntax node");
        recalculate_source_info(recalculate_source_info, left_child);
        recalculate_source_info(recalculate_source_info, right_child);
        Source_Info const& left = left_child->source_info;
        Source_Info const& right = right_child->source_info;
        // Both left and right have the same souce_path;
        node->source_info.source = left.source;
        node->source_info.line = left.line;
        node->source_info.column = left.column;
        node->source_info.offset = left.offset;
        node->source_info.end_offset = right.end_offset;
      };

      // expr_binary nodes do not have their source_info members filled properly because
      // it is impossible to do so while inserting (we do not know the final right child).
      // We have to manually fix up the source_info members.
      recalculate_source_info(recalculate_source_info, root_expr);

      return root_expr;
    }

    SNOT* try_expr_prefix(bool const disable_init)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();

      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return nullptr;
      }

      if(lookahead->kind != Token_Kind::tk_plus &&
         lookahead->kind != Token_Kind::tk_minus &&
         lookahead->kind != Token_Kind::tk_bang &&
         lookahead->kind != Token_Kind::tk_tilde) {
        return try_expr_postfix(disable_init);
      }

      _lexer.advance_token();

      auto expr = try_expr_prefix(disable_init);
      if(expr == nullptr) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      anton::IList<SNOT> snots;
      snots.insert_back(token_to_SNOT(*lookahead));
      snots.insert_back(expr);

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::expr_prefix, source, snots.unlink());
    }

    SNOT* try_expr_postfix(bool const disable_init)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      SNOT* expr = try_primary_expression(disable_init);
      if(!expr) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      while(true) {
        if(auto tk_dot = skipmatch(Token_Kind::tk_dot)) {
          auto identifier = skipmatch(Token_Kind::identifier);
          if(!identifier) {
            set_error(u8"expected identifier");
            _lexer.restore_state(begin_state);
            return nullptr;
          }

          anton::IList<SNOT> snots;
          snots.insert_back(expr);
          snots.insert_back(tk_dot);
          snots.insert_back(identifier);
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          expr = ALLOCATE_SNOT(SNOT_Kind::expr_field, source, snots.unlink());
        } else if(auto tk_lbracket = skipmatch(Token_Kind::tk_lbracket)) {
          auto index = try_expression();
          if(!index) {
            _lexer.restore_state(begin_state);
            return nullptr;
          }

          auto tk_rbracket = skipmatch(Token_Kind::tk_rbracket);
          if(!tk_rbracket) {
            set_error(u8"expected ']'");
            _lexer.restore_state(begin_state);
            return nullptr;
          }

          anton::IList<SNOT> snots;
          snots.insert_back(expr);
          snots.insert_back(tk_lbracket);
          snots.insert_back(index);
          snots.insert_back(tk_rbracket);
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          expr = ALLOCATE_SNOT(SNOT_Kind::expr_index, source, snots.unlink());
        } else {
          break;
        }
      }

      return expr;
    }

    SNOT* try_primary_expression(bool const disable_init)
    {
      ANNOTATE_FUNCTION()

      Lexer_State const begin_state = _lexer.get_current_state();
      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return nullptr;
      }

      switch(lookahead->kind) {
      case Token_Kind::tk_lparen: {
        anton::IList<SNOT> snots;
        EXPECT_TOKEN(Token_Kind::tk_lparen, "expected '('"_sv, snots);
        EXPECT_NODE(try_expression, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::expr_parentheses, source,
                             snots.unlink());
      }

        // TODO: reinterpret
        // case Token_Kind::kw_reinterpret:
        //   return try_expr_reinterpret();

      case Token_Kind::kw_if:
        return try_expr_if();

      case Token_Kind::lt_float: {
        _lexer.advance_token();
        anton::IList<SNOT> snots;
        snots.insert_back(token_to_SNOT(*lookahead));
        // We validate the suffix in later stages.
        if(auto suffix = match(Token_Kind::identifier)) {
          snots.insert_back(suffix);
        }

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::expr_lt_float, source, snots.unlink());
      }

      case Token_Kind::lt_bin_integer:
      case Token_Kind::lt_dec_integer:
      case Token_Kind::lt_hex_integer: {
        _lexer.advance_token();
        anton::IList<SNOT> snots;
        snots.insert_back(token_to_SNOT(*lookahead));

        // We validate suffix on the literals in later stages.
        if(auto suffix = match(Token_Kind::identifier)) {
          snots.insert_back(suffix);
        }

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::expr_lt_integer, source,
                             snots.unlink());
      }

      case Token_Kind::lt_bool: {
        anton::IList<SNOT> snots;
        EXPECT_TOKEN(Token_Kind::lt_bool, "expected bool literal"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::expr_lt_bool, source, snots.unlink());
      }

      case Token_Kind::identifier: {
        _lexer.advance_token();
        _lexer.ignore_whitespace_and_comments();
        Optional<Token> lookahead2 = _lexer.peek_token();
        // We want to unify the control flow a bit, hence we choose a token
        // that cannot occur. If any other token does not match, it's an
        // identifier expression.
        Token_Kind const kind =
          lookahead2 ? lookahead2->kind : Token_Kind::comment;
        switch(kind) {
        case Token_Kind::tk_lparen: {
          anton::IList<SNOT> snots;
          snots.insert_back(token_to_SNOT(*lookahead));
          EXPECT_NODE(try_call_arg_list, snots);
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          return ALLOCATE_SNOT(SNOT_Kind::expr_call, source, snots.unlink());
        }

        case Token_Kind::tk_lbrace: {
          if(!disable_init) {
            // To match expr init here, we must undo all token advances.
            _lexer.restore_state(begin_state);
            if(auto expr_init = try_expr_init()) {
              return expr_init;
            }
          }
        } break;

        default: {
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          return ALLOCATE_SNOT(SNOT_Kind::expr_identifier, source,
                               token_to_SNOT(*lookahead));
        }
        }
      } break;

      default: {
        if(!disable_init) {
          if(auto expr_init = try_expr_init()) {
            return expr_init;
          }
        }
      }
      }

      _lexer.restore_state(begin_state);
      return nullptr;
    }

    // SNOT* try_expr_reinterpret()
    // {
    //   ANNOTATE_FUNCTION()
    //   Lexer_State const begin_state = _lexer.get_current_state();
    //   anton::IList<SNOT> snots;
    //   EXPECT_TOKEN(Token_Kind::kw_reinterpret, "expected 'reinterpret'"_sv,
    //                snots);
    //   EXPECT_TOKEN_SKIP(Token_Kind::tk_langle, "expected '<'"_sv, snots);
    //   EXPECT_NODE(try_type, snots);
    //   EXPECT_TOKEN_SKIP(Token_Kind::tk_rangle, "expected '>'"_sv, snots);
    //   EXPECT_TOKEN_SKIP(Token_Kind::tk_lparen, "expected '('"_sv, snots);
    //   EXPECT_NODE(try_expression, snots);
    //   EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
    //   Lexer_State const end_state = _lexer.get_current_state_noskip();
    //   Source_Info const source = src_info(begin_state, end_state);
    //   return ALLOCATE_SNOT( _allocator, SNOT_Kind::expr_reinterpret,
    //                        source, snots.unlink());
    // }

    SNOT* try_expr_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_rbrace, "expected '}'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::expr_block, source, snots.unlink());
    }

    SNOT* try_expr_if()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::kw_if, "expected 'if'"_sv, snots);
      // condition
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(try_expr_block, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::kw_else, "expected 'else'"_sv, snots);
      if(auto expr_if = try_expr_if()) {
        snots.insert_back(expr_if);
      } else if(auto else_branch = try_expr_block()) {
        snots.insert_back(else_branch);
      } else {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::expr_if, source, snots.unlink());
    }

    SNOT* try_field_initializer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_dot, "expected '.'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_equals, "expected '='"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::field_initializer, source,
                           snots.unlink());
    }

    SNOT* try_index_initializer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lbracket, "expected '['"_sv, snots);
      EXPECT_NODE(try_expr_lt_integer, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_rbracket, "expected ']'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_equals, "expected '='"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::index_initializer, source,
                           snots.unlink());
    }

    SNOT* try_basic_initializer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_NODE(try_expression, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::basic_initializer, source,
                           snots.unlink());
    }

    SNOT* try_expr_init()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_NODE(try_type, snots);

      {
        anton::IList<SNOT> list_snots;
        EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, list_snots);

        // Match closing paren to allow empty parameter lists.
        if(auto tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          list_snots.insert_back(tk_rbrace);
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          return ALLOCATE_SNOT(SNOT_Kind::init_initializer_list, source,
                               list_snots.unlink());
        }

        while(true) {
          _lexer.ignore_whitespace_and_comments();
          Optional<Token> lookahead = _lexer.peek_token();
          if(!lookahead) {
            return nullptr;
          }

          if(lookahead->kind == Token_Kind::tk_dot) {
            EXPECT_NODE(try_field_initializer, list_snots);
          } else {
            if(auto index_initializer = try_index_initializer()) {
              list_snots.insert_back(index_initializer);
            } else if(auto basic_initializer = try_basic_initializer()) {
              list_snots.insert_back(basic_initializer);
            } else {
              set_error("expected initializer"_sv);
              _lexer.restore_state(begin_state);
              return nullptr;
            }
          }

          if(auto tk_comma = skipmatch(Token_Kind::tk_comma)) {
            list_snots.insert_back(tk_comma);
          } else {
            break;
          }
        }

        EXPECT_TOKEN_SKIP(Token_Kind::tk_rbrace, "expected '}'"_sv, list_snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        auto const list = ALLOCATE_SNOT(SNOT_Kind::init_initializer_list,
                                        source, list_snots.unlink());
        snots.insert_back(list);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::expr_init, source, snots.unlink());
    }

    SNOT* try_call_arg_list()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      EXPECT_TOKEN(Token_Kind::tk_lparen, "expected '('"_sv, snots);

      // Match closing paren to allow empty parameter lists.
      if(auto tk_rparen = skipmatch(Token_Kind::tk_rparen)) {
        snots.insert_back(tk_rparen);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::call_arg_list, source, snots.unlink());
      }

      while(true) {
        EXPECT_NODE(try_expression, snots);

        if(auto tk_comma = skipmatch(Token_Kind::tk_comma)) {
          snots.insert_back(tk_comma);
        } else {
          break;
        }
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::call_arg_list, source, snots.unlink());
    }

    SNOT* try_expr_lt_integer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;

      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return nullptr;
      }

      if(lookahead->kind != Token_Kind::lt_bin_integer &&
         lookahead->kind != Token_Kind::lt_dec_integer &&
         lookahead->kind != Token_Kind::lt_hex_integer) {
        set_error("expected integer literal"_sv);
        return nullptr;
      }

      _lexer.advance_token();
      snots.insert_back(token_to_SNOT(*lookahead));

      // We validate suffix on the literals in later stages.
      if(auto suffix = match(Token_Kind::identifier)) {
        snots.insert_back(suffix);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return ALLOCATE_SNOT(SNOT_Kind::expr_lt_integer, source, snots.unlink());
    }

    SNOT* try_type()
    {
      ANNOTATE_FUNCTION()

      Lexer_State const begin_state = _lexer.get_current_state();
      anton::IList<SNOT> snots;
      if(auto kw_mut = match(Token_Kind::kw_mut)) {
        snots.insert_back(kw_mut);
      }

      _lexer.ignore_whitespace_and_comments();
      Optional<Token> lookahead = _lexer.next_token();
      if(!lookahead) {
        _lexer.restore_state(begin_state);
        return nullptr;
      }

      switch(lookahead->kind) {
      case Token_Kind::tk_lbrace: {
        snots.insert_back(token_to_SNOT(*lookahead));

        if(auto type = try_type()) {
          Source_Info const source_info = type->source_info;
          snots.insert_back(
            ALLOCATE_SNOT(SNOT_Kind::type_array_base, source_info, type));
        } else {
          _lexer.restore_state(begin_state);
          return nullptr;
        }

        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);

        if(auto size = try_expr_lt_integer()) {
          Source_Info const source_info = size->source_info;
          snots.insert_back(
            ALLOCATE_SNOT(SNOT_Kind::type_array_size, source_info, size));
        }

        EXPECT_TOKEN_SKIP(Token_Kind::tk_rbracket, "expected ']'"_sv, snots);

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::type_array, source, snots.unlink());
      }

      case Token_Kind::identifier: {
        auto const identifier = token_to_SNOT(*lookahead);
        snots.insert_back(identifier);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return ALLOCATE_SNOT(SNOT_Kind::type_named, source, snots.unlink());
      }

      default: {
        _lexer.restore_state(begin_state);
        return nullptr;
      }
      }
    }
  };

  struct Insert_Location {
    Array<SNOT>* array;
    Array<SNOT>::iterator location;
  };

  // find_insert_location
  //
  // [[nodiscard]] static Insert_Location
  // find_insert_location(Array<SNOT>& snots, Syntax_Token const& token)
  // {
  //   auto get_snot_source_info = [](SNOT const& snot) {
  //     if(snot.is_left()) {
  //       return snot.left().source_info;
  //     } else {
  //       return snot.right().source_info;
  //     }
  //   };
  //
  //   Array<SNOT>::iterator i = snots.begin();
  //   for(Array<SNOT>::iterator const e = snots.end(); i != e; ++i) {
  //     Source_Info const snot_src = get_snot_source_info(*i);
  //     // Check whether token is before i.
  //     if(token.source_info.offset < snot_src.offset) {
  //       break;
  //     }
  //     // Check whether token is contained within a node.
  //     // Note that this may only happen when snot is a node
  //     // since tokens by definition are strings of contiguous symbols.
  //     if(token.source_info.offset >= snot_src.offset &&
  //        token.source_info.end_offset < snot_src.end_offset) {
  //       Syntax_Node& node = i->left();
  //       return find_insert_location(node.children, token);
  //     }
  //   }
  //
  //   return {.array = &snots, .location = i};
  // }

  struct Insert_Comments_And_Whitespace_Parameters {
    Allocator* allocator;
    anton::String_View source_path;
    Array<SNOT>& tl_node;
    Token const* begin;
    Token const* end;
  };

  // insert_comments_and_whitespace
  //
  // static void insert_comments_and_whitespace(
  //   Insert_Comments_And_Whitespace_Parameters const& p)
  // {
  //   for(auto token: anton::Range(p.begin, p.end)) {
  //     if(token.kind == Token_Kind::comment ||
  //        token.kind == Token_Kind::whitespace) {
  //       Source_Info source_info{
  //         .source_path = p.source_path,
  //         .line = token.line,
  //         .column = token.column,
  //         .offset = token.offset,
  //         .end_offset = token.end_offset,
  //       };
  //       // TODO: Fix syntax token source.
  //       Syntax_Token syntax_token(static_cast<SNOT_Kind>(token.kind),
  //                                 anton::String(nullptr, nullptr, p.allocator),
  //                                 source_info);
  //       Insert_Location const insert =
  //         find_insert_location(p.tl_node, syntax_token);
  //       insert.array->insert(insert.location, ANTON_MOV(syntax_token));
  //     }
  //   }
  // }

  anton::Expected<SNOT*, Error> parse_tokens(Context const& ctx,
                                             Source_Data const* source,
                                             anton::Slice<Token const> tokens,
                                             Parse_Syntax_Options const options)
  {
    Parser parser(ctx.allocator, source, Lexer(tokens.cbegin(), tokens.cend()));
    anton::Expected<SNOT*, Error> ast = parser.build_syntax_tree();
    // if(ast && options.include_whitespace_and_comments) {
    //   Insert_Comments_And_Whitespace_Parameters p{
    //     .allocator = ctx.allocator,
    //     .source_path = source_path,
    //     .tl_node = ast.value(),
    //     .begin = tokens.cbegin(),
    //     .end = tokens.cend(),
    //   };
    //   insert_comments_and_whitespace(p);
    // }
    return ast;
  }
} // namespace vush
