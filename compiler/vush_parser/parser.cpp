#include <vush_parser/parser.hpp>

#include <anton/assert.hpp>
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

#define EXPECT_TOKEN(token, message, snots)     \
  if(Optional _expect_token = match(token)) {   \
    snots.push_back(ANTON_MOV(*_expect_token)); \
  } else {                                      \
    set_error(message);                         \
    _lexer.restore_state(begin_state);          \
    return anton::null_optional;                \
  }

#define EXPECT_TOKEN_SKIP(token, message, snots)  \
  if(Optional _expect_token = skipmatch(token)) { \
    snots.push_back(ANTON_MOV(*_expect_token));   \
  } else {                                        \
    set_error(message);                           \
    _lexer.restore_state(begin_state);            \
    return anton::null_optional;                  \
  }

#define EXPECT_TOKEN2(result, token1, token2, message, snots)  \
  if(Optional _expect_token = match(result, token1, token2)) { \
    snots.push_back(ANTON_MOV(*_expect_token));                \
  } else {                                                     \
    set_error(message);                                        \
    _lexer.restore_state(begin_state);                         \
    return anton::null_optional;                               \
  }

#define EXPECT_TOKEN2_SKIP(result, token1, token2, message, snots) \
  if(Optional _expect_token = skipmatch(result, token1, token2)) { \
    snots.push_back(ANTON_MOV(*_expect_token));                    \
  } else {                                                         \
    set_error(message);                                            \
    _lexer.restore_state(begin_state);                             \
    return anton::null_optional;                                   \
  }

#define EXPECT_NODE(fn, snots)                 \
  if(Optional _expect_node = fn()) {           \
    snots.push_back(ANTON_MOV(*_expect_node)); \
  } else {                                     \
    _lexer.restore_state(begin_state);         \
    return anton::null_optional;               \
  }

#define WRAP_NODE(allocator, kind, node, source_info)                         \
  Syntax_Node(                                                                \
    kind, Array<SNOT>(allocator, anton::variadic_construct, ANTON_MOV(node)), \
    source_info)

  class Parser {
  public:
    Parser(Allocator* allocator, anton::String_View source_name,
           char8 const* const source_data, Lexer&& lexer)
      : _allocator(allocator), _source_name(source_name),
        _source_data(source_data), _lexer(ANTON_MOV(lexer))
    {
    }

    anton::Expected<Array<SNOT>, Error> build_syntax_tree()
    {
      ANNOTATE_FUNCTION()
      Array<SNOT> syntax_tree{_allocator};
      while(true) {
        if(_lexer.match_eof()) {
          return {anton::expected_value, ANTON_MOV(syntax_tree)};
        }

        if(Optional<Syntax_Node> declaration = try_declaration()) {
          syntax_tree.emplace_back(ANTON_MOV(*declaration));
        } else {
          return {anton::expected_error, _last_error.to_error(_source_name)};
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
    anton::String_View _source_name;
    char8 const* _source_data;
    Lexer _lexer;
    Parse_Error _last_error;

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
      return Source_Info{.source_path = _source_name,
                         .line = start_token.line,
                         .column = start_token.column,
                         .offset = start_token.offset,
                         // .end_line = end_token.end_line,
                         // .end_column = end_token.end_column,
                         .end_offset = end_token.end_offset};
    }

    // match
    // Matches the next token with a specified type in the token stream.
    //
    // Parameters:
    // type - the type of the token to match.
    //
    // Returns:
    // Syntax_Token of the specified type or null_optional if the next token's
    // type is not type.
    //
    [[nodiscard]] Optional<Syntax_Token> match(Token_Kind const type)
    {
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::Optional<Token> const token = _lexer.peek_token();
      if(!token) {
        return anton::null_optional;
      }

      Token_Kind const token_kind = token->kind;
      if(token_kind == type) {
        _lexer.advance_token();
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        anton::String7_View const value = token->get_value(_source_data);
        // Token_Kind and Syntax_Node_Kind have overlapping values, therefore we
        // convert one to the other via a cast.
        Syntax_Node_Kind const syntax_kind =
          static_cast<Syntax_Node_Kind>(token_kind);
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Token(
          syntax_kind, anton::String(value.begin(), value.end(), _allocator),
          source);
      } else {
        return anton::null_optional;
      }
    }

    // skipmatch
    // Matches the next token with a specified type in the token stream skipping
    // whitespace and comments. Does not match comments or whitespaces.
    //
    // Parameters:
    // type - the type of the token to match. Must not be Token_Kind::comment or
    //        Token_Kind::whitespace.
    //
    // Returns:
    // Syntax_Token of the specified type or null_optional if the next token's
    // type is not type.
    //
    [[nodiscard]] Optional<Syntax_Token> skipmatch(Token_Kind const type)
    {
      _lexer.ignore_whitespace_and_comments();
      return match(type);
    }

    // match
    // Matches identifier token with a specified content.
    //
    // Parameters:
    // value - the required identifier value.
    //
    // Returns:
    // An identifier Syntax_Token if the value of the token is the same as the
    // parameter. null_optional otherwise.
    //
    [[nodiscard]] Optional<Syntax_Token> match(anton::String7_View const value)
    {
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::Optional<Token> const token = _lexer.peek_token();
      if(!token) {
        return anton::null_optional;
      }

      Token_Kind const token_kind = token->kind;
      anton::String7_View const token_value = token->get_value(_source_data);
      if(token_kind == Token_Kind::identifier && token_value == value) {
        _lexer.advance_token();
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Token(
          Syntax_Node_Kind::identifier,
          anton::String(token_value.begin(), token_value.end(), _allocator),
          source);
      } else {
        return anton::null_optional;
      }
    }

    // skipmatch
    // Matches identifier token with a specified content.
    //
    // Parameters:
    // value - the required identifier value.
    //
    // Returns:
    // An identifier Syntax_Token if the value of the token is the same as the
    // parameter. null_optional otherwise.
    //
    [[nodiscard]] Optional<Syntax_Token>
    skipmatch(anton::String7_View const value)
    {
      _lexer.ignore_whitespace_and_comments();
      return match(value);
    }

    // combine
    // Combines two tokens that appear next to each other in the source. Tokens
    // are combined in the order they are passed to the function. Only value and
    // source_info members of the tokens are combined.
    //
    // Parameters:
    //    result_type - Syntax_Node_Kind to assign to the result token.
    // token1, token2 - tokens to combine.
    //
    // Returns:
    // Combined Syntax_Token with type member set to result_type.
    //
    [[nodiscard]] Syntax_Token combine(Syntax_Node_Kind const result_type,
                                       Syntax_Token const& token1,
                                       Syntax_Token const& token2)
    {
      Source_Info source_info;
      // Both tokens have the same souce_path;
      source_info.source_path = token1.source_info.source_path;
      source_info.line =
        anton::math::min(token1.source_info.line, token2.source_info.line);
      source_info.column =
        anton::math::min(token1.source_info.column, token2.source_info.column);
      source_info.offset =
        anton::math::min(token1.source_info.offset, token2.source_info.offset);
      source_info.end_offset = anton::math::max(token1.source_info.end_offset,
                                                token2.source_info.end_offset);
      return Syntax_Token(result_type,
                          anton::concat(_allocator, token1.value, token2.value),
                          source_info);
    }

    // combine
    // Combines three tokens that appear next to each other in the source.
    // Tokens are combined in the order they are passed to the function. Only
    // value and source_info members of the tokens are combined.
    //
    // Parameters:
    //            result_type - Syntax_Node_Kind to assign to the result token.
    // token1, token2, token3 - tokens to combine.
    //
    // Returns:
    // Combined Syntax_Token with type member set to result_type.
    //
    [[nodiscard]] Syntax_Token combine(Syntax_Node_Kind const result_type,
                                       Syntax_Token const& token1,
                                       Syntax_Token const& token2,
                                       Syntax_Token const& token3)
    {
      Source_Info source_info;
      // All tokens have the same souce_path;
      source_info.source_path = token1.source_info.source_path;
      source_info.line =
        anton::math::min(token1.source_info.line, token2.source_info.line,
                         token3.source_info.line);
      source_info.column =
        anton::math::min(token1.source_info.column, token2.source_info.column,
                         token3.source_info.column);
      source_info.offset =
        anton::math::min(token1.source_info.offset, token2.source_info.offset,
                         token3.source_info.offset);
      source_info.end_offset = anton::math::max(token1.source_info.end_offset,
                                                token2.source_info.end_offset,
                                                token3.source_info.end_offset);
      return Syntax_Token(
        result_type,
        anton::concat(_allocator, token1.value, token2.value, token3.value),
        source_info);
    }

    // match
    //
    [[nodiscard]] Optional<Syntax_Token>
    match(Syntax_Node_Kind const result_type, Token_Kind const tk_type1,
          Token_Kind const tk_type2)
    {
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Optional tk1 = match(tk_type1);
      if(!tk1) {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      Optional tk2 = match(tk_type2);
      if(!tk2) {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      return combine(result_type, *tk1, *tk2);
    }

    // match
    //
    [[nodiscard]] Optional<Syntax_Token>
    match(Syntax_Node_Kind const result_type, Token_Kind const tk_type1,
          Token_Kind const tk_type2, Token_Kind const tk_type3)
    {
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Optional tk1 = match(tk_type1);
      if(!tk1) {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      Optional tk2 = match(tk_type2);
      if(!tk2) {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      Optional tk3 = match(tk_type3);
      if(!tk3) {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      return combine(result_type, *tk1, *tk2, *tk3);
    }

    // skipmatch
    //
    [[nodiscard]] Optional<Syntax_Token>
    skipmatch(Syntax_Node_Kind const result_type, Token_Kind const tk_type1,
              Token_Kind const tk_type2)
    {
      _lexer.ignore_whitespace_and_comments();
      return match(result_type, tk_type1, tk_type2);
    }

    // skipmatch
    //
    [[nodiscard]] Optional<Syntax_Token>
    skipmatch(Syntax_Node_Kind const result_type, Token_Kind const tk_type1,
              Token_Kind const tk_type2, Token_Kind const tk_type3)
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
    [[nodiscard]] Optional<Syntax_Token> match_setting_string()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      anton::String value{_allocator};
      // We require at least one token to be present.
      if(!_lexer.peek_token()) {
        return anton::null_optional;
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

        anton::String7_View const token_value = token->get_value(_source_data);
        value += anton::String_View{token_value.begin(), token_value.end()};
        _lexer.advance_token();
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Token(Syntax_Node_Kind::tk_setting_string, ANTON_MOV(value),
                          source);
    }

    // TODO: Error inside try_attribute does not bubble upward.
    Optional<Syntax_Node> try_attribute()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_at, "expected '@'"_sv, snots);
      // No whitespace between '@' and identifier are allowed.
      EXPECT_TOKEN(Token_Kind::identifier, "expected identifier"_sv, snots);

      Lexer_State const parameter_list_begin_state = _lexer.get_current_state();
      Array<SNOT> parameter_list_snots{_allocator};
      if(Optional tk_lparen = skipmatch(Token_Kind::tk_lparen)) {
        parameter_list_snots.push_back(ANTON_MOV(*tk_lparen));

        while(true) {
          if(Optional tk_rparen = skipmatch(Token_Kind::tk_rparen)) {
            parameter_list_snots.push_back(ANTON_MOV(*tk_rparen));
            break;
          }

          Lexer_State const parameter_begin_state = _lexer.get_current_state();
          Array<SNOT> parameter_snots{_allocator};
          bool keyed = false;
          if(Optional key = match(Token_Kind::identifier)) {
            if(Optional tk_equals = skipmatch(Token_Kind::tk_equals)) {
              parameter_snots.push_back(ANTON_MOV(*key));
              parameter_snots.push_back(ANTON_MOV(*tk_equals));
              keyed = true;
            } else {
              _lexer.restore_state(parameter_begin_state);
            }
          }

          if(Optional value = try_expression_without_init()) {
            parameter_snots.push_back(ANTON_MOV(*value));
          } else {
            _lexer.restore_state(begin_state);
            return anton::null_optional;
          }

          Lexer_State const parameter_end_state =
            _lexer.get_current_state_noskip();
          Source_Info const source =
            src_info(parameter_begin_state, parameter_end_state);
          if(keyed) {
            parameter_list_snots.push_back(
              Syntax_Node(Syntax_Node_Kind::attribute_parameter_keyed,
                          ANTON_MOV(parameter_snots), source));
          } else {
            parameter_list_snots.push_back(
              Syntax_Node(Syntax_Node_Kind::attribute_parameter_positional,
                          ANTON_MOV(parameter_snots), source));
          }
        }

        Lexer_State const parameter_list_end_state =
          _lexer.get_current_state_noskip();
        Source_Info const parameter_list_source =
          src_info(parameter_list_begin_state, parameter_list_end_state);
        snots.push_back(Syntax_Node(Syntax_Node_Kind::attribute_parameter_list,
                                    ANTON_MOV(parameter_list_snots),
                                    parameter_list_source));
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::attribute, ANTON_MOV(snots), source);
    }

    Syntax_Node try_attribute_list()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      while(Optional attribute = try_attribute()) {
        snots.push_back(ANTON_MOV(*attribute));
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::attribute_list, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_declaration()
    {
      ANNOTATE_FUNCTION()

      _lexer.ignore_whitespace_and_comments();
      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return anton::null_optional;
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
      Syntax_Node attribute_list = try_attribute_list();

      _lexer.ignore_whitespace_and_comments();

      // Match declarations that do allow attribute lists.
      switch(lookahead->kind) {
      case Token_Kind::kw_struct:
        return try_decl_struct(attribute_list);

      case Token_Kind::kw_buffer:
        return try_decl_buffer(attribute_list);

      case Token_Kind::kw_fn: {
        // TODO: Merge these two functions.
        if(Optional decl_stage_function =
             try_decl_stage_function(attribute_list)) {
          return ANTON_MOV(*decl_stage_function);
        }

        if(Optional decl_function = try_decl_function(attribute_list)) {
          return ANTON_MOV(*decl_function);
        }
      } break;

      default:
        break;
      }

      // if(Optional decl_constant = try_variable(attribute_list)) {
      //   return ANTON_MOV(*decl_constant);
      // }

      set_error(u8"expected declaration");
      return anton::null_optional;
    }

    // try_decl_block
    // Match declaration list enclosed in braces
    //   '{' declaration_list '}'
    //
    Optional<Syntax_Node> try_decl_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);

      while(true) {
        if(Optional tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.push_back(ANTON_MOV(*tk_rbrace));
          break;
        }

        EXPECT_NODE(try_declaration, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_block, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_decl_import()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_import, "expected 'import'"_sv, snots);
      EXPECT_NODE(try_expr_lt_string, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_import, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_decl_if()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_if, "expected 'if'"_sv, snots);
      // condition
      EXPECT_NODE(try_expression_without_init, snots);
      // then branch
      EXPECT_NODE(try_decl_block, snots);

      if(Optional kw_else = skipmatch(Token_Kind::kw_else)) {
        snots.push_back(ANTON_MOV(*kw_else));
        if(Optional decl_if = try_decl_if()) {
          snots.push_back(ANTON_MOV(*decl_if));
        } else if(Optional decl_block = try_decl_block()) {
          snots.push_back(ANTON_MOV(*decl_block));
        } else {
          _lexer.restore_state(begin_state);
          return anton::null_optional;
        }
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_if, ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_variable()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Source_Info const source = src_info(begin_state, begin_state);
      Syntax_Node dummy_attribute_list{
        Syntax_Node_Kind::attribute_list, {}, source};
      return try_variable(dummy_attribute_list);
    }

    Optional<Syntax_Node> try_variable(Syntax_Node& attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_var, "expected 'var'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);
      EXPECT_NODE(try_type, snots);

      if(Optional tk_equals = skipmatch(Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*tk_equals));
        EXPECT_NODE(try_expression, snots);
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);

      snots.insert(snots.begin(), ANTON_MOV(attribute_list));

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::variable, ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_struct_field()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      // TODO: Attribute list does not propagate errors correctly.
      {
        Syntax_Node attribute_list = try_attribute_list();
        snots.push_back(ANTON_MOV(attribute_list));
      }

      // identifier ':' type
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);
      EXPECT_NODE(try_type, snots);

      if(Optional tk_equals = skipmatch(Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*tk_equals));
        EXPECT_NODE(try_expression, snots);
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::struct_field, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_struct_field_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(Optional tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.push_back(ANTON_MOV(*tk_rbrace));
          break;
        }

        EXPECT_NODE(try_struct_field, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node{Syntax_Node_Kind::struct_field_block, ANTON_MOV(snots),
                         source};
    }

    Optional<Syntax_Node> try_decl_struct(Syntax_Node& attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_struct, "expected 'struct'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_struct_field_block, snots);

      snots.insert(snots.begin(), ANTON_MOV(attribute_list));
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_struct, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_buffer_field()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      // TODO: Attribute list does not propagate errors correctly.
      {
        Syntax_Node attribute_list = try_attribute_list();
        snots.push_back(ANTON_MOV(attribute_list));
      }

      // TODO: change grammar to type ':' identifier ';'
      EXPECT_NODE(try_type, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);

      if(Optional tk_equals = skipmatch(Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*tk_equals));
        EXPECT_NODE(try_expression, snots);
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::buffer_field, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_buffer_field_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(Optional tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.push_back(ANTON_MOV(*tk_rbrace));
          break;
        }

        EXPECT_NODE(try_buffer_field, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node{Syntax_Node_Kind::buffer_field_block, ANTON_MOV(snots),
                         source};
    }

    Optional<Syntax_Node> try_decl_buffer(Syntax_Node& attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_buffer, "expected 'buffer'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN2_SKIP(Syntax_Node_Kind::tk_colon2, Token_Kind::tk_colon,
                         Token_Kind::tk_colon, "expected '::'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_buffer_field_block, snots);

      snots.insert(snots.begin(), ANTON_MOV(attribute_list));
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_buffer, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_setting()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_NODE(match_setting_string, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);

      // TODO: Flatten setting keys.

      if(Optional block = try_setting_block()) {
        snots.push_back(ANTON_MOV(*block));
      } else if(Optional value = match_setting_string()) {
        snots.push_back(ANTON_MOV(*value));
      } else {
        set_error("expected value string after ':'"_sv);
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::setting_keyval, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_setting_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(Optional tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.push_back(ANTON_MOV(*tk_rbrace));
          break;
        }

        EXPECT_NODE(try_setting, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::setting_block, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_decl_settings()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_settings, "expected 'settings'"_sv, snots);
      // pass
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_setting_block, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_settings, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_fn_parameter()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      if(Optional param_if = try_fn_parameter_if()) {
        return param_if;
      }

      {
        Syntax_Node attribute_list = try_attribute_list();
        snots.push_back(ANTON_MOV(attribute_list));
      }

      // identifier ':' type
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_colon, "expected ':'"_sv, snots);
      EXPECT_NODE(try_type, snots);

      if(Optional kw_from = skipmatch(Token_Kind::kw_from)) {
        snots.push_back(ANTON_MOV(*kw_from));
        // source
        EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                          snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::fn_parameter, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_fn_parameter_if()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_if, "expected 'if'"_sv, snots);
      // TODO: should this be expression-without-init?
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      EXPECT_NODE(try_fn_parameter, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_rbrace, "expected '}'"_sv, snots);
      if(Optional kw_else = skipmatch(Token_Kind::kw_else)) {
        snots.push_back(ANTON_MOV(*kw_else));
        if(Optional param_if = try_fn_parameter_if()) {
          snots.push_back(ANTON_MOV(*param_if));
        } else {
          EXPECT_TOKEN_SKIP(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
          EXPECT_NODE(try_fn_parameter, snots);
          EXPECT_TOKEN_SKIP(Token_Kind::tk_rbrace, "expected '}'"_sv, snots);
        }
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::fn_parameter_if, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_fn_parameter_list()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lparen, "expected '('"_sv, snots);
      // Match rparen to allow empty parameter lists.
      if(Optional tk_rparen = skipmatch(Token_Kind::tk_rparen)) {
        snots.push_back(ANTON_MOV(*tk_rparen));
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::fn_parameter_list,
                           ANTON_MOV(snots), source);
      }

      while(true) {
        EXPECT_NODE(try_fn_parameter, snots);
        if(Optional tk_comma = skipmatch(Token_Kind::tk_comma)) {
          snots.push_back(ANTON_MOV(*tk_comma));
        } else {
          break;
        }
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::fn_parameter_list, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_decl_stage_function(Syntax_Node& attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};

      EXPECT_TOKEN(Token_Kind::kw_fn, "expected 'fn'"_sv, snots);

      // pass
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN2_SKIP(Syntax_Node_Kind::tk_colon2, Token_Kind::tk_colon,
                         Token_Kind::tk_colon, "expected '::'"_sv, snots);

      // shader stage
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);

      EXPECT_NODE(try_fn_parameter_list, snots);

      EXPECT_NODE(try_stmt_block, snots);

      snots.insert(snots.begin(), ANTON_MOV(attribute_list));

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_stage_function,
                         ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_decl_function(Syntax_Node& attribute_list)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state_noskip();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_fn, "expected 'fn'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_NODE(try_fn_parameter_list, snots);

      EXPECT_TOKEN2_SKIP(Syntax_Node_Kind::tk_thin_arrow, Token_Kind::tk_minus,
                         Token_Kind::tk_rangle, "expected '->'"_sv, snots);

      EXPECT_NODE(try_type, snots);

      EXPECT_NODE(try_stmt_block, snots);

      snots.insert(snots.begin(), ANTON_MOV(attribute_list));

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::decl_function, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_statement()
    {
      ANNOTATE_FUNCTION()

      _lexer.ignore_whitespace_and_comments();
      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return anton::null_optional;
      }

      switch(lookahead->kind) {
      case Token_Kind::tk_semicolon: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::stmt_empty, ANTON_MOV(snots),
                           source);
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
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::kw_break, "expected 'break'"_sv, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::stmt_break, ANTON_MOV(snots),
                           source);
      }

      case Token_Kind::kw_continue: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::kw_continue, "expected 'continue'"_sv, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::stmt_continue, ANTON_MOV(snots),
                           source);
      }

      case Token_Kind::kw_discard: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::kw_discard, "expected 'discard'"_sv, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::stmt_discard, ANTON_MOV(snots),
                           source);
      }

      default: {
        if(Optional stmt_assignment = try_stmt_assignment()) {
          return ANTON_MOV(*stmt_assignment);
        }

        if(Optional stmt_expression = try_stmt_expression()) {
          return ANTON_MOV(*stmt_expression);
        }
      }
      }

      set_error(u8"expected a statement");
      return anton::null_optional;
    }

    // try_stmt_block
    // Match a list of statements enclosed in braces
    //   '{' statements '}'
    Optional<Syntax_Node> try_stmt_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      while(true) {
        if(Optional tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
          snots.push_back(ANTON_MOV(*tk_rbrace));
          break;
        }
        EXPECT_NODE(try_statement, snots);
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_block, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_stmt_if()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_if, "expected 'if'"_sv, snots);
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(try_stmt_block, snots);

      if(Optional kw_else = skipmatch(Token_Kind::kw_else)) {
        snots.push_back(ANTON_MOV(*kw_else));
        if(Optional stmt_if = try_stmt_if()) {
          snots.emplace_back(ANTON_MOV(*stmt_if));
        } else {
          EXPECT_NODE(try_stmt_block, snots);
        }
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_if, ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_stmt_switch()
    {
      ANNOTATE_FUNCTION()
      auto match_switch_arm_list = [this]() -> Optional<Syntax_Node> {
        auto match_switch_arm = [this]() -> Optional<Syntax_Node> {
          Lexer_State const begin_state = _lexer.get_current_state();
          Array<SNOT> snots{_allocator};
          while(true) {
            if(Optional kw_default = skipmatch(Token_Kind::kw_default)) {
              Source_Info const source_info = kw_default->source_info;
              Syntax_Node expr_default =
                WRAP_NODE(_allocator, Syntax_Node_Kind::expr_default,
                          *kw_default, source_info);
              Syntax_Node label =
                WRAP_NODE(_allocator, Syntax_Node_Kind::switch_arm_label,
                          expr_default, source_info);
              snots.push_back(ANTON_MOV(label));
            } else if(Optional literal = try_expr_lt_integer()) {
              Source_Info const source_info = literal->source_info;
              snots.push_back(WRAP_NODE(_allocator,
                                        Syntax_Node_Kind::switch_arm_label,
                                        *literal, source_info));
            } else {
              _lexer.restore_state(begin_state);
              return anton::null_optional;
            }

            if(Optional tk_comma = skipmatch(Token_Kind::tk_comma)) {
              snots.push_back(ANTON_MOV(*tk_comma));
            } else {
              break;
            }
          }
          EXPECT_TOKEN2_SKIP(Syntax_Node_Kind::tk_thick_arrow,
                             Token_Kind::tk_equals, Token_Kind::tk_rangle,
                             "expected '=>'"_sv, snots);
          EXPECT_NODE(try_stmt_block, snots);
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          return Syntax_Node(Syntax_Node_Kind::switch_arm, ANTON_MOV(snots),
                             source);
        };

        Lexer_State const begin_state = _lexer.get_current_state();
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
        while(true) {
          if(Optional tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
            snots.push_back(ANTON_MOV(*tk_rbrace));
            break;
          }
          EXPECT_NODE(match_switch_arm, snots);
        }

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::switch_arm_list, ANTON_MOV(snots),
                           source);
      };

      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_switch, "expected 'switch'"_sv, snots);
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(match_switch_arm_list, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_switch, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_stmt_for()
    {
      ANNOTATE_FUNCTION()
      auto match_for_stmt_variable = [this]() -> Optional<Syntax_Node> {
        Lexer_State const begin_state = _lexer.get_current_state();
        Array<SNOT> snots{_allocator};
        // TODO: consider changing to identifier ':' type
        EXPECT_NODE(try_type, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected 'identifier'"_sv,
                          snots);
        if(Optional tk_equals = skipmatch(Token_Kind::tk_equals)) {
          snots.push_back(ANTON_MOV(*tk_equals));
          EXPECT_NODE(try_expression, snots);
        }

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return WRAP_NODE(
          _allocator, Syntax_Node_Kind::for_variable,
          Syntax_Node(Syntax_Node_Kind::variable, ANTON_MOV(snots), source),
          source);
      };

      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_for, "expected 'for'"_sv, snots);
      if(Optional for_stmt_variable = match_for_stmt_variable()) {
        snots.push_back(ANTON_MOV(*for_stmt_variable));
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);

      if(Optional condition = try_expression()) {
        Source_Info const source_info = condition->source_info;
        snots.push_back(WRAP_NODE(_allocator, Syntax_Node_Kind::for_condition,
                                  *condition, source_info));
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      // TODO FIX: does not match assignment.
      if(Optional expression = try_expression_without_init()) {
        Source_Info const source_info = expression->source_info;
        snots.push_back(WRAP_NODE(_allocator, Syntax_Node_Kind::for_expression,
                                  *expression, source_info));
      }

      EXPECT_NODE(try_stmt_block, snots);

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_for, ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_stmt_while()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_while, "expected 'while'"_sv, snots);
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(try_stmt_block, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_while, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_stmt_do_while()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_do, "expected 'do'"_sv, snots);
      EXPECT_NODE(try_stmt_block, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::kw_while, "expected 'while'"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_do_while, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_stmt_return()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_return, "expected 'return'"_sv, snots);
      if(Optional return_expression = try_expression()) {
        Source_Info const source_info = return_expression->source_info;
        snots.push_back(WRAP_NODE(_allocator,
                                  Syntax_Node_Kind::return_expression,
                                  *return_expression, source_info));
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_return, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_stmt_assignment()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_NODE(try_expression, snots);
      // Match any of the possible assignment tokens.
      if(Optional op = match(Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_ampeq, Token_Kind::tk_amp,
                        Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_pipeeq, Token_Kind::tk_pipe,
                        Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_hateq, Token_Kind::tk_hat,
                        Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_shleq, Token_Kind::tk_langle,
                        Token_Kind::tk_langle, Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_shreq, Token_Kind::tk_rangle,
                        Token_Kind::tk_rangle, Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_percenteq, Token_Kind::tk_percent,
                        Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_slasheq, Token_Kind::tk_slash,
                        Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_asteriskeq,
                        Token_Kind::tk_asterisk, Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_pluseq, Token_Kind::tk_plus,
                        Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else if(Optional op =
                  match(Syntax_Node_Kind::tk_minuseq, Token_Kind::tk_minus,
                        Token_Kind::tk_equals)) {
        snots.push_back(ANTON_MOV(*op));
      } else {
        set_error("expected assignment"_sv);
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      EXPECT_NODE(try_expression, snots);
      Lexer_State const end_state = _lexer.get_current_state();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_assignment, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_stmt_expression()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::stmt_expression, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_expression()
    {
      ANNOTATE_FUNCTION()
      return try_expr_binary(false);
    }

    Optional<Syntax_Node> try_expression_without_init()
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
    Optional<Syntax_Node> try_expr_binary(bool const disable_init)
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

      auto insert_operator = [_allocator = this->_allocator](Syntax_Node& root,
                                                             Syntax_Token op) {
        enum Associativity { ASSOC_LEFT, ASSOC_RIGHT };

        auto get_associativity = [](Syntax_Node_Kind type) -> Associativity {
          switch(type) {
          default:
            return ASSOC_LEFT;
          }
        };

        auto get_precedence = [](Syntax_Node_Kind type) -> i32 {
          // Precedence values are exactly the same as in the GLSL Specification.
          switch(type) {
          case Syntax_Node_Kind::tk_pipe2:
            return 14;
          case Syntax_Node_Kind::tk_hat2:
            return 13;
          case Syntax_Node_Kind::tk_amp2:
            return 12;
          case Syntax_Node_Kind::tk_pipe:
            return 11;
          case Syntax_Node_Kind::tk_hat:
            return 10;
          case Syntax_Node_Kind::tk_amp:
            return 9;

          case Syntax_Node_Kind::tk_eq2:
          case Syntax_Node_Kind::tk_neq:
            return 8;

          case Syntax_Node_Kind::tk_langle:
          case Syntax_Node_Kind::tk_rangle:
          case Syntax_Node_Kind::tk_gteq:
          case Syntax_Node_Kind::tk_lteq:
            return 7;

          case Syntax_Node_Kind::tk_shl:
          case Syntax_Node_Kind::tk_shr:
            return 6;

          case Syntax_Node_Kind::tk_plus:
          case Syntax_Node_Kind::tk_minus:
            return 5;

          case Syntax_Node_Kind::tk_asterisk:
          case Syntax_Node_Kind::tk_slash:
          case Syntax_Node_Kind::tk_percent:
            return 4;

          default:
            ANTON_UNREACHABLE("invalid operator type");
          }
        };

        i32 const op_prec = get_precedence(op.kind);
        i32 const op_assoc = get_associativity(op.kind);
        Syntax_Node* dest_node = &root;
        while(true) {
          // We will replace the node if it is not a binary expression.
          if(dest_node->kind != Syntax_Node_Kind::expr_binary) {
            break;
          }

          ANTON_ASSERT(dest_node->children.size() == 3,
                       "expr_binary does not have exactly 3 children");
          ANTON_ASSERT(dest_node->children[1].is_right(),
                       "second SNOT of expr_binary is not a Syntax_Token");
          Syntax_Token const& dest_op = dest_node->children[1].right();
          i32 const dest_prec = get_precedence(dest_op.kind);
          if(op_assoc == ASSOC_LEFT) {
            if(op_prec <= dest_prec) {
              dest_node = &dest_node->children[2].left();
            } else {
              break;
            }
          } else {
            if(op_prec < dest_prec) {
              dest_node = &dest_node->children[2].left();
            } else {
              break;
            }
          }
        }

        // Replace dest_node with a new expr_binary node.
        Array<SNOT> snots{_allocator};
        snots.push_back(ANTON_MOV(*dest_node));
        snots.push_back(ANTON_MOV(op));
        *dest_node = Syntax_Node(Syntax_Node_Kind::expr_binary,
                                 ANTON_MOV(snots), Source_Info{});
      };

      auto insert_expression = [](Syntax_Node* root,
                                  Syntax_Node&& expr) -> void {
        // We only ever have to descend down the right children.
        while(root->children.size() == 3) {
          ANTON_ASSERT(root->kind == Syntax_Node_Kind::expr_binary,
                       "Syntax_Node is not expr_binary");
          ANTON_ASSERT(
            root->children[2].is_left(),
            "third child of a binary expression node is not a Syntax_Node");
          root = &root->children[2].left();
        }

        root->children.push_back(ANTON_MOV(expr));
      };

      auto match_binary_operator = [this]() -> Optional<Syntax_Token> {
        // We match operators in the following order:
        // 1. Logical
        // 2. Equality
        // 3. Relational X-equal
        // 4. Shift
        // 5. Remaining operators (bitwise, relational, additive,
        //    multiplicative)

        if(Optional op = match(Syntax_Node_Kind::tk_amp2, Token_Kind::tk_amp,
                               Token_Kind::tk_amp)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Syntax_Node_Kind::tk_pipe2, Token_Kind::tk_pipe,
                               Token_Kind::tk_pipe)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Syntax_Node_Kind::tk_hat2, Token_Kind::tk_hat,
                               Token_Kind::tk_hat)) {
          return ANTON_MOV(op);
        }

        if(Optional op = match(Syntax_Node_Kind::tk_eq2, Token_Kind::tk_equals,
                               Token_Kind::tk_equals)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Syntax_Node_Kind::tk_neq, Token_Kind::tk_bang,
                               Token_Kind::tk_equals)) {
          return ANTON_MOV(op);
        }

        if(Optional op = match(Syntax_Node_Kind::tk_lteq, Token_Kind::tk_langle,
                               Token_Kind::tk_equals)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Syntax_Node_Kind::tk_gteq, Token_Kind::tk_rangle,
                               Token_Kind::tk_equals)) {
          return ANTON_MOV(op);
        }

        if(Optional op = match(Syntax_Node_Kind::tk_shl, Token_Kind::tk_langle,
                               Token_Kind::tk_langle)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Syntax_Node_Kind::tk_shr, Token_Kind::tk_rangle,
                               Token_Kind::tk_rangle)) {
          return ANTON_MOV(op);
        }

        if(Optional op = match(Token_Kind::tk_amp)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_hat)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_pipe)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_langle)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_rangle)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_plus)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_minus)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_asterisk)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_slash)) {
          return ANTON_MOV(op);
        }
        if(Optional op = match(Token_Kind::tk_percent)) {
          return ANTON_MOV(op);
        }

        return anton::null_optional;
      };

      Lexer_State const begin_state = _lexer.get_current_state();
      Optional root_expr = try_expr_prefix(disable_init);
      if(!root_expr) {
        return anton::null_optional;
      }

      while(true) {
        _lexer.ignore_whitespace_and_comments();
        if(Optional op = match_binary_operator()) {
          insert_operator(*root_expr, ANTON_MOV(*op));
        } else {
          break;
        }

        if(Optional expr = try_expr_prefix(disable_init)) {
          insert_expression(&root_expr.value(), ANTON_MOV(*expr));
        } else {
          _lexer.restore_state(begin_state);
          return anton::null_optional;
        }
      }

      auto recalculate_source_info = [](auto& recalculate_source_info,
                                        Syntax_Node& node) -> void {
        if(node.kind != Syntax_Node_Kind::expr_binary) {
          return;
        }

        ANTON_ASSERT(node.children[0].is_left(),
                     "the left child of expr_binary is not a Syntax_Node");
        ANTON_ASSERT(node.children[2].is_left(),
                     "the right child of expr_binary is not a Syntax_Node");
        recalculate_source_info(recalculate_source_info,
                                node.children[0].left());
        recalculate_source_info(recalculate_source_info,
                                node.children[2].left());
        Source_Info const& left = node.children[0].left().source_info;
        Source_Info const& right = node.children[2].left().source_info;
        // Both left and right have the same souce_path;
        node.source_info.source_path = left.source_path;
        node.source_info.line = left.line;
        node.source_info.column = left.column;
        node.source_info.offset = left.offset;
        node.source_info.end_offset = right.end_offset;
      };

      // expr_binary nodes do not have their source_info members filled properly because
      // it is impossible to do so while inserting (we do not know the final right child).
      // We have to manually fix up the source_info members.
      recalculate_source_info(recalculate_source_info, *root_expr);

      return root_expr;
    }

    Optional<Syntax_Node> try_expr_prefix(bool const disable_init)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      if(Optional plus = match(Token_Kind::tk_plus)) {
        snots.push_back(ANTON_MOV(*plus));
      } else if(Optional minus = match(Token_Kind::tk_minus)) {
        snots.push_back(ANTON_MOV(*minus));
      } else if(Optional bang = match(Token_Kind::tk_bang)) {
        snots.push_back(ANTON_MOV(*bang));
      } else if(Optional tilde = match(Token_Kind::tk_tilde)) {
        snots.push_back(ANTON_MOV(*tilde));
      } else {
        return try_expr_postfix(disable_init);
      }

      if(Optional expr = try_expr_prefix(disable_init)) {
        snots.push_back(ANTON_MOV(*expr));
      } else {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_prefix, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_expr_postfix(bool const disable_init)
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Optional expr = try_primary_expression(disable_init);
      if(!expr) {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      while(true) {
        if(Optional tk_dot = skipmatch(Token_Kind::tk_dot)) {
          Optional identifier = skipmatch(Token_Kind::identifier);
          if(!identifier) {
            set_error(u8"expected identifier");
            _lexer.restore_state(begin_state);
            return anton::null_optional;
          }

          Array<SNOT> snots{_allocator};
          snots.push_back(ANTON_MOV(*expr));
          snots.push_back(ANTON_MOV(*tk_dot));
          snots.push_back(ANTON_MOV(*identifier));
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          expr =
            Syntax_Node(Syntax_Node_Kind::expr_field, ANTON_MOV(snots), source);
        } else if(Optional tk_lbracket = skipmatch(Token_Kind::tk_lbracket)) {
          Optional index = try_expression();
          if(!index) {
            _lexer.restore_state(begin_state);
            return anton::null_optional;
          }

          Optional tk_rbracket = skipmatch(Token_Kind::tk_rbracket);
          if(!tk_rbracket) {
            set_error(u8"expected ']'");
            _lexer.restore_state(begin_state);
            return anton::null_optional;
          }

          Array<SNOT> snots{_allocator};
          snots.push_back(ANTON_MOV(*expr));
          snots.push_back(ANTON_MOV(*tk_lbracket));
          snots.push_back(ANTON_MOV(*index));
          snots.push_back(ANTON_MOV(*tk_rbracket));
          Lexer_State const end_state = _lexer.get_current_state_noskip();
          Source_Info const source = src_info(begin_state, end_state);
          expr =
            Syntax_Node(Syntax_Node_Kind::expr_index, ANTON_MOV(snots), source);
        } else {
          break;
        }
      }

      return expr;
    }

    Optional<Syntax_Node> try_primary_expression(bool const disable_init)
    {
      ANNOTATE_FUNCTION()

      _lexer.ignore_whitespace_and_comments();
      Optional<Token> lookahead = _lexer.peek_token();
      if(!lookahead) {
        return anton::null_optional;
      }

      switch(lookahead->kind) {
      case Token_Kind::tk_lparen: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::tk_lparen, "expected '('"_sv, snots);
        EXPECT_NODE(try_expression, snots);
        EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::expr_parentheses, ANTON_MOV(snots),
                           source);
      }

      case Token_Kind::kw_reinterpret:
        return try_expr_reinterpret();

      case Token_Kind::kw_if:
        return try_expr_if();

      case Token_Kind::lt_float: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::lt_float, "expected float literal"_sv, snots);
        // We validate the suffix in later stages.
        if(Optional suffix = match(Token_Kind::identifier)) {
          snots.push_back(ANTON_MOV(*suffix));
        }

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::expr_lt_float, ANTON_MOV(snots),
                           source);
      }

      case Token_Kind::lt_bin_integer:
      case Token_Kind::lt_dec_integer:
      case Token_Kind::lt_hex_integer:
        return try_expr_lt_integer();

      case Token_Kind::lt_bool: {
        Lexer_State const begin_state = _lexer.get_current_state_noskip();
        Array<SNOT> snots{_allocator};
        EXPECT_TOKEN(Token_Kind::lt_bool, "expected bool literal"_sv, snots);
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::expr_lt_bool, ANTON_MOV(snots),
                           source);
      }

      default: {
        if(!disable_init) {
          if(Optional expr_init = try_expr_init()) {
            return ANTON_MOV(expr_init);
          }
        }

        if(Optional expr_call = try_expr_call()) {
          return ANTON_MOV(expr_call);
        }

        if(Optional expr_identifier = try_expr_identifier()) {
          return ANTON_MOV(expr_identifier);
        }
      }
      }

      return anton::null_optional;
    }

    Optional<Syntax_Node> try_expr_reinterpret()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_reinterpret, "expected 'reinterpret'"_sv,
                   snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_langle, "expected '<'"_sv, snots);
      EXPECT_NODE(try_type, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_rangle, "expected '>'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_lparen, "expected '('"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_reinterpret, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_expr_block()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_rbrace, "expected '}'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_block, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_expr_if()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::kw_if, "expected 'if'"_sv, snots);
      // condition
      EXPECT_NODE(try_expression_without_init, snots);
      EXPECT_NODE(try_expr_block, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::kw_else, "expected 'else'"_sv, snots);
      if(Optional expr_if = try_expr_if()) {
        snots.push_back(ANTON_MOV(*expr_if));
      } else if(Optional else_branch = try_expr_block()) {
        snots.push_back(ANTON_MOV(*else_branch));
      } else {
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_if, ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_field_initializer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_dot, "expected '.'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::identifier, "expected identifier"_sv,
                        snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_equals, "expected '='"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::field_initializer, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_index_initializer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbracket, "expected '['"_sv, snots);
      EXPECT_NODE(try_expr_lt_integer, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_rbracket, "expected ']'"_sv, snots);
      EXPECT_TOKEN_SKIP(Token_Kind::tk_equals, "expected '='"_sv, snots);
      EXPECT_NODE(try_expression, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::index_initializer, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_basic_initializer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_NODE(try_expression, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::basic_initializer, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_init_initializer_list()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lbrace, "expected '{'"_sv, snots);

      // Match closing paren to allow empty parameter lists.
      if(Optional tk_rbrace = skipmatch(Token_Kind::tk_rbrace)) {
        snots.push_back(ANTON_MOV(*tk_rbrace));
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::init_initializer_list,
                           ANTON_MOV(snots), source);
      }

      while(true) {
        if(Optional field_initializer = try_field_initializer()) {
          snots.push_back(ANTON_MOV(*field_initializer));
        } else if(Optional index_initializer = try_index_initializer()) {
          snots.push_back(ANTON_MOV(*index_initializer));
        } else if(Optional basic_initializer = try_basic_initializer()) {
          snots.push_back(ANTON_MOV(*basic_initializer));
        } else {
          set_error("expected initializer"_sv);
          _lexer.restore_state(begin_state);
          return anton::null_optional;
        }

        if(Optional tk_comma = skipmatch(Token_Kind::tk_comma)) {
          snots.push_back(ANTON_MOV(*tk_comma));
        } else {
          break;
        }
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_rbrace, "expected '}'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::init_initializer_list,
                         ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_expr_init()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_NODE(try_type, snots);
      EXPECT_NODE(try_init_initializer_list, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_init, ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_call_arg_list()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::tk_lparen, "expected '('"_sv, snots);

      // Match closing paren to allow empty parameter lists.
      if(Optional tk_rparen = skipmatch(Token_Kind::tk_rparen)) {
        snots.push_back(ANTON_MOV(*tk_rparen));
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::call_arg_list, ANTON_MOV(snots),
                           source);
      }

      while(true) {
        EXPECT_NODE(try_expression, snots);

        if(Optional tk_comma = skipmatch(Token_Kind::tk_comma)) {
          snots.push_back(ANTON_MOV(*tk_comma));
        } else {
          break;
        }
      }

      EXPECT_TOKEN_SKIP(Token_Kind::tk_rparen, "expected ')'"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::call_arg_list, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_expr_call()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::identifier, "expected identifier"_sv, snots);
      EXPECT_NODE(try_call_arg_list, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_call, ANTON_MOV(snots), source);
    }

    Optional<Syntax_Node> try_expr_identifier()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::identifier, "expected identifier"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_identifier, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_expr_lt_integer()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      if(Optional lt_bin_integer = match(Token_Kind::lt_bin_integer)) {
        snots.push_back(ANTON_MOV(*lt_bin_integer));
      } else if(Optional lt_dec_integer = match(Token_Kind::lt_dec_integer)) {
        snots.push_back(ANTON_MOV(*lt_dec_integer));
      } else if(Optional lt_hex_integer = match(Token_Kind::lt_hex_integer)) {
        snots.push_back(ANTON_MOV(*lt_hex_integer));
      } else {
        set_error("expected integer literal"_sv);
        return anton::null_optional;
      }

      // We validate suffix on the literals in later stages.
      if(Optional suffix = match(Token_Kind::identifier)) {
        snots.push_back(ANTON_MOV(*suffix));
      }

      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_lt_integer, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_expr_lt_string()
    {
      ANNOTATE_FUNCTION()
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      EXPECT_TOKEN(Token_Kind::lt_string, "expected string literal"_sv, snots);
      Lexer_State const end_state = _lexer.get_current_state_noskip();
      Source_Info const source = src_info(begin_state, end_state);
      return Syntax_Node(Syntax_Node_Kind::expr_lt_string, ANTON_MOV(snots),
                         source);
    }

    Optional<Syntax_Node> try_type()
    {
      ANNOTATE_FUNCTION()
      auto try_type_array = [this]() -> Optional<Syntax_Node> {
        Lexer_State const begin_state = _lexer.get_current_state();
        Array<SNOT> snots{_allocator};
        if(Optional kw_mut = match(Token_Kind::kw_mut)) {
          snots.push_back(ANTON_MOV(*kw_mut));
        }

        EXPECT_TOKEN_SKIP(Token_Kind::tk_lbracket, "expected '['"_sv, snots);

        if(Optional type = try_type()) {
          Source_Info const source_info = type->source_info;
          snots.push_back(WRAP_NODE(
            _allocator, Syntax_Node_Kind::type_array_base, *type, source_info));
        } else {
          _lexer.restore_state(begin_state);
          return anton::null_optional;
        }

        EXPECT_TOKEN_SKIP(Token_Kind::tk_semicolon, "expected ';'"_sv, snots);

        if(Optional size = try_expr_lt_integer()) {
          Source_Info const source_info = size->source_info;
          snots.push_back(WRAP_NODE(
            _allocator, Syntax_Node_Kind::type_array_size, *size, source_info));
        }

        EXPECT_TOKEN_SKIP(Token_Kind::tk_rbracket, "expected ']'"_sv, snots);

        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::type_array, ANTON_MOV(snots),
                           source);
      };

      if(Optional type_array = try_type_array()) {
        return ANTON_MOV(type_array);
      }

      // Match builtin or struct.
      Lexer_State const begin_state = _lexer.get_current_state();
      Array<SNOT> snots{_allocator};
      if(Optional kw_mut = match(Token_Kind::kw_mut)) {
        snots.push_back(ANTON_MOV(*kw_mut));
      }

      if(Optional type = skipmatch(Token_Kind::identifier)) {
        snots.push_back(ANTON_MOV(*type));
        Lexer_State const end_state = _lexer.get_current_state_noskip();
        Source_Info const source = src_info(begin_state, end_state);
        return Syntax_Node(Syntax_Node_Kind::type_named, ANTON_MOV(snots),
                           source);
      } else {
        set_error(u8"expected identifier");
        _lexer.restore_state(begin_state);
        return anton::null_optional;
      }
    }
  };

  struct Insert_Location {
    Array<SNOT>* array;
    Array<SNOT>::iterator location;
  };

  // find_insert_location
  //
  [[nodiscard]] static Insert_Location
  find_insert_location(Array<SNOT>& snots, Syntax_Token const& token)
  {
    auto get_snot_source_info = [](SNOT const& snot) {
      if(snot.is_left()) {
        return snot.left().source_info;
      } else {
        return snot.right().source_info;
      }
    };

    Array<SNOT>::iterator i = snots.begin();
    for(Array<SNOT>::iterator const e = snots.end(); i != e; ++i) {
      Source_Info const snot_src = get_snot_source_info(*i);
      // Check whether token is before i.
      if(token.source_info.offset < snot_src.offset) {
        break;
      }
      // Check whether token is contained within a node.
      // Note that this may only happen when snot is a node
      // since tokens by definition are strings of contiguous symbols.
      if(token.source_info.offset >= snot_src.offset &&
         token.source_info.end_offset < snot_src.end_offset) {
        Syntax_Node& node = i->left();
        return find_insert_location(node.children, token);
      }
    }

    return {.array = &snots, .location = i};
  }

  struct Insert_Comments_And_Whitespace_Parameters {
    Allocator* allocator;
    anton::String_View source_path;
    Array<SNOT>& tl_node;
    Token const* begin;
    Token const* end;
  };

  // insert_comments_and_whitespace
  //
  static void insert_comments_and_whitespace(
    Insert_Comments_And_Whitespace_Parameters const& p)
  {
    for(auto token: anton::Range(p.begin, p.end)) {
      if(token.kind == Token_Kind::comment ||
         token.kind == Token_Kind::whitespace) {
        Source_Info source_info{
          .source_path = p.source_path,
          .line = token.line,
          .column = token.column,
          .offset = token.offset,
          .end_offset = token.end_offset,
        };
        // TODO: Fix syntax token source.
        Syntax_Token syntax_token(static_cast<Syntax_Node_Kind>(token.kind),
                                  anton::String(nullptr, nullptr, p.allocator),
                                  source_info);
        Insert_Location const insert =
          find_insert_location(p.tl_node, syntax_token);
        insert.array->insert(insert.location, ANTON_MOV(syntax_token));
      }
    }
  }

  anton::Expected<Array<SNOT>, Error>
  parse_tokens(Context const& ctx, anton::String_View const source_path,
               char8 const* const source_data, anton::Slice<Token const> tokens,
               Parse_Syntax_Options const options)
  {
    Parser parser(ctx.allocator, source_path, source_data,
                  Lexer(tokens.cbegin(), tokens.cend()));
    anton::Expected<Array<SNOT>, Error> ast = parser.build_syntax_tree();
    if(ast && options.include_whitespace_and_comments) {
      Insert_Comments_And_Whitespace_Parameters p{
        .allocator = ctx.allocator,
        .source_path = source_path,
        .tl_node = ast.value(),
        .begin = tokens.cbegin(),
        .end = tokens.cend(),
      };
      insert_comments_and_whitespace(p);
    }
    return ast;
  }
} // namespace vush
