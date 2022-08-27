#include <parser.hpp>

#include <anton/assert.hpp>
#include <anton/intrinsics.hpp>
#include <anton/memory.hpp>
#include <anton/optional.hpp>
#include <anton/string.hpp>
#include <anton/string7_stream.hpp>
#include <anton/string7_view.hpp>
#include <anton/type_traits/utility.hpp>

#include <lexer.hpp>
#include <memory.hpp>

// TODO: Figure out a way to match operators that use overlapping symbols (+ and +=) in a clean way.
// TODO: const types.
// TODO: add constructors (currently function call which will break if we use an array type).

namespace vush {
    using anton::Optional;
    using namespace anton::literals;

    // attributes
    static constexpr anton::String_View attr_workgroup = "workgroup";
    static constexpr anton::String_View attr_invariant = "invariant";
    static constexpr anton::String_View attr_flat = "flat";
    static constexpr anton::String_View attr_smooth = "smooth";
    static constexpr anton::String_View attr_noperspective = "noperspective";

    // stages

    static constexpr anton::String7_View stage_vertex = u8"vertex";
    static constexpr anton::String7_View stage_fragment = u8"fragment";
    static constexpr anton::String7_View stage_compute = u8"compute";

    class Lexer_State {
    public:
        Lexed_Source::const_token_iterator current;
        i64 offset;
    };

    // TODO: Place this comment somewhere
    // The source string is ASCII only, so String7 will be the exact same size as String,
    // but String7 will avoid all Unicode function calls and thus accelerate parsing.

    struct Token_Data {
        Token token;
        Token_Source_Info source;
    };

    struct Lexer {
    public:
        Lexer(Lexed_Source::const_token_iterator begin, Lexed_Source::const_token_iterator end): current(begin), end(end), begin(begin) {}

        void advance_token() {
            if(current != end) {
                ++current;
            }
        }

        [[nodiscard]] anton::Optional<Token_Data> peek_token() {
            if(current != end) {
                auto [token, source] = *current;
                return Token_Data{token, source};
            } else {
                return anton::null_optional;
            }
        }

        [[nodiscard]] anton::Optional<Token_Data> next_token() {
            if(current != end) {
                auto [token, source] = *current;
                ++current;
                return Token_Data{token, source};
            } else {
                return anton::null_optional;
            }
        }

        [[nodiscard]] bool match_eof() {
            ignore_whitespace_and_comments();
            return current == end;
        }

        void ignore_whitespace_and_comments() {
            while(current != end) {
                auto [token, source] = *current;
                if(token.type == Token_Type::comment | token.type == Token_Type::whitespace) {
                    ++current;
                } else {
                    break;
                }
            }
        }

        [[nodiscard]] Lexer_State get_current_state() {
            ignore_whitespace_and_comments();
            return {current, current - begin};
        }

        [[nodiscard]] Lexer_State get_current_state_noskip() {
            return {current, current - begin};
        }

        [[nodiscard]] bool is_state_end(Lexer_State const& state) const {
            return state.current == end;
        }

        void restore_state(Lexer_State const& state) {
            current = state.current;
        }

    private:
        Lexed_Source::const_token_iterator current;
        Lexed_Source::const_token_iterator end;
        Lexed_Source::const_token_iterator begin;
    };

    class Parser {
    public:
        Parser(Allocator* allocator, anton::String_View source_name, Lexer&& lexer)
            : _allocator(allocator), _source_name(source_name), _lexer(ANTON_MOV(lexer)) {}

        anton::Expected<Array<SNOT>, Error> build_syntax_tree() {
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

            Error to_error(anton::String_View source) const {
                return Error{.source = anton::String(source), .diagnostic = message, .extended_diagnostic = ""_s, .line = line, .column = column};
            }
        };

        Allocator* _allocator;
        anton::String_View _source_name;
        Lexer _lexer;
        Parse_Error _last_error;

        void set_error(anton::String_View const message, Lexer_State const& state) {
            if(_lexer.is_state_end(state)) {
                auto const& [token, source] = *(state.current - 1);
                _last_error.message = message;
                _last_error.line = source.end_line;
                _last_error.column = source.end_column;
                _last_error.stream_offset = source.end_offset;
            }

            if(state.offset >= _last_error.stream_offset) {
                auto const& [token, source] = *state.current;
                _last_error.message = message;
                _last_error.line = source.line;
                _last_error.column = source.column;
                _last_error.stream_offset = source.offset;
            }
        }

        void set_error(anton::String_View const message) {
            Lexer_State const state = _lexer.get_current_state_noskip();
            if(_lexer.is_state_end(state)) {
                auto const& [token, source] = *(state.current - 1);
                _last_error.message = message;
                _last_error.line = source.end_line;
                _last_error.column = source.end_column;
                _last_error.stream_offset = source.end_offset;
            }

            if(state.offset >= _last_error.stream_offset) {
                auto const& [token, source] = *state.current;
                _last_error.message = message;
                _last_error.line = source.line;
                _last_error.column = source.column;
                _last_error.stream_offset = source.offset;
            }
        }

        [[nodiscard]] Source_Info src_info(Lexer_State const& start, Lexer_State const& end) {
            auto const& [start_token, start_source] = *start.current;
            auto const& [end_token, end_source] = *(end.current - 1);
            return Source_Info{.source_path = _source_name,
                               .line = start_source.line,
                               .column = start_source.column,
                               .offset = start_source.offset,
                               .end_line = end_source.end_line,
                               .end_column = end_source.end_column,
                               .end_offset = end_source.end_offset};
        }

        // match
        // Matches the next token with a specified type in the token stream.
        //
        // Parameters:
        // type - the type of the token to match.
        //
        // Returns:
        // Syntax_Token of the specified type or null_optional
        // if the next token's type is not type.
        //
        [[nodiscard]] Optional<Syntax_Token> match(Token_Type const type) {
            Lexer_State const begin_state = _lexer.get_current_state_noskip();
            anton::Optional<Token_Data> const token_data = _lexer.peek_token();
            if(!token_data) {
                return anton::null_optional;
            }

            Token_Type const token_type = token_data->token.type;
            if(token_type == type) {
                _lexer.advance_token();
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                anton::String7_View const value = token_data->token.value;
                // Token_Type and Syntax_Node_Type have overlapping values, therefore we convert one to the other via a cast.
                Syntax_Node_Type const syntax_node_type = static_cast<Syntax_Node_Type>(token_type);
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Token(syntax_node_type, anton::String(value.begin(), value.end(), _allocator), source);
            } else {
                return anton::null_optional;
            }
        }

        // skipmatch
        // Matches the next token with a specified type in the token stream
        // skipping whitespace and comments. Does not match comments or whitespaces.
        //
        // Parameters:
        // type - the type of the token to match. Must not be
        //        Token_Type::comment or Token_Type::whitespace.
        //
        // Returns:
        // Syntax_Token of the specified type or null_optional
        // if the next token's type is not type.
        //
        [[nodiscard]] Optional<Syntax_Token> skipmatch(Token_Type const type) {
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
        // An identifier Syntax_Token if the value of the token
        // is the same as the parameter. null_optional otherwise.
        //
        [[nodiscard]] Optional<Syntax_Token> match(anton::String7_View const value) {
            Lexer_State const begin_state = _lexer.get_current_state_noskip();
            anton::Optional<Token_Data> const token_data = _lexer.peek_token();
            if(!token_data) {
                return anton::null_optional;
            }

            Token_Type const token_type = token_data->token.type;
            anton::String7_View const token_value = token_data->token.value;
            if(token_type == Token_Type::identifier && token_value == value) {
                _lexer.advance_token();
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Token(Syntax_Node_Type::identifier, anton::String(token_value.begin(), token_value.end(), _allocator), source);
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
        // An identifier Syntax_Token if the value of the token
        // is the same as the parameter. null_optional otherwise.
        //
        [[nodiscard]] Optional<Syntax_Token> skipmatch(anton::String7_View const value) {
            _lexer.ignore_whitespace_and_comments();
            return match(value);
        }

        // combine
        // Combines two tokens that appear next to each other in the source.
        // Tokens are combined in the order they are passed to the function.
        // Only value and source_info members of the tokens are combined.
        //
        // Parameters:
        //    result_type - Syntax_Node_Type to assign to the result token.
        // token1, token2 - tokens to combine.
        //
        // Returns:
        // Combined Syntax_Token with type member set to result_type.
        //
        [[nodiscard]] Syntax_Token combine(Syntax_Node_Type const result_type, Syntax_Token const& token1, Syntax_Token const& token2) {
            Source_Info source_info;
            // Both tokens have the same souce_path;
            source_info.source_path = token1.source_info.source_path;
            source_info.line = anton::math::min(token1.source_info.line, token2.source_info.line);
            source_info.column = anton::math::min(token1.source_info.column, token2.source_info.column);
            source_info.offset = anton::math::min(token1.source_info.offset, token2.source_info.offset);
            source_info.end_line = anton::math::max(token1.source_info.end_line, token2.source_info.end_line);
            source_info.end_column = anton::math::max(token1.source_info.end_column, token2.source_info.end_column);
            source_info.end_offset = anton::math::max(token1.source_info.end_offset, token2.source_info.end_offset);
            return Syntax_Token(result_type, anton::concat(_allocator, token1.value, token2.value), source_info);
        }

        // combine
        // Combines three tokens that appear next to each other in the source.
        // Tokens are combined in the order they are passed to the function.
        // Only value and source_info members of the tokens are combined.
        //
        // Parameters:
        //            result_type - Syntax_Node_Type to assign to the result token.
        // token1, token2, token3 - tokens to combine.
        //
        // Returns:
        // Combined Syntax_Token with type member set to result_type.
        //
        [[nodiscard]] Syntax_Token combine(Syntax_Node_Type const result_type, Syntax_Token const& token1, Syntax_Token const& token2,
                                           Syntax_Token const& token3) {
            Source_Info source_info;
            // All tokens have the same souce_path;
            source_info.source_path = token1.source_info.source_path;
            source_info.line = anton::math::min(token1.source_info.line, token2.source_info.line, token3.source_info.line);
            source_info.column = anton::math::min(token1.source_info.column, token2.source_info.column, token3.source_info.column);
            source_info.offset = anton::math::min(token1.source_info.offset, token2.source_info.offset, token3.source_info.offset);
            source_info.end_line = anton::math::max(token1.source_info.end_line, token2.source_info.end_line, token3.source_info.end_line);
            source_info.end_column = anton::math::max(token1.source_info.end_column, token2.source_info.end_column, token3.source_info.end_column);
            source_info.end_offset = anton::math::max(token1.source_info.end_offset, token2.source_info.end_offset, token3.source_info.end_offset);
            return Syntax_Token(result_type, anton::concat(_allocator, token1.value, token2.value, token3.value), source_info);
        }

        // match
        //
        [[nodiscard]] Optional<Syntax_Token> match(Syntax_Node_Type const result_type, Token_Type const tk_type1, Token_Type const tk_type2) {
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
        [[nodiscard]] Optional<Syntax_Token> match(Syntax_Node_Type const result_type, Token_Type const tk_type1, Token_Type const tk_type2,
                                                   Token_Type const tk_type3) {
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
        [[nodiscard]] Optional<Syntax_Token> skipmatch(Syntax_Node_Type const result_type, Token_Type const tk_type1, Token_Type const tk_type2) {
            _lexer.ignore_whitespace_and_comments();
            return match(result_type, tk_type1, tk_type2);
        }

        // skipmatch
        //
        [[nodiscard]] Optional<Syntax_Token> skipmatch(Syntax_Node_Type const result_type, Token_Type const tk_type1, Token_Type const tk_type2,
                                                       Token_Type const tk_type3) {
            _lexer.ignore_whitespace_and_comments();
            return match(result_type, tk_type1, tk_type2, tk_type3);
        }

        // match_setting_string
        // Matches and combines a series of tokens in the stream
        // that are allowed within tk_setting_string.
        //
        // Returns:
        // tk_setting_string or null_optional.
        //
        [[nodiscard]] Optional<Syntax_Token> match_setting_string() {
            Lexer_State const begin_state = _lexer.get_current_state_noskip();
            anton::String value{_allocator};
            // We require at least one token to be present.
            if(!_lexer.peek_token()) {
                return anton::null_optional;
            }

            while(true) {
                anton::Optional<Token_Data> const token_data = _lexer.peek_token();
                if(!token_data) {
                    break;
                }

                Token_Type const token_type = token_data->token.type;
                if(token_type == Token_Type::whitespace || token_type == Token_Type::comment || token_type == Token_Type::tk_colon ||
                   token_type == Token_Type::tk_brace_close || token_type == Token_Type::tk_brace_open) {
                    break;
                }

                value += anton::String_View{token_data->token.value.begin(), token_data->token.value.end()};
                _lexer.advance_token();
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Token(Syntax_Node_Type::tk_setting_string, ANTON_MOV(value), source);
        }

        Optional<Syntax_Node> try_declaration() {
            if(Optional decl_if = try_decl_if()) {
                return ANTON_MOV(*decl_if);
            }

            if(Optional decl_import = try_decl_import()) {
                return ANTON_MOV(*decl_import);
            }

            if(Optional decl_settings = try_decl_settings()) {
                return ANTON_MOV(*decl_settings);
            }

            if(Optional decl_struct = try_decl_struct()) {
                return ANTON_MOV(*decl_struct);
            }

            if(Optional decl_stage_function = try_decl_stage_function()) {
                return ANTON_MOV(*decl_stage_function);
            }

            if(Optional decl_function = try_decl_function()) {
                return ANTON_MOV(*decl_function);
            }

            if(Optional decl_constant = try_decl_constant()) {
                return ANTON_MOV(*decl_constant);
            }

            set_error(u8"expected declaration");
            return anton::null_optional;
        }

        // try_decl_block
        // Match declaration list enclosed in braces
        //   '{' declaration_list '}'
        //
        Optional<Syntax_Node> try_decl_block() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_brace_open = match(Token_Type::tk_brace_open)) {
                snots.push_back(ANTON_MOV(*tk_brace_open));
            } else {
                set_error(u8"expected '{'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            while(true) {
                if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                    snots.push_back(ANTON_MOV(*tk_brace_close));
                    break;
                }

                // TODO: Unsure whether we should match eof here to end the loop
                //       once we reach the end of the token stream.
                //       try_declaration in theory should error once it runs
                //       out of tokens to match ending the loop.

                if(Optional declaration = try_declaration()) {
                    snots.push_back(ANTON_MOV(*declaration));
                } else {
                    return anton::null_optional;
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::decl_block, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_decl_import() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_import = match(Token_Type::kw_import)) {
                snots.push_back(ANTON_MOV(*kw_import));
            } else {
                set_error(u8"expected 'import'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional string = try_expr_lt_string()) {
                snots.push_back(ANTON_MOV(*string));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::decl_import, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_decl_if() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_if = match(Token_Type::kw_if)) {
                snots.push_back(ANTON_MOV(*kw_if));
            } else {
                set_error(u8"expected 'if'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional condition = try_expression()) {
                snots.push_back(ANTON_MOV(*condition));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional then_branch = try_decl_block()) {
                snots.push_back(ANTON_MOV(*then_branch));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional kw_else = skipmatch(Token_Type::kw_else)) {
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
            return Syntax_Node(Syntax_Node_Type::decl_if, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_decl_constant() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_const = match(Token_Type::kw_const)) {
                snots.push_back(ANTON_MOV(*kw_const));
            } else {
                set_error(u8"expected 'const'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional type = try_type()) {
                snots.push_back(ANTON_MOV(*type));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error(u8"expected variable identifier");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_equals = skipmatch(Token_Type::tk_equals)) {
                snots.push_back(ANTON_MOV(*tk_equals));
                if(Optional initializer = try_expression()) {
                    snots.push_back(ANTON_MOV(*initializer));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error(u8"expected ';' after constant declaration");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::decl_constant, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_struct_member() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            // We allow many attribute lists to be present.
            // TODO: We've removed qualifier validation from the parser
            //       in order to postpone it to a later stage where we will be
            //       able to output more meaningful error diagnostics.
            while(true) {
                if(Optional attribute_list = try_attribute_list()) {
                    snots.push_back(ANTON_MOV(*attribute_list));
                } else {
                    break;
                }
            }

            if(Optional type = try_type()) {
                snots.push_back(ANTON_MOV(*type));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error(u8"expected variable identifier");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_equals = skipmatch(Token_Type::tk_equals)) {
                snots.push_back(ANTON_MOV(*tk_equals));
                if(Optional initializer = try_expression()) {
                    snots.push_back(ANTON_MOV(*initializer));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error(u8"expected ';' after member declaration");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::struct_member, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_struct_member_block() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_brace_open = match(Token_Type::tk_brace_open)) {
                snots.push_back(ANTON_MOV(*tk_brace_open));
            } else {
                set_error(u8"expected '{'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            while(true) {
                if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                    snots.push_back(ANTON_MOV(*tk_brace_close));
                    break;
                }

                // TODO: Unsure whether we should match eof here to end the loop
                //       once we reach the end of the token stream.
                //       try_struct_member in theory should error once it runs
                //       out of tokens to match ending the loop.

                if(Optional declaration = try_struct_member()) {
                    snots.push_back(ANTON_MOV(*declaration));
                } else {
                    return anton::null_optional;
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node{Syntax_Node_Type::struct_member_block, ANTON_MOV(snots), source};
        }

        Optional<Syntax_Node> try_decl_struct() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_struct = match(Token_Type::kw_struct)) {
                snots.push_back(ANTON_MOV(*kw_struct));
            } else {
                set_error(u8"expected 'struct'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error(u8"expected struct name");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional struct_member_block = try_struct_member_block()) {
                snots.push_back(ANTON_MOV(*struct_member_block));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::decl_struct, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_setting() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional key = match_setting_string()) {
                snots.push_back(ANTON_MOV(*key));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_colon = match(Token_Type::tk_colon)) {
                snots.push_back(ANTON_MOV(*tk_colon));
            } else {
                set_error(u8"expected ':'"_sv);
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

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
            return Syntax_Node(Syntax_Node_Type::setting_keyval, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_setting_block() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_brace_open = match(Token_Type::tk_brace_open)) {
                snots.push_back(ANTON_MOV(*tk_brace_open));
            } else {
                set_error(u8"expected '{'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            while(true) {
                if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                    snots.push_back(ANTON_MOV(*tk_brace_close));
                    break;
                }

                if(Optional statement = try_setting()) {
                    snots.push_back(ANTON_MOV(*statement));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::setting_block, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_decl_settings() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_settings = match(Token_Type::kw_settings)) {
                snots.push_back(ANTON_MOV(*kw_settings));
            } else {
                set_error("expected 'settings'"_sv);
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional pass = try_identifier()) {
                snots.push_back(ANTON_MOV(*pass));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional setting_block = try_setting_block()) {
                snots.push_back(ANTON_MOV(*setting_block));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::decl_settings, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_attribute() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            Optional identifier = match(Token_Type::identifier);
            if(!identifier) {
                set_error("expected identifier"_sv);
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(identifier->value == attr_workgroup) {
                Array<SNOT> snots{_allocator};
                snots.push_back(ANTON_MOV(*identifier));
                if(Optional tk_lparen = skipmatch(Token_Type::tk_lparen)) {
                    snots.push_back(ANTON_MOV(*tk_lparen));
                } else {
                    set_error(u8"expected '('");
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                if(Optional x = try_expr_lt_integer()) {
                    snots.push_back(ANTON_MOV(*x));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                if(Optional y_comma = skipmatch(Token_Type::tk_comma)) {
                    snots.push_back(ANTON_MOV(*y_comma));
                    if(Optional y = try_expr_lt_integer()) {
                        snots.push_back(ANTON_MOV(*y));
                    } else {
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }

                    if(Optional z_comma = skipmatch(Token_Type::tk_comma)) {
                        snots.push_back(ANTON_MOV(*z_comma));
                        if(Optional z = try_expr_lt_integer()) {
                            snots.push_back(ANTON_MOV(*z));
                        } else {
                            _lexer.restore_state(begin_state);
                            return anton::null_optional;
                        }
                    }
                }

                if(Optional tk_rparen = skipmatch(Token_Type::tk_rparen)) {
                    snots.push_back(ANTON_MOV(*tk_rparen));
                } else {
                    set_error(u8"expected ')'");
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::attr_workgroup, ANTON_MOV(snots), source);
            }

            if(identifier->value == attr_invariant) {
                Array<SNOT> snots{_allocator};
                snots.push_back(ANTON_MOV(*identifier));
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::attr_invariant, ANTON_MOV(snots), source);
            }

            if(identifier->value == attr_flat) {
                Array<SNOT> snots{_allocator};
                snots.push_back(ANTON_MOV(*identifier));
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::attr_flat, ANTON_MOV(snots), source);
            }

            if(identifier->value == attr_smooth) {
                Array<SNOT> snots{_allocator};
                snots.push_back(ANTON_MOV(*identifier));
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::attr_smooth, ANTON_MOV(snots), source);
            }

            if(identifier->value == attr_noperspective) {
                Array<SNOT> snots{_allocator};
                snots.push_back(ANTON_MOV(*identifier));
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::attr_noperspective, ANTON_MOV(snots), source);
            }

            // TODO: Correctly parse unrecognized attributes and reject them
            //       in the later stages of compilation to improve the diagnostics.

            set_error(u8"unrecognised attribute");
            _lexer.restore_state(begin_state);
            return anton::null_optional;
        }

        Optional<Syntax_Node> try_attribute_list() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_lbracket = match(Token_Type::tk_lbracket)) {
                snots.push_back(ANTON_MOV(*tk_lbracket));
            } else {
                set_error(u8"expected '['");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            // TODO: Validate empty attribute lists (allow or emit diagnostic).

            while(true) {
                if(Optional attribute = try_attribute()) {
                    snots.push_back(ANTON_MOV(*attribute));
                } else {
                    break;
                }

                if(Optional tk_comma = skipmatch(Token_Type::tk_comma)) {
                    snots.push_back(ANTON_MOV(*tk_comma));
                } else {
                    break;
                }
            }

            if(Optional tk_rbracket = skipmatch(Token_Type::tk_rbracket)) {
                snots.push_back(ANTON_MOV(*tk_rbracket));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::attribute_list, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_func_parameter() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional param_if = try_func_parameter_if()) {
                return param_if;
            }

            if(Optional parameter_type = try_type()) {
                snots.push_back(ANTON_MOV(*parameter_type));
            } else {
                set_error(u8"expected parameter type");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error(u8"expected parameter identifier");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional kw_from = skipmatch(Token_Type::kw_from)) {
                snots.push_back(ANTON_MOV(*kw_from));
                if(Optional source = try_identifier()) {
                    snots.push_back(ANTON_MOV(*source));
                } else {
                    set_error(u8"expected parameter source after 'from'");
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::func_parameter, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_func_parameter_if() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_if = match(Token_Type::kw_if)) {
                snots.push_back(ANTON_MOV(*kw_if));
            } else {
                set_error(u8"expected 'if'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional condition = try_expression()) {
                snots.push_back(ANTON_MOV(*condition));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_brace_open = skipmatch(Token_Type::tk_brace_open)) {
                snots.push_back(ANTON_MOV(*tk_brace_open));
            } else {
                set_error(u8"expected '{'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional true_param = try_func_parameter()) {
                snots.push_back(ANTON_MOV(*true_param));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                snots.push_back(ANTON_MOV(*tk_brace_close));
            } else {
                set_error(u8"expected '}'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional kw_else = skipmatch(Token_Type::kw_else)) {
                snots.push_back(ANTON_MOV(*kw_else));
                if(Optional param_if = try_func_parameter_if()) {
                    snots.push_back(ANTON_MOV(*param_if));
                } else {
                    if(Optional tk_brace_open = skipmatch(Token_Type::tk_brace_open)) {
                        snots.push_back(ANTON_MOV(*tk_brace_open));
                    } else {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }

                    if(Optional false_param = try_func_parameter()) {
                        snots.push_back(ANTON_MOV(*false_param));
                    } else {
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }

                    if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                        snots.push_back(ANTON_MOV(*tk_brace_close));
                    } else {
                        set_error(u8"expected '}'");
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::func_parameter_if, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_func_parameter_list() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_lparen = match(Token_Type::tk_lparen)) {
                snots.push_back(ANTON_MOV(*tk_lparen));
            } else {
                set_error(u8"expected '('");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            // Match closing paren to allow empty parameter lists.
            if(Optional tk_rparen = skipmatch(Token_Type::tk_rparen)) {
                snots.push_back(ANTON_MOV(*tk_rparen));
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::func_parameter_list, ANTON_MOV(snots), source);
            }

            while(true) {
                if(Optional param = try_func_parameter()) {
                    snots.push_back(ANTON_MOV(*param));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                if(Optional tk_comma = skipmatch(Token_Type::tk_comma)) {
                    snots.push_back(ANTON_MOV(*tk_comma));
                } else {
                    break;
                }
            }

            if(Optional tk_rparen = skipmatch(Token_Type::tk_rparen)) {
                snots.push_back(ANTON_MOV(*tk_rparen));
            } else {
                set_error(u8"expected ')' after function parameter list");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::func_parameter_list, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_decl_stage_function() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            // We allow many attribute lists to be present.
            while(true) {
                if(Optional attribute_list = try_attribute_list()) {
                    snots.push_back(ANTON_MOV(*attribute_list));
                } else {
                    break;
                }
            }

            if(Optional return_type = try_type()) {
                snots.push_back(ANTON_MOV(*return_type));
            } else {
                set_error(u8"expected type");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional pass_identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*pass_identifier));
            } else {
                set_error(u8"expected pass identifier");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_colon2 = skipmatch(Syntax_Node_Type::tk_colon2, Token_Type::tk_colon, Token_Type::tk_colon)) {
                snots.push_back(ANTON_MOV(*tk_colon2));
            } else {
                set_error(u8"expected '::'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional stg_vertex = skipmatch(stage_vertex)) {
                snots.push_back(ANTON_MOV(*stg_vertex));
            } else if(Optional stg_fragment = skipmatch(stage_fragment)) {
                snots.push_back(ANTON_MOV(*stg_fragment));
            } else if(Optional stg_compute = skipmatch(stage_compute)) {
                snots.push_back(ANTON_MOV(*stg_compute));
            } else {
                set_error(u8"expected stage type");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional parameter_list = try_func_parameter_list()) {
                snots.push_back(ANTON_MOV(*parameter_list));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional statements = try_stmt_block()) {
                snots.push_back(ANTON_MOV(*statements));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::decl_stage_function, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_decl_function() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            // We allow many attribute lists to be present.
            while(true) {
                if(Optional attribute_list = try_attribute_list()) {
                    snots.push_back(ANTON_MOV(*attribute_list));
                } else {
                    break;
                }
            }

            if(Optional return_type = try_type()) {
                snots.push_back(ANTON_MOV(*return_type));
            } else {
                set_error(u8"expected type");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error(u8"expected function identifier");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional parameter_list = try_func_parameter_list()) {
                snots.push_back(ANTON_MOV(*parameter_list));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional statements = try_stmt_block()) {
                snots.push_back(ANTON_MOV(*statements));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::decl_function, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_statement() {
            if(Optional stmt_empty = try_stmt_empty()) {
                return ANTON_MOV(*stmt_empty);
            }

            if(Optional stmt_variable = try_stmt_variable()) {
                return ANTON_MOV(*stmt_variable);
            }

            if(Optional stmt_block = try_stmt_block()) {
                return ANTON_MOV(*stmt_block);
            }

            if(Optional stmt_if = try_stmt_if()) {
                return ANTON_MOV(*stmt_if);
            }

            if(Optional stmt_switch = try_stmt_switch()) {
                return ANTON_MOV(*stmt_switch);
            }

            if(Optional stmt_for = try_stmt_for()) {
                return ANTON_MOV(*stmt_for);
            }

            if(Optional stmt_while = try_stmt_while()) {
                return ANTON_MOV(*stmt_while);
            }

            if(Optional stmt_do_while = try_stmt_do_while()) {
                return ANTON_MOV(*stmt_do_while);
            }

            if(Optional stmt_return = try_stmt_return()) {
                return ANTON_MOV(*stmt_return);
            }

            if(Optional stmt_break = try_stmt_break()) {
                return ANTON_MOV(*stmt_break);
            }

            if(Optional stmt_continue = try_stmt_continue()) {
                return ANTON_MOV(*stmt_continue);
            }

            if(Optional stmt_discard = try_stmt_discard()) {
                return ANTON_MOV(*stmt_discard);
            }

            if(Optional stmt_expression = try_stmt_expression()) {
                return ANTON_MOV(*stmt_expression);
            }

            set_error(u8"expected a statement");
            return anton::null_optional;
        }

        // try_empty_statament
        // Match the empty statement
        //   ';'
        //
        Optional<Syntax_Node> try_stmt_empty() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_semicolon = match(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error("expected ';'"_sv);
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_empty, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_variable() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional type = try_type()) {
                snots.push_back(ANTON_MOV(*type));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error(u8"expected variable identifier");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_equals = skipmatch(Token_Type::tk_equals)) {
                snots.push_back(ANTON_MOV(*tk_equals));
                if(Optional initializer = try_expression()) {
                    snots.push_back(ANTON_MOV(*initializer));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error(u8"expected ';' after variable declaration");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_variable, ANTON_MOV(snots), source);
        }

        // try_stmt_block
        // Match a list of statements enclosed in braces
        //   '{' statements '}'
        Optional<Syntax_Node> try_stmt_block() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_brace_open = match(Token_Type::tk_brace_open)) {
                snots.push_back(ANTON_MOV(*tk_brace_open));
            } else {
                set_error(u8"expected '{'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            while(true) {
                if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                    snots.push_back(ANTON_MOV(*tk_brace_close));
                    break;
                }

                if(Optional statement = try_statement()) {
                    snots.push_back(ANTON_MOV(*statement));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_block, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_if() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_if = match(Token_Type::kw_if)) {
                snots.push_back(ANTON_MOV(*kw_if));
            } else {
                set_error(u8"expected 'if'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional condition = try_expression()) {
                snots.push_back(ANTON_MOV(*condition));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional then_branch = try_stmt_block()) {
                snots.push_back(ANTON_MOV(*then_branch));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional kw_else = skipmatch(Token_Type::kw_else)) {
                snots.push_back(ANTON_MOV(*kw_else));
                if(Optional stmt_if = try_stmt_if()) {
                    snots.emplace_back(ANTON_MOV(*stmt_if));
                } else {
                    if(Optional else_branch = try_stmt_block()) {
                        snots.push_back(ANTON_MOV(*else_branch));
                    } else {
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_if, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_switch() {
            auto match_switch_arm_list = [this]() -> Optional<Syntax_Node> {
                auto match_switch_arm = [this]() -> Optional<Syntax_Node> {
                    Lexer_State const begin_state = _lexer.get_current_state();
                    Array<SNOT> snots{_allocator};
                    while(true) {
                        if(Optional kw_default = skipmatch(Token_Type::kw_default)) {
                            Source_Info const source_info = kw_default->source_info;
                            Syntax_Node expr_default(Syntax_Node_Type::expr_default, Array<SNOT>(_allocator, anton::variadic_construct, ANTON_MOV(*kw_default)),
                                                     source_info);
                            Syntax_Node label(Syntax_Node_Type::switch_arm_label, Array<SNOT>(_allocator, anton::variadic_construct, ANTON_MOV(expr_default)),
                                              source_info);
                            snots.push_back(ANTON_MOV(label));
                        } else if(Optional literal = try_expr_lt_integer()) {
                            Source_Info const source_info = literal->source_info;
                            snots.push_back(Syntax_Node(Syntax_Node_Type::switch_arm_label,
                                                        Array<SNOT>(_allocator, anton::variadic_construct, ANTON_MOV(*literal)), source_info));
                        } else {
                            _lexer.restore_state(begin_state);
                            return anton::null_optional;
                        }

                        if(Optional tk_comma = skipmatch(Token_Type::tk_comma)) {
                            snots.push_back(ANTON_MOV(*tk_comma));
                        } else {
                            break;
                        }
                    }

                    if(Optional tk_thick_arrow = skipmatch(Syntax_Node_Type::tk_thick_arrow, Token_Type::tk_equals, Token_Type::tk_rangle)) {
                        snots.push_back(ANTON_MOV(*tk_thick_arrow));
                    } else {
                        set_error(u8"expected '=>'"_sv);
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }

                    if(Optional stmt_block = try_stmt_block()) {
                        snots.push_back(ANTON_MOV(*stmt_block));
                    } else {
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }

                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const source = src_info(begin_state, end_state);
                    return Syntax_Node(Syntax_Node_Type::switch_arm, ANTON_MOV(snots), source);
                };

                Lexer_State const begin_state = _lexer.get_current_state();
                Array<SNOT> snots{_allocator};
                if(Optional tk_brace_close = match(Token_Type::tk_brace_open)) {
                    snots.push_back(ANTON_MOV(*tk_brace_close));
                } else {
                    set_error(u8"expected '{'");
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                while(true) {
                    if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                        snots.push_back(ANTON_MOV(*tk_brace_close));
                        break;
                    }

                    if(Optional switch_arm = match_switch_arm()) {
                        snots.push_back(ANTON_MOV(*switch_arm));
                    } else {
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::switch_arm_list, ANTON_MOV(snots), source);
            };

            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_switch = match(Token_Type::kw_switch)) {
                snots.push_back(ANTON_MOV(*kw_switch));
            } else {
                set_error(u8"expected 'switch'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional expression = try_expression()) {
                snots.push_back(ANTON_MOV(*expression));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional switch_arm_list = match_switch_arm_list()) {
                snots.push_back(ANTON_MOV(*switch_arm_list));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_switch, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_for() {
            auto match_for_stmt_variable = [this]() -> Optional<Syntax_Node> {
                Lexer_State const begin_state = _lexer.get_current_state();
                Array<SNOT> snots{_allocator};
                if(Optional type = try_type()) {
                    snots.push_back(ANTON_MOV(*type));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                if(Optional identifier = try_identifier()) {
                    snots.push_back(ANTON_MOV(*identifier));
                } else {
                    set_error("expected variable identifier");
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                if(Optional tk_equals = skipmatch(Token_Type::tk_equals)) {
                    snots.push_back(ANTON_MOV(*tk_equals));
                    if(Optional initializer = try_expression()) {
                        snots.push_back(ANTON_MOV(*initializer));
                    } else {
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::for_variable, ANTON_MOV(snots), source);
            };

            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_for = match(Token_Type::kw_for)) {
                snots.push_back(ANTON_MOV(*kw_for));
            } else {
                set_error("expected 'for'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_lparen = skipmatch(Token_Type::tk_lparen)) {
                set_error("unexpected '(' after 'for'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional for_stmt_variable = match_for_stmt_variable()) {
                snots.push_back(ANTON_MOV(*for_stmt_variable));
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error("expected ';'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional condition = try_expression()) {
                Source_Info const source_info = condition->source_info;
                snots.push_back(
                    Syntax_Node(Syntax_Node_Type::for_condition, Array<SNOT>(_allocator, anton::variadic_construct, ANTON_MOV(*condition)), source_info));
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error("expected ';'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional expression = try_expression()) {
                Source_Info const source_info = expression->source_info;
                snots.push_back(
                    Syntax_Node(Syntax_Node_Type::for_expression, Array<SNOT>(_allocator, anton::variadic_construct, ANTON_MOV(*expression)), source_info));
            }

            if(Optional statements = try_stmt_block()) {
                snots.push_back(ANTON_MOV(*statements));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_for, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_while() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_while = match(Token_Type::kw_while)) {
                snots.push_back(ANTON_MOV(*kw_while));
            } else {
                set_error("expected 'while'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional condition = try_expression()) {
                snots.push_back(ANTON_MOV(*condition));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional stmt_block = try_stmt_block()) {
                snots.push_back(ANTON_MOV(*stmt_block));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_while, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_do_while() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_do = match(Token_Type::kw_do)) {
                snots.push_back(ANTON_MOV(*kw_do));
            } else {
                set_error("expected 'do'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional stmt_block = try_stmt_block()) {
                snots.push_back(ANTON_MOV(*stmt_block));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional kw_while = skipmatch(Token_Type::kw_while)) {
                snots.push_back(ANTON_MOV(*kw_while));
            } else {
                set_error("expected 'while'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional condition = try_expression()) {
                snots.push_back(ANTON_MOV(*condition));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error("expected ';' after do-while statement");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_do_while, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_return() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_return = match(Token_Type::kw_return)) {
                snots.push_back(ANTON_MOV(*kw_return));
            } else {
                set_error(u8"expected 'return'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional return_expression = try_expression()) {
                Source_Info const source_info = return_expression->source_info;
                snots.push_back(Syntax_Node(Syntax_Node_Type::return_expression,
                                            Array<SNOT>(_allocator, anton::variadic_construct, ANTON_MOV(*return_expression)), source_info));
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error(u8"expected ';' after return statement");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_return, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_break() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_break = match(Token_Type::kw_break)) {
                snots.push_back(ANTON_MOV(*kw_break));
            } else {
                set_error(u8"expected 'break'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error(u8"expected ';' after break statement");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_break, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_continue() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_continue = match(Token_Type::kw_continue)) {
                snots.push_back(ANTON_MOV(*kw_continue));
            } else {
                set_error(u8"expected 'continue'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error(u8"expected ';' after continue statement");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_discard, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_discard() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_discard = match(Token_Type::kw_discard)) {
                snots.push_back(ANTON_MOV(*kw_discard));
            } else {
                set_error(u8"expected 'discard'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error(u8"expected ';' after discard statement");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_discard, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_stmt_expression() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional expression = try_expression()) {
                snots.push_back(ANTON_MOV(*expression));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_semicolon = skipmatch(Token_Type::tk_semicolon)) {
                snots.push_back(ANTON_MOV(*tk_semicolon));
            } else {
                set_error("expected ';'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::stmt_expression, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expression() {
            return try_expr_binary();
        }

        Optional<Syntax_Node> try_expr_binary() {
            // Parsing binary expressions consists primarily of repeatedly calling two procedures.
            // to construct the expression trees - insert_operator and insert_expression.
            // insert_expression - inserts an expression into a "free slot" (if a binary expression
            //   does not have either right or left expression, we consider it a "free slot").
            // insert_operator - does the actual work of building the binary expression tree.
            //   Depending on the associativity of an operator and its precedence, the following might happen:
            //   - an operator is left associative: if the operator has a **higher or equal** precedence than a node we are visiting,
            //     then we make the operator a new parent of the node. Otherwise we descend to the right subtree.
            //   - an operator is right associative: if the operator has a **higher** precedence than a node we are visiting,
            //     then we make the operator a new parent of the node. Otherwise we descend to the right subtree.
            //   The difference between left and right associative operators is subtle, yet it results in vastly different
            //   expression trees being constructed.
            // Precedence is ranked from 1 (highest).

            auto insert_operator = [_allocator = this->_allocator](Syntax_Node& root, Syntax_Token op) {
                enum Associativity { ASSOC_LEFT, ASSOC_RIGHT };

                auto get_associativity = [](Syntax_Node_Type type) -> Associativity {
                    switch(type) {
                        case Syntax_Node_Type::tk_equals:
                        case Syntax_Node_Type::tk_pluseq:
                        case Syntax_Node_Type::tk_minuseq:
                        case Syntax_Node_Type::tk_asteriskeq:
                        case Syntax_Node_Type::tk_slasheq:
                        case Syntax_Node_Type::tk_percenteq:
                        case Syntax_Node_Type::tk_shleq:
                        case Syntax_Node_Type::tk_shreq:
                        case Syntax_Node_Type::tk_ampeq:
                        case Syntax_Node_Type::tk_hateq:
                        case Syntax_Node_Type::tk_pipeeq:
                            return ASSOC_RIGHT;

                        default:
                            return ASSOC_LEFT;
                    }
                };

                auto get_precedence = [](Syntax_Node_Type type) -> i32 {
                    // Precedence values are exactly the same as in the GLSL Specification.
                    switch(type) {
                        case Syntax_Node_Type::tk_equals:
                        case Syntax_Node_Type::tk_pluseq:
                        case Syntax_Node_Type::tk_minuseq:
                        case Syntax_Node_Type::tk_asteriskeq:
                        case Syntax_Node_Type::tk_slasheq:
                        case Syntax_Node_Type::tk_percenteq:
                        case Syntax_Node_Type::tk_shleq:
                        case Syntax_Node_Type::tk_shreq:
                        case Syntax_Node_Type::tk_ampeq:
                        case Syntax_Node_Type::tk_hateq:
                        case Syntax_Node_Type::tk_pipeeq:
                            return 16;

                        case Syntax_Node_Type::tk_pipe2:
                            return 14;
                        case Syntax_Node_Type::tk_hat2:
                            return 13;
                        case Syntax_Node_Type::tk_amp2:
                            return 12;
                        case Syntax_Node_Type::tk_pipe:
                            return 11;
                        case Syntax_Node_Type::tk_hat:
                            return 10;
                        case Syntax_Node_Type::tk_amp:
                            return 9;

                        case Syntax_Node_Type::tk_eq2:
                        case Syntax_Node_Type::tk_neq:
                            return 8;

                        case Syntax_Node_Type::tk_langle:
                        case Syntax_Node_Type::tk_rangle:
                        case Syntax_Node_Type::tk_gteq:
                        case Syntax_Node_Type::tk_lteq:
                            return 7;

                        case Syntax_Node_Type::tk_shl:
                        case Syntax_Node_Type::tk_shr:
                            return 6;

                        case Syntax_Node_Type::tk_plus:
                        case Syntax_Node_Type::tk_minus:
                            return 5;

                        case Syntax_Node_Type::tk_asterisk:
                        case Syntax_Node_Type::tk_slash:
                        case Syntax_Node_Type::tk_percent:
                            return 4;

                        default:
                            ANTON_ASSERT(false, "invalid operator type");
                            ANTON_UNREACHABLE();
                    }
                };

                i32 const op_prec = get_precedence(op.type);
                i32 const op_assoc = get_associativity(op.type);
                Syntax_Node* dest_node = &root;
                while(true) {
                    // We will replace the node if it is not a binary expression.
                    if(dest_node->type != Syntax_Node_Type::expr_binary) {
                        break;
                    }

                    ANTON_ASSERT(dest_node->children.size() == 3, "expr_binary does not have exactly 3 children");
                    ANTON_ASSERT(dest_node->children[1].is_right(), "second SNOT of expr_binary is not a Syntax_Token");
                    Syntax_Token const& dest_op = dest_node->children[1].right();
                    i32 const dest_prec = get_precedence(dest_op.type);
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
                *dest_node = Syntax_Node(Syntax_Node_Type::expr_binary, ANTON_MOV(snots), Source_Info{});
            };

            auto insert_expression = [](Syntax_Node* root, Syntax_Node&& expr) -> void {
                // We only ever have to descend down the right children.
                while(root->children.size() == 3) {
                    ANTON_ASSERT(root->type == Syntax_Node_Type::expr_binary, "Syntax_Node is not expr_binary");
                    ANTON_ASSERT(root->children[2].is_left(), "third child of a binary expression node is not a Syntax_Node");
                    root = &root->children[2].left();
                }

                root->children.push_back(ANTON_MOV(expr));
            };

            auto match_binary_operator = [this]() -> Optional<Syntax_Token> {
                // We match operators in the following order:
                // 1. Arithmetic assignments
                // 2. Logical
                // 3. Equality
                // 4. Relational X-equal
                // 5. Shift
                // 6. Remaining operators (assignment, bitwise, relational, additive, multiplicative)

                if(Optional op = match(Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_ampeq, Token_Type::tk_amp, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_pipeeq, Token_Type::tk_pipe, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_hateq, Token_Type::tk_hat, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_shleq, Token_Type::tk_langle, Token_Type::tk_langle, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_shreq, Token_Type::tk_rangle, Token_Type::tk_rangle, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_percenteq, Token_Type::tk_percent, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_slasheq, Token_Type::tk_slash, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_asteriskeq, Token_Type::tk_asterisk, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_pluseq, Token_Type::tk_plus, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_minuseq, Token_Type::tk_minus, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }

                if(Optional op = match(Syntax_Node_Type::tk_amp2, Token_Type::tk_amp, Token_Type::tk_amp)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_pipe2, Token_Type::tk_pipe, Token_Type::tk_pipe)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_hat2, Token_Type::tk_hat, Token_Type::tk_hat)) {
                    return ANTON_MOV(op);
                }

                if(Optional op = match(Syntax_Node_Type::tk_eq2, Token_Type::tk_equals, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_neq, Token_Type::tk_bang, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }

                if(Optional op = match(Syntax_Node_Type::tk_lteq, Token_Type::tk_langle, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_gteq, Token_Type::tk_rangle, Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }

                if(Optional op = match(Syntax_Node_Type::tk_shl, Token_Type::tk_langle, Token_Type::tk_langle)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Syntax_Node_Type::tk_shr, Token_Type::tk_rangle, Token_Type::tk_rangle)) {
                    return ANTON_MOV(op);
                }

                if(Optional op = match(Token_Type::tk_equals)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_amp)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_hat)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_pipe)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_langle)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_rangle)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_plus)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_minus)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_asterisk)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_slash)) {
                    return ANTON_MOV(op);
                }
                if(Optional op = match(Token_Type::tk_percent)) {
                    return ANTON_MOV(op);
                }

                return anton::null_optional;
            };

            Lexer_State const begin_state = _lexer.get_current_state();
            Optional root_expr = try_expr_prefix();
            if(!root_expr) {
                return anton::null_optional;
            }

            while(true) {
                if(Optional op = match_binary_operator()) {
                    insert_operator(*root_expr, ANTON_MOV(*op));
                } else {
                    break;
                }

                if(Optional expr = try_expr_prefix()) {
                    insert_expression(&root_expr.value(), ANTON_MOV(*expr));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }
            }

            auto recalculate_source_info = [](auto& recalculate_source_info, Syntax_Node& node) -> void {
                if(node.type != Syntax_Node_Type::expr_binary) {
                    return;
                }

                ANTON_ASSERT(node.children[0].is_left(), "the left child of expr_binary is not a Syntax_Node");
                ANTON_ASSERT(node.children[2].is_left(), "the right child of expr_binary is not a Syntax_Node");
                recalculate_source_info(recalculate_source_info, node.children[0].left());
                recalculate_source_info(recalculate_source_info, node.children[2].left());
                Source_Info const& left = node.children[0].left().source_info;
                Source_Info const& right = node.children[2].left().source_info;
                // Both left and right have the same souce_path;
                node.source_info.source_path = left.source_path;
                node.source_info.line = left.line;
                node.source_info.column = left.column;
                node.source_info.offset = left.offset;
                node.source_info.end_line = right.end_line;
                node.source_info.end_column = right.end_column;
                node.source_info.end_offset = right.end_offset;
            };

            // expr_binary nodes do not have their source_info members filled properly because
            // it is impossible to do so while inserting (we do not know the final right child).
            // We have to manually fix up the source_info members.
            recalculate_source_info(recalculate_source_info, *root_expr);

            return root_expr;
        }

        Optional<Syntax_Node> try_expr_prefix() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional plus2 = match(Syntax_Node_Type::tk_plus2, Token_Type::tk_plus, Token_Type::tk_plus)) {
                snots.push_back(ANTON_MOV(*plus2));
            } else if(Optional minus2 = match(Syntax_Node_Type::tk_minus2, Token_Type::tk_minus, Token_Type::tk_minus)) {
                snots.push_back(ANTON_MOV(*minus2));
            } else if(Optional plus = match(Token_Type::tk_plus)) {
                snots.push_back(ANTON_MOV(*plus));
            } else if(Optional minus = match(Token_Type::tk_minus)) {
                snots.push_back(ANTON_MOV(*minus));
            } else if(Optional bang = match(Token_Type::tk_bang)) {
                snots.push_back(ANTON_MOV(*bang));
            } else if(Optional tilde = match(Token_Type::tk_tilde)) {
                snots.push_back(ANTON_MOV(*tilde));
            } else {
                return try_expr_postfix();
            }

            if(Optional expr = try_expr_prefix()) {
                snots.push_back(ANTON_MOV(*expr));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_prefix, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_postfix() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Optional expr = try_primary_expression();
            if(!expr) {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            while(true) {
                if(Optional tk_dot = skipmatch(Token_Type::tk_dot)) {
                    Optional identifier = try_identifier();
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
                    expr = Syntax_Node(Syntax_Node_Type::expr_member_access, ANTON_MOV(snots), source);
                } else if(Optional tk_lbracket = skipmatch(Token_Type::tk_lbracket)) {
                    Optional index = try_expression();
                    if(!index) {
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }

                    Optional tk_rbracket = skipmatch(Token_Type::tk_rbracket);
                    if(!tk_rbracket) {
                        set_error(u8"expected ']'");
                        _lexer.restore_state(begin_state);
                        return anton::null_optional;
                    }

                    Array<SNOT> snots{_allocator};
                    snots.push_back(ANTON_MOV(*tk_lbracket));
                    snots.push_back(ANTON_MOV(*index));
                    snots.push_back(ANTON_MOV(*tk_rbracket));
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const source = src_info(begin_state, end_state);
                    expr = Syntax_Node(Syntax_Node_Type::expr_array_access, ANTON_MOV(snots), source);
                } else if(Optional plus2 = skipmatch(Syntax_Node_Type::tk_plus2, Token_Type::tk_plus, Token_Type::tk_plus)) {
                    Array<SNOT> snots{_allocator};
                    snots.push_back(ANTON_MOV(*expr));
                    snots.push_back(ANTON_MOV(*plus2));
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const source = src_info(begin_state, end_state);
                    expr = Syntax_Node(Syntax_Node_Type::expr_postfix, ANTON_MOV(snots), source);
                } else if(Optional minus2 = skipmatch(Syntax_Node_Type::tk_minus2, Token_Type::tk_minus, Token_Type::tk_minus)) {
                    Array<SNOT> snots{_allocator};
                    snots.push_back(ANTON_MOV(*expr));
                    snots.push_back(ANTON_MOV(*minus2));
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const source = src_info(begin_state, end_state);
                    expr = Syntax_Node(Syntax_Node_Type::expr_postfix, ANTON_MOV(snots), source);
                } else {
                    break;
                }
            }

            return expr;
        }

        Optional<Syntax_Node> try_primary_expression() {
            if(Optional expr_parentheses = try_expr_parentheses()) {
                return ANTON_MOV(expr_parentheses);
            }

            if(Optional expr_reinterpret = try_expr_reinterpret()) {
                return ANTON_MOV(expr_reinterpret);
            }

            if(Optional expr_if = try_expr_if()) {
                return ANTON_MOV(expr_if);
            }

            if(Optional lt_float = try_expr_lt_float()) {
                return ANTON_MOV(lt_float);
            }

            if(Optional lt_integer = try_expr_lt_integer()) {
                return ANTON_MOV(lt_integer);
            }

            if(Optional lt_bool = try_expr_lt_bool()) {
                return ANTON_MOV(lt_bool);
            }

            if(Optional expr_call = try_expr_call()) {
                return ANTON_MOV(expr_call);
            }

            if(Optional expr_identifier = try_expr_identifier()) {
                return ANTON_MOV(expr_identifier);
            }

            return anton::null_optional;
        }

        Optional<Syntax_Node> try_expr_parentheses() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_rparen = match(Token_Type::tk_lparen)) {
                snots.push_back(ANTON_MOV(*tk_rparen));
            } else {
                set_error(u8"expected '('");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional expr = try_expression()) {
                snots.push_back(ANTON_MOV(*expr));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_rparen = skipmatch(Token_Type::tk_rparen)) {
                snots.push_back(ANTON_MOV(*tk_rparen));
            } else {
                set_error(u8"expected ')'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_parentheses, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_reinterpret() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_reinterpret = match(Token_Type::kw_reinterpret)) {
                snots.push_back(ANTON_MOV(*kw_reinterpret));
            } else {
                set_error(u8"expected 'reinterpret'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_langle = skipmatch(Token_Type::tk_langle)) {
                snots.push_back(ANTON_MOV(*tk_langle));
            } else {
                set_error(u8"expected '<'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional target_type = try_type()) {
                snots.push_back(ANTON_MOV(*target_type));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_rangle = skipmatch(Token_Type::tk_rangle)) {
                snots.push_back(ANTON_MOV(*tk_rangle));
            } else {
                set_error(u8"expected '>'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_lparen = skipmatch(Token_Type::tk_lparen)) {
                snots.push_back(ANTON_MOV(*tk_lparen));
            } else {
                set_error(u8"expected '('");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional source = try_expression()) {
                snots.push_back(ANTON_MOV(*source));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_comma = skipmatch(Token_Type::tk_comma)) {
                snots.push_back(ANTON_MOV(*tk_comma));
            } else {
                set_error(u8"expected ','");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional index = try_expression()) {
                snots.push_back(ANTON_MOV(*index));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_rparen = skipmatch(Token_Type::tk_rparen)) {
                snots.push_back(ANTON_MOV(*tk_rparen));
            } else {
                set_error(u8"expected ')'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_reinterpret, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_block() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_brace_open = match(Token_Type::tk_brace_open)) {
                snots.push_back(ANTON_MOV(*tk_brace_open));
            } else {
                set_error(u8"expected '{'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional expr = try_expression()) {
                snots.push_back(ANTON_MOV(*expr));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional tk_brace_close = skipmatch(Token_Type::tk_brace_close)) {
                snots.push_back(ANTON_MOV(*tk_brace_close));
            } else {
                set_error(u8"expected '}'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_block, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_if() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional kw_if = match(Token_Type::kw_if)) {
                snots.push_back(ANTON_MOV(*kw_if));
            } else {
                set_error(u8"expected 'if'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional condition = try_expression()) {
                snots.push_back(ANTON_MOV(*condition));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional then_branch = try_expr_block()) {
                snots.push_back(ANTON_MOV(*then_branch));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional kw_else = skipmatch(Token_Type::kw_else)) {
                snots.push_back(ANTON_MOV(*kw_else));
            } else {
                set_error(u8"expected 'else");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

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
            return Syntax_Node(Syntax_Node_Type::expr_if, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_call_arg_list() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional tk_lparen = match(Token_Type::tk_lparen)) {
                snots.push_back(ANTON_MOV(*tk_lparen));
            } else {
                set_error(u8"expected '('");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            // Match closing paren to allow empty parameter lists.
            if(Optional tk_rparen = skipmatch(Token_Type::tk_rparen)) {
                snots.push_back(ANTON_MOV(*tk_rparen));
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const source = src_info(begin_state, end_state);
                return Syntax_Node(Syntax_Node_Type::call_arg_list, ANTON_MOV(snots), source);
            }

            while(true) {
                if(Optional arg = try_expression()) {
                    snots.push_back(ANTON_MOV(*arg));
                } else {
                    _lexer.restore_state(begin_state);
                    return anton::null_optional;
                }

                if(Optional tk_comma = skipmatch(Token_Type::tk_comma)) {
                    snots.push_back(ANTON_MOV(*tk_comma));
                } else {
                    break;
                }
            }

            if(Optional tk_rparen = skipmatch(Token_Type::tk_rparen)) {
                snots.push_back(ANTON_MOV(*tk_rparen));
            } else {
                set_error(u8"expected ')'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::call_arg_list, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_call() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional identifier = try_identifier()) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error(u8"expected identifier");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            if(Optional arg_list = try_call_arg_list()) {
                snots.push_back(ANTON_MOV(*arg_list));
            } else {
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_call, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_identifier() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional identifier = match(Token_Type::identifier)) {
                snots.push_back(ANTON_MOV(*identifier));
            } else {
                set_error("expected identifier"_sv);
                return anton::null_optional;
            }
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_identifier, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_lt_float() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional lt_float = match(Token_Type::lt_float)) {
                snots.push_back(ANTON_MOV(*lt_float));
            } else {
                set_error("expected float literal"_sv);
                return anton::null_optional;
            }

            // We validate suffix on the literals in later stages.
            if(Optional suffix = match(Token_Type::identifier)) {
                snots.push_back(ANTON_MOV(*suffix));
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_lt_float, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_lt_integer() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional lt_bin_integer = match(Token_Type::lt_bin_integer)) {
                snots.push_back(ANTON_MOV(*lt_bin_integer));
            } else if(Optional lt_dec_integer = match(Token_Type::lt_dec_integer)) {
                snots.push_back(ANTON_MOV(*lt_dec_integer));
            } else if(Optional lt_hex_integer = match(Token_Type::lt_hex_integer)) {
                snots.push_back(ANTON_MOV(*lt_hex_integer));
            } else {
                set_error("expected integer literal"_sv);
                return anton::null_optional;
            }

            // We validate suffix on the literals in later stages.
            if(Optional suffix = match(Token_Type::identifier)) {
                snots.push_back(ANTON_MOV(*suffix));
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_lt_integer, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_lt_bool() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional lt_token = match(Token_Type::lt_bool)) {
                snots.push_back(ANTON_MOV(*lt_token));
            } else {
                set_error("expected bool literal"_sv);
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_lt_bool, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Node> try_expr_lt_string() {
            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            if(Optional lt_token = match(Token_Type::lt_string)) {
                snots.push_back(ANTON_MOV(*lt_token));
            } else {
                set_error("expected string literal"_sv);
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::expr_lt_string, ANTON_MOV(snots), source);
        }

        Optional<Syntax_Token> try_identifier() {
            return skipmatch(Token_Type::identifier);
        }

        Optional<Syntax_Node> try_type() {
            auto match_base_type = [this]() -> Optional<Syntax_Node> {
                Lexer_State const begin_state = _lexer.get_current_state();
                Array<SNOT> snots{_allocator};
                if(Optional type = match(Token_Type::identifier)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    anton::String_View const type_name = type->value;
                    snots.push_back(ANTON_MOV(*type));
                    if(anton::Optional<ast::GLSL_Type> res = ast::enumify_glsl_type(type_name); res) {
                        Source_Info const source = src_info(begin_state, end_state);
                        return Syntax_Node(Syntax_Node_Type::type_builtin, ANTON_MOV(snots), source);
                    } else {
                        Source_Info const source = src_info(begin_state, end_state);
                        return Syntax_Node(Syntax_Node_Type::type_user_defined, ANTON_MOV(snots), source);
                    }
                } else {
                    set_error(u8"expected type identifier");
                    return anton::null_optional;
                }
            };

            Lexer_State const begin_state = _lexer.get_current_state();
            Array<SNOT> snots{_allocator};
            Optional base = match_base_type();
            if(!base) {
                return anton::null_optional;
            }

            // We have to explicitly ignore whitespaces between the base type and the bracket.
            Optional tk_lbracket = skipmatch(Token_Type::tk_lbracket);
            if(!tk_lbracket) {
                return base;
            }

            snots.push_back(ANTON_MOV(*base));
            snots.push_back(ANTON_MOV(*tk_lbracket));

            if(Optional array_size = try_expr_lt_integer()) {
                snots.push_back(ANTON_MOV(*array_size));
            }

            if(Optional tk_rbracket = skipmatch(Token_Type::tk_rbracket)) {
                snots.push_back(ANTON_MOV(*tk_rbracket));
            } else {
                set_error(u8"expected ']'");
                _lexer.restore_state(begin_state);
                return anton::null_optional;
            }

            // We don't support nested array types (yet), so we don't continue checking for brackets.
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const source = src_info(begin_state, end_state);
            return Syntax_Node(Syntax_Node_Type::type_array, ANTON_MOV(snots), source);
        }
    };

    struct Insert_Location {
        Array<SNOT>* array;
        Array<SNOT>::iterator location;
    };

    // find_insert_location
    //
    [[nodiscard]] static Insert_Location find_insert_location(Array<SNOT>& snots, Syntax_Token const& token) {
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
            if(token.source_info.offset >= snot_src.offset && token.source_info.end_offset < snot_src.end_offset) {
                Syntax_Node& node = i->left();
                return find_insert_location(node.children, token);
            }
        }

        return {.array = &snots, .location = i};
    }

    struct Insert_Comments_And_Whitespace_Parameters {
        Allocator* const allocator;
        anton::String_View source_path;
        Array<SNOT>& tl_node;
        Lexed_Source::const_token_iterator const begin;
        Lexed_Source::const_token_iterator const end;
    };

    // insert_comments_and_whitespace
    //
    static void insert_comments_and_whitespace(Insert_Comments_And_Whitespace_Parameters const& p) {
        for(auto [token, source]: anton::Range(p.begin, p.end)) {
            if(token.type == Token_Type::comment || token.type == Token_Type::whitespace) {
                Source_Info source_info{
                    .source_path = p.source_path,
                    .line = source.line,
                    .column = source.column,
                    .offset = source.offset,
                    .end_line = source.end_line,
                    .end_column = source.end_column,
                    .end_offset = source.end_offset,
                };
                Syntax_Token syntax_token(static_cast<Syntax_Node_Type>(token.type), anton::String(token.value.begin(), token.value.end(), p.allocator),
                                          source_info);
                Insert_Location const insert = find_insert_location(p.tl_node, syntax_token);
                insert.array->insert(insert.location, ANTON_MOV(syntax_token));
            }
        }
    }

    anton::Expected<Array<SNOT>, Error> parse_source_to_syntax_tree(Allocator* const allocator, anton::String_View const source_path,
                                                                    anton::String_View const source_code, Parse_Syntax_Options const options) {
        anton::Expected<Lexed_Source, Error> lex_result = lex_source(allocator, anton::String7_View{source_code.bytes_begin(), source_code.bytes_end()});
        if(!lex_result) {
            return {anton::expected_error, ANTON_MOV(lex_result.error())};
        }
        Parser parser(allocator, source_path, Lexer(lex_result->cbegin(), lex_result->cend()));
        anton::Expected<Array<SNOT>, Error> ast = parser.build_syntax_tree();
        if(ast && options.include_whitespace_and_comments) {
            Insert_Comments_And_Whitespace_Parameters p{
                .allocator = allocator,
                .source_path = source_path,
                .tl_node = ast.value(),
                .begin = lex_result->cbegin(),
                .end = lex_result->cend(),
            };
            insert_comments_and_whitespace(p);
        }
        return ast;
    }
} // namespace vush
