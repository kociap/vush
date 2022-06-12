#include <parser.hpp>

#include <lexer.hpp>
#include <memory.hpp>

#include <anton/optional.hpp>
#include <anton/string7_stream.hpp>
#include <anton/string7_view.hpp>

// TODO: Figure out a way to match operators that use overlapping symbols (+ and +=) in a clean way.
// TODO: const types.
// TODO: add constructors (currently function call which will break if we use an array type).

namespace vush {
    using namespace anton::literals;

    // attributes
    static constexpr anton::String7_View attrib_workgroup = u8"workgroup";

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

    enum struct Operator {
        plus,
        minus,
        multiply,
        divide,
        remainder,
        logic_and,
        bit_and,
        logic_or,
        logic_xor,
        bit_or,
        bit_xor,
        logic_not,
        bit_not,
        bit_lshift,
        bit_rshift,
        equal,
        not_equal,
        less,
        greater,
        less_equal,
        greater_equal,
        assign,
        increment,
        decrement,
        compound_plus,
        compound_minus,
        compound_multiply,
        compound_divide,
        compound_remainder,
        compound_bit_and,
        compound_bit_or,
        compound_bit_xor,
        compound_bit_lshift,
        compound_bit_rshift,
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

        [[nodiscard]] anton::Optional<Token_Data> match_noskip(Token_Type type) {
            if(current != end) {
                auto [token, source] = *current;
                if(token.type == type) {
                    ++current;
                    return Token_Data{token, source};
                }
            }
            return anton::null_optional;
        }

        [[nodiscard]] anton::Optional<Token_Data> match(Token_Type type) {
            ignore_whitespace_and_comments();
            if(current != end) {
                auto [token, source] = *current;
                if(token.type == type) {
                    ++current;
                    return Token_Data{token, source};
                }
            }
            return anton::null_optional;
        }

        [[nodiscard]] anton::Optional<Token_Data> match_identifier(anton::String7_View const identifier) {
            ignore_whitespace_and_comments();
            if(current != end) {
                auto [token, source] = *current;
                if(token.type == Token_Type::identifier && token.value == identifier) {
                    ++current;
                    return Token_Data{token, source};
                }
            }
            return anton::null_optional;
        }

        [[nodiscard]] bool match_operator(Operator op) {
            switch(op) {
                case Operator::plus: {
                    return match(Token_Type::tk_plus);
                }

                case Operator::minus: {
                    return match(Token_Type::tk_minus);
                }

                case Operator::multiply: {
                    return match(Token_Type::tk_asterisk);
                }

                case Operator::divide: {
                    return match(Token_Type::tk_slash);
                }

                case Operator::remainder: {
                    return match(Token_Type::tk_percent);
                }

                case Operator::logic_and: {
                    return match_many(Token_Type::tk_amp, Token_Type::tk_amp);
                }

                case Operator::bit_and: {
                    return match(Token_Type::tk_amp);
                }

                case Operator::logic_or: {
                    return match_many(Token_Type::tk_pipe, Token_Type::tk_pipe);
                }

                case Operator::logic_xor: {
                    return match_many(Token_Type::tk_hat, Token_Type::tk_hat);
                }

                case Operator::bit_or: {
                    return match(Token_Type::tk_pipe);
                }

                case Operator::bit_xor: {
                    return match(Token_Type::tk_hat);
                }

                case Operator::logic_not: {
                    return match(Token_Type::tk_bang);
                }

                case Operator::bit_not: {
                    return match(Token_Type::tk_tilde);
                }

                case Operator::bit_lshift: {
                    return match_many(Token_Type::tk_langle, Token_Type::tk_langle);
                }

                case Operator::bit_rshift: {
                    return match_many(Token_Type::tk_rangle, Token_Type::tk_rangle);
                }

                case Operator::equal: {
                    return match_many(Token_Type::tk_equals, Token_Type::tk_equals);
                }

                case Operator::not_equal: {
                    return match_many(Token_Type::tk_bang, Token_Type::tk_equals);
                }

                case Operator::less: {
                    return match(Token_Type::tk_langle);
                }

                case Operator::greater: {
                    return match(Token_Type::tk_rangle);
                }

                case Operator::less_equal: {
                    return match_many(Token_Type::tk_langle, Token_Type::tk_equals);
                }

                case Operator::greater_equal: {
                    return match_many(Token_Type::tk_rangle, Token_Type::tk_equals);
                }

                case Operator::assign: {
                    return match(Token_Type::tk_equals);
                }

                case Operator::increment: {
                    return match_many(Token_Type::tk_plus, Token_Type::tk_plus);
                }

                case Operator::decrement: {
                    return match_many(Token_Type::tk_minus, Token_Type::tk_minus);
                }

                case Operator::compound_plus: {
                    return match_many(Token_Type::tk_plus, Token_Type::tk_equals);
                }

                case Operator::compound_minus: {
                    return match_many(Token_Type::tk_minus, Token_Type::tk_equals);
                }

                case Operator::compound_multiply: {
                    return match_many(Token_Type::tk_asterisk, Token_Type::tk_equals);
                }

                case Operator::compound_divide: {
                    return match_many(Token_Type::tk_slash, Token_Type::tk_equals);
                }

                case Operator::compound_remainder: {
                    return match_many(Token_Type::tk_percent, Token_Type::tk_equals);
                }

                case Operator::compound_bit_and: {
                    return match_many(Token_Type::tk_amp, Token_Type::tk_equals);
                }

                case Operator::compound_bit_or: {
                    return match_many(Token_Type::tk_pipe, Token_Type::tk_equals);
                }

                case Operator::compound_bit_xor: {
                    return match_many(Token_Type::tk_hat, Token_Type::tk_equals);
                }

                case Operator::compound_bit_lshift: {
                    return match_many(Token_Type::tk_langle, Token_Type::tk_langle, Token_Type::tk_equals);
                }

                case Operator::compound_bit_rshift: {
                    return match_many(Token_Type::tk_rangle, Token_Type::tk_rangle, Token_Type::tk_equals);
                }
            }
        }

        [[nodiscard]] bool match_many(Token_Type const type1, Token_Type const type2) {
            ignore_whitespace_and_comments();
            Lexed_Source::const_token_iterator const backup = current;
            Token_Type types[] = {type1, type2};
            for(Token_Type type: anton::Slice{types, types + 2}) {
                if(current == end) {
                    current = backup;
                    return false;
                }

                auto [token, source] = *current;
                if(token.type != type) {
                    current = backup;
                    return false;
                }

                ++current;
            }
            return true;
        }

        [[nodiscard]] bool match_many(Token_Type const type1, Token_Type const type2, Token_Type const type3) {
            ignore_whitespace_and_comments();
            Lexed_Source::const_token_iterator const backup = current;
            Token_Type types[] = {type1, type2, type3};
            for(Token_Type type: anton::Slice{types, types + 3}) {
                if(current == end) {
                    current = backup;
                    return false;
                }

                auto [token, source] = *current;
                if(token.type != type) {
                    current = backup;
                    return false;
                }

                ++current;
            }
            return true;
        }

        [[nodiscard]] bool match_eof() {
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

#define ALLOC(type, ...) allocate_owning<type>(_allocator, __VA_ARGS__)
#define OWNING_NULL(type) nullptr

    class Parser {
    public:
        Parser(Allocator* allocator, anton::String_View source_name, Lexer&& lexer)
            : _allocator(allocator), _source_name(source_name), _lexer(ANTON_MOV(lexer)) {}

        anton::Expected<Declaration_List, Error> build_ast() {
            Declaration_List ast{_allocator};
            while(true) {
                _lexer.ignore_whitespace_and_comments();
                if(_lexer.match_eof()) {
                    return {anton::expected_value, ANTON_MOV(ast)};
                }
                if(Owning_Ptr declaration = try_declaration()) {
                    ast.emplace_back(ANTON_MOV(declaration));
                } else {
                    return {anton::expected_error, _last_error.to_error(_source_name)};
                }
            }
        }

        anton::Expected<Declaration_List, Error> parse_builtin_functions() {
            Declaration_List builtin_functions{_allocator};
            while(true) {
                _lexer.ignore_whitespace_and_comments();
                if(_lexer.match_eof()) {
                    return {anton::expected_value, ANTON_MOV(builtin_functions)};
                }
                if(Owning_Ptr fn = try_function_declaration()) {
                    fn->builtin = true;
                    fn->source_info.line = 1;
                    fn->source_info.end_line = 1;
                    builtin_functions.emplace_back(ANTON_MOV(fn));
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

        Source_Info src_info([[maybe_unused]] Lexer_State const& start, [[maybe_unused]] Lexer_State const& end) {
            auto const& [start_token, start_source] = *start.current;
            auto const& [end_token, end_source] = *(end.current - 1);
            return Source_Info{_source_name,    start_source.line, start_source.column, start_source.offset,
                               end_source.line, end_source.column, end_source.offset};
        }

        Owning_Ptr<Declaration> try_declaration() {
            if(Owning_Ptr declaration_if = try_declaration_if()) {
                return declaration_if;
            }

            if(Owning_Ptr import_declaration = try_import_declaration()) {
                return import_declaration;
            }

            if(Owning_Ptr settings_declaration = try_settings_declaration()) {
                return settings_declaration;
            }

            if(Owning_Ptr struct_declaration = try_struct_declaration()) {
                return struct_declaration;
            }

            if(Owning_Ptr pass_stage = try_pass_stage_declaration()) {
                return pass_stage;
            }

            if(Owning_Ptr function_declaration = try_function_declaration()) {
                return function_declaration;
            }

            if(Owning_Ptr variable_declaration = try_variable_declaration()) {
                return variable_declaration;
            }

            if(Owning_Ptr constant = try_constant_declaration()) {
                return constant;
            }

            set_error(u8"expected declaration");
            return OWNING_NULL(Declaration);
        }

        Owning_Ptr<Declaration_If> try_declaration_if() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_if)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Declaration_If);
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Declaration_If);
            }

            if(!_lexer.match(Token_Type::tk_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Declaration_If);
            }

            Declaration_List true_declarations{_allocator};
            while(!_lexer.match(Token_Type::tk_brace_close)) {
                _lexer.ignore_whitespace_and_comments();
                if(_lexer.match_eof()) {
                    set_error(u8"unexpected end of file");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Declaration_If);
                }

                if(Owning_Ptr declaration = try_declaration()) {
                    true_declarations.emplace_back(ANTON_MOV(declaration));
                } else {
                    return OWNING_NULL(Declaration_If);
                }
            }

            Declaration_List false_declarations{_allocator};
            if(_lexer.match(Token_Type::kw_else)) {
                if(Owning_Ptr if_declaration = try_declaration_if()) {
                    false_declarations.emplace_back(ANTON_MOV(if_declaration));
                } else {
                    if(!_lexer.match(Token_Type::tk_brace_open)) {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Declaration_If);
                    }

                    while(!_lexer.match(Token_Type::tk_brace_close)) {
                        if(Owning_Ptr declaration = try_declaration()) {
                            false_declarations.emplace_back(ANTON_MOV(declaration));
                        } else {
                            _lexer.restore_state(state_backup);
                            return OWNING_NULL(Declaration_If);
                        }
                    }
                }
            }
            return ALLOC(Declaration_If, ANTON_MOV(condition), ANTON_MOV(true_declarations), ANTON_MOV(false_declarations),
                         src_info(state_backup, state_backup));
        }

        Owning_Ptr<Import_Declaration> try_import_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_import)) {
                set_error(u8"expected 'import'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Import_Declaration);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            if(Owning_Ptr string = try_string_literal()) {
                return ALLOC(Import_Declaration, ANTON_MOV(string), src);
            } else {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Import_Declaration);
            }
        }

        Owning_Ptr<Variable_Declaration> try_variable_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr var_type = try_type();
            if(!var_type) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Variable_Declaration);
            }

            Owning_Ptr var_name = try_identifier();
            if(!var_name) {
                set_error(u8"expected variable name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Variable_Declaration);
            }

            Owning_Ptr<Expression> initializer = OWNING_NULL(Expression);
            if(_lexer.match(Token_Type::tk_equals)) {
                initializer = try_expression();
                if(!initializer) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Variable_Declaration);
                }
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error(u8"expected ';' after variable declaration");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Variable_Declaration);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Variable_Declaration, ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), src);
        }

        Owning_Ptr<Constant_Declaration> try_constant_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_const)) {
                set_error(u8"expected 'const'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Constant_Declaration);
            }

            Owning_Ptr var_type = try_type();
            if(!var_type) {
                set_error(u8"expected type");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Constant_Declaration);
            }

            Owning_Ptr var_name = try_identifier();
            if(!var_name) {
                set_error(u8"expected variable name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Constant_Declaration);
            }

            Owning_Ptr<Expression> initializer = OWNING_NULL(Expression);
            if(_lexer.match(Token_Type::tk_equals)) {
                initializer = try_expression();
                if(!initializer) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Constant_Declaration);
                }
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error(u8"expected ';' after constant declaration");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Constant_Declaration);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Constant_Declaration, ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), src);
        }

        Owning_Ptr<Struct_Member> try_struct_member() {
            Lexer_State const state_backup = _lexer.get_current_state();
            bool has_invariant = false;
            bool has_interpolation = false;
            Interpolation interpolation = Interpolation::none;
            while(true) {
                if(_lexer.match(Token_Type::kw_invariant)) {
                    if(has_invariant) {
                        set_error(u8"multiple invariant qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Struct_Member);
                    }

                    has_invariant = true;
                    continue;
                }

                if(_lexer.match(Token_Type::kw_smooth)) {
                    if(has_interpolation) {
                        set_error(u8"multiple interpolation qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Struct_Member);
                    }

                    interpolation = Interpolation::smooth;
                    has_interpolation = true;
                    continue;
                }

                if(_lexer.match(Token_Type::kw_flat)) {
                    if(has_interpolation) {
                        set_error(u8"multiple interpolation qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Struct_Member);
                    }

                    interpolation = Interpolation::flat;
                    has_interpolation = true;
                    continue;
                }

                if(_lexer.match(Token_Type::kw_noperspective)) {
                    if(has_interpolation) {
                        set_error(u8"multiple interpolation qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Struct_Member);
                    }

                    interpolation = Interpolation::noperspective;
                    has_interpolation = true;
                    continue;
                }

                break;
            }

            Owning_Ptr var_type = try_type();
            if(!var_type) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Struct_Member);
            }

            Owning_Ptr var_name = try_identifier();
            if(!var_name) {
                set_error(u8"expected member name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Struct_Member);
            }

            Owning_Ptr<Expression> initializer(nullptr, _allocator);
            if(_lexer.match(Token_Type::tk_equals)) {
                initializer = try_expression();
                if(!initializer) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Struct_Member);
                }
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error(u8"expected ';'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Struct_Member);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Struct_Member, ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), interpolation, has_invariant, src);
        }

        Owning_Ptr<Struct_Declaration> try_struct_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_struct)) {
                set_error(u8"expected 'struct'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Struct_Declaration);
            }

            Owning_Ptr struct_name = try_identifier();
            if(!struct_name) {
                set_error(u8"expected struct name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Struct_Declaration);
            }

            if(!_lexer.match(Token_Type::tk_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Struct_Declaration);
            }

            Array<Owning_Ptr<Struct_Member>> members(_allocator);
            while(!_lexer.match(Token_Type::tk_brace_close)) {
                if(Owning_Ptr decl = try_struct_member()) {
                    members.emplace_back(ANTON_MOV(decl));
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Struct_Declaration);
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Struct_Declaration, ANTON_MOV(struct_name), ANTON_MOV(members), src);
        }

        Owning_Ptr<Settings_Declaration> try_settings_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();

            auto match_string = [this, &state_backup]() -> anton::Optional<anton::String> {
                _lexer.ignore_whitespace_and_comments();
                anton::String string(_allocator);
                while(true) {
                    anton::Optional<Token_Data> token = _lexer.peek_token();
                    if(!token) {
                        set_error(u8"unexpected end of file"_sv);
                        _lexer.restore_state(state_backup);
                        return anton::null_optional;
                    }

                    Token_Type const type = token->token.type;
                    if(type == Token_Type::whitespace || type == Token_Type::comment || type == Token_Type::tk_colon || type == Token_Type::tk_brace_close ||
                       type == Token_Type::tk_brace_open) {
                        return {ANTON_MOV(string)};
                    } else {
                        anton::String_View const value{token->token.value.begin(), token->token.value.end()};
                        string += value;
                        _lexer.advance_token();
                    }
                }
            };

            auto match_nested_settings = [this, &state_backup, &match_string](auto match_nested_settings, Array<Setting_Key_Value>& settings,
                                                                              anton::String const& setting_name) -> bool {
                while(true) {
                    if(_lexer.match(Token_Type::tk_brace_close)) {
                        return true;
                    }

                    auto key_name = match_string();
                    if(!key_name) {
                        _lexer.restore_state(state_backup);
                        return false;
                    }

                    if(key_name.value().size_bytes() == 0) {
                        set_error(u8"expected a name"_sv);
                        _lexer.restore_state(state_backup);
                        return false;
                    }

                    if(!_lexer.match(Token_Type::tk_colon)) {
                        set_error(u8"expected ':'"_sv);
                        _lexer.restore_state(state_backup);
                        return false;
                    }

                    anton::String setting_key(setting_name);
                    if(setting_key.size_bytes() > 0) {
                        setting_key += u8"_"_sv;
                    }
                    setting_key += key_name.value();

                    if(_lexer.match(Token_Type::tk_brace_open)) {
                        if(!match_nested_settings(match_nested_settings, settings, setting_key)) {
                            _lexer.restore_state(state_backup);
                            return false;
                        }
                    } else {
                        auto value = match_string();
                        if(!value) {
                            _lexer.restore_state(state_backup);
                            return false;
                        }

                        if(value.value().size_bytes() == 0) {
                            set_error(u8"expected value string after ':'"_sv);
                            _lexer.restore_state(state_backup);
                            return false;
                        }

                        settings.emplace_back(Setting_Key_Value{ANTON_MOV(setting_key), ANTON_MOV(value.value())});
                    }
                }
            };

            if(!_lexer.match(Token_Type::kw_settings)) {
                set_error(u8"expected 'settings'"_sv);
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Settings_Declaration);
            }

            Owning_Ptr pass_name = try_identifier();
            if(!pass_name) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Settings_Declaration);
            }

            Owning_Ptr settings_declaration =
                ALLOC(Settings_Declaration, ANTON_MOV(pass_name), Array<Setting_Key_Value>(_allocator), src_info(state_backup, state_backup));
            if(!_lexer.match(Token_Type::tk_brace_open)) {
                set_error(u8"expected '{'"_sv);
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Settings_Declaration);
            }

            if(!match_nested_settings(match_nested_settings, settings_declaration->settings, anton::String(_allocator))) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Settings_Declaration);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            settings_declaration->source_info = src;
            return settings_declaration;
        }

        Owning_Ptr<Attribute> try_attribute() {
            Lexer_State const state_backup = _lexer.get_current_state();

            // workgroup attribute
            if(_lexer.match_identifier(attrib_workgroup)) {
                if(!_lexer.match(Token_Type::tk_lparen)) {
                    set_error(u8"expected '('");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Attribute);
                }

                Owning_Ptr x = try_integer_literal();
                if(!x) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Attribute);
                }

                Owning_Ptr<Integer_Literal> y = OWNING_NULL(Integer_Literal);
                Owning_Ptr<Integer_Literal> z = OWNING_NULL(Integer_Literal);
                if(_lexer.match(Token_Type::tk_comma)) {
                    y = try_integer_literal();
                    if(!y) {
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Attribute);
                    }

                    if(_lexer.match(Token_Type::tk_comma)) {
                        z = try_integer_literal();
                        if(!z) {
                            _lexer.restore_state(state_backup);
                            return OWNING_NULL(Attribute);
                        }
                    }
                }

                if(!_lexer.match(Token_Type::tk_rparen)) {
                    set_error(u8"expected ')'");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Attribute);
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                return ALLOC(Workgroup_Attribute, ANTON_MOV(x), ANTON_MOV(y), ANTON_MOV(z), src);
            }

            // TODO: Add a diagnostic for unrecognized attributes
            set_error(u8"expected identifier");
            _lexer.restore_state(state_backup);
            return OWNING_NULL(Attribute);
        }

        anton::Optional<Attribute_List> try_attribute_list() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::tk_lbracket)) {
                set_error(u8"expected '['");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            if(_lexer.match(Token_Type::tk_rbracket)) {
                set_error(u8"empty attribute list (TODO: PROVIDE A PROPER DIAGNOSTIC MESSAGE WITH EXACT LOCATION AND CODE SNIPPET)");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            Attribute_List attributes{_allocator};
            while(true) {
                Owning_Ptr attribute = try_attribute();
                if(!attribute) {
                    _lexer.restore_state(state_backup);
                    return anton::null_optional;
                }

                attributes.emplace_back(ANTON_MOV(attribute));

                if(!_lexer.match(Token_Type::tk_comma)) {
                    break;
                }
            }

            if(!_lexer.match(Token_Type::tk_rbracket)) {
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            return ANTON_MOV(attributes);
        }

        // Owning_Ptr<Image_Layout_Qualifier> try_image_layout_qualifier() {
        //     Image_Layout_Type qualifiers[] = {Image_Layout_Type::rgba32f,
        //                                       Image_Layout_Type::rgba16f,
        //                                       Image_Layout_Type::rg32f,
        //                                       Image_Layout_Type::rg16f,
        //                                       Image_Layout_Type::r11f_g11f_b10f,
        //                                       Image_Layout_Type::r32f,
        //                                       Image_Layout_Type::r16f,
        //                                       Image_Layout_Type::rgba16,
        //                                       Image_Layout_Type::rgb10_a2,
        //                                       Image_Layout_Type::rgba8,
        //                                       Image_Layout_Type::rg16,
        //                                       Image_Layout_Type::rg8,
        //                                       Image_Layout_Type::r16,
        //                                       Image_Layout_Type::r8,
        //                                       Image_Layout_Type::rgba16_snorm,
        //                                       Image_Layout_Type::rgba8_snorm,
        //                                       Image_Layout_Type::rg16_snorm,
        //                                       Image_Layout_Type::rg8_snorm,
        //                                       Image_Layout_Type::r16_snorm,
        //                                       Image_Layout_Type::r8_snorm,
        //                                       Image_Layout_Type::rgba32i,
        //                                       Image_Layout_Type::rgba16i,
        //                                       Image_Layout_Type::rgba8i,
        //                                       Image_Layout_Type::rg32i,
        //                                       Image_Layout_Type::rg16i,
        //                                       Image_Layout_Type::rg8i,
        //                                       Image_Layout_Type::r32i,
        //                                       Image_Layout_Type::r16i,
        //                                       Image_Layout_Type::r8i,
        //                                       Image_Layout_Type::rgba32ui,
        //                                       Image_Layout_Type::rgba16ui,
        //                                       Image_Layout_Type::rgb10_a2ui,
        //                                       Image_Layout_Type::rgba8ui,
        //                                       Image_Layout_Type::rg32ui,
        //                                       Image_Layout_Type::rg16ui,
        //                                       Image_Layout_Type::rg8ui,
        //                                       Image_Layout_Type::r32ui,
        //                                       Image_Layout_Type::r16ui,
        //                                       Image_Layout_Type::r8ui};

        //     Lexer_State const state_backup = _lexer.get_current_state();
        //     constexpr i64 array_size = sizeof(qualifiers) / sizeof(Image_Layout_Type);
        //     for(i64 i = 0; i < array_size; ++i) {
        //         anton::String_View const string{stringify(qualifiers[i])};
        //         anton::String7_View const string7{string.bytes_begin(), string.bytes_end()};
        //         if(_lexer.match(string7, true)) {
        //             Lexer_State const end_state = _lexer.get_current_state_noskip();
        //             Source_Info const src = src_info(state_backup, end_state);
        //             return ALLOC(Image_Layout_Qualifier, qualifiers[i], src);
        //         }
        //     }

        //     return OWNING_NULL(Image_Layout_Qualifier);
        // }

        Owning_Ptr<Pass_Stage_Declaration> try_pass_stage_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();

            // Parse the attribute lists. We allow many attribute lists
            // to be present on a stage declaration. We merge the lists into
            // a single attribute array.
            Attribute_List attributes{_allocator};
            while(true) {
                anton::Optional<Attribute_List> attributes_result = try_attribute_list();
                if(!attributes_result) {
                    break;
                }

                for(auto& attribute: attributes_result.value()) {
                    attributes.emplace_back(ANTON_MOV(attribute));
                }
            }

            Owning_Ptr return_type = try_type();
            if(!return_type) {
                set_error(u8"expected type");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Pass_Stage_Declaration);
            }

            Owning_Ptr pass = try_identifier();
            if(!pass) {
                set_error(u8"expected pass name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Pass_Stage_Declaration);
            }

            bool const double_colon = _lexer.match(Token_Type::tk_colon) && _lexer.match(Token_Type::tk_colon);
            if(!double_colon) {
                set_error(u8"expected '::' after pass name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Pass_Stage_Declaration);
            }

            static constexpr anton::String7_View stage_types_strings[] = {stage_vertex, stage_fragment, stage_compute};
            static constexpr Stage_Type stage_types[] = {Stage_Type::vertex, Stage_Type::fragment, Stage_Type::compute};

            Stage_Type stage_type;
            {
                bool found = false;
                for(i64 i = 0; i < 3; ++i) {
                    if(_lexer.match_identifier(stage_types_strings[i])) {
                        stage_type = stage_types[i];
                        found = true;
                        break;
                    }
                }

                if(!found) {
                    set_error(u8"expected stage type");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Pass_Stage_Declaration);
                }
            }

            anton::Optional<Parameter_List> parameter_list = try_function_param_list();
            if(!parameter_list) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Pass_Stage_Declaration);
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Pass_Stage_Declaration);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Pass_Stage_Declaration, ANTON_MOV(attributes), ANTON_MOV(return_type), ANTON_MOV(pass), stage_type, ANTON_MOV(parameter_list.value()),
                         ANTON_MOV(statements.value()), src);
        }

        Owning_Ptr<Function_Declaration> try_function_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();

            // Parse the attribute lists. We allow many attribute lists
            // to be present on a function declaration. We merge the lists
            // into a single attribute array.
            Attribute_List attributes{_allocator};
            while(true) {
                anton::Optional<Attribute_List> attributes_result = try_attribute_list();
                if(!attributes_result) {
                    break;
                }

                for(auto& attribute: attributes_result.value()) {
                    attributes.emplace_back(ANTON_MOV(attribute));
                }
            }

            Owning_Ptr return_type = try_type();
            if(!return_type) {
                set_error(u8"expected type");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Declaration);
            }

            Owning_Ptr name = try_identifier();
            if(!name) {
                set_error(u8"expected function name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Declaration);
            }

            auto param_list = try_function_param_list();
            if(!param_list) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Declaration);
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Declaration);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Function_Declaration, ANTON_MOV(attributes), ANTON_MOV(return_type), ANTON_MOV(name), ANTON_MOV(param_list.value()),
                         ANTON_MOV(statements.value()), src);
        }

        anton::Optional<Parameter_List> try_function_param_list() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::tk_lparen)) {
                set_error(u8"expected '('");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            if(_lexer.match(Token_Type::tk_rparen)) {
                return Parameter_List{_allocator};
            }

            Parameter_List param_list{_allocator};
            // Match parameters
            do {
                Owning_Ptr param = try_function_parameter();
                if(param) {
                    param_list.emplace_back(ANTON_MOV(param));
                } else {
                    _lexer.restore_state(state_backup);
                    return anton::null_optional;
                }
            } while(_lexer.match(Token_Type::tk_comma));

            if(!_lexer.match(Token_Type::tk_rparen)) {
                set_error(u8"expected ')' after function parameter list");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            return ANTON_MOV(param_list);
        }

        Owning_Ptr<Function_Parameter_Node> try_function_parameter() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(Owning_Ptr param_if = try_function_param_if()) {
                return param_if;
            }

            // Owning_Ptr image_layout = try_image_layout_qualifier();

            Owning_Ptr parameter_type = try_type();
            if(!parameter_type) {
                set_error(u8"expected parameter type");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Parameter);
            }

            Owning_Ptr identifier = try_identifier();
            if(!identifier) {
                set_error(u8"expected parameter name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Parameter);
            }

            Owning_Ptr<Identifier> source = OWNING_NULL(Identifier);
            if(_lexer.match(Token_Type::kw_from)) {
                source = try_identifier();
                if(!source) {
                    set_error(u8"expected parameter source after 'from'");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Function_Parameter);
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Function_Parameter, ANTON_MOV(identifier), ANTON_MOV(parameter_type), ANTON_MOV(source), OWNING_NULL(Image_Layout_Qualifier), src);
            // return ALLOC(Function_Parameter, ANTON_MOV(identifier), ANTON_MOV(parameter_type), ANTON_MOV(source), ANTON_MOV(image_layout), src);
        }

        Owning_Ptr<Function_Param_If> try_function_param_if() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_if)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Param_If);
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Param_If);
            }

            if(!_lexer.match(Token_Type::tk_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Param_If);
            }

            Owning_Ptr true_param = try_function_parameter();
            if(!true_param) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Param_If);
            }

            if(!_lexer.match(Token_Type::tk_brace_close)) {
                set_error(u8"expected '}'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Param_If);
            }

            if(_lexer.match(Token_Type::kw_else)) {
                if(Owning_Ptr param_if = try_function_param_if()) {
                    return ALLOC(Function_Param_If, ANTON_MOV(condition), ANTON_MOV(true_param), ANTON_MOV(param_if), src_info(state_backup, state_backup));
                } else {
                    if(!_lexer.match(Token_Type::tk_brace_open)) {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Function_Param_If);
                    }

                    Owning_Ptr false_param = try_function_parameter();
                    if(!false_param) {
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Function_Param_If);
                    }

                    if(!_lexer.match(Token_Type::tk_brace_close)) {
                        set_error(u8"expected '}'");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Function_Param_If);
                    }

                    return ALLOC(Function_Param_If, ANTON_MOV(condition), ANTON_MOV(true_param), ANTON_MOV(false_param), src_info(state_backup, state_backup));
                }
            } else {
                return ALLOC(Function_Param_If, ANTON_MOV(condition), ANTON_MOV(true_param), nullptr, src_info(state_backup, state_backup));
            }
        }

        // try_empty_statament
        // Match empty statement ';'.
        //
        bool try_empty_statement() {
            return _lexer.match(Token_Type::tk_semicolon);
        }

        Owning_Ptr<Statement> try_statement() {
            if(Owning_Ptr block_statement = try_block_statement()) {
                return block_statement;
            }

            if(Owning_Ptr if_statement = try_if_statement()) {
                return if_statement;
            }

            if(Owning_Ptr switch_statement = try_switch_statement()) {
                return switch_statement;
            }

            if(Owning_Ptr for_statement = try_for_statement()) {
                return for_statement;
            }

            if(Owning_Ptr while_statement = try_while_statement()) {
                return while_statement;
            }

            if(Owning_Ptr do_while_statement = try_do_while_statement()) {
                return do_while_statement;
            }

            if(Owning_Ptr return_statement = try_return_statement()) {
                return return_statement;
            }

            if(Owning_Ptr break_statement = try_break_statement()) {
                return break_statement;
            }

            if(Owning_Ptr continue_statement = try_continue_statement()) {
                return continue_statement;
            }

            if(Owning_Ptr discard_statement = try_discard_statement()) {
                return discard_statement;
            }

            if(Owning_Ptr decl = try_variable_declaration()) {
                Source_Info const src = decl->source_info;
                Owning_Ptr decl_stmt = ALLOC(Declaration_Statement, ANTON_MOV(decl), src);
                return decl_stmt;
            }

            if(Owning_Ptr decl = try_constant_declaration()) {
                Source_Info const src = decl->source_info;
                Owning_Ptr decl_stmt = ALLOC(Declaration_Statement, ANTON_MOV(decl), src);
                return decl_stmt;
            }

            if(Owning_Ptr expr_stmt = try_expression_statement()) {
                return expr_stmt;
            }

            set_error(u8"expected a statement");
            return OWNING_NULL(Statement);
        }

        // try_braced_statement_list
        // Match '{' statements '}'
        anton::Optional<Statement_List> try_braced_statement_list() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::tk_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            Statement_List body{_allocator};
            while(true) {
                if(_lexer.match(Token_Type::tk_brace_close)) {
                    return {ANTON_MOV(body)};
                }

                if(try_empty_statement()) {
                    continue;
                }

                Owning_Ptr<Statement> statement = try_statement();
                if(statement) {
                    body.emplace_back(ANTON_MOV(statement));
                    continue;
                }

                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }
        }

        Owning_Ptr<Type> try_type() {
            Lexer_State const state_backup = _lexer.get_current_state();
            auto type = _lexer.match(Token_Type::identifier);
            if(!type) {
                set_error(u8"expected type identifier");
                return OWNING_NULL(Type);
            }

            anton::String_View const type_name{type->token.value.begin(), type->token.value.end()};
            Owning_Ptr<Type> base_type = OWNING_NULL(Type);
            if(anton::Optional<Builtin_GLSL_Type> res = enumify_builtin_glsl_type(type_name); res) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                base_type = ALLOC(Builtin_Type, res.value(), src);
            } else {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                base_type = ALLOC(User_Defined_Type, anton::String{type_name, _allocator}, src);
            }

            if(!_lexer.match(Token_Type::tk_lbracket)) {
                return base_type;
            } else {
                Owning_Ptr array_size = try_integer_literal();
                if(!_lexer.match(Token_Type::tk_rbracket)) {
                    set_error(u8"expected ']'");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Type);
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                // We don't support nested array types (yet), so we don't continue checking for brackets.
                return ALLOC(Array_Type, ANTON_MOV(base_type), ANTON_MOV(array_size), src);
            }
        }

        Owning_Ptr<Block_Statement> try_block_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Block_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Block_Statement, ANTON_MOV(statements.value()), src);
        }

        Owning_Ptr<If_Statement> try_if_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_if)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(If_Statement);
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(If_Statement);
            }

            anton::Optional<Statement_List> true_statements = try_braced_statement_list();
            if(!true_statements) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(If_Statement);
            }

            Statement_List false_statements{_allocator};
            if(_lexer.match(Token_Type::kw_else)) {
                if(Owning_Ptr if_statement = try_if_statement()) {
                    false_statements.emplace_back(ANTON_MOV(if_statement));
                } else {
                    anton::Optional<Statement_List> statements = try_braced_statement_list();
                    if(!statements) {
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(If_Statement);
                    }

                    false_statements = ANTON_MOV(statements.value());
                }
            }
            return ALLOC(If_Statement, ANTON_MOV(condition), ANTON_MOV(true_statements.value()), ANTON_MOV(false_statements),
                         src_info(state_backup, state_backup));
        }

        Owning_Ptr<Switch_Statement> try_switch_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_switch)) {
                set_error(u8"expected 'switch'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Switch_Statement);
            }

            Owning_Ptr match_expression = try_expression();
            if(!match_expression) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Switch_Statement);
            }

            if(!_lexer.match(Token_Type::tk_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Switch_Statement);
            }

            Array<Owning_Ptr<Case_Statement>> cases(_allocator);
            while(true) {
                Lexer_State const case_state = _lexer.get_current_state();

                if(_lexer.match(Token_Type::tk_brace_close)) {
                    break;
                }

                Expression_List labels{_allocator};
                do {
                    Lexer_State const label_state = _lexer.get_current_state();
                    if(_lexer.match(Token_Type::kw_default)) {
                        Lexer_State const end_state = _lexer.get_current_state_noskip();
                        Source_Info const src = src_info(label_state, end_state);
                        labels.emplace_back(ALLOC(Default_Expression, src));
                    } else if(Owning_Ptr literal = try_integer_literal()) {
                        labels.emplace_back(ANTON_MOV(literal));
                    } else {
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Switch_Statement);
                    }
                } while(_lexer.match(Token_Type::tk_comma));

                bool const arrow = _lexer.match(Token_Type::tk_equals) && _lexer.match(Token_Type::tk_rangle);
                if(!arrow) {
                    set_error(u8"expected '=>'"_sv);
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Switch_Statement);
                }

                anton::Optional<Statement_List> statements = try_braced_statement_list();
                if(!statements) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Switch_Statement);
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(case_state, end_state);
                Owning_Ptr case_statement = ALLOC(Case_Statement, ANTON_MOV(labels), ANTON_MOV(statements.value()), src);
                cases.emplace_back(ANTON_MOV(case_statement));
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Switch_Statement, ANTON_MOV(match_expression), ANTON_MOV(cases), src);
        }

        Owning_Ptr<For_Statement> try_for_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_for)) {
                set_error("expected 'for'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(For_Statement);
            }

            if(_lexer.match(Token_Type::tk_lparen)) {
                set_error("unexpected '(' after 'for'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(For_Statement);
            }

            // Match variable

            Owning_Ptr<Variable_Declaration> variable_declaration = OWNING_NULL(Variable_Declaration);
            if(!_lexer.match(Token_Type::tk_semicolon)) {
                Lexer_State const var_decl_state = _lexer.get_current_state();
                Owning_Ptr var_type = try_type();
                if(!var_type) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(For_Statement);
                }

                Owning_Ptr var_name = try_identifier();
                if(!var_name) {
                    set_error("expected variable name");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(For_Statement);
                }

                Owning_Ptr<Expression> initializer = OWNING_NULL(Expression);
                if(_lexer.match(Token_Type::tk_equals)) {
                    initializer = try_expression();
                    if(!initializer) {
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(For_Statement);
                    }
                }

                if(!_lexer.match(Token_Type::tk_semicolon)) {
                    set_error("expected ';' in 'for' statement");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(For_Statement);
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(var_decl_state, end_state);
                variable_declaration = ALLOC(Variable_Declaration, ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), src);
            }

            Owning_Ptr<Expression> condition = OWNING_NULL(Expression);
            if(!_lexer.match(Token_Type::tk_semicolon)) {
                condition = try_expression();
                if(!condition) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(For_Statement);
                }

                if(!_lexer.match(Token_Type::tk_semicolon)) {
                    set_error("expected ';' in 'for' statement");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(For_Statement);
                }
            }

            Owning_Ptr post_expression = try_expression();

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(For_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(For_Statement, ANTON_MOV(variable_declaration), ANTON_MOV(condition), ANTON_MOV(post_expression), ANTON_MOV(statements.value()), src);
        }

        Owning_Ptr<While_Statement> try_while_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_while)) {
                set_error("expected 'while'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(While_Statement);
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(While_Statement);
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(While_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(While_Statement, ANTON_MOV(condition), ANTON_MOV(statements.value()), src);
        }

        Owning_Ptr<Do_While_Statement> try_do_while_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_do)) {
                set_error("expected 'do'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Do_While_Statement);
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Do_While_Statement);
            }

            if(!_lexer.match(Token_Type::kw_while)) {
                set_error("expected 'while'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Do_While_Statement);
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Do_While_Statement);
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error("expected ';' after do-while statement");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Do_While_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Do_While_Statement, ANTON_MOV(condition), ANTON_MOV(statements.value()), src);
        }

        Owning_Ptr<Return_Statement> try_return_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_return)) {
                set_error("expected 'return'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Return_Statement);
            }

            Owning_Ptr return_expression = try_expression();

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error("expected ';' after return statement");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Return_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Return_Statement, ANTON_MOV(return_expression), src);
        }

        Owning_Ptr<Break_Statement> try_break_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_break)) {
                set_error(u8"expected 'break'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Break_Statement);
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error(u8"expected ';' after break statement");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Break_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Break_Statement, src);
        }

        Owning_Ptr<Continue_Statement> try_continue_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_continue)) {
                set_error(u8"expected 'continue'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Continue_Statement);
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error(u8"expected ';' after continue statement");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Continue_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Continue_Statement, src);
        }

        Owning_Ptr<Discard_Statement> try_discard_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_discard)) {
                set_error(u8"expected 'discard'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Discard_Statement);
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error(u8"expected ';' after discard statement");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Discard_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Discard_Statement, src);
        }

        Owning_Ptr<Expression_Statement> try_expression_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr expression = try_expression();
            if(!expression) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_Statement);
            }

            if(!_lexer.match(Token_Type::tk_semicolon)) {
                set_error("expected ';' at the end of statement");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_Statement);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Expression_Statement, ANTON_MOV(expression), src);
        }

        Owning_Ptr<Expression> try_expression() {
            return try_assignment_expression();
        }

        Owning_Ptr<Expression> try_assignment_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_binary_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression);
            }

            Lexer_State const op_state = _lexer.get_current_state();
            auto token = _lexer.next_token();
            if(!token) {
                return lhs;
            }

            bool matched = false;
            bool is_direct = false;
            Arithmetic_Assignment_Type type;
            switch(token->token.type) {
                // =
                case Token_Type::tk_equals: {
                    is_direct = true;
                    matched = true;
                } break;
                // +=
                case Token_Type::tk_plus: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::plus;
                } break;
                // -=
                case Token_Type::tk_minus: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::minus;
                } break;
                // *=
                case Token_Type::tk_asterisk: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::multiply;
                } break;
                // /=
                case Token_Type::tk_slash: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::divide;
                } break;
                // %=
                case Token_Type::tk_percent: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::remainder;
                } break;
                // &=
                case Token_Type::tk_amp: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::bit_and;
                } break;
                // |=
                case Token_Type::tk_pipe: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::bit_or;
                } break;
                // ^=
                case Token_Type::tk_hat: {
                    matched = _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::bit_xor;
                } break;
                // <<=
                case Token_Type::tk_langle: {
                    matched = _lexer.match(Token_Type::tk_langle) && _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::lshift;
                } break;
                // >>=
                case Token_Type::tk_rangle: {
                    matched = _lexer.match(Token_Type::tk_rangle) && _lexer.match(Token_Type::tk_equals);
                    type = Arithmetic_Assignment_Type::rshift;
                } break;

                default:
                    break; // Nothing
            }

            if(!matched) {
                _lexer.restore_state(op_state);
                return lhs;
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(op_state, end_state);

            Owning_Ptr rhs = try_assignment_expression();
            if(!rhs) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression);
            }

            if(is_direct) {
                return ALLOC(Assignment_Expression, ANTON_MOV(lhs), ANTON_MOV(rhs), src);
            } else {
                return ALLOC(Arithmetic_Assignment_Expression, type, ANTON_MOV(lhs), ANTON_MOV(rhs), src);
            }
        }

        // Owning_Ptr<Expression> try_elvis_expression() {
        //     Lexer_State const state_backup = _lexer.get_current_state();
        //     Owning_Ptr cond = try_logic_or_expr();
        //     if(!cond) {
        //         _lexer.restore_state(state_backup);
        //         return nullptr;
        //     }

        //     if(!_lexer.match(token_question)) {
        //         return cond;
        //     }

        //     // TODO: is using try_expression here correct?

        //     Owning_Ptr true_expression = try_expression();
        //     if(!true_expression) {
        //         _lexer.restore_state(state_backup);
        //         return nullptr;
        //     }

        //     if(!_lexer.match(token_colon)) {
        //         set_error(u8"expected ':'");
        //         _lexer.restore_state(state_backup);
        //         return nullptr;
        //     }

        //     Owning_Ptr false_expression = try_expression();
        //     if(!false_expression) {
        //         _lexer.restore_state(state_backup);
        //         return nullptr;
        //     }

        //     Lexer_State const end_state = _lexer.get_current_state_noskip();
        //     Source_Info const src = src_info(state_backup, end_state);
        //     return ALLOC(Elvis_Expression, ANTON_MOV(cond), ANTON_MOV(true_expression), ANTON_MOV(false_expression), src);
        // }

        Owning_Ptr<Expression> try_binary_expression() {
            auto insert_node = [](Owning_Ptr<Expression> root, Owning_Ptr<Binary_Expression> node) -> Owning_Ptr<Expression> {
                auto get_precedence = [](Binary_Expression& expression) -> i32 {
                    switch(expression.type) {
                        case Binary_Expression_Type::logic_or:
                            return 11;
                        case Binary_Expression_Type::logic_xor:
                            return 10;
                        case Binary_Expression_Type::logic_and:
                            return 9;
                        case Binary_Expression_Type::equal:
                            return 5;
                        case Binary_Expression_Type::unequal:
                            return 5;
                        case Binary_Expression_Type::greater_than:
                            return 4;
                        case Binary_Expression_Type::less_than:
                            return 4;
                        case Binary_Expression_Type::greater_equal:
                            return 4;
                        case Binary_Expression_Type::less_equal:
                            return 4;
                        case Binary_Expression_Type::bit_or:
                            return 8;
                        case Binary_Expression_Type::bit_xor:
                            return 7;
                        case Binary_Expression_Type::bit_and:
                            return 6;
                        case Binary_Expression_Type::lshift:
                            return 3;
                        case Binary_Expression_Type::rshift:
                            return 3;
                        case Binary_Expression_Type::add:
                            return 2;
                        case Binary_Expression_Type::sub:
                            return 2;
                        case Binary_Expression_Type::mul:
                            return 1;
                        case Binary_Expression_Type::div:
                            return 1;
                        case Binary_Expression_Type::mod:
                            return 1;
                    }
                };

                i32 const node_precedence = get_precedence(*node);
                Owning_Ptr<Expression>* insertion_node = &root;
                while(true) {
                    if((**insertion_node).node_type != AST_Node_Type::binary_expression) {
                        break;
                    }

                    Binary_Expression& expression = static_cast<Binary_Expression&>(**insertion_node);
                    i32 const expression_precedence = get_precedence(expression);
                    if(expression_precedence < node_precedence) {
                        // Precedence is smaller
                        insertion_node = &expression.rhs;
                    } else {
                        break;
                    }
                }

                node->lhs = ANTON_MOV(*insertion_node);
                *insertion_node = ANTON_MOV(node);

                return root;
            };

            Owning_Ptr<Expression> root = try_unary_expression();
            if(!root) {
                return OWNING_NULL(Expression);
            }

            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
                bool const matched_operator = _lexer.match_operator(Operator::compound_bit_and) || _lexer.match_operator(Operator::compound_bit_or) ||
                                              _lexer.match_operator(Operator::compound_bit_xor) || _lexer.match_operator(Operator::compound_bit_lshift) ||
                                              _lexer.match_operator(Operator::compound_bit_rshift) || _lexer.match_operator(Operator::compound_remainder) ||
                                              _lexer.match_operator(Operator::compound_divide) || _lexer.match_operator(Operator::compound_multiply) ||
                                              _lexer.match_operator(Operator::compound_plus) || _lexer.match_operator(Operator::compound_minus);
                if(matched_operator) {
                    _lexer.restore_state(op_state);
                    return root;
                }

                if(_lexer.match_operator(Operator::logic_or)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::logic_or, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::logic_xor)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::logic_xor, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::logic_and)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::logic_and, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::bit_or)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::bit_or, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::bit_xor)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::bit_xor, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::bit_and)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::bit_and, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::bit_lshift)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::lshift, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::bit_rshift)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::rshift, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::equal, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::not_equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::unequal, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::less_equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::less_equal, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::greater_equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::greater_equal, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::greater)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::greater_than, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::less)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::less_than, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::plus)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::add, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::minus)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::sub, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::multiply)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::mul, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::divide)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::div, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match_operator(Operator::remainder)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return OWNING_NULL(Expression);
                    }

                    Owning_Ptr expression = ALLOC(Binary_Expression, Binary_Expression_Type::mod, nullptr, ANTON_MOV(rhs), src);
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else {
                    break;
                }
            }

            return root;
        }

        Owning_Ptr<Expression> try_unary_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match_operator(Operator::increment)) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return ALLOC(Prefix_Increment_Expression, ANTON_MOV(expr), src);
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression);
                }
            } else if(_lexer.match_operator(Operator::decrement)) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return ALLOC(Prefix_Decrement_Expression, ANTON_MOV(expr), src);
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression);
                }
            } else if(_lexer.match_operator(Operator::plus)) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return ALLOC(Unary_Expression, Unary_Type::plus, ANTON_MOV(expr), src);
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression);
                }
            } else if(_lexer.match_operator(Operator::minus)) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return ALLOC(Unary_Expression, Unary_Type::minus, ANTON_MOV(expr), src);
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression);
                }
            } else if(_lexer.match_operator(Operator::logic_not)) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return ALLOC(Unary_Expression, Unary_Type::logic_not, ANTON_MOV(expr), src);
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression);
                }
            } else if(_lexer.match_operator(Operator::bit_not)) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return ALLOC(Unary_Expression, Unary_Type::bit_not, ANTON_MOV(expr), src);
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression);
                }
            } else {
                return try_postfix_expression();
            }
        }

        Owning_Ptr<Expression> try_postfix_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr primary_expr = try_primary_expression();
            if(!primary_expr) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression);
            }

            Owning_Ptr<Expression> expr = ANTON_MOV(primary_expr);
            while(true) {
                if(_lexer.match(Token_Type::tk_dot)) {
                    if(Owning_Ptr member_name = try_identifier()) {
                        Lexer_State const end_state = _lexer.get_current_state_noskip();
                        Source_Info const src = src_info(state_backup, end_state);
                        expr = ALLOC(Member_Access_Expression, ANTON_MOV(expr), ANTON_MOV(member_name), src);
                    } else {
                        set_error(u8"expected member name");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Expression);
                    }
                } else if(_lexer.match(Token_Type::tk_lbracket)) {
                    Owning_Ptr index = try_expression();
                    if(!index) {
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Expression);
                    }

                    if(!_lexer.match(Token_Type::tk_rbracket)) {
                        set_error(u8"expected ']'");
                        _lexer.restore_state(state_backup);
                        return OWNING_NULL(Expression);
                    }

                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(state_backup, end_state);
                    expr = ALLOC(Array_Access_Expression, ANTON_MOV(expr), ANTON_MOV(index), src);
                } else if(_lexer.match_operator(Operator::increment)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(state_backup, end_state);
                    expr = ALLOC(Postfix_Increment_Expression, ANTON_MOV(expr), src);
                } else if(_lexer.match_operator(Operator::decrement)) {
                    Lexer_State const end_state = _lexer.get_current_state_noskip();
                    Source_Info const src = src_info(state_backup, end_state);
                    expr = ALLOC(Postfix_Decrement_Expression, ANTON_MOV(expr), src);
                } else {
                    break;
                }
            }

            return expr;
        }

        Owning_Ptr<Expression> try_primary_expression() {
            if(Owning_Ptr parenthesised_expression = try_paren_expr()) {
                return parenthesised_expression;
            }

            if(Owning_Ptr expr = try_reinterpret_expr()) {
                return expr;
            }

            if(Owning_Ptr expression_if = try_expression_if()) {
                return expression_if;
            }

            if(Owning_Ptr float_literal = try_float_literal()) {
                return float_literal;
            }

            if(Owning_Ptr integer_literal = try_integer_literal()) {
                return integer_literal;
            }

            if(Owning_Ptr bool_literal = try_bool_literal()) {
                return bool_literal;
            }

            if(Owning_Ptr function_call = try_function_call_expression()) {
                return function_call;
            }

            if(Owning_Ptr identifier_expression = try_identifier_expression()) {
                return identifier_expression;
            }

            return OWNING_NULL(Expression);
        }

        Owning_Ptr<Parenthesised_Expression> try_paren_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::tk_lparen)) {
                set_error(u8"expected '('");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Parenthesised_Expression);
            }

            Owning_Ptr paren_expression = try_expression();
            if(!paren_expression) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Parenthesised_Expression);
            }

            if(!_lexer.match(Token_Type::tk_rparen)) {
                set_error(u8"expected ')'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Parenthesised_Expression);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Parenthesised_Expression, ANTON_MOV(paren_expression), src);
        }

        Owning_Ptr<Reinterpret_Expression> try_reinterpret_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_reinterpret)) {
                set_error(u8"expected 'reinterpret'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            if(!_lexer.match(Token_Type::tk_langle)) {
                set_error(u8"expected '<'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            Owning_Ptr target_type = try_type();
            if(!target_type) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            if(!_lexer.match(Token_Type::tk_rangle)) {
                set_error(u8"expected '>'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            if(!_lexer.match(Token_Type::tk_lparen)) {
                set_error(u8"expected '('");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            Owning_Ptr source = try_expression();
            if(!source) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            if(!_lexer.match(Token_Type::tk_comma)) {
                set_error(u8"expected ','");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            Owning_Ptr index = try_expression();
            if(!index) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            if(!_lexer.match(Token_Type::tk_rparen)) {
                set_error(u8"expected ')'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Reinterpret_Expression);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Reinterpret_Expression, ANTON_MOV(target_type), ANTON_MOV(source), ANTON_MOV(index), src);
        }

        Owning_Ptr<Expression_If> try_expression_if() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(Token_Type::kw_if)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_If);
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_If);
            }

            if(!_lexer.match(Token_Type::tk_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_If);
            }

            if(_lexer.match(Token_Type::tk_brace_close)) {
                set_error(u8"expected an expression before '}'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_If);
            }

            Owning_Ptr true_expression = try_expression();
            if(!true_expression) {
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_If);
            }

            if(!_lexer.match(Token_Type::tk_brace_close)) {
                set_error(u8"expected '}' after expression");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_If);
            }

            if(!_lexer.match(Token_Type::kw_else)) {
                set_error(u8"expected an 'else' branch");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Expression_If);
            }

            if(Owning_Ptr expression_if = try_expression_if()) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                return ALLOC(Expression_If, ANTON_MOV(condition), ANTON_MOV(true_expression), ANTON_MOV(expression_if), src);
            } else {
                if(!_lexer.match(Token_Type::tk_brace_open)) {
                    set_error(u8"expected '{'");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression_If);
                }

                if(_lexer.match(Token_Type::tk_brace_close)) {
                    set_error(u8"expected an expression before '}'");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression_If);
                }

                Owning_Ptr false_expression = try_expression();
                if(!false_expression) {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression_If);
                }

                if(!_lexer.match(Token_Type::tk_brace_close)) {
                    set_error(u8"expected '}' after expression");
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Expression_If);
                }

                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                return ALLOC(Expression_If, ANTON_MOV(condition), ANTON_MOV(true_expression), ANTON_MOV(false_expression), src);
            }
        }

        Owning_Ptr<Function_Call_Expression> try_function_call_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr identifier = try_identifier();
            if(!identifier) {
                set_error(u8"expected function name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Call_Expression);
            }

            if(!_lexer.match(Token_Type::tk_lparen)) {
                set_error(u8"expected '(' after function name");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Call_Expression);
            }

            Expression_List arguments{_allocator};
            if(_lexer.match(Token_Type::tk_rparen)) {
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                return ALLOC(Function_Call_Expression, ANTON_MOV(identifier), ANTON_MOV(arguments), src);
            }

            do {
                if(Owning_Ptr expression = try_expression()) {
                    arguments.emplace_back(ANTON_MOV(expression));
                } else {
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Function_Call_Expression);
                }
            } while(_lexer.match(Token_Type::tk_comma));

            if(!_lexer.match(Token_Type::tk_rparen)) {
                set_error(u8"expected ')'");
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Function_Call_Expression);
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Function_Call_Expression, ANTON_MOV(identifier), ANTON_MOV(arguments), src);
        }

        Owning_Ptr<Float_Literal> try_float_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::Optional<Token_Data> float_literal = _lexer.match(Token_Type::lt_float);
            if(!float_literal) {
                return OWNING_NULL(Float_Literal);
            }

            Float_Literal_Type type = Float_Literal_Type::f32;
            Lexer_State const suffix_backup = _lexer.get_current_state_noskip();
            anton::Optional<Token_Data> suffix_token = _lexer.match_noskip(Token_Type::identifier);
            if(suffix_token) {
                anton::String_View const suffix{suffix_token->token.value.begin(), suffix_token->token.value.end()};
                if(suffix == "d"_sv || suffix == "D"_sv) {
                    type = Float_Literal_Type::f64;
                } else {
                    anton::String msg = anton::concat(_allocator, u8"invalid suffix '"_sv, suffix, u8"' on floating-point literal"_sv);
                    set_error(msg, suffix_backup);
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Float_Literal);
                }
            }

            anton::String_View const number{float_literal->token.value.begin(), float_literal->token.value.end()};
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Float_Literal, anton::String{number, _allocator}, type, src);
        }

        Owning_Ptr<Integer_Literal> try_integer_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::String_View number;
            Integer_Literal_Base base;
            anton::Optional<Token_Data> bin_literal = _lexer.match(Token_Type::lt_bin_integer);
            if(bin_literal) {
                base = Integer_Literal_Base::bin;
                number = anton::String_View{bin_literal->token.value.begin(), bin_literal->token.value.end()};
            }

            anton::Optional<Token_Data> oct_literal = _lexer.match(Token_Type::lt_oct_integer);
            if(oct_literal) {
                base = Integer_Literal_Base::oct;
                number = anton::String_View{oct_literal->token.value.begin(), oct_literal->token.value.end()};
            }

            anton::Optional<Token_Data> hex_literal = _lexer.match(Token_Type::lt_hex_integer);
            if(hex_literal) {
                base = Integer_Literal_Base::hex;
                number = anton::String_View{hex_literal->token.value.begin(), hex_literal->token.value.end()};
            }

            anton::Optional<Token_Data> dec_literal = _lexer.match(Token_Type::lt_dec_integer);
            if(dec_literal) {
                base = Integer_Literal_Base::dec;
                number = anton::String_View{dec_literal->token.value.begin(), dec_literal->token.value.end()};
            }

            if(!bin_literal && !oct_literal && !hex_literal && !dec_literal) {
                set_error("expected integer literal"_sv);
                return OWNING_NULL(Integer_Literal);
            }

            Integer_Literal_Type type = Integer_Literal_Type::i32;
            Lexer_State const suffix_backup = _lexer.get_current_state_noskip();
            anton::Optional<Token_Data> suffix_token = _lexer.match_noskip(Token_Type::identifier);
            if(suffix_token) {
                anton::String_View const suffix{suffix_token->token.value.begin(), suffix_token->token.value.end()};
                if(suffix == "u"_sv || suffix == "U"_sv) {
                    type = Integer_Literal_Type::u32;
                } else {
                    anton::String msg = anton::concat(_allocator, u8"invalid suffix '"_sv, suffix, u8"' on integer literal"_sv);
                    set_error(msg, suffix_backup);
                    _lexer.restore_state(state_backup);
                    return OWNING_NULL(Float_Literal);
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Integer_Literal, anton::String{number, _allocator}, type, base, src);
        }

        Owning_Ptr<Bool_Literal> try_bool_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::Optional<Token_Data> bool_literal = _lexer.match_noskip(Token_Type::lt_bool);
            if(bool_literal) {
                bool const value = bool_literal->token.value == "true"_sv7;
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                return ALLOC(Bool_Literal, value, src);
            } else {
                set_error("expected bool literal"_sv);
                return OWNING_NULL(Bool_Literal);
            }
        }

        Owning_Ptr<String_Literal> try_string_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::Optional<Token_Data> string_literal = _lexer.match_noskip(Token_Type::lt_string);
            if(string_literal) {
                // The token value includes both '"', therefore we add 1 and subtract 1 to remove them.
                anton::String_View const value{string_literal->token.value.begin() + 1, string_literal->token.value.end() - 1};
                Lexer_State const end_state = _lexer.get_current_state_noskip();
                Source_Info const src = src_info(state_backup, end_state);
                return ALLOC(String_Literal, anton::String{value, _allocator}, src);
            } else {
                set_error("expected bool literal"_sv);
                return OWNING_NULL(String_Literal);
            }
        }

        Owning_Ptr<Identifier> try_identifier() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::Optional<Token_Data> identifier = _lexer.match_noskip(Token_Type::identifier);
            if(!identifier) {
                set_error(u8"expected an identifier"_sv);
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Identifier);
            }

            anton::String_View const value{identifier->token.value.begin(), identifier->token.value.end()};
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Identifier, anton::String{value, _allocator}, src);
        }

        Owning_Ptr<Identifier_Expression> try_identifier_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::Optional<Token_Data> identifier = _lexer.match_noskip(Token_Type::identifier);
            if(!identifier) {
                set_error(u8"expected an identifier"_sv);
                _lexer.restore_state(state_backup);
                return OWNING_NULL(Identifier_Expression);
            }

            anton::String_View const value{identifier->token.value.begin(), identifier->token.value.end()};
            Lexer_State const end_state = _lexer.get_current_state_noskip();
            Source_Info const src = src_info(state_backup, end_state);
            return ALLOC(Identifier_Expression, anton::String{value, _allocator}, src);
        }
    };

    anton::Expected<Declaration_List, Error> parse_source(Allocator* allocator, anton::String_View const source_name, anton::String_View const source_code) {
        anton::Expected<Lexed_Source, Error> lex_result = lex_source(allocator, anton::String7_View{source_code.bytes_begin(), source_code.bytes_end()});
        if(!lex_result) {
            return {anton::expected_error, ANTON_MOV(lex_result.error())};
        }
        Lexer lexer(lex_result->cbegin(), lex_result->cend());
        Parser parser(allocator, source_name, ANTON_MOV(lexer));
        anton::Expected<Declaration_List, Error> ast = parser.build_ast();
        return ast;
    }

    anton::Expected<Declaration_List, Error> parse_builtin_functions(Allocator* allocator, anton::String_View const source_name,
                                                                     anton::String_View const source_code) {
        anton::Expected<Lexed_Source, Error> lex_result = lex_source(allocator, anton::String7_View{source_code.bytes_begin(), source_code.bytes_end()});
        if(!lex_result) {
            return {anton::expected_error, ANTON_MOV(lex_result.error())};
        }
        Lexer lexer(lex_result->cbegin(), lex_result->cend());
        Parser parser(allocator, source_name, ANTON_MOV(lexer));
        anton::Expected<Declaration_List, Error> ast = parser.parse_builtin_functions();
        return ast;
    }
} // namespace vush
