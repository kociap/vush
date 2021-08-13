#include <parser.hpp>

#include <anton/optional.hpp>
#include <anton/string7_stream.hpp>
#include <anton/string7_view.hpp>

// TODO: When matching keywords, ensure that the keyword is followed by non-identifier character (Find a cleaner way).
// TODO: Figure out a way to match operators that use overlapping symbols (+ and +=) in a clean way.
// TODO: const types.
// TODO: add constructors (currently function call which will break if we use an array type).

namespace vush {
    using namespace anton::literals;

    // keywords

    static constexpr anton::String7_View kw_if = u8"if";
    static constexpr anton::String7_View kw_else = u8"else";
    static constexpr anton::String7_View kw_switch = u8"switch";
    static constexpr anton::String7_View kw_case = u8"case";
    static constexpr anton::String7_View kw_default = u8"default";
    static constexpr anton::String7_View kw_for = u8"for";
    static constexpr anton::String7_View kw_while = u8"while";
    static constexpr anton::String7_View kw_do = u8"do";
    static constexpr anton::String7_View kw_return = u8"return";
    static constexpr anton::String7_View kw_break = u8"break";
    static constexpr anton::String7_View kw_continue = u8"continue";
    static constexpr anton::String7_View kw_discard = u8"discard";
    static constexpr anton::String7_View kw_true = u8"true";
    static constexpr anton::String7_View kw_false = u8"false";
    static constexpr anton::String7_View kw_from = u8"from";
    static constexpr anton::String7_View kw_struct = u8"struct";
    static constexpr anton::String7_View kw_import = u8"import";
    static constexpr anton::String7_View kw_const = u8"const";
    static constexpr anton::String7_View kw_settings = u8"settings";
    static constexpr anton::String7_View kw_reinterpret = u8"reinterpret";
    static constexpr anton::String7_View kw_invariant = u8"invariant";
    static constexpr anton::String7_View kw_smooth = u8"smooth";
    static constexpr anton::String7_View kw_flat = u8"flat";
    static constexpr anton::String7_View kw_noperspective = u8"noperspective";

    // attributes
    static constexpr anton::String7_View attrib_workgroup = u8"workgroup";

    // stages

    static constexpr anton::String7_View stage_vertex = u8"vertex";
    static constexpr anton::String7_View stage_fragment = u8"fragment";
    static constexpr anton::String7_View stage_compute = u8"compute";

    // separators and operators

    static constexpr anton::String7_View token_brace_open = u8"{";
    static constexpr anton::String7_View token_brace_close = u8"}";
    static constexpr anton::String7_View token_bracket_open = u8"[";
    static constexpr anton::String7_View token_bracket_close = u8"]";
    static constexpr anton::String7_View token_paren_open = u8"(";
    static constexpr anton::String7_View token_paren_close = u8")";
    static constexpr anton::String7_View token_angle_open = u8"<";
    static constexpr anton::String7_View token_angle_close = u8">";
    static constexpr anton::String7_View token_semicolon = u8";";
    static constexpr anton::String7_View token_colon = u8":";
    static constexpr anton::String7_View token_scope_resolution = u8"::";
    static constexpr anton::String7_View token_arrow = u8"=>";
    static constexpr anton::String7_View token_comma = u8",";
    static constexpr anton::String7_View token_question = u8"?";
    static constexpr anton::String7_View token_dot = u8".";
    static constexpr anton::String7_View token_double_quote = u8"\"";
    static constexpr anton::String7_View token_plus = u8"+";
    static constexpr anton::String7_View token_minus = u8"-";
    static constexpr anton::String7_View token_multiply = u8"*";
    static constexpr anton::String7_View token_divide = u8"/";
    static constexpr anton::String7_View token_remainder = u8"%";
    static constexpr anton::String7_View token_logic_and = u8"&&";
    static constexpr anton::String7_View token_bit_and = u8"&";
    static constexpr anton::String7_View token_logic_or = u8"||";
    static constexpr anton::String7_View token_logic_xor = u8"^^";
    static constexpr anton::String7_View token_bit_or = u8"|";
    static constexpr anton::String7_View token_bit_xor = u8"^";
    static constexpr anton::String7_View token_logic_not = u8"!";
    static constexpr anton::String7_View token_bit_not = u8"~";
    static constexpr anton::String7_View token_bit_lshift = u8"<<";
    static constexpr anton::String7_View token_bit_rshift = u8">>";
    static constexpr anton::String7_View token_equal = u8"==";
    static constexpr anton::String7_View token_not_equal = u8"!=";
    static constexpr anton::String7_View token_less = u8"<";
    static constexpr anton::String7_View token_greater = u8">";
    static constexpr anton::String7_View token_less_equal = u8"<=";
    static constexpr anton::String7_View token_greater_equal = u8">=";
    static constexpr anton::String7_View token_assign = u8"=";
    static constexpr anton::String7_View token_increment = u8"++";
    static constexpr anton::String7_View token_decrement = u8"--";
    static constexpr anton::String7_View token_compound_plus = u8"+=";
    static constexpr anton::String7_View token_compound_minus = u8"-=";
    static constexpr anton::String7_View token_compound_multiply = u8"*=";
    static constexpr anton::String7_View token_compound_divide = u8"/=";
    static constexpr anton::String7_View token_compound_remainder = u8"%=";
    static constexpr anton::String7_View token_compound_bit_and = u8"&=";
    static constexpr anton::String7_View token_compound_bit_or = u8"|=";
    static constexpr anton::String7_View token_compound_bit_xor = u8"^=";
    static constexpr anton::String7_View token_compound_bit_lshift = u8"<<=";
    static constexpr anton::String7_View token_compound_bit_rshift = u8">>=";

    [[nodiscard]] static bool is_whitespace(char32 c) {
        return (c <= 32) | (c == 127);
    }

    [[nodiscard]] static bool is_binary_digit(char32 c) {
        return c == 48 || c == 49;
    }

    [[nodiscard]] static bool is_hexadecimal_digit(char32 c) {
        return (c >= 48 && c <= 57) || (c >= 65 && c <= 70) || (c >= 97 && c <= 102);
    }

    [[nodiscard]] static bool is_octal_digit(char32 c) {
        return c >= 48 && c <= 55;
    }

    [[nodiscard]] static bool is_digit(char32 c) {
        return c >= 48 && c <= 57;
    }

    [[nodiscard]] static bool is_alpha(char32 c) {
        return (c >= 97 && c < 123) || (c >= 65 && c < 91);
    }

    [[nodiscard]] static bool is_first_identifier_character(char32 c) {
        return c == '_' || is_alpha(c);
    }

    [[nodiscard]] static bool is_identifier_character(char32 c) {
        return c == '_' || is_digit(c) || is_alpha(c);
    }

    [[nodiscard]] static bool is_keyword(anton::String_View string) {
        static constexpr anton::String7_View keywords[] = {
            kw_if,       kw_else,    kw_switch, kw_case,  kw_default, kw_for,    kw_while,  kw_do,    kw_return,   kw_break,
            kw_continue, kw_discard, kw_true,   kw_false, kw_from,    kw_struct, kw_import, kw_const, kw_settings, kw_reinterpret,
        };

        anton::String7_View string7{string.bytes_begin(), string.bytes_end()};
        constexpr i64 array_size = sizeof(keywords) / sizeof(anton::String7_View);
        for(anton::String7_View const *i = keywords, *end = keywords + array_size; i != end; ++i) {
            if(*i == string7) {
                return true;
            }
        }
        return false;
    }

    class Lexer_State {
    public:
        i64 stream_offset;
        i64 line;
        i64 column;
    };

    constexpr char32 eof_char32 = (char32)EOF;

    class Lexer {
    public:
        Lexer(anton::String_View source_code): _stream(anton::String7_View{source_code.bytes_begin(), source_code.bytes_end()}) {}

        bool match(anton::String7_View const string, bool const must_not_be_followed_by_identifier_char = false) {
            Lexer_State const state_backup = get_current_state();
            for(char32 c: string) {
                if(get_next() != c) {
                    restore_state(state_backup);
                    return false;
                }
            }

            if(must_not_be_followed_by_identifier_char) {
                if(is_identifier_character(peek_next())) {
                    restore_state(state_backup);
                    return false;
                } else {
                    return true;
                }
            } else {
                return true;
            }
        }

        bool match_identifier(anton::String& out) {
            ignore_whitespace_and_comments();

            // No need to backup the lexer state since we can predict whether the next
            // sequence of characters is an identifier using only the first character.

            char32 const next_char = _stream.peek();
            if(!is_first_identifier_character(next_char)) {
                return false;
            }

            i64 peek_size = 2;
            anton::String7_View peek = _stream.peek(peek_size);
            while(is_identifier_character(peek[peek_size - 1])) {
                peek = _stream.peek(peek_size + 1);
                peek_size += 1;
            }

            anton::String7_View const data = _stream.read(peek_size - 1);
            out += anton::String_View{data.begin(), data.end()};
            return true;
        }

        bool match_eof() {
            ignore_whitespace_and_comments();
            char32 const next_char = peek_next();
            return next_char == eof_char32;
        }

        void ignore_whitespace_and_comments() {
            while(true) {
                char32 const next_char = peek_next();
                if(is_whitespace(next_char)) {
                    get_next();
                    continue;
                }

                if(next_char == U'/') {
                    get_next();
                    char32 const next_next_char = peek_next();
                    if(next_next_char == U'/') {
                        get_next();
                        while(get_next() != U'\n') {}
                        continue;
                    } else if(next_next_char == U'*') {
                        get_next();
                        for(char32 c1 = get_next(), c2 = peek_next(); c1 != U'*' || c2 != U'/'; c1 = get_next(), c2 = _stream.peek()) {}
                        get_next();
                        continue;
                    } else {
                        // Not a comment. End skipping.
                        unget();
                        break;
                    }
                }

                break;
            }
        }

        Lexer_State get_current_state() {
            ignore_whitespace_and_comments();
            return {_stream.tell(), _line, _column};
        }

        Lexer_State get_current_state_no_skip() {
            return {_stream.tell(), _line, _column};
        }

        void restore_state(Lexer_State const state) {
            _stream.seek(anton::Seek_Dir::beg, state.stream_offset);
            _line = state.line;
            _column = state.column;
        }

        char32 get_next() {
            char32 const c = _stream.get();
            if(c == '\n') {
                _line += 1;
                _column = 1;
            } else {
                _column += 1;
            }
            return c;
        }

        char32 peek_next() {
            return _stream.peek();
        }

        void unget() {
            _stream.unget();
        }

    private:
        // The source string is ASCII, so String7 will be the exact same size as String,
        // but String7 will avoid all Unicode function calls and thus accelerate parsing.
        anton::Input_String7_Stream _stream;
        i64 _line = 1;
        i64 _column = 1;
    };

    class Parser {
    public:
        Parser(anton::String_View source_code, anton::String_View source_name): _source_name(source_name), _lexer(source_code) {}

        anton::Expected<Declaration_List, Parse_Error> build_ast() {
            Declaration_List ast;
            while(!_lexer.match_eof()) {
                if(Owning_Ptr declaration = try_declaration()) {
                    ast.emplace_back(ANTON_MOV(declaration));
                } else {
                    return {anton::expected_error, _last_error};
                }
            }
            return {anton::expected_value, ANTON_MOV(ast)};
        }

    private:
        anton::String_View _source_name;
        Lexer _lexer;
        Parse_Error _last_error;

        void set_error(anton::String_View const message, Lexer_State const& state) {
            if(state.stream_offset >= _last_error.stream_offset) {
                _last_error.message = message;
                _last_error.line = state.line;
                _last_error.column = state.column;
                _last_error.stream_offset = state.stream_offset;
            }
        }

        void set_error(anton::String_View const message) {
            Lexer_State const state = _lexer.get_current_state_no_skip();
            if(state.stream_offset >= _last_error.stream_offset) {
                _last_error.message = message;
                _last_error.line = state.line;
                _last_error.column = state.column;
                _last_error.stream_offset = state.stream_offset;
            }
        }

        Source_Info src_info(Lexer_State const& start, Lexer_State const& end) {
            return Source_Info{_source_name, start.line, start.column, start.stream_offset, end.line, end.column, end.stream_offset};
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
            return nullptr;
        }

        Owning_Ptr<Declaration_If> try_declaration_if() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_if, true)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Declaration_List true_declarations;
            while(!_lexer.match(token_brace_close)) {
                if(_lexer.match_eof()) {
                    set_error(u8"unexpected end of file");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(Owning_Ptr declaration = try_declaration()) {
                    true_declarations.emplace_back(ANTON_MOV(declaration));
                } else {
                    return nullptr;
                }
            }

            Declaration_List false_declarations;
            if(_lexer.match(kw_else, true)) {
                if(Owning_Ptr if_declaration = try_declaration_if()) {
                    false_declarations.emplace_back(ANTON_MOV(if_declaration));
                } else {
                    if(!_lexer.match(token_brace_open)) {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    while(!_lexer.match(token_brace_close)) {
                        if(Owning_Ptr declaration = try_declaration()) {
                            false_declarations.emplace_back(ANTON_MOV(declaration));
                        } else {
                            _lexer.restore_state(state_backup);
                            return nullptr;
                        }
                    }
                }
            }
            return Owning_Ptr{
                new Declaration_If(ANTON_MOV(condition), ANTON_MOV(true_declarations), ANTON_MOV(false_declarations), src_info(state_backup, state_backup))};
        }

        Owning_Ptr<Import_Declaration> try_import_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_import, true)) {
                set_error(u8"expected 'import'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            if(Owning_Ptr string = try_string_literal()) {
                return Owning_Ptr{new Import_Declaration(ANTON_MOV(string), src)};
            } else {
                _lexer.restore_state(state_backup);
                return nullptr;
            }
        }

        Owning_Ptr<Variable_Declaration> try_variable_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr var_type = try_type();
            if(!var_type) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr var_name = try_identifier();
            if(!var_name) {
                set_error(u8"expected variable name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Expression> initializer = nullptr;
            if(_lexer.match(token_assign)) {
                initializer = try_expression();
                if(!initializer) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            if(!_lexer.match(token_semicolon)) {
                set_error(u8"expected ';' after variable declaration");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Variable_Declaration(ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), src)};
        }

        Owning_Ptr<Constant_Declaration> try_constant_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_const)) {
                set_error(u8"expected 'const'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr var_type = try_type();
            if(!var_type) {
                set_error(u8"expected type");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr var_name = try_identifier();
            if(!var_name) {
                set_error(u8"expected variable name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Expression> initializer = nullptr;
            if(_lexer.match(token_assign)) {
                initializer = try_expression();
                if(!initializer) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            if(!_lexer.match(token_semicolon)) {
                set_error(u8"expected ';' after constant declaration");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Constant_Declaration(ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), src)};
        }

        Owning_Ptr<Struct_Member> try_struct_member() {
            Lexer_State const state_backup = _lexer.get_current_state();
            bool has_invariant = false;
            bool has_interpolation = false;
            Interpolation interpolation = Interpolation::none;
            while(true) {
                if(_lexer.match(kw_invariant, true)) {
                    if(has_invariant) {
                        set_error(u8"multiple invariance qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    has_invariant = true;
                    continue;
                }

                if(_lexer.match(kw_smooth, true)) {
                    if(has_interpolation) {
                        set_error(u8"multiple interpolation qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    interpolation = Interpolation::smooth;
                    has_interpolation = true;
                    continue;
                }

                if(_lexer.match(kw_flat, true)) {
                    if(has_interpolation) {
                        set_error(u8"multiple interpolation qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    interpolation = Interpolation::flat;
                    has_interpolation = true;
                    continue;
                }

                if(_lexer.match(kw_noperspective, true)) {
                    if(has_interpolation) {
                        set_error(u8"multiple interpolation qualifiers are not allowed");
                        _lexer.restore_state(state_backup);
                        return nullptr;
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
                return nullptr;
            }

            Owning_Ptr var_name = try_identifier();
            if(!var_name) {
                set_error(u8"expected member name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Expression> initializer = nullptr;
            if(_lexer.match(token_assign)) {
                initializer = try_expression();
                if(!initializer) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            if(!_lexer.match(token_semicolon)) {
                set_error(u8"expected ';'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Struct_Member(ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), interpolation, has_invariant, src)};
        }

        Owning_Ptr<Struct_Declaration> try_struct_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_struct, true)) {
                set_error(u8"expected 'struct'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr struct_name = try_identifier();
            if(!struct_name) {
                set_error(u8"expected struct name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Array<Owning_Ptr<Struct_Member>> members;
            while(!_lexer.match(token_brace_close)) {
                if(Owning_Ptr decl = try_struct_member()) {
                    members.emplace_back(ANTON_MOV(decl));
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Struct_Declaration(ANTON_MOV(struct_name), ANTON_MOV(members), src)};
        }

        Owning_Ptr<Settings_Declaration> try_settings_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();

            auto match_string = [this, &state_backup]() -> anton::Optional<anton::String> {
                _lexer.ignore_whitespace_and_comments();
                anton::String string;
                while(true) {
                    char32 next_char = _lexer.peek_next();
                    if(next_char == eof_char32) {
                        set_error(u8"unexpected end of file");
                        _lexer.restore_state(state_backup);
                        return anton::null_optional;
                    } else if(is_whitespace(next_char) || next_char == U':' || next_char == U'}' || next_char == U'{') {
                        return {ANTON_MOV(string)};
                    } else {
                        string += next_char;
                        _lexer.get_next();
                    }
                }
            };

            auto match_nested_settings = [this, &state_backup, &match_string](auto match_nested_settings, anton::Array<Setting_Key_Value>& settings,
                                                                              anton::String const& setting_name) -> bool {
                while(true) {
                    if(_lexer.match(token_brace_close)) {
                        return true;
                    }

                    auto key_name = match_string();
                    if(!key_name) {
                        _lexer.restore_state(state_backup);
                        return false;
                    }

                    if(key_name.value().size_bytes() == 0) {
                        set_error(u8"expected name");
                        _lexer.restore_state(state_backup);
                        return false;
                    }

                    if(!_lexer.match(token_colon)) {
                        set_error(u8"expected ':'");
                        _lexer.restore_state(state_backup);
                        return false;
                    }

                    anton::String setting_key = setting_name;
                    if(setting_key.size_bytes() > 0) {
                        setting_key += u8"_";
                    }
                    setting_key += key_name.value();

                    if(_lexer.match(token_brace_open)) {
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
                            set_error(u8"expected value string after ':'");
                            _lexer.restore_state(state_backup);
                            return false;
                        }

                        settings.emplace_back(Setting_Key_Value{ANTON_MOV(setting_key), ANTON_MOV(value.value())});
                    }
                }
            };

            if(!_lexer.match(kw_settings, true)) {
                set_error(u8"expected 'settings'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr pass_name = try_identifier();
            if(!pass_name) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr settings_declaration{new Settings_Declaration(ANTON_MOV(pass_name), src_info(state_backup, state_backup))};
            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!match_nested_settings(match_nested_settings, settings_declaration->settings, anton::String{})) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            settings_declaration->source_info = src;
            return settings_declaration;
        }

        Owning_Ptr<Attribute> try_attribute() {
            Lexer_State const state_backup = _lexer.get_current_state();

            // workgroup attribute
            if(_lexer.match(attrib_workgroup, true)) {
                if(!_lexer.match(token_paren_open)) {
                    set_error(u8"expected '('");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr x = try_integer_literal();
                if(!x) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr<Integer_Literal> y;
                Owning_Ptr<Integer_Literal> z;
                if(_lexer.match(token_comma)) {
                    y = try_integer_literal();
                    if(!y) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    if(_lexer.match(token_comma)) {
                        z = try_integer_literal();
                        if(!z) {
                            _lexer.restore_state(state_backup);
                            return nullptr;
                        }
                    }
                }

                if(!_lexer.match(token_paren_close)) {
                    set_error(u8"expected ')'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Workgroup_Attribute(ANTON_MOV(x), ANTON_MOV(y), ANTON_MOV(z), src)};
            }

            // TODO: Add a diagnostic for unrecognized attributes
            set_error(u8"expected identifier");
            _lexer.restore_state(state_backup);
            return nullptr;
        }

        anton::Optional<Attribute_List> try_attribute_list() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_bracket_open)) {
                set_error(u8"expected '['");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            if(_lexer.match(token_bracket_close)) {
                set_error(u8"empty attribute list (TODO: PROVIDE A PROPER DIAGNOSTIC MESSAGE WITH EXACT LOCATION AND CODE SNIPPET)");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            Attribute_List attributes;
            while(true) {
                Owning_Ptr attribute = try_attribute();
                if(!attribute) {
                    _lexer.restore_state(state_backup);
                    return anton::null_optional;
                }

                attributes.emplace_back(ANTON_MOV(attribute));

                if(!_lexer.match(token_comma)) {
                    break;
                }
            }

            if(!_lexer.match(token_bracket_close)) {
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            return ANTON_MOV(attributes);
        }

        Owning_Ptr<Image_Layout_Qualifier> try_image_layout_qualifier() {
            Image_Layout_Type qualifiers[] = {Image_Layout_Type::rgba32f,
                                              Image_Layout_Type::rgba16f,
                                              Image_Layout_Type::rg32f,
                                              Image_Layout_Type::rg16f,
                                              Image_Layout_Type::r11f_g11f_b10f,
                                              Image_Layout_Type::r32f,
                                              Image_Layout_Type::r16f,
                                              Image_Layout_Type::rgba16,
                                              Image_Layout_Type::rgb10_a2,
                                              Image_Layout_Type::rgba8,
                                              Image_Layout_Type::rg16,
                                              Image_Layout_Type::rg8,
                                              Image_Layout_Type::r16,
                                              Image_Layout_Type::r8,
                                              Image_Layout_Type::rgba16_snorm,
                                              Image_Layout_Type::rgba8_snorm,
                                              Image_Layout_Type::rg16_snorm,
                                              Image_Layout_Type::rg8_snorm,
                                              Image_Layout_Type::r16_snorm,
                                              Image_Layout_Type::r8_snorm,
                                              Image_Layout_Type::rgba32i,
                                              Image_Layout_Type::rgba16i,
                                              Image_Layout_Type::rgba8i,
                                              Image_Layout_Type::rg32i,
                                              Image_Layout_Type::rg16i,
                                              Image_Layout_Type::rg8i,
                                              Image_Layout_Type::r32i,
                                              Image_Layout_Type::r16i,
                                              Image_Layout_Type::r8i,
                                              Image_Layout_Type::rgba32ui,
                                              Image_Layout_Type::rgba16ui,
                                              Image_Layout_Type::rgb10_a2ui,
                                              Image_Layout_Type::rgba8ui,
                                              Image_Layout_Type::rg32ui,
                                              Image_Layout_Type::rg16ui,
                                              Image_Layout_Type::rg8ui,
                                              Image_Layout_Type::r32ui,
                                              Image_Layout_Type::r16ui,
                                              Image_Layout_Type::r8ui};

            Lexer_State const state_backup = _lexer.get_current_state();
            constexpr i64 array_size = sizeof(qualifiers) / sizeof(Image_Layout_Type);
            for(i64 i = 0; i < array_size; ++i) {
                anton::String_View const string{stringify(qualifiers[i])};
                anton::String7_View const string7{string.bytes_begin(), string.bytes_end()};
                if(_lexer.match(string7, true)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(state_backup, end_state);
                    return Owning_Ptr{new Image_Layout_Qualifier(qualifiers[i], src)};
                }
            }

            return nullptr;
        }

        Owning_Ptr<Pass_Stage_Declaration> try_pass_stage_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();

            // Parse the attribute lists. We allow many attribute lists
            // to be present on a stage declaration. We merge the lists into
            // a single attribute array.
            Attribute_List attributes;
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
                return nullptr;
            }

            Owning_Ptr pass = try_identifier();
            if(!pass) {
                set_error(u8"expected pass name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_scope_resolution)) {
                set_error(u8"expected '::' after pass name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            static constexpr anton::String7_View stage_types_strings[] = {stage_vertex, stage_fragment, stage_compute};
            static constexpr Stage_Type stage_types[] = {Stage_Type::vertex, Stage_Type::fragment, Stage_Type::compute};

            Stage_Type stage_type;
            {
                bool found = false;
                for(i64 i = 0; i < 3; ++i) {
                    if(_lexer.match(stage_types_strings[i], true)) {
                        stage_type = stage_types[i];
                        found = true;
                        break;
                    }
                }

                if(!found) {
                    set_error(u8"expected stage type");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            anton::Optional<Parameter_List> parameter_list = try_function_param_list();
            if(!parameter_list) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Pass_Stage_Declaration(ANTON_MOV(attributes), ANTON_MOV(return_type), ANTON_MOV(pass), stage_type,
                                                         ANTON_MOV(parameter_list.value()), ANTON_MOV(statements.value()), src)};
        }

        Owning_Ptr<Function_Declaration> try_function_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();

            // Parse the attribute lists. We allow many attribute lists
            // to be present on a function declaration. We merge the lists
            // into a single attribute array.
            Attribute_List attributes;
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
                return nullptr;
            }

            Owning_Ptr name = try_identifier();
            if(!name) {
                set_error(u8"expected function name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            auto param_list = try_function_param_list();
            if(!param_list) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Function_Declaration(ANTON_MOV(attributes), ANTON_MOV(return_type), ANTON_MOV(name), ANTON_MOV(param_list.value()),
                                                       ANTON_MOV(statements.value()), src)};
        }

        anton::Optional<Parameter_List> try_function_param_list() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_paren_open)) {
                set_error(u8"expected '('");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            if(_lexer.match(token_paren_close)) {
                return Parameter_List{};
            }

            Parameter_List param_list;
            // Match parameters
            do {
                Owning_Ptr param = try_function_parameter();
                if(param) {
                    param_list.emplace_back(ANTON_MOV(param));
                } else {
                    _lexer.restore_state(state_backup);
                    return anton::null_optional;
                }
            } while(_lexer.match(token_comma));

            if(!_lexer.match(token_paren_close)) {
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

            Owning_Ptr image_layout = try_image_layout_qualifier();

            Owning_Ptr parameter_type = try_type();
            if(!parameter_type) {
                set_error(u8"expected parameter type");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr identifier = try_identifier();
            if(!identifier) {
                set_error(u8"expected parameter name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Identifier> source;
            if(_lexer.match(kw_from, true)) {
                source = try_identifier();
                if(!source) {
                    set_error(u8"expected parameter source after 'from'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Function_Parameter(ANTON_MOV(identifier), ANTON_MOV(parameter_type), ANTON_MOV(source), ANTON_MOV(image_layout), src)};
        }

        Owning_Ptr<Function_Param_If> try_function_param_if() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_if, true)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr true_param = try_function_parameter();
            if(!true_param) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_close)) {
                set_error(u8"expected '}'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(kw_else, true)) {
                if(Owning_Ptr param_if = try_function_param_if()) {
                    return Owning_Ptr{
                        new Function_Param_If(ANTON_MOV(condition), ANTON_MOV(true_param), ANTON_MOV(param_if), src_info(state_backup, state_backup))};
                } else {
                    if(!_lexer.match(token_brace_open)) {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    Owning_Ptr false_param = try_function_parameter();
                    if(!false_param) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    if(!_lexer.match(token_brace_close)) {
                        set_error(u8"expected '}'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    return Owning_Ptr{
                        new Function_Param_If(ANTON_MOV(condition), ANTON_MOV(true_param), ANTON_MOV(false_param), src_info(state_backup, state_backup))};
                }
            } else {
                return Owning_Ptr{new Function_Param_If(ANTON_MOV(condition), ANTON_MOV(true_param), nullptr, src_info(state_backup, state_backup))};
            }
        }

        // try_empty_statament
        // Match empty statement ';'.
        //
        bool try_empty_statement() {
            return _lexer.match(token_semicolon);
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
                Owning_Ptr decl_stmt{new Declaration_Statement(ANTON_MOV(decl), src)};
                return decl_stmt;
            }

            if(Owning_Ptr decl = try_constant_declaration()) {
                Source_Info const src = decl->source_info;
                Owning_Ptr decl_stmt{new Declaration_Statement(ANTON_MOV(decl), src)};
                return decl_stmt;
            }

            if(Owning_Ptr expr_stmt = try_expression_statement()) {
                return expr_stmt;
            }

            set_error(u8"expected a statement");
            return nullptr;
        }

        // try_braced_statement_list
        // Match '{' statements '}'
        anton::Optional<Statement_List> try_braced_statement_list() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            Statement_List body;
            while(true) {
                if(_lexer.match(token_brace_close)) {
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
            anton::String type_name;
            if(!_lexer.match_identifier(type_name)) {
                set_error(u8"expected type identifier");
                return nullptr;
            }

            if(is_keyword(type_name)) {
                anton::String msg = u8"expected type name, got '" + type_name + "' instead";
                set_error(msg, state_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Type> base_type;
            if(anton::Optional<Builtin_GLSL_Type> res = enumify_builtin_glsl_type(type_name); res) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                base_type = Owning_Ptr{new Builtin_Type(res.value(), src)};
            } else {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                base_type = Owning_Ptr{new User_Defined_Type(ANTON_MOV(type_name), src)};
            }

            if(!_lexer.match(token_bracket_open)) {
                return base_type;
            } else {
                Owning_Ptr array_size = try_integer_literal();
                if(!_lexer.match(token_bracket_close)) {
                    set_error(u8"expected ']'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                // We don't support nested array types (yet), so we don't continue checking for brackets.
                return Owning_Ptr{new Array_Type(ANTON_MOV(base_type), ANTON_MOV(array_size), src)};
            }
        }

        Owning_Ptr<Block_Statement> try_block_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Block_Statement(ANTON_MOV(statements.value()), src)};
        }

        Owning_Ptr<If_Statement> try_if_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_if, true)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Optional<Statement_List> true_statements = try_braced_statement_list();
            if(!true_statements) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Statement_List false_statements;
            if(_lexer.match(kw_else, true)) {
                if(Owning_Ptr if_statement = try_if_statement()) {
                    false_statements.emplace_back(ANTON_MOV(if_statement));
                } else {
                    anton::Optional<Statement_List> statements = try_braced_statement_list();
                    if(!statements) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    false_statements = ANTON_MOV(statements.value());
                }
            }
            return Owning_Ptr{
                new If_Statement(ANTON_MOV(condition), ANTON_MOV(true_statements.value()), ANTON_MOV(false_statements), src_info(state_backup, state_backup))};
        }

        Owning_Ptr<Switch_Statement> try_switch_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_switch, true)) {
                set_error(u8"expected 'switch'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr match_expression = try_expression();
            if(!match_expression) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Array<Owning_Ptr<Case_Statement>> cases;
            while(true) {
                Lexer_State const case_state = _lexer.get_current_state();

                if(_lexer.match(token_brace_close)) {
                    break;
                }

                Expression_List labels;
                do {
                    Lexer_State const label_state = _lexer.get_current_state();
                    if(_lexer.match(kw_default, true)) {
                        Lexer_State const end_state = _lexer.get_current_state_no_skip();
                        Source_Info const src = src_info(label_state, end_state);
                        labels.emplace_back(Owning_Ptr{new Default_Expression(src)});
                    } else if(Owning_Ptr literal = try_integer_literal()) {
                        labels.emplace_back(ANTON_MOV(literal));
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } while(_lexer.match(token_comma));

                if(!_lexer.match(token_arrow)) {
                    set_error(u8"expected '=>'"_sv);
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                anton::Optional<Statement_List> statements = try_braced_statement_list();
                if(!statements) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(case_state, end_state);
                Owning_Ptr case_statement = Owning_Ptr{new Case_Statement(ANTON_MOV(labels), ANTON_MOV(statements.value()), src)};
                cases.emplace_back(ANTON_MOV(case_statement));
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Switch_Statement(ANTON_MOV(match_expression), ANTON_MOV(cases), src)};
        }

        Owning_Ptr<For_Statement> try_for_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_for, true)) {
                set_error("expected 'for'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(token_paren_open)) {
                set_error("unexpected '(' after 'for'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            // Match variable

            Owning_Ptr<Variable_Declaration> variable_declaration;
            if(!_lexer.match(token_semicolon)) {
                Lexer_State const var_decl_state = _lexer.get_current_state();
                Owning_Ptr var_type = try_type();
                if(!var_type) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr var_name = try_identifier();
                if(!var_name) {
                    set_error("expected variable name");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr<Expression> initializer;
                if(_lexer.match(token_assign)) {
                    initializer = try_expression();
                    if(!initializer) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                }

                if(!_lexer.match(token_semicolon)) {
                    set_error("expected ';' in 'for' statement");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(var_decl_state, end_state);
                variable_declaration = Owning_Ptr{new Variable_Declaration(ANTON_MOV(var_type), ANTON_MOV(var_name), ANTON_MOV(initializer), src)};
            }

            Owning_Ptr<Expression> condition;
            if(!_lexer.match(token_semicolon)) {
                condition = try_expression();
                if(!condition) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(!_lexer.match(token_semicolon)) {
                    set_error("expected ';' in 'for' statement");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            Owning_Ptr post_expression = try_expression();

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{
                new For_Statement(ANTON_MOV(variable_declaration), ANTON_MOV(condition), ANTON_MOV(post_expression), ANTON_MOV(statements.value()), src)};
        }

        Owning_Ptr<While_Statement> try_while_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_while, true)) {
                set_error("expected 'while'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new While_Statement(ANTON_MOV(condition), ANTON_MOV(statements.value()), src)};
        }

        Owning_Ptr<Do_While_Statement> try_do_while_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_do, true)) {
                set_error("expected 'do'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Optional<Statement_List> statements = try_braced_statement_list();
            if(!statements) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(kw_while, true)) {
                set_error("expected 'while'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_semicolon)) {
                set_error("expected ';' after do-while statement");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Do_While_Statement(ANTON_MOV(condition), ANTON_MOV(statements.value()), src)};
        }

        Owning_Ptr<Return_Statement> try_return_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_return, true)) {
                set_error("expected 'return'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr return_expression = try_expression();

            if(!_lexer.match(token_semicolon)) {
                set_error("expected ';' after return statement");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Return_Statement(ANTON_MOV(return_expression), src)};
        }

        Owning_Ptr<Break_Statement> try_break_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_break, true)) {
                set_error(u8"expected 'break'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_semicolon)) {
                set_error(u8"expected ';' after break statement");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Break_Statement(src)};
        }

        Owning_Ptr<Continue_Statement> try_continue_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_continue, true)) {
                set_error(u8"expected 'continue'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_semicolon)) {
                set_error(u8"expected ';' after continue statement");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Continue_Statement(src)};
        }

        Owning_Ptr<Discard_Statement> try_discard_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_discard, true)) {
                set_error(u8"expected 'discard'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_semicolon)) {
                set_error(u8"expected ';' after discard statement");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Discard_Statement(src)};
        }

        Owning_Ptr<Expression_Statement> try_expression_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr expression = try_expression();
            if(!expression) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_semicolon)) {
                set_error("expected ';' at the end of statement");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Expression_Statement(ANTON_MOV(expression), src)};
        }

        Owning_Ptr<Expression> try_expression() {
            return try_assignment_expression();
        }

        Owning_Ptr<Expression> try_assignment_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_binary_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const op_state = _lexer.get_current_state();
            Arithmetic_Assignment_Type type;
            bool is_direct = false;
            if(_lexer.match(token_assign)) {
                is_direct = true;
            } else if(_lexer.match(token_compound_plus)) {
                type = Arithmetic_Assignment_Type::plus;
            } else if(_lexer.match(token_compound_minus)) {
                type = Arithmetic_Assignment_Type::minus;
            } else if(_lexer.match(token_compound_multiply)) {
                type = Arithmetic_Assignment_Type::multiply;
            } else if(_lexer.match(token_compound_divide)) {
                type = Arithmetic_Assignment_Type::divide;
            } else if(_lexer.match(token_compound_remainder)) {
                type = Arithmetic_Assignment_Type::remainder;
            } else if(_lexer.match(token_compound_bit_lshift)) {
                type = Arithmetic_Assignment_Type::lshift;
            } else if(_lexer.match(token_compound_bit_rshift)) {
                type = Arithmetic_Assignment_Type::rshift;
            } else if(_lexer.match(token_compound_bit_and)) {
                type = Arithmetic_Assignment_Type::bit_and;
            } else if(_lexer.match(token_compound_bit_or)) {
                type = Arithmetic_Assignment_Type::bit_or;
            } else if(_lexer.match(token_compound_bit_xor)) {
                type = Arithmetic_Assignment_Type::bit_xor;
            } else {
                return lhs;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(op_state, end_state);

            Owning_Ptr rhs = try_assignment_expression();
            if(!rhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(is_direct) {
                return Owning_Ptr{new Assignment_Expression(ANTON_MOV(lhs), ANTON_MOV(rhs), src)};
            } else {
                return Owning_Ptr{new Arithmetic_Assignment_Expression(type, ANTON_MOV(lhs), ANTON_MOV(rhs), src)};
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

        //     Lexer_State const end_state = _lexer.get_current_state_no_skip();
        //     Source_Info const src = src_info(state_backup, end_state);
        //     return Owning_Ptr{new Elvis_Expression(ANTON_MOV(cond), ANTON_MOV(true_expression), ANTON_MOV(false_expression), src)};
        // }

        Owning_Ptr<Expression> try_binary_expression() {
            auto insert_node = [](Owning_Ptr<Expression> root, Owning_Ptr<Binary_Expression> node) -> Owning_Ptr<Expression> {
                auto get_precedence = [](Binary_Expression& expression) -> i32 {
                    i64 precedence[19] = {};
                    precedence[(i64)Binary_Expression_Type::logic_or] = 11;
                    precedence[(i64)Binary_Expression_Type::logic_xor] = 10;
                    precedence[(i64)Binary_Expression_Type::logic_and] = 9;
                    precedence[(i64)Binary_Expression_Type::equal] = 5;
                    precedence[(i64)Binary_Expression_Type::unequal] = 5;
                    precedence[(i64)Binary_Expression_Type::greater_than] = 4;
                    precedence[(i64)Binary_Expression_Type::less_than] = 4;
                    precedence[(i64)Binary_Expression_Type::greater_equal] = 4;
                    precedence[(i64)Binary_Expression_Type::less_equal] = 4;
                    precedence[(i64)Binary_Expression_Type::bit_or] = 8;
                    precedence[(i64)Binary_Expression_Type::bit_xor] = 7;
                    precedence[(i64)Binary_Expression_Type::bit_and] = 6;
                    precedence[(i64)Binary_Expression_Type::lshift] = 3;
                    precedence[(i64)Binary_Expression_Type::rshift] = 3;
                    precedence[(i64)Binary_Expression_Type::add] = 2;
                    precedence[(i64)Binary_Expression_Type::sub] = 2;
                    precedence[(i64)Binary_Expression_Type::mul] = 1;
                    precedence[(i64)Binary_Expression_Type::div] = 1;
                    precedence[(i64)Binary_Expression_Type::mod] = 1;
                    return precedence[(i64)expression.type];
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
                return nullptr;
            }

            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
                if(_lexer.match(token_compound_bit_and) || _lexer.match(token_compound_bit_or) || _lexer.match(token_compound_bit_xor) ||
                   _lexer.match(token_compound_bit_lshift) || _lexer.match(token_compound_bit_rshift) || _lexer.match(token_compound_remainder) ||
                   _lexer.match(token_compound_divide) || _lexer.match(token_compound_multiply) || _lexer.match(token_compound_plus) ||
                   _lexer.match(token_compound_minus)) {
                    _lexer.restore_state(op_state);
                    return root;
                }

                if(_lexer.match(token_logic_or)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::logic_or, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_logic_xor)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::logic_xor, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_logic_and)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::logic_and, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_bit_or)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::bit_or, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_bit_xor)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::bit_xor, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_bit_and)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::bit_and, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_bit_lshift)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::lshift, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_bit_rshift)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::rshift, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::equal, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_not_equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::unequal, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_less_equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::less_equal, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_greater_equal)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::greater_equal, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_greater)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::greater_than, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_less)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::less_than, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_plus)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::add, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_minus)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::sub, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_multiply)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::mul, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_divide)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::div, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else if(_lexer.match(token_remainder)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(op_state, end_state);

                    Owning_Ptr<Expression> rhs = try_unary_expression();
                    if(!rhs) {
                        return nullptr;
                    }

                    Owning_Ptr expression{new Binary_Expression{Binary_Expression_Type::mod, nullptr, ANTON_MOV(rhs), src}};
                    root = insert_node(ANTON_MOV(root), ANTON_MOV(expression));
                } else {
                    break;
                }
            }

            return root;
        }

        Owning_Ptr<Expression> try_unary_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match(token_increment)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Prefix_Increment_Expression(ANTON_MOV(expr), src)};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_decrement)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Prefix_Decrement_Expression(ANTON_MOV(expr), src)};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_plus)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::plus, ANTON_MOV(expr), src)};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_minus)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::minus, ANTON_MOV(expr), src)};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_logic_not)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::logic_not, ANTON_MOV(expr), src)};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_bit_not)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::bit_not, ANTON_MOV(expr), src)};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
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
                return nullptr;
            }

            Owning_Ptr<Expression> expr = ANTON_MOV(primary_expr);
            while(true) {
                if(_lexer.match(token_dot)) {
                    if(Owning_Ptr member_name = try_identifier()) {
                        Lexer_State const end_state = _lexer.get_current_state_no_skip();
                        Source_Info const src = src_info(state_backup, end_state);
                        expr = Owning_Ptr{new Member_Access_Expression(ANTON_MOV(expr), ANTON_MOV(member_name), src)};
                    } else {
                        set_error(u8"expected member name");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_bracket_open)) {
                    Owning_Ptr index = try_expression();
                    if(!index) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    if(!_lexer.match(token_bracket_close)) {
                        set_error(u8"expected ']'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(state_backup, end_state);
                    expr = Owning_Ptr{new Array_Access_Expression(ANTON_MOV(expr), ANTON_MOV(index), src)};
                } else if(_lexer.match(token_increment)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(state_backup, end_state);
                    expr = Owning_Ptr{new Postfix_Increment_Expression(ANTON_MOV(expr), src)};
                } else if(_lexer.match(token_decrement)) {
                    Lexer_State const end_state = _lexer.get_current_state_no_skip();
                    Source_Info const src = src_info(state_backup, end_state);
                    expr = Owning_Ptr{new Postfix_Decrement_Expression(ANTON_MOV(expr), src)};
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

            return nullptr;
        }

        Owning_Ptr<Parenthesised_Expression> try_paren_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_paren_open)) {
                set_error(u8"expected '('");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr paren_expression = try_expression();
            if(!paren_expression) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_paren_close)) {
                set_error(u8"expected ')'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Parenthesised_Expression(ANTON_MOV(paren_expression), src)};
        }

        Owning_Ptr<Reinterpret_Expression> try_reinterpret_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_reinterpret, true)) {
                set_error(u8"expected 'reinterpret'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_angle_open)) {
                set_error(u8"expected '<'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr target_type = try_type();
            if(!target_type) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_angle_close)) {
                set_error(u8"expected '>'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_paren_open)) {
                set_error(u8"expected '('");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr source = try_expression();
            if(!source) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_comma)) {
                set_error(u8"expected ','");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr index = try_expression();
            if(!index) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_paren_close)) {
                set_error(u8"expected ')'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Reinterpret_Expression{ANTON_MOV(target_type), ANTON_MOV(source), ANTON_MOV(index), src}};
        }

        Owning_Ptr<Expression_If> try_expression_if() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_if, true)) {
                set_error(u8"expected 'if'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr condition = try_expression();
            if(!condition) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(token_brace_close)) {
                set_error(u8"expected an expression before '}'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr true_expression = try_expression();
            if(!true_expression) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_close)) {
                set_error(u8"expected '}' after expression");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(kw_else, true)) {
                set_error(u8"expected an 'else' branch");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Owning_Ptr expression_if = try_expression_if()) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Expression_If(ANTON_MOV(condition), ANTON_MOV(true_expression), ANTON_MOV(expression_if), src)};
            } else {
                if(!_lexer.match(token_brace_open)) {
                    set_error(u8"expected '{'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(_lexer.match(token_brace_close)) {
                    set_error(u8"expected an expression before '}'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr false_expression = try_expression();
                if(!false_expression) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(!_lexer.match(token_brace_close)) {
                    set_error(u8"expected '}' after expression");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Expression_If(ANTON_MOV(condition), ANTON_MOV(true_expression), ANTON_MOV(false_expression), src)};
            }
        }

        Owning_Ptr<Function_Call_Expression> try_function_call_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr identifier = try_identifier();
            if(!identifier) {
                set_error(u8"expected function name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_paren_open)) {
                set_error(u8"expected '(' after function name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            anton::Array<Owning_Ptr<Expression>> arguments;
            if(_lexer.match(token_paren_close)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Function_Call_Expression(ANTON_MOV(identifier), ANTON_MOV(arguments), src)};
            }

            do {
                if(Owning_Ptr expression = try_expression()) {
                    arguments.emplace_back(ANTON_MOV(expression));
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } while(_lexer.match(token_comma));

            if(!_lexer.match(token_paren_close)) {
                set_error(u8"expected ')'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Function_Call_Expression(ANTON_MOV(identifier), ANTON_MOV(arguments), src)};
        }

        Owning_Ptr<Float_Literal> try_float_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();

            anton::String number;
            if(char32 const next_char = _lexer.peek_next(); next_char == '-' || next_char == U'+') {
                number += next_char;
                _lexer.get_next();
            }

            i64 pre_point_digits = 0;
            for(char32 next_char = _lexer.peek_next(); is_digit(next_char); ++pre_point_digits) {
                number += next_char;
                _lexer.get_next();
                next_char = _lexer.peek_next();
            }

            // A decimal number must not have leading 0 except when the number is 0
            if(number.size_bytes() > 1 && number.data()[0] == '0') {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(pre_point_digits == 0) {
                number += U'0';
            }

            bool has_period = false;
            i64 post_point_digits = 0;
            if(_lexer.peek_next() == '.') {
                has_period = true;
                number += '.';
                _lexer.get_next();
                for(char32 next_char = _lexer.peek_next(); is_digit(next_char); ++post_point_digits) {
                    number += next_char;
                    _lexer.get_next();
                    next_char = _lexer.peek_next();
                }

                if(post_point_digits == 0) {
                    number += U'0';
                }
            }

            if(pre_point_digits == 0 && post_point_digits == 0) {
                set_error(u8"not a floating point constant", state_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            bool has_e = false;
            if(char32 const e = _lexer.peek_next(); e == U'e' || e == U'E') {
                has_e = true;
                number += 'E';
                _lexer.get_next();
                if(char32 const sign = _lexer.peek_next(); sign == '-' || sign == U'+') {
                    number += sign;
                    _lexer.get_next();
                }

                i64 e_digits = 0;
                for(char32 next_char = _lexer.peek_next(); is_digit(next_char); ++e_digits) {
                    number += next_char;
                    _lexer.get_next();
                    next_char = _lexer.peek_next();
                }

                if(e_digits == 0) {
                    set_error("exponent has no digits");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            if(!has_e && !has_period) {
                set_error(u8"not a floating point constant", state_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Float_Literal_Type type = Float_Literal_Type::f32;
            Lexer_State const suffix_backup = _lexer.get_current_state_no_skip();
            anton::String suffix;
            for(char32 next = _lexer.peek_next(); is_identifier_character(next); next = _lexer.peek_next()) {
                suffix += next;
                _lexer.get_next();
            }

            if(suffix == u8"d" || suffix == u8"D") {
                type = Float_Literal_Type::f64;
            } else if(suffix != u8"") {
                set_error(u8"invalid suffix '" + suffix + u8"' on floating point literal", suffix_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Float_Literal(ANTON_MOV(number), type, src)};
        }

        Owning_Ptr<Integer_Literal> try_integer_literal() {
            // Section 4.1.3 of The OpenGL Shading Language 4.60.7 states that integer literals are never negative.
            // Instead the leading '-' is always interpreted as a unary minus operator. We follow the same convention.

            Lexer_State const state_backup = _lexer.get_current_state();
            // Binary literal
            bool const has_bin_prefix = _lexer.match(u8"0b") || _lexer.match(u8"0B");
            if(has_bin_prefix) {
                anton::String out;
                while(is_binary_digit(_lexer.peek_next())) {
                    char32 const digit = _lexer.get_next();
                    out += digit;
                }

                if(char32 const next = _lexer.peek_next(); is_digit(next)) {
                    set_error(u8"invalid digit '" + anton::String::from_utf32(&next, 4) + u8"' in binary integer literal");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Integer_Literal_Type type = Integer_Literal_Type::i32;
                Lexer_State const suffix_backup = _lexer.get_current_state_no_skip();
                anton::String suffix;
                for(char32 next = _lexer.peek_next(); is_identifier_character(next); next = _lexer.peek_next()) {
                    suffix += next;
                    _lexer.get_next();
                }

                if(suffix == u8"u" || suffix == u8"U") {
                    type = Integer_Literal_Type::u32;
                } else if(suffix != u8"") {
                    set_error(u8"invalid suffix '" + suffix + u8"' on integer literal", suffix_backup);
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Integer_Literal{ANTON_MOV(out), type, Integer_Literal_Base::bin, src}};
            }

            // Octal literal
            bool const has_oct_prefix = _lexer.match(u8"0o") || _lexer.match(u8"0O");
            if(has_oct_prefix) {
                anton::String out;
                while(is_octal_digit(_lexer.peek_next())) {
                    char32 const digit = _lexer.get_next();
                    out += digit;
                }

                if(char32 const next = _lexer.peek_next(); is_digit(next)) {
                    set_error(u8"invalid digit '" + anton::String::from_utf32(&next, 4) + u8"' in octal integer literal");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Integer_Literal_Type type = Integer_Literal_Type::i32;
                Lexer_State const suffix_backup = _lexer.get_current_state_no_skip();
                anton::String suffix;
                for(char32 next = _lexer.peek_next(); is_identifier_character(next); next = _lexer.peek_next()) {
                    suffix += next;
                    _lexer.get_next();
                }

                if(suffix == u8"u" || suffix == u8"U") {
                    type = Integer_Literal_Type::u32;
                } else if(suffix != u8"") {
                    set_error(u8"invalid suffix '" + suffix + u8"' on integer literal", suffix_backup);
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Integer_Literal{ANTON_MOV(out), type, Integer_Literal_Base::oct, src}};
            }

            // Hexadecimal literal
            bool const has_hex_prefix = _lexer.match(u8"0x") || _lexer.match(u8"0X");
            if(has_hex_prefix) {
                anton::String out;
                while(is_hexadecimal_digit(_lexer.peek_next())) {
                    char32 const digit = _lexer.get_next();
                    out += digit;
                }

                Integer_Literal_Type type = Integer_Literal_Type::i32;
                Lexer_State const suffix_backup = _lexer.get_current_state_no_skip();
                anton::String suffix;
                for(char32 next = _lexer.peek_next(); is_identifier_character(next); next = _lexer.peek_next()) {
                    suffix += next;
                    _lexer.get_next();
                }

                if(suffix == u8"u" || suffix == u8"U") {
                    type = Integer_Literal_Type::u32;
                } else if(suffix != u8"") {
                    set_error(u8"invalid suffix '" + suffix + u8"' on integer literal", suffix_backup);
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Integer_Literal{ANTON_MOV(out), type, Integer_Literal_Base::hex, src}};
            }

            // Decimal literal
            anton::String out;
            if(char32 const next = _lexer.peek_next(); !is_digit(next)) {
                set_error(u8"expected integer literal");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(is_digit(_lexer.peek_next())) {
                char32 const digit = _lexer.get_next();
                out += digit;
            }

            Integer_Literal_Type type = Integer_Literal_Type::i32;
            Lexer_State const suffix_backup = _lexer.get_current_state_no_skip();
            anton::String suffix;
            for(char32 next = _lexer.peek_next(); is_identifier_character(next); next = _lexer.peek_next()) {
                suffix += next;
                _lexer.get_next();
            }

            if(suffix == u8"u" || suffix == u8"U") {
                type = Integer_Literal_Type::u32;
            } else if(suffix != u8"") {
                set_error(u8"invalid suffix '" + suffix + "' on integer literal", suffix_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Integer_Literal{ANTON_MOV(out), type, Integer_Literal_Base::dec, src}};
        }

        Owning_Ptr<Bool_Literal> try_bool_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match(kw_true, true)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Bool_Literal(true, src)};
            } else if(_lexer.match(kw_false, true)) {
                Lexer_State const end_state = _lexer.get_current_state_no_skip();
                Source_Info const src = src_info(state_backup, end_state);
                return Owning_Ptr{new Bool_Literal(false, src)};
            } else {
                set_error("expected bool literal");
                return nullptr;
            }
        }

        Owning_Ptr<String_Literal> try_string_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_double_quote)) {
                set_error(u8"expected \"");
                return nullptr;
            }

            anton::String string;
            while(true) {
                char32 next_char = _lexer.peek_next();
                if(next_char == U'\n') {
                    // We disallow newlines inside string literals
                    set_error(u8"newlines are not allowed in string literals");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                } else if(next_char == eof_char32) {
                    set_error(u8"unexpected end of file");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                } else if(next_char == U'\\') {
                    string += _lexer.get_next();
                    string += _lexer.get_next();
                } else if(next_char == U'\"') {
                    _lexer.get_next();
                    break;
                } else {
                    string += next_char;
                    _lexer.get_next();
                }
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new String_Literal(ANTON_MOV(string), src)};
        }

        Owning_Ptr<Identifier> try_identifier() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::String identifier;
            if(!_lexer.match_identifier(identifier)) {
                set_error(u8"expected an identifier");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(is_keyword(identifier)) {
                anton::String msg = u8"keyword '" + identifier + "' may not be used as identifier";
                set_error(msg, state_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Identifier(ANTON_MOV(identifier), src)};
        }

        Owning_Ptr<Identifier_Expression> try_identifier_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::String identifier;
            if(!_lexer.match_identifier(identifier)) {
                set_error(u8"expected an identifier");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(is_keyword(identifier)) {
                anton::String msg = u8"keyword '" + identifier + "' may not be used as identifier";
                set_error(msg, state_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const end_state = _lexer.get_current_state_no_skip();
            Source_Info const src = src_info(state_backup, end_state);
            return Owning_Ptr{new Identifier_Expression(ANTON_MOV(identifier), src)};
        }
    };

    anton::Expected<Declaration_List, Parse_Error> parse_source(anton::String_View const source_code, anton::String_View const source_name) {
        Parser parser(source_code, source_name);
        anton::Expected<Declaration_List, Parse_Error> ast = parser.build_ast();
        return ast;
    }
} // namespace vush
