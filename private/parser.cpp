#include <parser.hpp>

#include <fstream>

// TODO: When matching keywords, ensure that the keyword is followed by non-identifier character (Find a cleaner way).
// TODO: Figure out a way to match operators that use overlapping symbols (+ and +=) in a clean way.
// TODO: Add settings block.
// TODO: Add imports.

namespace vush {
    // keywords
    static constexpr std::string_view kw_if = "if";
    static constexpr std::string_view kw_else = "else";
    static constexpr std::string_view kw_switch = "switch";
    static constexpr std::string_view kw_case = "case";
    static constexpr std::string_view kw_default = "default";
    static constexpr std::string_view kw_for = "for";
    static constexpr std::string_view kw_while = "while";
    static constexpr std::string_view kw_do = "do";
    static constexpr std::string_view kw_return = "return";
    static constexpr std::string_view kw_break = "break";
    static constexpr std::string_view kw_continue = "continue";
    static constexpr std::string_view kw_true = "true";
    static constexpr std::string_view kw_false = "false";
    static constexpr std::string_view kw_from = "from";
    static constexpr std::string_view kw_struct = "struct";

    // separators and operators
    static constexpr std::string_view token_brace_open = "{";
    static constexpr std::string_view token_brace_close = "}";
    static constexpr std::string_view token_bracket_open = "[";
    static constexpr std::string_view token_bracket_close = "]";
    static constexpr std::string_view token_paren_open = "(";
    static constexpr std::string_view token_paren_close = ")";
    static constexpr std::string_view token_semicolon = ";";
    static constexpr std::string_view token_colon = ":";
    static constexpr std::string_view token_scope_resolution = "::";
    static constexpr std::string_view token_comma = ",";
    static constexpr std::string_view token_question = "?";
    static constexpr std::string_view token_dot = ".";
    static constexpr std::string_view token_plus = "+";
    static constexpr std::string_view token_minus = "-";
    static constexpr std::string_view token_multiply = "*";
    static constexpr std::string_view token_divide = "/";
    static constexpr std::string_view token_remainder = "%";
    static constexpr std::string_view token_logic_and = "&&";
    static constexpr std::string_view token_bit_and = "&";
    static constexpr std::string_view token_logic_or = "||";
    static constexpr std::string_view token_logic_xor = "^^";
    static constexpr std::string_view token_bit_or = "|";
    static constexpr std::string_view token_bit_xor = "^";
    static constexpr std::string_view token_logic_not = "!";
    static constexpr std::string_view token_bit_not = "~";
    static constexpr std::string_view token_bit_lshift = "<<";
    static constexpr std::string_view token_bit_rshift = ">>";
    static constexpr std::string_view token_equal = "==";
    static constexpr std::string_view token_not_equal = "!=";
    static constexpr std::string_view token_less = "<";
    static constexpr std::string_view token_greater = ">";
    static constexpr std::string_view token_less_equal = "<=";
    static constexpr std::string_view token_greater_equal = ">=";
    static constexpr std::string_view token_assign = "=";
    static constexpr std::string_view token_drill = "->";
    static constexpr std::string_view token_increment = "++";
    static constexpr std::string_view token_decrement = "--";
    static constexpr std::string_view token_compound_plus = "+=";
    static constexpr std::string_view token_compound_minus = "-=";
    static constexpr std::string_view token_compound_multiply = "*=";
    static constexpr std::string_view token_compound_divide = "/=";
    static constexpr std::string_view token_compound_remainder = "%=";
    static constexpr std::string_view token_compound_bit_and = "&=";
    static constexpr std::string_view token_compound_bit_or = "|=";
    static constexpr std::string_view token_compound_bit_xor = "^=";
    static constexpr std::string_view token_compound_bit_lshift = "<<=";
    static constexpr std::string_view token_compound_bit_rshift = ">>=";

    static bool is_whitespace(char32 c) {
        return (c <= 32) | (c == 127);
    }

    static bool is_digit(char32 c) {
        return c >= 48 && c < 58;
    }

    static bool is_alpha(char32 c) {
        return (c >= 97 && c < 123) || (c >= 65 && c < 91);
    }

    static bool is_first_identifier_character(char32 c) {
        return c == '_' || is_alpha(c);
    }

    static bool is_identifier_character(char32 c) {
        return c == '_' || is_digit(c) || is_alpha(c);
    }

    class Lexer_State {
    public:
        i64 stream_offset;
        i64 line;
        i64 column;
    };

    class Lexer {
    public:
        Lexer(std::istream& file): _stream(file) {}

        bool match(std::string_view const string, bool const must_not_be_followed_by_identifier_char = false) {
            ignore_whitespace_and_comments();

            Lexer_State const state_backup = get_current_state();
            for(char c: string) {
                if(get_next() != (char32)c) {
                    restore_state(state_backup);
                    return false;
                }
            }
            if(must_not_be_followed_by_identifier_char) {
                return !is_identifier_character(peek_next());
            } else {
                return true;
            }
        }

        // TODO: String interning if it becomes too slow/memory heavy.
        bool match_identifier(std::string& out) {
            ignore_whitespace_and_comments();

            // No need to backup the lexer state since we can predict whether the next
            // sequence of characters is an identifier using only the first character.
            char32 const next_char = peek_next();
            if(!is_first_identifier_character(next_char)) {
                return false;
            }

            get_next();
            out += next_char;
            for(char32 peek = peek_next(); is_identifier_character(peek); peek = peek_next()) {
                out += peek;
                get_next();
            }
            return true;
        }

        bool match_eof() {
            ignore_whitespace_and_comments();
            char32 const next_char = peek_next();
            return next_char == (char32)std::char_traits<char>::eof();
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
            return {_stream.tellg(), _line, _column};
        }

        void restore_state(Lexer_State const state) {
            _stream.seekg(state.stream_offset, std::ios_base::beg);
            _line = state.line;
            _column = state.column;
        }

        char32 get_next() {
            char32 const c = _stream.get();
            if(c == '\n') {
                _line += 1;
                _column = 0;
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
        std::istream& _stream;
        i64 _line = 0;
        i64 _column = 0;
    };

    class Parser {
    public:
        Parser(std::istream& stream): _lexer(stream) {}

        Owning_Ptr<Syntax_Tree_Node> build_ast() {
            Owning_Ptr declarations = new Declaration_List;
            while(!_lexer.match_eof()) {
                if(Declaration* declaration = try_declaration()) {
                    declarations->append(declaration);
                } else {
                    return nullptr;
                }
            }
            return declarations.release();
        }

        [[nodiscard]] Parse_Error get_last_error() const {
            return _last_error;
        }

    private:
        Lexer _lexer;
        Parse_Error _last_error;

        void set_error(std::string_view const message, Lexer_State const state) {
            if(state.stream_offset > _last_error.file_offset) {
                _last_error.message = message;
                _last_error.line = state.line;
                _last_error.column = state.column;
                _last_error.file_offset = state.stream_offset;
            }
        }

        void set_error(std::string_view const message) {
            Lexer_State const state = _lexer.get_current_state();
            if(state.stream_offset > _last_error.file_offset) {
                _last_error.message = message;
                _last_error.line = state.line;
                _last_error.column = state.column;
                _last_error.file_offset = state.stream_offset;
            }
        }

        Declaration* try_declaration() {
            if(Declaration_If* declaration_if = try_declaration_if()) {
                return declaration_if;
            }

            if(Struct_Decl* struct_decl = try_struct_decl()) {
                return struct_decl;
            }

            if(Pass_Stage_Declaration* pass_stage = try_pass_stage_declaration()) {
                return pass_stage;
            }

            if(Function_Declaration* function_declaration = try_function_declaration()) {
                return function_declaration;
            }

            if(Variable_Declaration* variable_declaration = try_variable_declaration()) {
                return variable_declaration;
            }

            return nullptr;
        }

        Declaration_If* try_declaration_if() {
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

            Owning_Ptr true_declarations = new Declaration_List;
            while(!_lexer.match(token_brace_close)) {
                if(_lexer.match_eof()) {
                    set_error(u8"unexpected end of file");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(Declaration* declaration = try_declaration()) {
                    true_declarations->append(declaration);
                } else {
                    return nullptr;
                }
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

            if(Declaration_If* if_declaration = try_declaration_if()) {
                Owning_Ptr false_declarations = new Declaration_List;
                false_declarations->append(if_declaration);
                return new Declaration_If(condition.release(), true_declarations.release(), false_declarations.release());
            } else {
                if(!_lexer.match(token_brace_open)) {
                    set_error(u8"expected '{'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr false_declarations = new Declaration_List;
                while(!_lexer.match(token_brace_close)) {
                    if(_lexer.match_eof()) {
                        set_error(u8"unexpected end of file");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    if(Declaration* declaration = try_declaration()) {
                        false_declarations->append(declaration);
                    } else {
                        return nullptr;
                    }
                }

                if(!_lexer.match(token_brace_close)) {
                    set_error(u8"expected '}' after expression");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                return new Declaration_If(condition.release(), true_declarations.release(), false_declarations.release());
            }
        }

        Variable_Declaration* try_variable_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr var_type = try_type();
            if(!var_type) {
                set_error("expected type");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Identifier> var_name = nullptr;
            if(std::string identifier; _lexer.match_identifier(identifier)) {
                var_name = new Identifier(std::move(identifier));
            } else {
                set_error("expected variable name");
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
                set_error("expected ';' after variable declaration");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new Variable_Declaration(var_type.release(), var_name.release(), initializer.release());
        }

        Struct_Decl* try_struct_decl() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_struct, true)) {
                set_error(u8"expected 'struct'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Identifier> struct_name;
            if(std::string name; _lexer.match_identifier(name)) {
                struct_name = new Identifier(std::move(name));
            } else {
                set_error("expected identifier");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(token_brace_close)) {
                set_error(u8"empty structs are not allowed");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr struct_decl = new Struct_Decl(struct_name.release());
            while(true) {
                if(Variable_Declaration* decl = try_variable_declaration()) {
                    struct_decl->append(decl);
                } else {
                    break;
                }
            }

            if(struct_decl->size() == 0) {
                set_error(u8"empty structs are not allowed");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_close)) {
                set_error(u8"expected '}'");
                _lexer.restore_state(state_backup);
            }

            return struct_decl.release();
        }

        Pass_Stage_Declaration* try_pass_stage_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr return_type = try_type();
            if(!return_type) {
                set_error("expected type");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Identifier> pass = nullptr;
            if(std::string pass_name_str; _lexer.match_identifier(pass_name_str)) {
                pass = new Identifier(std::move(pass_name_str));
            } else {
                set_error("expected pass name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_scope_resolution)) {
                set_error("expected '::' after pass name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Identifier> name = nullptr;
            if(std::string fn_name; _lexer.match_identifier(fn_name)) {
                name = new Identifier(std::move(fn_name));
            } else {
                set_error("expected function name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr param_list = try_function_param_list(true);
            if(!param_list) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr function_body = try_function_body();
            if(!function_body) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new Pass_Stage_Declaration(pass.release(), name.release(), param_list.release(), return_type.release(), function_body.release());
        }

        Function_Declaration* try_function_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr return_type = try_type();
            if(!return_type) {
                set_error("expected type");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Identifier> name = nullptr;
            if(std::string fn_name; _lexer.match_identifier(fn_name)) {
                name = new Identifier(std::move(fn_name));
            } else {
                set_error("expected function name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr param_list = try_function_param_list(false);
            if(!param_list) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr function_body = try_function_body();
            if(!function_body) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new Function_Declaration(name.release(), param_list.release(), return_type.release(), function_body.release());
        }

        Function_Param_List* try_function_param_list(bool const allow_sourced_params) {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_paren_open)) {
                set_error("expected '('");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(token_paren_close)) {
                return new Function_Param_List;
            }

            // Match parameters
            Owning_Ptr param_list = new Function_Param_List;
            {
                Lexer_State const param_list_backup = _lexer.get_current_state();
                do {
                    if(Function_Param* param = try_function_param(allow_sourced_params)) {
                        param_list->append_parameter(param);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } while(_lexer.match(token_comma));
            }

            if(!_lexer.match(token_paren_close)) {
                set_error("expected ')' after function parameter list");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return param_list.release();
        }

        Function_Param* try_function_param(bool const allow_sourced_params) {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(Function_Param_If* param_if = try_function_param_if(allow_sourced_params)) {
                return param_if;
            }

            Owning_Ptr<Identifier> identifier = nullptr;
            if(std::string identifier_str; _lexer.match_identifier(identifier_str)) {
                identifier = new Identifier(std::move(identifier_str));
            } else {
                set_error("expected parameter name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_colon)) {
                set_error("expected ':' after parameter name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr parameter_type = try_type();
            if(!parameter_type) {
                set_error("expected parameter type");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(kw_from, true)) {
                if(!allow_sourced_params) {
                    set_error(u8"illegal sourced parameter declaration");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(std::string identifier_str; _lexer.match_identifier(identifier_str)) {
                    Identifier* source = new Identifier(std::move(identifier_str));
                    return new Sourced_Function_Param(identifier.release(), parameter_type.release(), source);
                } else {
                    set_error("expected parameter source after 'from'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else {
                return new Ordinary_Function_Param(identifier.release(), parameter_type.release());
            }
        }

        Function_Param_If* try_function_param_if(bool const allow_sourced_params) {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_if, true)) {
                set_error(u8"expected '{'");
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

            Owning_Ptr true_param = try_function_param(allow_sourced_params);
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
                if(Function_Param_If* param_if = try_function_param_if(allow_sourced_params)) {
                    return new Function_Param_If(condition.release(), true_param.release(), param_if);
                } else {
                    if(!_lexer.match(token_brace_open)) {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    Owning_Ptr false_param = try_function_param(allow_sourced_params);
                    if(!false_param) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    if(!_lexer.match(token_brace_close)) {
                        set_error(u8"expected '}'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    return new Function_Param_If(condition.release(), true_param.release(), false_param.release());
                }
            } else {
                return new Function_Param_If(condition.release(), true_param.release(), nullptr);
            }
        }

        Function_Body* try_function_body() {
            if(!_lexer.match(token_brace_open)) {
                set_error("expected '{' at the beginning of function body");
                return nullptr;
            }

            if(_lexer.match(token_brace_close)) {
                return new Function_Body(nullptr);
            }

            Owning_Ptr statements = try_statement_list();
            if(statements->size() == 0) {
                return nullptr;
            }

            if(!_lexer.match(token_brace_close)) {
                set_error("expected '}' at the end of the function body");
                return nullptr;
            }

            return new Function_Body(statements.release());
        }

        Statement_List* try_statement_list() {
            Statement_List* statements = new Statement_List;
            while(true) {
                if(Block_Statement* block_statement = try_block_statement()) {
                    statements->append(block_statement);
                    continue;
                }

                if(If_Statement* if_statement = try_if_statement()) {
                    statements->append(if_statement);
                    continue;
                }

                if(Switch_Statement* switch_statement = try_switch_statement()) {
                    statements->append(switch_statement);
                    continue;
                }

                if(For_Statement* for_statement = try_for_statement()) {
                    statements->append(for_statement);
                    continue;
                }

                if(While_Statement* while_statement = try_while_statement()) {
                    statements->append(while_statement);
                    continue;
                }

                if(Do_While_Statement* do_while_statement = try_do_while_statement()) {
                    statements->append(do_while_statement);
                    continue;
                }

                if(Return_Statement* return_statement = try_return_statement()) {
                    statements->append(return_statement);
                    continue;
                }

                if(Break_Statement* break_statement = try_break_statement()) {
                    statements->append(break_statement);
                    continue;
                }

                if(Continue_Statement* continue_statement = try_continue_statement()) {
                    statements->append(continue_statement);
                    continue;
                }

                if(Variable_Declaration* decl = try_variable_declaration()) {
                    Declaration_Statement* decl_stmt = new Declaration_Statement(decl);
                    statements->append(decl_stmt);
                    continue;
                }

                if(Expression_Statement* expr_stmt = try_expression_statement()) {
                    statements->append(expr_stmt);
                    continue;
                }

                return statements;
            }
        }

        Type* try_type() {
            if(std::string name; _lexer.match_identifier(name)) {
                return new Type(std::move(name));
            } else {
                set_error("expected identifier");
                return nullptr;
            }
        }

        Block_Statement* try_block_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_brace_open)) {
                set_error("expected '{' at the start of the block");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(token_brace_close)) {
                return new Block_Statement(nullptr);
            }

            Owning_Ptr statements = try_statement_list();
            if(statements->size() == 0) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_close)) {
                set_error("expected '}' at the end of the block");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new Block_Statement(statements.release());
        }

        If_Statement* try_if_statement() {
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

            Owning_Ptr true_statement = try_block_statement();
            if(!true_statement) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(kw_else, true)) {
                if(If_Statement* if_statement = try_if_statement()) {
                    return new If_Statement(condition.release(), true_statement.release(), if_statement);
                } else {
                    if(Block_Statement* false_statement = try_block_statement()) {
                        return new If_Statement(condition.release(), true_statement.release(), false_statement);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                }
            } else {
                return new If_Statement(condition.release(), true_statement.release(), nullptr);
            }
        }

        Switch_Statement* try_switch_statement() {
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

            Owning_Ptr switch_statement = new Switch_Statement;
            while(true) {
                if(_lexer.match(kw_case, true)) {
                    Owning_Ptr condition = try_expression();
                    if(!condition) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    if(!_lexer.match(token_colon)) {
                        set_error(u8"expected ':' after case label");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    Owning_Ptr statement_list = try_statement_list();
                    if(statement_list) {
                        switch_statement->append(new Case_Statement(condition.release(), statement_list.release()));
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(kw_default, true)) {
                    if(!_lexer.match(token_colon)) {
                        set_error(u8"expected ':' after case label");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    Owning_Ptr statement_list = try_statement_list();
                    if(statement_list) {
                        switch_statement->append(new Default_Case_Statement(statement_list.release()));
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    break;
                }
            }

            if(!_lexer.match(token_brace_close)) {
                set_error(u8"expected '}'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return switch_statement.release();
        }

        For_Statement* try_for_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_for, true)) {
                set_error("expected 'for'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            // Match variable

            Owning_Ptr<Variable_Declaration> variable_declaration = nullptr;
            if(!_lexer.match(token_semicolon)) {
                Owning_Ptr var_type = try_type();
                if(!var_type) {
                    set_error("expected type");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr<Identifier> var_name = nullptr;
                if(std::string identifier; _lexer.match_identifier(identifier)) {
                    var_name = new Identifier(std::move(identifier));
                } else {
                    set_error("expected variable name");
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
                    set_error("expected ';' after variable declaration");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                variable_declaration = new Variable_Declaration(var_type.release(), var_name.release(), initializer.release());
            }

            Owning_Ptr<Expression> condition = nullptr;
            if(!_lexer.match(token_semicolon)) {
                condition = try_expression();
                if(!condition) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(!_lexer.match(token_semicolon)) {
                    set_error("expected ';' after variable declaration");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            Owning_Ptr post_expression = try_expression();

            Owning_Ptr block = try_block_statement();
            if(!block) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new For_Statement(variable_declaration.release(), condition.release(), post_expression.release(), block.release());
        }

        While_Statement* try_while_statement() {
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

            Owning_Ptr block = try_block_statement();
            if(!block) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new While_Statement(condition.release(), block.release());
        }

        Do_While_Statement* try_do_while_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_do, true)) {
                set_error("expected 'do'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr block = try_block_statement();
            if(!block) {
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

            return new Do_While_Statement(condition.release(), block.release());
        }

        Return_Statement* try_return_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_return, true)) {
                set_error("expected 'return'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr return_expr = try_expression();
            if(!return_expr) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_semicolon)) {
                set_error("expected ';' after return statement");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new Return_Statement(return_expr.release());
        }

        Break_Statement* try_break_statement() {
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

            return new Break_Statement();
        }

        Continue_Statement* try_continue_statement() {
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

            return new Continue_Statement();
        }

        Expression_Statement* try_expression_statement() {
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

            return new Expression_Statement(expression.release());
        }

        Expression* try_expression() {
            if(Expression* expr = try_assignment_expression()) {
                return expr;
            } else {
                return nullptr;
            }
        }

        Expression* try_assignment_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_logic_or_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

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
                return lhs.release();
            }

            Owning_Ptr rhs = try_assignment_expression();
            if(!rhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(is_direct) {
                return new Assignment_Expression(lhs.release(), rhs.release());
            } else {
                return new Arithmetic_Assignment_Expression(type, lhs.release(), rhs.release());
            }
        }

        Expression* try_logic_or_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_logic_xor_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(_lexer.match(token_logic_or)) {
                if(Expression* rhs = try_logic_xor_expr()) {
                    lhs = new Logic_Or_Expr(lhs.release(), rhs);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return lhs.release();
        }

        Expression* try_logic_xor_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_logic_and_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(_lexer.match(token_logic_xor)) {
                if(Expression* rhs = try_logic_and_expr()) {
                    lhs = new Logic_Xor_Expr(lhs.release(), rhs);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return lhs.release();
        }

        Expression* try_logic_and_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_bit_or_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(_lexer.match(token_logic_and)) {
                if(Expression* rhs = try_bit_or_expr()) {
                    lhs = new Logic_And_Expr(lhs.release(), rhs);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return lhs.release();
        }

        Expression* try_bit_or_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_bit_xor_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_bit_or) || _lexer.match(token_logic_or)) {
                _lexer.restore_state(check_backup);
                return lhs.release();
            }

            while(_lexer.match(token_bit_or)) {
                if(Expression* rhs = try_bit_xor_expr()) {
                    lhs = new Bit_Or_Expr(lhs.release(), rhs);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return lhs.release();
        }

        Expression* try_bit_xor_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_bit_and_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_bit_xor) || _lexer.match(token_logic_xor)) {
                _lexer.restore_state(check_backup);
                return lhs.release();
            }

            while(_lexer.match(token_bit_xor)) {
                if(Expression* rhs = try_bit_and_expr()) {
                    lhs = new Bit_Xor_Expr(lhs.release(), rhs);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return lhs.release();
        }

        Expression* try_bit_and_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_relational_equality_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_bit_and) || _lexer.match(token_logic_and)) {
                _lexer.restore_state(check_backup);
                return lhs.release();
            }

            while(_lexer.match(token_bit_and)) {
                if(Expression* rhs = try_relational_equality_expression()) {
                    lhs = new Bit_And_Expr(lhs.release(), rhs);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return lhs.release();
        }

        Expression* try_relational_equality_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_relational_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(true) {
                if(_lexer.match(token_equal)) {
                    if(Expression* rhs = try_relational_expression()) {
                        lhs = new Relational_Equality_Expression(true, lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_not_equal)) {
                    if(Expression* rhs = try_relational_expression()) {
                        lhs = new Relational_Equality_Expression(false, lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs.release();
                }
            }
        }

        Expression* try_relational_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_lshift_rshift_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(true) {
                Relational_Type type;
                if(_lexer.match(token_less_equal)) {
                    type = Relational_Type::less_equal;
                } else if(_lexer.match(token_greater_equal)) {
                    type = Relational_Type::greater_equal;
                } else if(_lexer.match(token_less)) {
                    type = Relational_Type::less_than;
                } else if(_lexer.match(token_greater)) {
                    type = Relational_Type::greater_than;
                } else {
                    return lhs.release();
                }

                if(Expression* rhs = try_lshift_rshift_expr()) {
                    lhs = new Relational_Expression(type, lhs.release(), rhs);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }
        }

        Expression* try_lshift_rshift_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_add_sub_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state();
               _lexer.match(token_compound_bit_lshift) || _lexer.match(token_compound_bit_rshift)) {
                _lexer.restore_state(check_backup);
                return lhs.release();
            }

            while(true) {
                if(_lexer.match(token_bit_lshift)) {
                    if(Expression* rhs = try_add_sub_expr()) {
                        lhs = new LShift_Expr(lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_bit_rshift)) {
                    if(Expression* rhs = try_add_sub_expr()) {
                        lhs = new RShift_Expr(lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs.release();
                }
            }
        }

        Expression* try_add_sub_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_mul_div_mod_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_plus) || _lexer.match(token_compound_minus)) {
                _lexer.restore_state(check_backup);
                return lhs.release();
            }

            while(true) {
                if(_lexer.match(token_plus)) {
                    if(Expression* rhs = try_mul_div_mod_expr()) {
                        lhs = new Add_Expr(lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_minus)) {
                    if(Expression* rhs = try_mul_div_mod_expr()) {
                        lhs = new Sub_Expr(lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs.release();
                }
            }
        }

        Expression* try_mul_div_mod_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_unary_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state();
               _lexer.match(token_compound_multiply) || _lexer.match(token_compound_divide) || _lexer.match(token_compound_remainder)) {
                _lexer.restore_state(check_backup);
                return lhs.release();
            }

            while(true) {
                if(_lexer.match(token_multiply)) {
                    if(Expression* rhs = try_unary_expression()) {
                        lhs = new Mul_Expr(lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_divide)) {
                    if(Expression* rhs = try_unary_expression()) {
                        lhs = new Div_Expr(lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_remainder)) {
                    if(Expression* rhs = try_unary_expression()) {
                        lhs = new Mod_Expr(lhs.release(), rhs);
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs.release();
                }
            }
        }

        Expression* try_unary_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match(token_increment)) {
                if(Expression* expr = try_unary_expression()) {
                    return new Prefix_Inc_Dec_Expression(true, expr);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_decrement)) {
                if(Expression* expr = try_unary_expression()) {
                    return new Prefix_Inc_Dec_Expression(false, expr);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_plus)) {
                if(Expression* expr = try_unary_expression()) {
                    return new Unary_Expression(Unary_Type::plus, expr);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_minus)) {
                if(Expression* expr = try_unary_expression()) {
                    return new Unary_Expression(Unary_Type::minus, expr);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_logic_not)) {
                if(Expression* expr = try_unary_expression()) {
                    return new Unary_Expression(Unary_Type::logic_not, expr);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_bit_not)) {
                if(Expression* expr = try_unary_expression()) {
                    return new Unary_Expression(Unary_Type::bit_not, expr);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else {
                return try_postfix_expression();
            }
        }

        Expression* try_postfix_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr primary_expr = try_primary_expression();
            if(!primary_expr) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr<Expression> expr = primary_expr.release();
            while(true) {
                if(_lexer.match(token_dot)) {
                    if(std::string name; _lexer.match_identifier(name)) {
                        expr = new Member_Access_Expression(expr.release(), new Identifier(std::move(name)));
                    } else {
                        set_error("expected function name");
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
                        set_error("expected ']'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    expr = new Array_Access_Expression(expr.release(), index.release());
                } else if(_lexer.match(token_increment)) {
                    expr = new Postfix_Inc_Dec_Expression(true, expr.release());
                } else if(_lexer.match(token_decrement)) {
                    expr = new Postfix_Inc_Dec_Expression(false, expr.release());
                } else {
                    break;
                }
            }

            return expr.release();
        }

        Expression* try_primary_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match(token_paren_open)) {
                Owning_Ptr paren_expression = try_expression();
                if(!paren_expression) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(_lexer.match(token_paren_close)) {
                    return paren_expression.release();
                } else {
                    set_error("expected ')'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            if(Expression_If* expression_if = try_expression_if()) {
                return expression_if;
            }

            if(Float_Literal* float_literal = try_float_literal()) {
                return float_literal;
            }

            if(Integer_Literal* integer_literal = try_integer_literal()) {
                return integer_literal;
            }

            if(Bool_Literal* bool_literal = try_bool_literal()) {
                return bool_literal;
            }

            if(Function_Call_Expression* function_call = try_function_call_expression()) {
                return function_call;
            }

            if(Identifier_Expression* identifier_expression = try_identifier_expression()) {
                return identifier_expression;
            }

            return nullptr;
        }

        Expression_If* try_expression_if() {
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

            if(Expression_If* expression_if = try_expression_if()) {
                return new Expression_If(condition.release(), true_expression.release(), expression_if);
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

                return new Expression_If(condition.release(), true_expression.release(), false_expression.release());
            }
        }

        Function_Call_Expression* try_function_call_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr<Identifier> identifier = nullptr;
            if(std::string name; _lexer.match_identifier(name)) {
                identifier = new Identifier(std::move(name));
            } else {
                set_error("expected function name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_paren_open)) {
                set_error("expected '(' after function name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr arg_list = new Argument_List;
            if(_lexer.match(token_paren_close)) {
                return new Function_Call_Expression(identifier.release(), arg_list.release());
            }

            do {
                if(Expression* expression = try_expression()) {
                    arg_list->append(expression);
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } while(_lexer.match(token_comma));

            if(!_lexer.match(token_paren_close)) {
                set_error("expected ')'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return new Function_Call_Expression(identifier.release(), arg_list.release());
        }

        Float_Literal* try_float_literal() {
            _lexer.ignore_whitespace_and_comments();
            Lexer_State const state_backup = _lexer.get_current_state();

            std::string number;
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
                set_error("not a floating point constant");
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
                set_error(u8"not a floating point constant");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(is_identifier_character(_lexer.peek_next())) {
                std::string suffix;
                while(is_identifier_character(_lexer.peek_next())) {
                    suffix += _lexer.get_next();
                }

                if(suffix == "f" || suffix == "F" || suffix == "lf" || suffix == "LF") {
                    number += suffix;
                } else {
                    std::string error = "invalid suffix '" + suffix + "' on floating point constant";
                    set_error(error);
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return new Float_Literal(std::move(number));
        }

        Integer_Literal* try_integer_literal() {
            _lexer.ignore_whitespace_and_comments();

            Lexer_State const state_backup = _lexer.get_current_state();

            std::string out;
            char32 next = _lexer.peek_next();
            if(next == '-' || next == '+') {
                out += next;
                _lexer.get_next();
            }

            // TODO: Add handling for leading zeros.
            // TODO: Add hexadecimal and binary literals.

            i64 length = 0;
            while(is_digit(_lexer.peek_next())) {
                out += _lexer.get_next();
                length += 1;
            }

            if(length != 0) {
                return new Integer_Literal(std::move(out));
            } else {
                set_error("expected more than 0 digits");
                _lexer.restore_state(state_backup);
                return nullptr;
            }
        }

        Bool_Literal* try_bool_literal() {
            if(_lexer.match(kw_true, true)) {
                return new Bool_Literal(true);
            } else if(_lexer.match(kw_false, true)) {
                return new Bool_Literal(false);
            } else {
                set_error("expected bool literal");
                return nullptr;
            }
        }

        Identifier_Expression* try_identifier_expression() {
            if(std::string name; _lexer.match_identifier(name)) {
                Identifier* identifier = new Identifier(name);
                return new Identifier_Expression(identifier);
            } else {
                set_error("expected an identifer");
                return nullptr;
            }
        }
    };

    Expected<Owning_Ptr<Syntax_Tree_Node>, Parse_Error> parse_file(std::string const& path) {
        std::string const path_str(path);
        std::ifstream file(path_str);
        if(!file) {
            std::string msg = "could not open file " + path + " for reading.";
            return {expected_error, std::move(msg), 0, 0, 0};
        }

        Parser parser(file);
        Owning_Ptr ast = parser.build_ast();
        if(ast) {
            return {expected_value, std::move(ast)};
        } else {
            Parse_Error error = parser.get_last_error();
            return {expected_error, std::move(error)};
        }
    }
} // namespace vush
