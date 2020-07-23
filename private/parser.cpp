#include <parser.hpp>

#include <anton/optional.hpp>
#include <anton/stream.hpp>
#include <anton/string_view.hpp>

// TODO: When matching keywords, ensure that the keyword is followed by non-identifier character (Find a cleaner way).
// TODO: Figure out a way to match operators that use overlapping symbols (+ and +=) in a clean way.
// TODO: const types.
// TODO: add constructors (currently function call which will break if we use an array type).

namespace vush {
    // keywords

    static constexpr anton::String_View kw_if = u8"if";
    static constexpr anton::String_View kw_else = u8"else";
    static constexpr anton::String_View kw_switch = u8"switch";
    static constexpr anton::String_View kw_case = u8"case";
    static constexpr anton::String_View kw_default = u8"default";
    static constexpr anton::String_View kw_for = u8"for";
    static constexpr anton::String_View kw_while = u8"while";
    static constexpr anton::String_View kw_do = u8"do";
    static constexpr anton::String_View kw_return = u8"return";
    static constexpr anton::String_View kw_break = u8"break";
    static constexpr anton::String_View kw_continue = u8"continue";
    static constexpr anton::String_View kw_true = u8"true";
    static constexpr anton::String_View kw_false = u8"false";
    static constexpr anton::String_View kw_from = u8"from";
    static constexpr anton::String_View kw_struct = u8"struct";
    static constexpr anton::String_View kw_import = u8"import";
    static constexpr anton::String_View kw_const = u8"const";
    static constexpr anton::String_View kw_in = u8"in";
    static constexpr anton::String_View kw_source_definition = u8"source_definition";
    static constexpr anton::String_View kw_source = u8"source";
    static constexpr anton::String_View kw_emit = u8"emit";
    static constexpr anton::String_View kw_settings = u8"settings";

    // attributes
    static constexpr anton::String_View attrib_workgroup = u8"workgroup";

    // stages

    static constexpr anton::String_View stage_vertex = u8"vertex";
    static constexpr anton::String_View stage_fragment = u8"fragment";
    static constexpr anton::String_View stage_compute = u8"compute";

    // types

    static constexpr anton::String_View type_void = u8"void";
    static constexpr anton::String_View type_bool = u8"bool";
    static constexpr anton::String_View type_int = u8"int";
    static constexpr anton::String_View type_uint = u8"uint";
    static constexpr anton::String_View type_float = u8"float";
    static constexpr anton::String_View type_double = u8"double";
    static constexpr anton::String_View type_vec2 = u8"vec2";
    static constexpr anton::String_View type_vec3 = u8"vec3";
    static constexpr anton::String_View type_vec4 = u8"vec4";
    static constexpr anton::String_View type_dvec2 = u8"dvec2";
    static constexpr anton::String_View type_dvec3 = u8"dvec3";
    static constexpr anton::String_View type_dvec4 = u8"dvec4";
    static constexpr anton::String_View type_bvec2 = u8"bvec2";
    static constexpr anton::String_View type_bvec3 = u8"bvec3";
    static constexpr anton::String_View type_bvec4 = u8"bvec4";
    static constexpr anton::String_View type_ivec2 = u8"ivec2";
    static constexpr anton::String_View type_ivec3 = u8"ivec3";
    static constexpr anton::String_View type_ivec4 = u8"ivec4";
    static constexpr anton::String_View type_uvec2 = u8"uvec2";
    static constexpr anton::String_View type_uvec3 = u8"uvec3";
    static constexpr anton::String_View type_uvec4 = u8"uvec4";
    static constexpr anton::String_View type_mat2 = u8"mat2";
    static constexpr anton::String_View type_mat2x2 = u8"mat2x2";
    static constexpr anton::String_View type_mat3 = u8"mat3";
    static constexpr anton::String_View type_mat3x3 = u8"mat3x3";
    static constexpr anton::String_View type_mat4 = u8"mat4";
    static constexpr anton::String_View type_mat4x4 = u8"mat4x4";
    static constexpr anton::String_View type_mat2x3 = u8"mat2x3";
    static constexpr anton::String_View type_mat2x4 = u8"mat2x4";
    static constexpr anton::String_View type_mat3x2 = u8"mat3x2";
    static constexpr anton::String_View type_mat3x4 = u8"mat3x4";
    static constexpr anton::String_View type_mat4x2 = u8"mat4x2";
    static constexpr anton::String_View type_mat4x3 = u8"mat4x3";
    static constexpr anton::String_View type_dmat2 = u8"dmat2";
    static constexpr anton::String_View type_dmat2x2 = u8"dmat2x2";
    static constexpr anton::String_View type_dmat3 = u8"dmat3";
    static constexpr anton::String_View type_dmat3x3 = u8"dmat3x3";
    static constexpr anton::String_View type_dmat4 = u8"dmat4";
    static constexpr anton::String_View type_dmat4x4 = u8"dmat4x4";
    static constexpr anton::String_View type_dmat2x3 = u8"dmat2x3";
    static constexpr anton::String_View type_dmat2x4 = u8"dmat2x4";
    static constexpr anton::String_View type_dmat3x2 = u8"dmat3x2";
    static constexpr anton::String_View type_dmat3x4 = u8"dmat3x4";
    static constexpr anton::String_View type_dmat4x2 = u8"dmat4x2";
    static constexpr anton::String_View type_dmat4x3 = u8"dmat4x3";

    // opaque floating

    static constexpr anton::String_View type_sampler1D = u8"sampler1D";
    static constexpr anton::String_View type_texture1D = u8"texture1D";
    static constexpr anton::String_View type_image1D = u8"image1D";
    static constexpr anton::String_View type_sampler1DShadow = u8"sampler1DShadow";
    static constexpr anton::String_View type_sampler1DArray = u8"sampler1DArray";
    static constexpr anton::String_View type_texture1DArray = u8"texture1DArray";
    static constexpr anton::String_View type_image1DArray = u8"image1DArray";
    static constexpr anton::String_View type_sampler1DArrayShadow = u8"sampler1DArrayShadow";
    static constexpr anton::String_View type_sampler2D = u8"sampler2D";
    static constexpr anton::String_View type_texture2D = u8"texture2D";
    static constexpr anton::String_View type_image2D = u8"image2D";
    static constexpr anton::String_View type_sampler2DShadow = u8"sampler2DShadow";
    static constexpr anton::String_View type_sampler2DArray = u8"sampler2DArray";
    static constexpr anton::String_View type_texture2DArray = u8"texture2DArray";
    static constexpr anton::String_View type_image2DArray = u8"image2DArray";
    static constexpr anton::String_View type_sampler2DArrayShadow = u8"sampler2DArrayShadow";
    static constexpr anton::String_View type_sampler2DMS = u8"sampler2DMS";
    static constexpr anton::String_View type_texture2DMS = u8"texture2DMS";
    static constexpr anton::String_View type_image2DMS = u8"image2DMS";
    static constexpr anton::String_View type_sampler2DMSArray = u8"sampler2DMSArray";
    static constexpr anton::String_View type_texture2DMSArray = u8"texture2DMSArray";
    static constexpr anton::String_View type_image2DMSArray = u8"image2DMSArray";
    static constexpr anton::String_View type_sampler2DRect = u8"sampler2DRect";
    static constexpr anton::String_View type_texture2DRect = u8"texture2DRect";
    static constexpr anton::String_View type_image2DRect = u8"image2DRect";
    static constexpr anton::String_View type_sampler2DRectShadow = u8"sampler2DRectShadow";
    static constexpr anton::String_View type_sampler3D = u8"sampler3D";
    static constexpr anton::String_View type_texture3D = u8"texture3D";
    static constexpr anton::String_View type_image3D = u8"image3D";
    static constexpr anton::String_View type_samplerCube = u8"samplerCube";
    static constexpr anton::String_View type_textureCube = u8"textureCube";
    static constexpr anton::String_View type_imageCube = u8"imageCube";
    static constexpr anton::String_View type_samplerCubeShadow = u8"samplerCubeShadow";
    static constexpr anton::String_View type_samplerCubeArray = u8"samplerCubeArray";
    static constexpr anton::String_View type_textureCubeArray = u8"textureCubeArray";
    static constexpr anton::String_View type_imageCubeArray = u8"imageCubeArray";
    static constexpr anton::String_View type_samplerCubeArrayShadow = u8"samplerCubeArrayShadow";
    static constexpr anton::String_View type_samplerBuffer = u8"samplerBuffer";
    static constexpr anton::String_View type_textureBuffer = u8"textureBuffer";
    static constexpr anton::String_View type_imageBuffer = u8"imageBuffer";
    static constexpr anton::String_View type_subpassInput = u8"subpassInput";
    static constexpr anton::String_View type_subpassInputMS = u8"subpassInputMS";

    // opaque signed

    static constexpr anton::String_View type_isampler1D = u8"isampler1D";
    static constexpr anton::String_View type_itexture1D = u8"itexture1D";
    static constexpr anton::String_View type_iimage1D = u8"iimage1D";
    static constexpr anton::String_View type_isampler1DArray = u8"isampler1DArray";
    static constexpr anton::String_View type_itexture1DArray = u8"itexture1DArray";
    static constexpr anton::String_View type_iimage1DArray = u8"iimage1DArray";
    static constexpr anton::String_View type_isampler2D = u8"isampler2D";
    static constexpr anton::String_View type_itexture2D = u8"itexture2D";
    static constexpr anton::String_View type_iimage2D = u8"iimage2D";
    static constexpr anton::String_View type_isampler2DArray = u8"isampler2DArray";
    static constexpr anton::String_View type_itexture2DArray = u8"itexture2DArray";
    static constexpr anton::String_View type_iimage2DArray = u8"iimage2DArray";
    static constexpr anton::String_View type_isampler2DMS = u8"isampler2DMS";
    static constexpr anton::String_View type_itexture2DMS = u8"itexture2DMS";
    static constexpr anton::String_View type_iimage2DMS = u8"iimage2DMS";
    static constexpr anton::String_View type_isampler2DMSArray = u8"isampler2DMSArray";
    static constexpr anton::String_View type_itexture2DMSArray = u8"itexture2DMSArray";
    static constexpr anton::String_View type_iimage2DMSArray = u8"iimage2DMSArray";
    static constexpr anton::String_View type_isampler2DRect = u8"isampler2DRect";
    static constexpr anton::String_View type_itexture2DRect = u8"itexture2DRect";
    static constexpr anton::String_View type_iimage2DRect = u8"iimage2DRect";
    static constexpr anton::String_View type_isampler3D = u8"isampler3D";
    static constexpr anton::String_View type_itexture3D = u8"itexture3D";
    static constexpr anton::String_View type_iimage3D = u8"iimage3D";
    static constexpr anton::String_View type_isamplerCube = u8"isamplerCube";
    static constexpr anton::String_View type_itextureCube = u8"itextureCube";
    static constexpr anton::String_View type_iimageCube = u8"iimageCube";
    static constexpr anton::String_View type_isamplerCubeArray = u8"isamplerCubeArray";
    static constexpr anton::String_View type_itextureCubeArray = u8"itextureCubeArray";
    static constexpr anton::String_View type_iimageCubeArray = u8"iimageCubeArray";
    static constexpr anton::String_View type_isamplerBuffer = u8"isamplerBuffer";
    static constexpr anton::String_View type_itextureBuffer = u8"itextureBuffer";
    static constexpr anton::String_View type_iimageBuffer = u8"iimageBuffer";
    static constexpr anton::String_View type_isubpassInput = u8"isubpassInput";
    static constexpr anton::String_View type_isubpassInputMS = u8"isubpassInputMS";

    // opaque unsigned

    static constexpr anton::String_View type_usampler1D = u8"usampler1D";
    static constexpr anton::String_View type_utexture1D = u8"utexture1D";
    static constexpr anton::String_View type_uimage1D = u8"uimage1D";
    static constexpr anton::String_View type_usampler1DArray = u8"usampler1DArray";
    static constexpr anton::String_View type_utexture1DArray = u8"utexture1DArray";
    static constexpr anton::String_View type_uimage1DArray = u8"uimage1DArray";
    static constexpr anton::String_View type_usampler2D = u8"usampler2D";
    static constexpr anton::String_View type_utexture2D = u8"utexture2D";
    static constexpr anton::String_View type_uimage2D = u8"uimage2D";
    static constexpr anton::String_View type_usampler2DArray = u8"usampler2DArray";
    static constexpr anton::String_View type_utexture2DArray = u8"utexture2DArray";
    static constexpr anton::String_View type_uimage2DArray = u8"uimage2DArray";
    static constexpr anton::String_View type_usampler2DMS = u8"usampler2DMS";
    static constexpr anton::String_View type_utexture2DMS = u8"utexture2DMS";
    static constexpr anton::String_View type_uimage2DMS = u8"uimage2DMS";
    static constexpr anton::String_View type_usampler2DMSArray = u8"usampler2DMSArray";
    static constexpr anton::String_View type_utexture2DMSArray = u8"utexture2DMSArray";
    static constexpr anton::String_View type_uimage2DMSArray = u8"uimage2DMSArray";
    static constexpr anton::String_View type_usampler2DRect = u8"usampler2DRect";
    static constexpr anton::String_View type_utexture2DRect = u8"utexture2DRect";
    static constexpr anton::String_View type_uimage2DRect = u8"uimage2DRect";
    static constexpr anton::String_View type_usampler3D = u8"usampler3D";
    static constexpr anton::String_View type_utexture3D = u8"utexture3D";
    static constexpr anton::String_View type_uimage3D = u8"uimage3D";
    static constexpr anton::String_View type_usamplerCube = u8"usamplerCube";
    static constexpr anton::String_View type_utextureCube = u8"utextureCube";
    static constexpr anton::String_View type_uimageCube = u8"uimageCube";
    static constexpr anton::String_View type_usamplerCubeArray = u8"usamplerCubeArray";
    static constexpr anton::String_View type_utextureCubeArray = u8"utextureCubeArray";
    static constexpr anton::String_View type_uimageCubeArray = u8"uimageCubeArray";
    static constexpr anton::String_View type_usamplerBuffer = u8"usamplerBuffer";
    static constexpr anton::String_View type_utextureBuffer = u8"utextureBuffer";
    static constexpr anton::String_View type_uimageBuffer = u8"uimageBuffer";
    static constexpr anton::String_View type_usubpassInput = u8"usubpassInput";
    static constexpr anton::String_View type_usubpassInputMS = u8"usubpassInputMS";

    // opaque sampler

    static constexpr anton::String_View type_sampler = u8"sampler";
    static constexpr anton::String_View type_samplerShadow = u8"samplerShadow";

    // separators and operators

    static constexpr anton::String_View token_brace_open = u8"{";
    static constexpr anton::String_View token_brace_close = u8"}";
    static constexpr anton::String_View token_bracket_open = u8"[";
    static constexpr anton::String_View token_bracket_close = u8"]";
    static constexpr anton::String_View token_paren_open = u8"(";
    static constexpr anton::String_View token_paren_close = u8")";
    static constexpr anton::String_View token_semicolon = u8";";
    static constexpr anton::String_View token_colon = u8":";
    static constexpr anton::String_View token_scope_resolution = u8"::";
    static constexpr anton::String_View token_comma = u8",";
    static constexpr anton::String_View token_question = u8"?";
    static constexpr anton::String_View token_dot = u8".";
    static constexpr anton::String_View token_double_quote = u8"\"";
    static constexpr anton::String_View token_plus = u8"+";
    static constexpr anton::String_View token_minus = u8"-";
    static constexpr anton::String_View token_multiply = u8"*";
    static constexpr anton::String_View token_divide = u8"/";
    static constexpr anton::String_View token_remainder = u8"%";
    static constexpr anton::String_View token_logic_and = u8"&&";
    static constexpr anton::String_View token_bit_and = u8"&";
    static constexpr anton::String_View token_logic_or = u8"||";
    static constexpr anton::String_View token_logic_xor = u8"^^";
    static constexpr anton::String_View token_bit_or = u8"|";
    static constexpr anton::String_View token_bit_xor = u8"^";
    static constexpr anton::String_View token_logic_not = u8"!";
    static constexpr anton::String_View token_bit_not = u8"~";
    static constexpr anton::String_View token_bit_lshift = u8"<<";
    static constexpr anton::String_View token_bit_rshift = u8">>";
    static constexpr anton::String_View token_equal = u8"==";
    static constexpr anton::String_View token_not_equal = u8"!=";
    static constexpr anton::String_View token_less = u8"<";
    static constexpr anton::String_View token_greater = u8">";
    static constexpr anton::String_View token_less_equal = u8"<=";
    static constexpr anton::String_View token_greater_equal = u8">=";
    static constexpr anton::String_View token_assign = u8"=";
    static constexpr anton::String_View token_drill = u8"->";
    static constexpr anton::String_View token_increment = u8"++";
    static constexpr anton::String_View token_decrement = u8"--";
    static constexpr anton::String_View token_compound_plus = u8"+=";
    static constexpr anton::String_View token_compound_minus = u8"-=";
    static constexpr anton::String_View token_compound_multiply = u8"*=";
    static constexpr anton::String_View token_compound_divide = u8"/=";
    static constexpr anton::String_View token_compound_remainder = u8"%=";
    static constexpr anton::String_View token_compound_bit_and = u8"&=";
    static constexpr anton::String_View token_compound_bit_or = u8"|=";
    static constexpr anton::String_View token_compound_bit_xor = u8"^=";
    static constexpr anton::String_View token_compound_bit_lshift = u8"<<=";
    static constexpr anton::String_View token_compound_bit_rshift = u8">>=";

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

    constexpr char32 eof_char32 = (char32)EOF;

    class Lexer {
    public:
        Lexer(anton::Input_Stream& stream): _stream(stream) {}

        bool match(anton::String_View const string, bool const must_not_be_followed_by_identifier_char = false) {
            ignore_whitespace_and_comments();

            Lexer_State const state_backup = get_current_state();
            for(char32 c: string.chars()) {
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

        // TODO: String interning if it becomes too slow/memory heavy.
        bool match_identifier(anton::String& out) {
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

        void restore_state(Lexer_State const state) {
            _stream.seek(anton::Seek_Dir::beg, state.stream_offset);
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
        anton::Input_Stream& _stream;
        i64 _line = 0;
        i64 _column = 0;
    };

    class Parser {
    public:
        Parser(anton::Input_Stream& stream, anton::String_View source_name): _source_name(source_name), _lexer(stream) {}

        Owning_Ptr<Declaration_List> build_ast() {
            Owning_Ptr declarations{new Declaration_List};
            while(!_lexer.match_eof()) {
                if(Owning_Ptr declaration = try_declaration()) {
                    declarations->append(anton::move(declaration));
                } else {
                    return nullptr;
                }
            }
            return declarations;
        }

        [[nodiscard]] Parse_Error get_last_error() const {
            return _last_error;
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
            Lexer_State const state = _lexer.get_current_state();
            if(state.stream_offset >= _last_error.stream_offset) {
                _last_error.message = message;
                _last_error.line = state.line;
                _last_error.column = state.column;
                _last_error.stream_offset = state.stream_offset;
            }
        }

        Source_Info src_info(Lexer_State const& state) {
            return Source_Info{_source_name, state.line, state.column, state.stream_offset};
        }

        Owning_Ptr<Declaration> try_declaration() {
            if(Owning_Ptr declaration_if = try_declaration_if()) {
                return declaration_if;
            }

            if(Owning_Ptr import_decl = try_import_decl()) {
                return import_decl;
            }

            if(Owning_Ptr settings_decl = try_settings_decl()) {
                return settings_decl;
            }

            if(Owning_Ptr src_def = try_source_definition_decl()) {
                return src_def;
            }

            if(Owning_Ptr src_decl = try_source_declaration()) {
                return src_decl;
            }

            if(Owning_Ptr struct_decl = try_struct_decl()) {
                return struct_decl;
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

            Owning_Ptr true_declarations{new Declaration_List};
            while(!_lexer.match(token_brace_close)) {
                if(_lexer.match_eof()) {
                    set_error(u8"unexpected end of file");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(Owning_Ptr declaration = try_declaration()) {
                    true_declarations->append(anton::move(declaration));
                } else {
                    return nullptr;
                }
            }

            if(_lexer.match(kw_else, true)) {
                if(Owning_Ptr if_declaration = try_declaration_if()) {
                    Owning_Ptr false_declarations{new Declaration_List};
                    false_declarations->append(anton::move(if_declaration));
                    return Owning_Ptr{
                        new Declaration_If(anton::move(condition), anton::move(true_declarations), anton::move(false_declarations), src_info(state_backup))};
                } else {
                    if(!_lexer.match(token_brace_open)) {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    Owning_Ptr false_declarations{new Declaration_List};
                    while(!_lexer.match(token_brace_close)) {
                        if(_lexer.match_eof()) {
                            set_error(u8"unexpected end of file");
                            _lexer.restore_state(state_backup);
                            return nullptr;
                        }

                        if(Owning_Ptr declaration = try_declaration()) {
                            false_declarations->append(anton::move(declaration));
                        } else {
                            return nullptr;
                        }
                    }

                    if(!_lexer.match(token_brace_close)) {
                        set_error(u8"expected '}' after expression");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    return Owning_Ptr{
                        new Declaration_If(anton::move(condition), anton::move(true_declarations), anton::move(false_declarations), src_info(state_backup))};
                }
            } else {
                return Owning_Ptr{new Declaration_If(anton::move(condition), anton::move(true_declarations), nullptr, src_info(state_backup))};
            }
        }

        Owning_Ptr<Import_Decl> try_import_decl() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_import, true)) {
                set_error(u8"expected 'import'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Owning_Ptr string = try_string_literal()) {
                return Owning_Ptr{new Import_Decl(string->value, src_info(state_backup))};
            } else {
                _lexer.restore_state(state_backup);
                return nullptr;
            }
        }

        Owning_Ptr<Source_Definition_Statement> try_source_definition_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match(kw_emit, true)) {
                Owning_Ptr str = try_string_literal();
                if(!str) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(!_lexer.match(token_semicolon)) {
                    set_error(u8"expected ';' after the string literal");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                return Owning_Ptr{new Source_Definition_Emit_Statement(anton::move(str), src_info(state_backup))};
            }

            if(_lexer.match(kw_for, true)) {
                Owning_Ptr lhs = try_identifier();
                if(!lhs) {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(!_lexer.match(kw_in, true)) {
                    set_error(u8"expected 'in'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                // TODO: We hack around the requirement that all variables be builtin by matching $ and the identifier manually.
                //       Either allow all identifiers or extend the identifier matching function.
                Owning_Ptr<Identifier> range_expr;
                {
                    anton::String _range_expr;
                    Lexer_State const _range_expr_state = _lexer.get_current_state();
                    if(_lexer.match(u8"$") && _lexer.match_identifier(_range_expr)) {
                        _range_expr = u8"$" + _range_expr;
                        range_expr = Owning_Ptr{new Identifier(anton::move(_range_expr), src_info(_range_expr_state))};
                    } else {
                        set_error(u8"expected builtin variable");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                }

                if(!_lexer.match(token_brace_open)) {
                    set_error(u8"expected '{'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                Owning_Ptr for_statement{new Source_Definition_For_Statement(anton::move(lhs), anton::move(range_expr), src_info(state_backup))};
                while(Owning_Ptr statement = try_source_definition_statement()) {
                    for_statement->append(anton::move(statement));
                }

                if(!_lexer.match(token_brace_close)) {
                    set_error(u8"expected '}'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                return for_statement;
            }

            if(_lexer.match(kw_if, true)) {
                // TODO: We hack around the requirement that all variables be builtin by matching $ and the identifier manually.
                //       Either allow all identifiers or extend the identifier matching function.
                Owning_Ptr<Identifier> condition;
                {
                    anton::String _condition;
                    Lexer_State const _condition_state = _lexer.get_current_state();
                    if(_lexer.match(u8"$") && _lexer.match_identifier(_condition)) {
                        _condition = u8"$" + _condition;
                        condition = Owning_Ptr{new Identifier(anton::move(_condition), src_info(_condition_state))};
                    } else {
                        set_error(u8"expected builtin variable");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                }

                Owning_Ptr if_statement{new Source_Definition_If_Statement(anton::move(condition), src_info(state_backup))};

                if(!_lexer.match(token_brace_open)) {
                    set_error(u8"expected '{'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                while(Owning_Ptr statement = try_source_definition_statement()) {
                    if_statement->true_branch.emplace_back(anton::move(statement));
                }

                if(!_lexer.match(token_brace_close)) {
                    set_error(u8"expected '}'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(_lexer.match(kw_else, true)) {
                    if(!_lexer.match(token_brace_open)) {
                        set_error(u8"expected '{'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    while(Owning_Ptr statement = try_source_definition_statement()) {
                        if_statement->false_branch.emplace_back(anton::move(statement));
                    }

                    if(!_lexer.match(token_brace_close)) {
                        set_error(u8"expected '}'");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                }

                return if_statement;
            }

            return nullptr;
        }

        Owning_Ptr<Source_Definition_Decl> try_source_definition_decl() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_source_definition, true)) {
                set_error(u8"expected 'source_definition'");
                return nullptr;
            }

            Owning_Ptr source_name = try_identifier();
            if(!source_name) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{' after source name");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr source_definition{new Source_Definition_Decl(anton::move(source_name), src_info(state_backup))};
            while(true) {
                Lexer_State const property_state = _lexer.get_current_state();
                Owning_Ptr property_name = try_identifier();
                if(!property_name) {
                    break;
                }

                if(!_lexer.match(token_drill)) {
                    set_error(u8"expected '->' after property name");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(property_name->value == u8"declaration") {
                    if(source_definition->decl_prop) {
                        set_error(u8"duplicate 'declaration' property", property_state);
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    source_definition->decl_prop = Owning_Ptr{new Source_Definition_Declaration_Property(src_info(property_state))};

                    if(!_lexer.match(token_brace_open)) {
                        set_error(u8"expected '{' after property name");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    while(Owning_Ptr statement = try_source_definition_statement()) {
                        source_definition->decl_prop->append(anton::move(statement));
                    }

                    if(!_lexer.match(token_brace_close)) {
                        set_error(u8"expected '}' after property");
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(property_name->value == u8"bind") {
                    if(source_definition->bind_prop) {
                        set_error(u8"duplicate 'bind' property", property_state);
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    Owning_Ptr string = try_string_literal();
                    if(!string) {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }

                    source_definition->bind_prop = Owning_Ptr{new Source_Definition_Bind_Property(anton::move(string), src_info(property_state))};
                } else {
                    set_error(u8"unknown property '" + property_name->value + "'", property_state);
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            if(!_lexer.match(token_brace_close)) {
                set_error(u8"expected '}' at the end of source definition");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return source_definition;
        }

        Owning_Ptr<Sourced_Global_Decl> try_source_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();

            if(!_lexer.match(kw_source, true)) {
                set_error(u8"expected 'source'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr type = try_type();
            if(!type) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State const name_backup = _lexer.get_current_state();
            Owning_Ptr name = try_identifier();
            if(!name) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(name->value == kw_from) {
                set_error(u8"expected name before 'from'", name_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(kw_from, true)) {
                set_error(u8"expected 'from'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr source = try_identifier();
            if(!source) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_semicolon)) {
                set_error(u8"expected ';' after sourced global");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return Owning_Ptr{new Sourced_Global_Decl(anton::move(type), anton::move(name), anton::move(source), src_info(state_backup))};
        }

        Owning_Ptr<Variable_Declaration> try_variable_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
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
                set_error(u8"expected ';' after variable declaration");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return Owning_Ptr{new Variable_Declaration(anton::move(var_type), anton::move(var_name), anton::move(initializer), src_info(state_backup))};
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

            return Owning_Ptr{new Constant_Declaration(anton::move(var_type), anton::move(var_name), anton::move(initializer), src_info(state_backup))};
        }

        Owning_Ptr<Struct_Decl> try_struct_decl() {
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

            if(_lexer.match(token_brace_close)) {
                set_error(u8"empty structs are not allowed");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr struct_decl{new Struct_Decl(anton::move(struct_name), src_info(state_backup))};
            while(true) {
                if(Owning_Ptr decl = try_variable_declaration()) {
                    struct_decl->append(anton::move(decl));
                } else {
                    break;
                }
            }

            if(struct_decl->size() == 0) {
                set_error(u8"empty structs are not allowed", state_backup);
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_brace_close)) {
                set_error(u8"expected '}'");
                _lexer.restore_state(state_backup);
            }

            return struct_decl;
        }

        Owning_Ptr<Settings_Decl> try_settings_decl() {
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
                        return {anton::move(string)};
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

                        settings.emplace_back(Setting_Key_Value{anton::move(setting_key), anton::move(value.value())});
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

            Owning_Ptr settings_decl{new Settings_Decl(anton::move(pass_name), src_info(state_backup))};
            if(!_lexer.match(token_brace_open)) {
                set_error(u8"expected '{'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!match_nested_settings(match_nested_settings, settings_decl->settings, anton::String{})) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return settings_decl;
        }

        Owning_Ptr<Workgroup_Attribute> try_workgroup_attribute() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_bracket_open)) {
                set_error(u8"expected '['");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(attrib_workgroup, true)) {
                set_error(u8"expected 'workgroup'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

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

            if(!_lexer.match(token_bracket_close)) {
                set_error(u8"expected ']'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return Owning_Ptr{new Workgroup_Attribute(anton::move(x), anton::move(y), anton::move(z), src_info(state_backup))};
        }

        Owning_Ptr<Function_Attribute> try_function_attribute() {
            if(Owning_Ptr attrib = try_workgroup_attribute()) {
                return attrib;
            }

            return nullptr;
        }

        Owning_Ptr<Pass_Stage_Declaration> try_pass_stage_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::Array<Owning_Ptr<Function_Attribute>> attributes;
            while(Owning_Ptr attrib = try_function_attribute()) {
                attributes.emplace_back(anton::move(attrib));
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

            static constexpr anton::String_View stage_types_strings[] = {stage_vertex, stage_fragment, stage_compute};
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

            auto param_list = try_function_param_list(true);
            if(!param_list) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            // function body

            if(!_lexer.match(token_brace_open)) {
                set_error("expected '{' at the beginning of function body");
                return nullptr;
            }

            Owning_Ptr body = try_statement_list();

            if(!_lexer.match(token_brace_close)) {
                set_error("expected '}' at the end of the function body");
                return nullptr;
            }

            return Owning_Ptr{new Pass_Stage_Declaration(anton::move(attributes), anton::move(return_type), anton::move(pass), stage_type,
                                                         anton::move(param_list.value()), anton::move(body), src_info(state_backup))};
        }

        Owning_Ptr<Function_Declaration> try_function_declaration() {
            Lexer_State const state_backup = _lexer.get_current_state();
            anton::Array<Owning_Ptr<Function_Attribute>> attributes;
            while(Owning_Ptr attrib = try_function_attribute()) {
                attributes.emplace_back(anton::move(attrib));
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

            auto param_list = try_function_param_list(false);
            if(!param_list) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            // function body

            if(!_lexer.match(token_brace_open)) {
                set_error("expected '{' at the beginning of function body");
                return nullptr;
            }

            Owning_Ptr body = try_statement_list();

            if(!_lexer.match(token_brace_close)) {
                set_error("expected '}' at the end of the function body");
                return nullptr;
            }

            return Owning_Ptr{new Function_Declaration(anton::move(attributes), anton::move(return_type), anton::move(name), anton::move(param_list.value()),
                                                       anton::move(body), src_info(state_backup))};
        }

        anton::Optional<anton::Array<Owning_Ptr<Function_Param>>> try_function_param_list(bool const allow_sourced_params) {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_paren_open)) {
                set_error(u8"expected '('");
                _lexer.restore_state(state_backup);
                return anton::null_optional;
            }

            if(_lexer.match(token_paren_close)) {
                return anton::Array<Owning_Ptr<Function_Param>>{};
            }

            anton::Array<Owning_Ptr<Function_Param>> param_list;
            // Match parameters
            do {
                if(Owning_Ptr param = try_function_param(allow_sourced_params)) {
                    param_list.emplace_back(anton::move(param));
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

            return anton::move(param_list);
        }

        Owning_Ptr<Function_Param> try_function_param(bool const allow_sourced_params) {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(Owning_Ptr param_if = try_function_param_if(allow_sourced_params)) {
                return param_if;
            }

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

            if(_lexer.match(kw_from, true)) {
                if(!allow_sourced_params) {
                    set_error(u8"illegal sourced parameter declaration");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                if(_lexer.match(kw_in, true)) {
                    return Owning_Ptr{new Vertex_Input_Param(anton::move(identifier), anton::move(parameter_type), src_info(state_backup))};
                } else if(Owning_Ptr source = try_identifier()) {
                    return Owning_Ptr{
                        new Sourced_Function_Param(anton::move(identifier), anton::move(parameter_type), anton::move(source), src_info(state_backup))};
                } else {
                    set_error(u8"expected parameter source after 'from'");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else {
                return Owning_Ptr{new Ordinary_Function_Param(anton::move(identifier), anton::move(parameter_type), src_info(state_backup))};
            }
        }

        Owning_Ptr<Function_Param_If> try_function_param_if(bool const allow_sourced_params) {
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
                if(Owning_Ptr param_if = try_function_param_if(allow_sourced_params)) {
                    return Owning_Ptr{new Function_Param_If(anton::move(condition), anton::move(true_param), anton::move(param_if), src_info(state_backup))};
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

                    return Owning_Ptr{new Function_Param_If(anton::move(condition), anton::move(true_param), anton::move(false_param), src_info(state_backup))};
                }
            } else {
                return Owning_Ptr{new Function_Param_If(anton::move(condition), anton::move(true_param), nullptr, src_info(state_backup))};
            }
        }

        Owning_Ptr<Statement_List> try_statement_list() {
            Owning_Ptr statements{new Statement_List};
            while(true) {
                Lexer_State const statement_state = _lexer.get_current_state();
                if(Owning_Ptr block_statement = try_block_statement()) {
                    statements->append(anton::move(block_statement));
                    continue;
                }

                if(Owning_Ptr if_statement = try_if_statement()) {
                    statements->append(anton::move(if_statement));
                    continue;
                }

                if(Owning_Ptr switch_statement = try_switch_statement()) {
                    statements->append(anton::move(switch_statement));
                    continue;
                }

                if(Owning_Ptr for_statement = try_for_statement()) {
                    statements->append(anton::move(for_statement));
                    continue;
                }

                if(Owning_Ptr while_statement = try_while_statement()) {
                    statements->append(anton::move(while_statement));
                    continue;
                }

                if(Owning_Ptr do_while_statement = try_do_while_statement()) {
                    statements->append(anton::move(do_while_statement));
                    continue;
                }

                if(Owning_Ptr return_statement = try_return_statement()) {
                    statements->append(anton::move(return_statement));
                    continue;
                }

                if(Owning_Ptr break_statement = try_break_statement()) {
                    statements->append(anton::move(break_statement));
                    continue;
                }

                if(Owning_Ptr continue_statement = try_continue_statement()) {
                    statements->append(anton::move(continue_statement));
                    continue;
                }

                if(Owning_Ptr decl = try_variable_declaration()) {
                    Owning_Ptr decl_stmt{new Declaration_Statement(anton::move(decl), src_info(statement_state))};
                    statements->append(anton::move(decl_stmt));
                    continue;
                }

                if(Owning_Ptr decl = try_constant_declaration()) {
                    Owning_Ptr decl_stmt{new Declaration_Statement(anton::move(decl), src_info(statement_state))};
                    statements->append(anton::move(decl_stmt));
                    continue;
                }

                if(Owning_Ptr expr_stmt = try_expression_statement()) {
                    statements->append(anton::move(expr_stmt));
                    continue;
                }

                return statements;
            }
        }

        Owning_Ptr<Type> try_type() {
            static constexpr anton::String_View builtin_types_strings[] = {
                type_void,
                type_bool,
                type_int,
                type_uint,
                type_float,
                type_double,
                type_vec2,
                type_vec3,
                type_vec4,
                type_dvec2,
                type_dvec3,
                type_dvec4,
                type_bvec2,
                type_bvec3,
                type_bvec4,
                type_ivec2,
                type_ivec3,
                type_ivec4,
                type_uvec2,
                type_uvec3,
                type_uvec4,
                type_mat2,
                type_mat2x2,
                type_mat3,
                type_mat3x3,
                type_mat4,
                type_mat4x4,
                type_mat2x3,
                type_mat2x4,
                type_mat3x2,
                type_mat3x4,
                type_mat4x2,
                type_mat4x3,
                type_dmat2,
                type_dmat2x2,
                type_dmat3,
                type_dmat3x3,
                type_dmat4,
                type_dmat4x4,
                type_dmat2x3,
                type_dmat2x4,
                type_dmat3x2,
                type_dmat3x4,
                type_dmat4x2,
                type_dmat4x3,
                type_sampler1D,
                type_texture1D,
                type_image1D,
                type_sampler1DShadow,
                type_sampler1DArray,
                type_texture1DArray,
                type_image1DArray,
                type_sampler1DArrayShadow,
                type_sampler2D,
                type_texture2D,
                type_image2D,
                type_sampler2DShadow,
                type_sampler2DArray,
                type_texture2DArray,
                type_image2DArray,
                type_sampler2DArrayShadow,
                type_sampler2DMS,
                type_texture2DMS,
                type_image2DMS,
                type_sampler2DMSArray,
                type_texture2DMSArray,
                type_image2DMSArray,
                type_sampler2DRect,
                type_texture2DRect,
                type_image2DRect,
                type_sampler2DRectShadow,
                type_sampler3D,
                type_texture3D,
                type_image3D,
                type_samplerCube,
                type_textureCube,
                type_imageCube,
                type_samplerCubeShadow,
                type_samplerCubeArray,
                type_textureCubeArray,
                type_imageCubeArray,
                type_samplerCubeArrayShadow,
                type_samplerBuffer,
                type_textureBuffer,
                type_imageBuffer,
                type_subpassInput,
                type_subpassInputMS,
                type_isampler1D,
                type_itexture1D,
                type_iimage1D,
                type_isampler1DArray,
                type_itexture1DArray,
                type_iimage1DArray,
                type_isampler2D,
                type_itexture2D,
                type_iimage2D,
                type_isampler2DArray,
                type_itexture2DArray,
                type_iimage2DArray,
                type_isampler2DMS,
                type_itexture2DMS,
                type_iimage2DMS,
                type_isampler2DMSArray,
                type_itexture2DMSArray,
                type_iimage2DMSArray,
                type_isampler2DRect,
                type_itexture2DRect,
                type_iimage2DRect,
                type_isampler3D,
                type_itexture3D,
                type_iimage3D,
                type_isamplerCube,
                type_itextureCube,
                type_iimageCube,
                type_isamplerCubeArray,
                type_itextureCubeArray,
                type_iimageCubeArray,
                type_isamplerBuffer,
                type_itextureBuffer,
                type_iimageBuffer,
                type_isubpassInput,
                type_isubpassInputMS,
                type_usampler1D,
                type_utexture1D,
                type_uimage1D,
                type_usampler1DArray,
                type_utexture1DArray,
                type_uimage1DArray,
                type_usampler2D,
                type_utexture2D,
                type_uimage2D,
                type_usampler2DArray,
                type_utexture2DArray,
                type_uimage2DArray,
                type_usampler2DMS,
                type_utexture2DMS,
                type_uimage2DMS,
                type_usampler2DMSArray,
                type_utexture2DMSArray,
                type_uimage2DMSArray,
                type_usampler2DRect,
                type_utexture2DRect,
                type_uimage2DRect,
                type_usampler3D,
                type_utexture3D,
                type_uimage3D,
                type_usamplerCube,
                type_utextureCube,
                type_uimageCube,
                type_usamplerCubeArray,
                type_utextureCubeArray,
                type_uimageCubeArray,
                type_usamplerBuffer,
                type_utextureBuffer,
                type_uimageBuffer,
                type_usubpassInput,
                type_usubpassInputMS,
                type_sampler,
                type_samplerShadow,
            };

            static constexpr Builtin_GLSL_Type builtin_types[] = {
                Builtin_GLSL_Type::glsl_void,
                Builtin_GLSL_Type::glsl_bool,
                Builtin_GLSL_Type::glsl_int,
                Builtin_GLSL_Type::glsl_uint,
                Builtin_GLSL_Type::glsl_float,
                Builtin_GLSL_Type::glsl_double,
                Builtin_GLSL_Type::glsl_vec2,
                Builtin_GLSL_Type::glsl_vec3,
                Builtin_GLSL_Type::glsl_vec4,
                Builtin_GLSL_Type::glsl_dvec2,
                Builtin_GLSL_Type::glsl_dvec3,
                Builtin_GLSL_Type::glsl_dvec4,
                Builtin_GLSL_Type::glsl_bvec2,
                Builtin_GLSL_Type::glsl_bvec3,
                Builtin_GLSL_Type::glsl_bvec4,
                Builtin_GLSL_Type::glsl_ivec2,
                Builtin_GLSL_Type::glsl_ivec3,
                Builtin_GLSL_Type::glsl_ivec4,
                Builtin_GLSL_Type::glsl_uvec2,
                Builtin_GLSL_Type::glsl_uvec3,
                Builtin_GLSL_Type::glsl_uvec4,
                Builtin_GLSL_Type::glsl_mat2,
                Builtin_GLSL_Type::glsl_mat2,
                Builtin_GLSL_Type::glsl_mat3,
                Builtin_GLSL_Type::glsl_mat3,
                Builtin_GLSL_Type::glsl_mat4,
                Builtin_GLSL_Type::glsl_mat4,
                Builtin_GLSL_Type::glsl_mat2x3,
                Builtin_GLSL_Type::glsl_mat2x4,
                Builtin_GLSL_Type::glsl_mat3x2,
                Builtin_GLSL_Type::glsl_mat3x4,
                Builtin_GLSL_Type::glsl_mat4x2,
                Builtin_GLSL_Type::glsl_mat4x3,
                Builtin_GLSL_Type::glsl_dmat2,
                Builtin_GLSL_Type::glsl_dmat2,
                Builtin_GLSL_Type::glsl_dmat3,
                Builtin_GLSL_Type::glsl_dmat3,
                Builtin_GLSL_Type::glsl_dmat4,
                Builtin_GLSL_Type::glsl_dmat4,
                Builtin_GLSL_Type::glsl_dmat2x3,
                Builtin_GLSL_Type::glsl_dmat2x4,
                Builtin_GLSL_Type::glsl_dmat3x2,
                Builtin_GLSL_Type::glsl_dmat3x4,
                Builtin_GLSL_Type::glsl_dmat4x2,
                Builtin_GLSL_Type::glsl_dmat4x3,
                Builtin_GLSL_Type::glsl_sampler1D,
                Builtin_GLSL_Type::glsl_texture1D,
                Builtin_GLSL_Type::glsl_image1D,
                Builtin_GLSL_Type::glsl_sampler1DShadow,
                Builtin_GLSL_Type::glsl_sampler1DArray,
                Builtin_GLSL_Type::glsl_texture1DArray,
                Builtin_GLSL_Type::glsl_image1DArray,
                Builtin_GLSL_Type::glsl_sampler1DArrayShadow,
                Builtin_GLSL_Type::glsl_sampler2D,
                Builtin_GLSL_Type::glsl_texture2D,
                Builtin_GLSL_Type::glsl_image2D,
                Builtin_GLSL_Type::glsl_sampler2DShadow,
                Builtin_GLSL_Type::glsl_sampler2DArray,
                Builtin_GLSL_Type::glsl_texture2DArray,
                Builtin_GLSL_Type::glsl_image2DArray,
                Builtin_GLSL_Type::glsl_sampler2DArrayShadow,
                Builtin_GLSL_Type::glsl_sampler2DMS,
                Builtin_GLSL_Type::glsl_texture2DMS,
                Builtin_GLSL_Type::glsl_image2DMS,
                Builtin_GLSL_Type::glsl_sampler2DMSArray,
                Builtin_GLSL_Type::glsl_texture2DMSArray,
                Builtin_GLSL_Type::glsl_image2DMSArray,
                Builtin_GLSL_Type::glsl_sampler2DRect,
                Builtin_GLSL_Type::glsl_texture2DRect,
                Builtin_GLSL_Type::glsl_image2DRect,
                Builtin_GLSL_Type::glsl_sampler2DRectShadow,
                Builtin_GLSL_Type::glsl_sampler3D,
                Builtin_GLSL_Type::glsl_texture3D,
                Builtin_GLSL_Type::glsl_image3D,
                Builtin_GLSL_Type::glsl_samplerCube,
                Builtin_GLSL_Type::glsl_textureCube,
                Builtin_GLSL_Type::glsl_imageCube,
                Builtin_GLSL_Type::glsl_samplerCubeShadow,
                Builtin_GLSL_Type::glsl_samplerCubeArray,
                Builtin_GLSL_Type::glsl_textureCubeArray,
                Builtin_GLSL_Type::glsl_imageCubeArray,
                Builtin_GLSL_Type::glsl_samplerCubeArrayShadow,
                Builtin_GLSL_Type::glsl_samplerBuffer,
                Builtin_GLSL_Type::glsl_textureBuffer,
                Builtin_GLSL_Type::glsl_imageBuffer,
                Builtin_GLSL_Type::glsl_subpassInput,
                Builtin_GLSL_Type::glsl_subpassInputMS,
                Builtin_GLSL_Type::glsl_isampler1D,
                Builtin_GLSL_Type::glsl_itexture1D,
                Builtin_GLSL_Type::glsl_iimage1D,
                Builtin_GLSL_Type::glsl_isampler1DArray,
                Builtin_GLSL_Type::glsl_itexture1DArray,
                Builtin_GLSL_Type::glsl_iimage1DArray,
                Builtin_GLSL_Type::glsl_isampler2D,
                Builtin_GLSL_Type::glsl_itexture2D,
                Builtin_GLSL_Type::glsl_iimage2D,
                Builtin_GLSL_Type::glsl_isampler2DArray,
                Builtin_GLSL_Type::glsl_itexture2DArray,
                Builtin_GLSL_Type::glsl_iimage2DArray,
                Builtin_GLSL_Type::glsl_isampler2DMS,
                Builtin_GLSL_Type::glsl_itexture2DMS,
                Builtin_GLSL_Type::glsl_iimage2DMS,
                Builtin_GLSL_Type::glsl_isampler2DMSArray,
                Builtin_GLSL_Type::glsl_itexture2DMSArray,
                Builtin_GLSL_Type::glsl_iimage2DMSArray,
                Builtin_GLSL_Type::glsl_isampler2DRect,
                Builtin_GLSL_Type::glsl_itexture2DRect,
                Builtin_GLSL_Type::glsl_iimage2DRect,
                Builtin_GLSL_Type::glsl_isampler3D,
                Builtin_GLSL_Type::glsl_itexture3D,
                Builtin_GLSL_Type::glsl_iimage3D,
                Builtin_GLSL_Type::glsl_isamplerCube,
                Builtin_GLSL_Type::glsl_itextureCube,
                Builtin_GLSL_Type::glsl_iimageCube,
                Builtin_GLSL_Type::glsl_isamplerCubeArray,
                Builtin_GLSL_Type::glsl_itextureCubeArray,
                Builtin_GLSL_Type::glsl_iimageCubeArray,
                Builtin_GLSL_Type::glsl_isamplerBuffer,
                Builtin_GLSL_Type::glsl_itextureBuffer,
                Builtin_GLSL_Type::glsl_iimageBuffer,
                Builtin_GLSL_Type::glsl_isubpassInput,
                Builtin_GLSL_Type::glsl_isubpassInputMS,
                Builtin_GLSL_Type::glsl_usampler1D,
                Builtin_GLSL_Type::glsl_utexture1D,
                Builtin_GLSL_Type::glsl_uimage1D,
                Builtin_GLSL_Type::glsl_usampler1DArray,
                Builtin_GLSL_Type::glsl_utexture1DArray,
                Builtin_GLSL_Type::glsl_uimage1DArray,
                Builtin_GLSL_Type::glsl_usampler2D,
                Builtin_GLSL_Type::glsl_utexture2D,
                Builtin_GLSL_Type::glsl_uimage2D,
                Builtin_GLSL_Type::glsl_usampler2DArray,
                Builtin_GLSL_Type::glsl_utexture2DArray,
                Builtin_GLSL_Type::glsl_uimage2DArray,
                Builtin_GLSL_Type::glsl_usampler2DMS,
                Builtin_GLSL_Type::glsl_utexture2DMS,
                Builtin_GLSL_Type::glsl_uimage2DMS,
                Builtin_GLSL_Type::glsl_usampler2DMSArray,
                Builtin_GLSL_Type::glsl_utexture2DMSArray,
                Builtin_GLSL_Type::glsl_uimage2DMSArray,
                Builtin_GLSL_Type::glsl_usampler2DRect,
                Builtin_GLSL_Type::glsl_utexture2DRect,
                Builtin_GLSL_Type::glsl_uimage2DRect,
                Builtin_GLSL_Type::glsl_usampler3D,
                Builtin_GLSL_Type::glsl_utexture3D,
                Builtin_GLSL_Type::glsl_uimage3D,
                Builtin_GLSL_Type::glsl_usamplerCube,
                Builtin_GLSL_Type::glsl_utextureCube,
                Builtin_GLSL_Type::glsl_uimageCube,
                Builtin_GLSL_Type::glsl_usamplerCubeArray,
                Builtin_GLSL_Type::glsl_utextureCubeArray,
                Builtin_GLSL_Type::glsl_uimageCubeArray,
                Builtin_GLSL_Type::glsl_usamplerBuffer,
                Builtin_GLSL_Type::glsl_utextureBuffer,
                Builtin_GLSL_Type::glsl_uimageBuffer,
                Builtin_GLSL_Type::glsl_usubpassInput,
                Builtin_GLSL_Type::glsl_usubpassInputMS,
                Builtin_GLSL_Type::glsl_sampler,
                Builtin_GLSL_Type::glsl_samplerShadow,
            };

            Lexer_State const state_backup = _lexer.get_current_state();
            anton::String type_name;
            if(!_lexer.match_identifier(type_name)) {
                set_error(u8"expected type identifier");
                return nullptr;
            }

            Owning_Ptr<Type> base_type;
            constexpr i64 array_size = sizeof(builtin_types_strings) / sizeof(anton::String_View);
            for(i64 i = 0; i < array_size; ++i) {
                if(type_name == builtin_types_strings[i]) {
                    base_type = Owning_Ptr{new Builtin_Type(builtin_types[i], src_info(state_backup))};
                    break;
                }
            }

            if(!base_type) {
                base_type = Owning_Ptr{new User_Defined_Type(anton::move(type_name), src_info(state_backup))};
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

                // We don't support nested array types (yet), so we don't continue checking for brackets.
                return Owning_Ptr{new Array_Type(anton::move(base_type), anton::move(array_size), src_info(state_backup))};
            }
        }

        Owning_Ptr<Block_Statement> try_block_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(token_brace_open)) {
                set_error("expected '{' at the start of the block");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(token_brace_close)) {
                return Owning_Ptr{new Block_Statement(nullptr, src_info(state_backup))};
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

            return Owning_Ptr{new Block_Statement(anton::move(statements), src_info(state_backup))};
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

            Owning_Ptr true_statement = try_block_statement();
            if(!true_statement) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(_lexer.match(kw_else, true)) {
                if(Owning_Ptr if_statement = try_if_statement()) {
                    return Owning_Ptr{new If_Statement(anton::move(condition), anton::move(true_statement), anton::move(if_statement), src_info(state_backup))};
                } else {
                    if(Owning_Ptr false_statement = try_block_statement()) {
                        return Owning_Ptr{
                            new If_Statement(anton::move(condition), anton::move(true_statement), anton::move(false_statement), src_info(state_backup))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                }
            } else {
                return Owning_Ptr{new If_Statement(anton::move(condition), anton::move(true_statement), nullptr, src_info(state_backup))};
            }
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

            Owning_Ptr switch_statement = Owning_Ptr{new Switch_Statement(anton::move(match_expression), src_info(state_backup))};
            while(true) {
                Lexer_State const case_state = _lexer.get_current_state();
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
                        Owning_Ptr case_statement = Owning_Ptr{new Case_Statement(anton::move(condition), anton::move(statement_list), src_info(case_state))};
                        switch_statement->append(anton::move(case_statement));
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
                        Owning_Ptr default_statement = Owning_Ptr{new Default_Case_Statement(anton::move(statement_list), src_info(case_state))};
                        switch_statement->append(anton::move(default_statement));
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

            return switch_statement;
        }

        Owning_Ptr<For_Statement> try_for_statement() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(!_lexer.match(kw_for, true)) {
                set_error("expected 'for'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            // Match variable

            Owning_Ptr<Variable_Declaration> variable_declaration;
            if(!_lexer.match(token_semicolon)) {
                Lexer_State const var_decl_state = _lexer.get_current_state();
                Owning_Ptr var_type = try_type();
                if(!var_type) {
                    set_error("expected type");
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
                    set_error("expected ';' after variable declaration");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }

                variable_declaration =
                    Owning_Ptr{new Variable_Declaration(anton::move(var_type), anton::move(var_name), anton::move(initializer), src_info(var_decl_state))};
            }

            Owning_Ptr<Expression> condition;
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

            return Owning_Ptr{new For_Statement(anton::move(variable_declaration), anton::move(condition), anton::move(post_expression), anton::move(block),
                                                src_info(state_backup))};
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

            Owning_Ptr block = try_block_statement();
            if(!block) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return Owning_Ptr{new While_Statement(anton::move(condition), anton::move(block), src_info(state_backup))};
        }

        Owning_Ptr<Do_While_Statement> try_do_while_statement() {
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

            return Owning_Ptr{new Do_While_Statement(anton::move(condition), anton::move(block), src_info(state_backup))};
        }

        Owning_Ptr<Return_Statement> try_return_statement() {
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

            return Owning_Ptr{new Return_Statement(anton::move(return_expr), src_info(state_backup))};
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

            return Owning_Ptr{new Break_Statement(src_info(state_backup))};
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

            return Owning_Ptr{new Continue_Statement(src_info(state_backup))};
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

            return Owning_Ptr{new Expression_Statement(anton::move(expression), src_info(state_backup))};
        }

        Owning_Ptr<Expression> try_expression() {
            return try_assignment_expression();
        }

        Owning_Ptr<Expression> try_assignment_expression() {
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
                return lhs;
            }

            Owning_Ptr rhs = try_assignment_expression();
            if(!rhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(is_direct) {
                return Owning_Ptr{new Assignment_Expression(anton::move(lhs), anton::move(rhs), src_info(state_backup))};
            } else {
                return Owning_Ptr{new Arithmetic_Assignment_Expression(type, anton::move(lhs), anton::move(rhs), src_info(state_backup))};
            }
        }

        Owning_Ptr<Expression> try_elvis_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr cond = try_logic_or_expr();
            if(!cond) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_question)) {
                return cond;
            }

            // TODO: is using try_expression here correct?

            Owning_Ptr true_expr = try_expression();
            if(!true_expr) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(!_lexer.match(token_colon)) {
                set_error(u8"expected ':'");
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Owning_Ptr false_expr = try_expression();
            if(!false_expr) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            return Owning_Ptr{new Elvis_Expr(anton::move(cond), anton::move(true_expr), anton::move(false_expr), src_info(state_backup))};
        }

        Owning_Ptr<Expression> try_logic_or_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_logic_xor_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State op_state = _lexer.get_current_state();
            while(_lexer.match(token_logic_or)) {
                if(Owning_Ptr rhs = try_logic_xor_expr()) {
                    lhs = Owning_Ptr{new Logic_Or_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
                op_state = _lexer.get_current_state();
            }

            return lhs;
        }

        Owning_Ptr<Expression> try_logic_xor_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_logic_and_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State op_state = _lexer.get_current_state();
            while(_lexer.match(token_logic_xor)) {
                if(Owning_Ptr rhs = try_logic_and_expr()) {
                    lhs = Owning_Ptr{new Logic_Xor_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
                op_state = _lexer.get_current_state();
            }

            return lhs;
        }

        Owning_Ptr<Expression> try_logic_and_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_bit_or_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            Lexer_State op_state = _lexer.get_current_state();
            while(_lexer.match(token_logic_and)) {
                if(Owning_Ptr rhs = try_bit_or_expr()) {
                    lhs = Owning_Ptr{new Logic_And_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
                op_state = _lexer.get_current_state();
            }

            return lhs;
        }

        Owning_Ptr<Expression> try_bit_or_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_bit_xor_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_bit_or) || _lexer.match(token_logic_or)) {
                _lexer.restore_state(check_backup);
                return lhs;
            }

            Lexer_State op_state = _lexer.get_current_state();
            while(_lexer.match(token_bit_or)) {
                if(Owning_Ptr rhs = try_bit_xor_expr()) {
                    lhs = Owning_Ptr{new Bit_Or_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
                op_state = _lexer.get_current_state();
            }

            return lhs;
        }

        Owning_Ptr<Expression> try_bit_xor_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_bit_and_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_bit_xor) || _lexer.match(token_logic_xor)) {
                _lexer.restore_state(check_backup);
                return lhs;
            }

            Lexer_State op_state = _lexer.get_current_state();
            while(_lexer.match(token_bit_xor)) {
                if(Owning_Ptr rhs = try_bit_and_expr()) {
                    lhs = Owning_Ptr{new Bit_Xor_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
                op_state = _lexer.get_current_state();
            }

            return lhs;
        }

        Owning_Ptr<Expression> try_bit_and_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_relational_equality_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_bit_and) || _lexer.match(token_logic_and)) {
                _lexer.restore_state(check_backup);
                return lhs;
            }

            Lexer_State op_state = _lexer.get_current_state();
            while(_lexer.match(token_bit_and)) {
                if(Owning_Ptr rhs = try_relational_equality_expression()) {
                    lhs = Owning_Ptr{new Bit_And_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
                op_state = _lexer.get_current_state();
            }

            return lhs;
        }

        Owning_Ptr<Expression> try_relational_equality_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_relational_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
                if(_lexer.match(token_equal)) {
                    if(Owning_Ptr rhs = try_relational_expression()) {
                        lhs = Owning_Ptr{new Relational_Equality_Expression(true, anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_not_equal)) {
                    if(Owning_Ptr rhs = try_relational_expression()) {
                        lhs = Owning_Ptr{new Relational_Equality_Expression(false, anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs;
                }
            }
        }

        Owning_Ptr<Expression> try_relational_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_lshift_rshift_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
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
                    return lhs;
                }

                if(Owning_Ptr rhs = try_lshift_rshift_expr()) {
                    lhs = Owning_Ptr{new Relational_Expression(type, anton::move(lhs), anton::move(rhs), src_info(op_state))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }
        }

        Owning_Ptr<Expression> try_lshift_rshift_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_add_sub_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state();
               _lexer.match(token_compound_bit_lshift) || _lexer.match(token_compound_bit_rshift)) {
                _lexer.restore_state(check_backup);
                return lhs;
            }

            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
                if(_lexer.match(token_bit_lshift)) {
                    if(Owning_Ptr rhs = try_add_sub_expr()) {
                        lhs = Owning_Ptr{new LShift_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_bit_rshift)) {
                    if(Owning_Ptr rhs = try_add_sub_expr()) {
                        lhs = Owning_Ptr{new RShift_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs;
                }
            }
        }

        Owning_Ptr<Expression> try_add_sub_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_mul_div_mod_expr();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state(); _lexer.match(token_compound_plus) || _lexer.match(token_compound_minus)) {
                _lexer.restore_state(check_backup);
                return lhs;
            }

            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
                if(_lexer.match(token_plus)) {
                    if(Owning_Ptr rhs = try_mul_div_mod_expr()) {
                        lhs = Owning_Ptr{new Add_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_minus)) {
                    if(Owning_Ptr rhs = try_mul_div_mod_expr()) {
                        lhs = Owning_Ptr{new Sub_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs;
                }
            }
        }

        Owning_Ptr<Expression> try_mul_div_mod_expr() {
            Lexer_State const state_backup = _lexer.get_current_state();
            Owning_Ptr lhs = try_unary_expression();
            if(!lhs) {
                _lexer.restore_state(state_backup);
                return nullptr;
            }

            if(Lexer_State const check_backup = _lexer.get_current_state();
               _lexer.match(token_compound_multiply) || _lexer.match(token_compound_divide) || _lexer.match(token_compound_remainder)) {
                _lexer.restore_state(check_backup);
                return lhs;
            }

            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
                if(_lexer.match(token_multiply)) {
                    if(Owning_Ptr rhs = try_unary_expression()) {
                        lhs = Owning_Ptr{new Mul_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_divide)) {
                    if(Owning_Ptr rhs = try_unary_expression()) {
                        lhs = Owning_Ptr{new Div_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else if(_lexer.match(token_remainder)) {
                    if(Owning_Ptr rhs = try_unary_expression()) {
                        lhs = Owning_Ptr{new Mod_Expr(anton::move(lhs), anton::move(rhs), src_info(op_state))};
                    } else {
                        _lexer.restore_state(state_backup);
                        return nullptr;
                    }
                } else {
                    return lhs;
                }
            }
        }

        Owning_Ptr<Expression> try_unary_expression() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match(token_increment)) {
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Prefix_Inc_Expr(anton::move(expr), src_info(state_backup))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_decrement)) {
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Prefix_Dec_Expr(anton::move(expr), src_info(state_backup))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_plus)) {
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::plus, anton::move(expr), src_info(state_backup))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_minus)) {
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::minus, anton::move(expr), src_info(state_backup))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_logic_not)) {
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::logic_not, anton::move(expr), src_info(state_backup))};
                } else {
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            } else if(_lexer.match(token_bit_not)) {
                if(Owning_Ptr expr = try_unary_expression()) {
                    return Owning_Ptr{new Unary_Expression(Unary_Type::bit_not, anton::move(expr), src_info(state_backup))};
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

            Owning_Ptr<Expression> expr = anton::move(primary_expr);
            while(true) {
                Lexer_State const op_state = _lexer.get_current_state();
                if(_lexer.match(token_dot)) {
                    if(Owning_Ptr member_name = try_identifier()) {
                        expr = Owning_Ptr{new Member_Access_Expression(anton::move(expr), anton::move(member_name), src_info(op_state))};
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

                    expr = Owning_Ptr{new Array_Access_Expression(anton::move(expr), anton::move(index), src_info(op_state))};
                } else if(_lexer.match(token_increment)) {
                    expr = Owning_Ptr{new Postfix_Inc_Expr(anton::move(expr), src_info(op_state))};
                } else if(_lexer.match(token_decrement)) {
                    expr = Owning_Ptr{new Postfix_Dec_Expr(anton::move(expr), src_info(op_state))};
                } else {
                    break;
                }
            }

            return expr;
        }

        Owning_Ptr<Expression> try_primary_expression() {
            if(Owning_Ptr paren_expr = try_paren_expr()) {
                return paren_expr;
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

        Owning_Ptr<Paren_Expr> try_paren_expr() {
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
            } else {
                return Owning_Ptr{new Paren_Expr(anton::move(paren_expression), src_info(state_backup))};
            }
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
                return Owning_Ptr{new Expression_If(anton::move(condition), anton::move(true_expression), anton::move(expression_if), src_info(state_backup))};
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

                return Owning_Ptr{
                    new Expression_If(anton::move(condition), anton::move(true_expression), anton::move(false_expression), src_info(state_backup))};
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

            Owning_Ptr arg_list{new Argument_List};
            if(_lexer.match(token_paren_close)) {
                return Owning_Ptr{new Function_Call_Expression(anton::move(identifier), anton::move(arg_list), src_info(state_backup))};
            }

            do {
                if(Owning_Ptr expression = try_expression()) {
                    arg_list->append(anton::move(expression));
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

            return Owning_Ptr{new Function_Call_Expression(anton::move(identifier), anton::move(arg_list), src_info(state_backup))};
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
                anton::String suffix;
                while(is_identifier_character(_lexer.peek_next())) {
                    suffix += _lexer.get_next();
                }

                if(suffix == "f" || suffix == "F" || suffix == "lf" || suffix == "LF") {
                    number += suffix;
                } else {
                    anton::String error = "invalid suffix '" + suffix + "' on floating point constant";
                    set_error(error);
                    _lexer.restore_state(state_backup);
                    return nullptr;
                }
            }

            return Owning_Ptr{new Float_Literal(anton::move(number), src_info(state_backup))};
        }

        Owning_Ptr<Integer_Literal> try_integer_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();

            anton::String out;
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
                return Owning_Ptr{new Integer_Literal(anton::move(out), src_info(state_backup))};
            } else {
                set_error("expected more than 0 digits");
                _lexer.restore_state(state_backup);
                return nullptr;
            }
        }

        Owning_Ptr<Bool_Literal> try_bool_literal() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(_lexer.match(kw_true, true)) {
                return Owning_Ptr{new Bool_Literal(true, src_info(state_backup))};
            } else if(_lexer.match(kw_false, true)) {
                return Owning_Ptr{new Bool_Literal(false, src_info(state_backup))};
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
                if(next_char == U'\\') {
                    string += _lexer.get_next();
                    string += _lexer.get_next();
                } else if(next_char == eof_char32) {
                    set_error(u8"unexpected end of file");
                    _lexer.restore_state(state_backup);
                    return nullptr;
                } else if(next_char == U'\"') {
                    _lexer.get_next();
                    return Owning_Ptr{new String_Literal(anton::move(string), src_info(state_backup))};
                } else {
                    string += next_char;
                    _lexer.get_next();
                }
            }
        }

        Owning_Ptr<Identifier> try_identifier() {
            Lexer_State const state_backup = _lexer.get_current_state();
            if(anton::String _identifier; _lexer.match_identifier(_identifier)) {
                return Owning_Ptr{new Identifier(anton::move(_identifier), src_info(state_backup))};
            } else {
                set_error(u8"expected an identifier");
                _lexer.restore_state(state_backup);
                return nullptr;
            }
        }

        Owning_Ptr<Identifier_Expression> try_identifier_expression() {
            Lexer_State const state = _lexer.get_current_state();
            if(Owning_Ptr identifier = try_identifier()) {
                return Owning_Ptr{new Identifier_Expression(anton::move(identifier), src_info(state))};
            } else {
                return nullptr;
            }
        }
    };

    anton::Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_source(anton::Input_Stream& stream, anton::String_View const source_name) {
        Parser parser(stream, source_name);
        Owning_Ptr ast = parser.build_ast();
        if(ast) {
            return {anton::expected_value, anton::move(ast)};
        } else {
            Parse_Error error = parser.get_last_error();
            return {anton::expected_error, anton::move(error)};
        }
    }
} // namespace vush
