import subprocess

from builtin_functions_definitions import ParamT, Array_Type, Return_Placeholder, return_, Fn, fn_definitions

def indent_multiline(string, level):
    return "\n".join(("    " * level + s for s in string.splitlines()))

def generate_function_constructors(fn):
    def create_static_string(name, value):
        return f"str_{name}", f"static constexpr anton::String_View str_{name} = \"{value}\"_sv;"
    def create_static_identifier(name, ss_identifier):
        return f"ident_{name}", f"static constexpr Identifier ident_{name} = Identifier({ss_identifier}, Source_Info{{}});"
    def create_static_builtin(name):
        return f"builtin_{name}", f"static constexpr Builtin_Type builtin_{name} = Builtin_Type(Builtin_GLSL_Type::glsl_{name}, Source_Info{{}});"
        
    def allocate_integer_literal(ss_value):
        return f"allocate_owning<Integer_Literal>(user_allocator, {ss_value}, Integer_Literal_Type::i32, Integer_Literal_Base::dec, Source_Info{{}})"
    def allocate_array_type(sb_base, ss_size):
        return f"allocate_owning<Array_Type>(user_allocator, {create_owning('Builtin_Type', sb_base)}, {allocate_integer_literal(ss_size)}, Source_Info{{}})"
    def allocate_function_parameter(ss_identifier, ss_type):
        return f"allocate_owning<Function_Parameter>(user_allocator, {ss_identifier},\n    {ss_type}, nullptr, nullptr, Source_Info{{}})"

    def create_owning(t, value):
        return f"Owning_Ptr(const_cast<{t}*>(&{value}), &alloc)"
    def allocate_function_declaration(identifier, return_type, parameter_generator):
        static_strings = set()
        static_identifiers = set()
        static_builtins = set()
        result = "allocate_owning<Function_Declaration>(user_allocator, Attribute_List(user_allocator),\n"
        sb_return, sb_return_string = create_static_builtin(return_type)
        static_builtins.add(sb_return_string)
        result += "    " # 1 level of indent.
        result += create_owning("Builtin_Type", sb_return)
        result += ", "
        ss_identifier, ss_identifier_string = create_static_string(identifier, identifier)
        static_strings.add(ss_identifier_string)
        si_identifier, si_identifier_string = create_static_identifier(identifier, ss_identifier)
        static_identifiers.add(si_identifier_string)
        result += create_owning("Identifier", si_identifier)
        result += ",\n"
        result += "    " # 1 level of indent.
        result += "Parameter_List(user_allocator, anton::variadic_construct"
        # We use 2 levels of indent as base in this loop.
        for n, t in parameter_generator:
            result += ",\n"
            ss_pidentifier, ss_pidentifier_string = create_static_string(n, n)
            static_strings.add(ss_pidentifier_string)
            si_pidentifier, si_pidentifier_string = create_static_identifier(n, ss_pidentifier)
            static_identifiers.add(si_pidentifier_string)
            if isinstance(t, Array_Type):
                sb_base, sb_base_string = create_static_builtin(t.base)
                static_builtins.add(sb_base_string)
                ss_size, ss_size_string = create_static_string(t.size, t.size)
                static_strings.add(ss_size_string)
                result += indent_multiline(allocate_function_parameter(create_owning("Identifier", si_pidentifier), 
                                                                       allocate_array_type(sb_base, ss_size)), 2)
            else:
                sb_t, sb_t_string = create_static_builtin(t)
                static_builtins.add(sb_t_string)
                result += indent_multiline(allocate_function_parameter(create_owning("Identifier", si_pidentifier), 
                                                                       create_owning("Builtin_Type", sb_t)), 2)
        result += "),\n"
        result += "    " # 1 level of indent.
        result += "Statement_List(user_allocator), true, Source_Info{})" 
        return result, static_strings, static_identifiers, static_builtins

    def generate_name(signature):
        for v in signature:
            unpack = v
            if isinstance(v, tuple):
                unpack = v[0]
            if isinstance(unpack, str):
                yield unpack

    # generate_type_generator
    #
    # Yields:
    # Generator returning types.
    #
    def generate_type_generator(signature, replacement):
        # For some reason the value of ParamT is a tuple containing one element,
        # therefore we have to unwrap it everywhere by accessing the first element.

        def calculate_min_length(t):
            max_length = 1
            for v in replacement:
                if isinstance(v, ParamT):
                    max_length = max(len(v.value[0]), max_length)
            min_length = max_length
            for v in replacement:
                if isinstance(v, ParamT):
                    min_length = min(len(v.value[0]), min_length)
            return min_length

        def sew_signature_replacement(signature, replacement):
            for p in signature:
                if isinstance(p, tuple):
                    yield p[1]
                else:
                    yield next(replacement)
                    
        if isinstance(replacement, str) or isinstance(replacement, Array_Type):
            yield sew_signature_replacement(signature, (replacement for _ in range(0, len(signature))))
        elif isinstance(replacement, ParamT):
            for t in replacement.value[0]:
                yield sew_signature_replacement(signature, (t for _ in range(0, len(signature))))
        elif isinstance(replacement, tuple):
            min_length = calculate_min_length(replacement)
            for i in range(0, min_length):
                def unwrap(replacement, i):
                    for v in replacement:
                        if isinstance(v, ParamT):
                            yield v.value[0][i]
                        else:
                            yield v

                yield sew_signature_replacement(signature, unwrap(replacement, i))
        else:
            raise TypeError(f"invalid replacement type {type(replacement)} ({replacement})")

    for replacement in fn.replacements:
        for type_gen in generate_type_generator(fn.signature, replacement):
            yield allocate_function_declaration(fn.name, next(type_gen), zip(generate_name(fn.signature), type_gen))

def write_get_builtin_functions_declarations(file, functions):
    file.write(f"""\
    Array<Owning_Ptr<Function_Declaration>> get_builtin_functions_declarations(Allocator* const user_allocator) {{
        Array<Owning_Ptr<Function_Declaration>> result{{anton::reserve, {len(functions)}, user_allocator}};
        Dummy_Allocator alloc;
""")

    for f in functions:
        string = "    " * 2 + "result.push_back("
        # We may indent by 2 levels only since the inner part of allocate_owning is
        # already indented and we want an indentation level of 3. We lstrip whitespace
        # in order to remove indentation from the first line, so that it follows the
        # opening lparen immediately.
        string += indent_multiline(f, 2).lstrip()
        string += ");\n"
        file.write(string)

    file.write("""\
        return result;
    }
""")

def write_statics(file, statics):
    for ss in statics:
        file.write("    ")
        file.write(ss)
        file.write("\n")

def write_preamble(file):
    file.write("""\
#include <ast.hpp>
#include <memory.hpp>

namespace vush {
    using namespace anton::literals;

    struct Dummy_Allocator: public Allocator {
        void* allocate([[maybe_unused]] i64 size, [[maybe_unused]] i64 alignment) override { return nullptr; }
        virtual void deallocate([[maybe_unused]] void* memory, [[maybe_unused]] i64 size, [[maybe_unused]] i64 alignment) override {}
        bool is_equal([[maybe_unused]] Memory_Allocator const& allocator) const override { return true; }
    };

""")

def write_epilogue(file):
    file.write("""\
}
""")

def main():
    # TODO: --directory,-d option with default ./private/
    # TODO: --filename,-f option with default builtin_symbols_autogen.cpp

    static_strings = set()
    static_identifiers = set()
    static_builtins = set()
    functions = []
    for fn in fn_definitions:
        for f, ss, si, sb in generate_function_constructors(fn):
            functions.append(f)
            static_strings.update(ss)
            static_identifiers.update(si)
            static_builtins.update(sb)

    file = open("./private/builtin_symbols_autogen.cpp", "w")
    write_preamble(file)
    write_statics(file, static_strings)
    if len(static_strings) > 0:
        file.write("\n")
    write_statics(file, static_identifiers)
    if len(static_identifiers) > 0:
        file.write("\n")
    write_statics(file, static_builtins)
    if len(static_builtins) > 0:
        file.write("\n")
    write_get_builtin_functions_declarations(file, functions)
    file.write("\n")
    write_epilogue(file)
    file.close()

main()

