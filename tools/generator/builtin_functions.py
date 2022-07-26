import subprocess

from builtin_functions_definitions import ParamT, Array_Type, Return_Placeholder, return_, Fn, fn_definitions

def indent_multiline(string, level):
    return "\n".join(("    " * level + s for s in string.splitlines()))

def generate_function_constructors(fn):
    def allocate_identifier(value):
        return f"allocate_owning<Identifier>(allocator, anton::String(\"{value}\"_sv, allocator), Source_Info{{}})"
    def allocate_builtin_type(t):
        return f"allocate_owning<Builtin_Type>(allocator, Builtin_GLSL_Type::glsl_{t}, Source_Info{{}})"
    def allocate_integer_literal(v):
        return f"allocate_owning<Integer_Literal>(allocator, anton::String(\"{v}\"_sv, allocator), Integer_Literal_Type::i32, Integer_Literal_Base::dec, Source_Info{{}})"
    def allocate_array_type(t):
        return f"allocate_owning<Array_Type>(allocator,\n    {allocate_builtin_type(t.base)},\n    {allocate_integer_literal(t.size)}, Source_Info{{}})"
    def allocate_function_parameter(identifier_value, type_value):
        if isinstance(type_value, Array_Type):
            return f"allocate_owning<Function_Parameter>(allocator,\n    {allocate_identifier(identifier_value)},\n{indent_multiline(allocate_array_type(type_value), 1)},\n    nullptr, nullptr, Source_Info{{}})"
        else:
            return f"allocate_owning<Function_Parameter>(allocator,\n    {allocate_identifier(identifier_value)},\n    {allocate_builtin_type(type_value)},\n    nullptr, nullptr, Source_Info{{}})"
    def allocate_function_declaration(identifier, return_type, parameter_generator):
        result = "allocate_owning<Function_Declaration>(allocator, Attribute_List(allocator),\n"
        result += "    " # 1 level of indent.
        result += allocate_builtin_type(return_type)
        result += ",\n"
        result += "    " # 1 level of indent.
        result += allocate_identifier(identifier)
        result += ",\n"
        result += "    " # 1 level of indent.
        result += "Parameter_List(allocator, anton::variadic_construct"
        for parameter in (allocate_function_parameter(n, t) for n, t in parameter_generator):
            result += ",\n"
            result += indent_multiline(parameter, 2)
        result += "),\n"
        result += "    " # 1 level of indent.
        result += "Statement_List(allocator), true, Source_Info{})" 
        return result

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

def write_preamble(file, function_count):
    file.write(f"""\
#include <ast.hpp>
#include <memory.hpp>

namespace vush {{
    using namespace anton::literals;

    Array<Owning_Ptr<Function_Declaration>> get_builtin_functions_declarations(Allocator* const allocator) {{
        Array<Owning_Ptr<Function_Declaration>> result{{anton::reserve, {function_count}, allocator}};
""")

def write_epilogue(file):
    file.write(f"""\
        return result;
    }}
}}
""")

def main():
    # TODO: --directory,-d option with default ./private/
    # TODO: --filename,-f option with default builtin_symbols_autogen.cpp

    functions = ""
    function_count = 0
    for fn in fn_definitions:
        for f in generate_function_constructors(fn):
            functions += "    " * 2 + "result.push_back("
            # We may indent by 2 levels only since the inner part of allocate_owning is
            # already indented and we want an indentation level of 3. We lstrip whitespace
            # in order to remove indentation from the first line, so that it follows the
            # opening lparen immediately.
            functions += indent_multiline(f, 2).lstrip()
            functions += ");\n"
            function_count += 1

    file = open("./private/builtin_symbols_autogen.cpp", "w")
    write_preamble(file, function_count)
    file.write(functions)
    write_epilogue(file)
    file.close()

main()

