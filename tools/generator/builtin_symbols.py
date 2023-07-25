from builtin_functions import ParamT, Array_Type, Return_Placeholder, return_, Fn, fn_definitions
from builtin_type import Builtin_Type, stringify_builtin_type
from builtin_operator import Builtin_Operator, builtin_operator_definitions

def get_static_type_builtin_id(name):
    return f"builtin_{name}"

def get_static_type_builtin_string(name):
    return f"static constexpr ast::Type_Builtin {get_static_type_builtin_id(name)}(ast::GLSL_Type::glsl_{name}, {{}});"

def get_static_identifier_id(name):
    return f"ident_{name}"

def get_static_identifier_string(name, value):
    return f"static constexpr ast::Identifier {get_static_identifier_id(name)}(\"{value}\"_sv, {{}});"

def get_static_parameter_id(name):
    return f"param_{name}"

def get_static_parameter_string(name, identifier_id, type_id):
    return f"static constexpr ast::Fn_Parameter {get_static_parameter_id(name)}(&{identifier_id}, &{type_id}, nullptr, {{}});"

def get_static_parameter_list_id(name):
    return f"paramlist_{name}"

def get_static_parameter_list_string(name, parameters):
    return f"static constexpr ast::Fn_Parameter const* {get_static_parameter_list_id(name)}[{len(parameters)}] = {{{', '.join(map(lambda v: f'&{v}', parameters))}}};"

def get_function_discriminator(parameter_types):
    def stringify_type(t):
        if isinstance(t, str):
            return t

        if isinstance(t, Builtin_Type):
            return stringify_builtin_type(t)
        else:
            raise TypeError(f"invalid type {t}")

    return "_".join(map(stringify_type, parameter_types))

def get_static_function_id(name):
    return f"fn_{name}"

def get_static_function_string(name, identifier_id, parameter_list_id, return_type_id):
    return f"static constexpr ast::Decl_Function {get_static_function_id(name)}({{}}, &{identifier_id}, {{{parameter_list_id}}}, &{return_type_id}, {{}}, true, {{}});"

def generate_builtin_types(statics):
    for v in Builtin_Type:
        name = stringify_builtin_type(v)
        identifier = get_static_type_builtin_id(name)
        string = get_static_type_builtin_string(name)
        statics["types"][identifier] = string

def mangle_identifier(value):
    replacements = {
        "+": "plus",
        "-": "minus",
        "*": "multiply",
        "/": "divide",
        "%": "modulus",
        "<": "less",
        ">": "greater",
        "=": "equal",
        "!": "not",
        "&": "amp",
        "|": "pipe",
        "^": "hat",
        "~": "tilde"
    }
    result = ""
    replaced = False
    for v in value:
        if v in replacements:
            result += replacements[v]
            replaced = True
        else:
            result += v
    if replaced == True:
        return f"__vush_{result}"
    else:
        return result

def generate_builtin_operators(statics):
    functions = {}
    for op in builtin_operator_definitions:
        if op.identifier not in functions:
            functions[op.identifier] = []
        return_type = op.signature[0]
        parameters = op.signature[1:]
        discriminator = get_function_discriminator(parameters)
        # Mangle identifier to replace all illegal symbols such as '+'.
        mangled_identifier = mangle_identifier(op.identifier)
        identifier_id = get_static_identifier_id(mangled_identifier)
        statics["identifiers"][identifier_id] = get_static_identifier_string(mangled_identifier, op.identifier)

        parameter_ids = []
        for pname, ptype in zip(("left", "right"), parameters):
            pname = f"{mangled_identifier}_{pname}_{stringify_builtin_type(ptype)}"
            pname_id = get_static_identifier_id(pname)
            statics["identifiers"][pname_id] = get_static_identifier_string(pname, pname)
            ptype_id = get_static_type_builtin_id(stringify_builtin_type(ptype))

            pid = get_static_parameter_id(pname)
            statics["parameters"][pid] = get_static_parameter_string(pname, pname_id, ptype_id)
            parameter_ids.append(pid)

        function_name = f"{mangled_identifier}_{discriminator}";

        parameter_list_id = get_static_parameter_list_id(function_name)
        statics["parameter_lists"][parameter_list_id] = get_static_parameter_list_string(function_name, parameter_ids)

        return_type_id = get_static_type_builtin_id(stringify_builtin_type(return_type))
        fn_id = get_static_function_id(function_name)
        statics["functions"][fn_id] = get_static_function_string(function_name, identifier_id, parameter_list_id, return_type_id)
        functions[op.identifier].append(fn_id)
    return functions

def generate_functions(fn):
    def create_function_declaration(identifier, return_type, parameter_generator):
        def create_static_identifier(name, value):
            return f"ident_{name}", f"static constexpr ast::Identifier ident_{name}(\"{value}\"_sv, {{}});"
        def create_static_type_builtin(name):
            return f"builtin_{name}", f"static constexpr ast::Type_Builtin builtin_{name}(ast::GLSL_Type::glsl_{name}, {{}});"
        def create_static_literal_integer(value):
            return f"int_{value}", f"static constexpr ast::Lt_Integer int_{value}(\"{value}\"_sv, ast::Lt_Integer_Kind::i32, ast::Lt_Integer_Base::dec, {{}});"
        def create_static_type_array(name, base, size):
            return f"array_{name}", f"static constexpr ast::Type_Array array_{name}(&{base}, &{size}, {{}});"
        def create_static_parameter(name, ss_identifier, ss_type):
            return f"param_{name}", f"static constexpr ast::Fn_Parameter param_{name}(&{ss_identifier}, &{ss_type}, nullptr, {{}});"
        def create_static_parameter_array(name, parameters):
            return f"paramlist_{name}", f"static constexpr ast::Fn_Parameter const* paramlist_{name}[{len(parameters)}] = {{{', '.join(map(lambda v: f'&{v}', parameters))}}};"
        def create_static_function(name, ss_identifier, ss_return_type, parameter_count, ss_parameters):
            return f"fn_{name}", f"static constexpr ast::Decl_Function fn_{name}({{}}, &{ss_identifier}, {{{ss_parameters}, {parameter_count}}}, &{ss_return_type}, {{}}, true, {{}});"

        static_identifiers = dict()
        static_integers = dict()
        static_types = dict()
        static_parameters = dict()
        static_arrays = dict()
        static_functions = dict()
        # Create function identifier.
        si_identifier, si_identifier_string = create_static_identifier(identifier, identifier)
        static_identifiers[si_identifier] = si_identifier_string
        # Create return type.
        stb_return, stb_return_string = create_static_type_builtin(return_type)
        static_types[stb_return] = stb_return_string
        parameter_types = []
        parameter_statics = []
        for n, t in parameter_generator:
            # Create parameter identifier.
            si_pidentifier, si_pidentifier_string = create_static_identifier(n, n)
            static_identifiers[si_pidentifier] = si_pidentifier_string
            if isinstance(t, Array_Type):
                stb_base, stb_base_string = create_static_type_builtin(t.base)
                static_types[stb_base] = stb_base_string
                sli_size, sli_size_string = create_static_literal_integer(t.size)
                static_integers[sli_size] = sli_size_string
                stringified_type = f"{t.base}_{t.size}"
                sta_ptype, sta_ptype_string = create_static_type_array(stringified_type, stb_base, sli_size)
                static_types[sta_ptype] = sta_ptype_string
                sp_param, sp_param_string = create_static_parameter(f"{identifier}_{n}_{stringified_type}", si_pidentifier, sta_ptype)
                static_parameters[sp_param] = sp_param_string
                parameter_statics.append(sp_param)
                parameter_types.append(stringified_type)
            else:
                stb_ptype, stb_ptype_string = create_static_type_builtin(t)
                static_types[stb_ptype] = stb_ptype_string
                sp_param, sp_param_string = create_static_parameter(f"{identifier}_{n}_{t}", si_pidentifier, stb_ptype)
                static_parameters[sp_param] = sp_param_string
                parameter_statics.append(sp_param)
                parameter_types.append(t)
        if len(parameter_statics) > 0:
            discriminator = "_".join(parameter_types)
            spa, spa_string = create_static_parameter_array(identifier + "_" + discriminator, parameter_statics)
            static_arrays[spa] = spa_string
            fn, fn_string = create_static_function(identifier + "_" + discriminator, si_identifier, stb_return, len(parameter_statics), spa)
            static_functions[fn] = fn_string
        else:
            fn, fn_string = create_static_function(identifier, si_identifier, stb_return, 0, "nullptr")
            static_functions[fn] = fn_string

        return fn, static_identifiers, static_integers, static_types, static_parameters, static_arrays, static_functions

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

    # Handle case where a function has no replacements,
    # i.e. all parameters are provided in the signature.
    if len(fn.replacements) > 0:
        for replacement in fn.replacements:
            for type_gen in generate_type_generator(fn.signature, replacement):
                yield create_function_declaration(fn.name, next(type_gen), zip(generate_name(fn.signature), type_gen))
    else:
        for type_gen in generate_type_generator(fn.signature, ()):
            yield create_function_declaration(fn.name, next(type_gen), zip(generate_name(fn.signature), type_gen))


def write_get_builtin_functions_declarations(file, functions):
    file.write(f"    static constexpr ast::Decl_Overloaded_Function const* builtin_functions_declarations[] = {{\n")
    for f in functions:
        string = "    " * 2 + f"&{f},\n"
        file.write(string)
    file.write("    };\n\n")

    file.write(f"""\
    anton::Slice<ast::Decl_Overloaded_Function const* const> get_builtin_functions_declarations() {{
        return anton::Slice<ast::Decl_Overloaded_Function const* const>{{builtin_functions_declarations}};
    }}
""")

def write_get_builtin_type(file):
    file.write(f"""    ast::Type_Builtin const* get_builtin_type(ast::GLSL_Type const type) {{
        switch(type) {{
""")

    for v in Builtin_Type:
        name = v.value[0]
        file.write(f"            case ast::GLSL_Type::glsl_{name}: return &{get_static_type_builtin_id(name)};\n")

    file.write("""        }
    }
""")

def write_statics(file, statics):
    for name, string in statics.items():
        file.write("    ")
        file.write(string)
        file.write("\n")

def write_preamble(file):
    file.write("""\
// clang-format off
#include <builtin_symbols.hpp>

#include <ast.hpp>

namespace vush {
    using namespace anton::literals;

""")

def write_epilogue(file):
    file.write("""\
}
""")

statics_categories = ("identifiers", "integers", "types", "parameters", "parameter_lists", "functions", "ofn_lists", "ofns")

def main():
    # TODO: --directory,-d option with default ./private/
    # TODO: --filename,-f option with default builtin_symbols_autogen.cpp

    statics = {
        "identifiers": {},
        "integers": {},
        "types": {},
        "parameters": {},
        "parameter_lists": {},
        "functions": {},
        "ofn_lists": {},
        "ofns": {},
    }

    generate_builtin_types(statics)

    def create_overloaded_function(name, functions):
        mangled_name = mangle_identifier(name)
        ofn_fnlist = f"ofn_fnlist_{mangled_name}"
        ofn_fnlist_string = f"static constexpr ast::Decl_Function const* {ofn_fnlist}[{len(functions)}] = {{{', '.join(map(lambda v: f'&{v}', functions))}}};"
        ofn = f"ofn_{mangled_name}"
        ofn_string = f"static constexpr ast::Decl_Overloaded_Function {ofn}(\"{name}\"_sv, {{{ofn_fnlist}, {len(functions)}}});"
        return ofn, ofn_string, ofn_fnlist, ofn_fnlist_string

    functions = {}
    for fn in fn_definitions:
        if fn.name not in functions:
            functions[fn.name] = []
        for f, sidentifier, sinteger, stype, sparameter, sparameterarray, sfunction in generate_functions(fn):
            functions[fn.name].append(f)
            statics["identifiers"].update(sidentifier)
            statics["integers"].update(sinteger)
            statics["types"].update(stype)
            statics["parameters"].update(sparameter)
            statics["parameter_lists"].update(sparameterarray)
            statics["functions"].update(sfunction)

    operators = generate_builtin_operators(statics)
    functions.update(operators)

    ofns = []
    for name, fns in functions.items():
        ofn, ofn_string, ofn_fnlist, ofn_fnlist_string = create_overloaded_function(name, fns)
        statics["ofn_lists"][ofn_fnlist] = ofn_fnlist_string
        statics["ofns"][ofn] = ofn_string
        ofns.append(ofn)

    file = open("./private/builtin_symbols_autogen.cpp", "w")
    write_preamble(file)

    for key in statics_categories:
        write_statics(file, statics[key])
        if len(statics[key]) > 0:
            file.write("\n")

    write_get_builtin_functions_declarations(file, ofns)
    write_get_builtin_type(file)

    write_epilogue(file)
    file.close()

main()
