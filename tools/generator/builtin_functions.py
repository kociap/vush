import subprocess

from builtin_functions_definitions import ParamT, Array_Type, Return_Placeholder, return_, Fn, fn_definitions

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
            return f"param_{name}", f"static constexpr ast::Func_Parameter param_{name}(&{ss_identifier}, &{ss_type}, nullptr, {{}});"
        def create_static_parameter_array(name, parameters):
            return f"paramlist_{name}", f"static constexpr ast::Func_Parameter const* paramlist_{name}[{len(parameters)}] = {{{', '.join(map(lambda v: f'&{v}', parameters))}}};"
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
    file.write(f"    static constexpr ast::Decl_Overloaded_Function const* builtin_functions_declarations[{len(functions)}] = {{\n")
    for f in functions:
        string = "    " * 2 + f"&{f},\n"
        file.write(string)
    file.write("    };\n\n")

    file.write(f"""\
    anton::Slice<ast::Decl_Overloaded_Function const* const> get_builtin_functions_declarations() {{
        return anton::Slice<ast::Decl_Overloaded_Function const* const>{{builtin_functions_declarations, {len(functions)}}};
    }}
""")

def write_statics(file, statics):
    for name, string in statics.items():
        file.write("    ")
        file.write(string)
        file.write("\n")

def write_preamble(file):
    file.write("""\
#include <builtin_symbols.hpp>

#include <ast2.hpp>

namespace vush {
    using namespace anton::literals;

""")

def write_epilogue(file):
    file.write("""\
}
""")

def main():
    # TODO: --directory,-d option with default ./private/
    # TODO: --filename,-f option with default builtin_symbols_autogen.cpp

    statics = {
        "identifiers": {},
        "integers": {},
        "types": {},
        "parameters": {},
        "parameter_arrays": {},
        "functions": {},
        "ofn_arrays": {},
        "ofns": {},
    }

    def create_overloaded_function(name, functions):
        ofn_fnlist = f"ofn_fnlist_{name}"
        ofn_fnlist_string = f"static constexpr ast::Decl_Function const* {ofn_fnlist}[{len(functions)}] = {{{', '.join(map(lambda v: f'&{v}', functions))}}};"
        ofn = f"ofn_{name}"
        ofn_string = f"static constexpr ast::Decl_Overloaded_Function {ofn}(&ident_{name}, {{{ofn_fnlist}, {len(functions)}}});"
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
            statics["parameter_arrays"].update(sparameterarray)
            statics["functions"].update(sfunction)
    
    ofns = []
    for name, fns in functions.items():
        ofn, ofn_string, ofn_fnlist, ofn_fnlist_string = create_overloaded_function(name, fns)
        statics["ofn_arrays"][ofn_fnlist] = ofn_fnlist_string
        statics["ofns"][ofn] = ofn_string
        ofns.append(ofn)

    file = open("./private/builtin_symbols_autogen.cpp", "w")
    write_preamble(file)
    for k, v in statics.items():
        write_statics(file, v)
        if len(v) > 0:
            file.write("\n")
    write_get_builtin_functions_declarations(file, ofns)
    file.write("\n")
    write_epilogue(file)
    file.close()

main()
