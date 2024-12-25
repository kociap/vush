import subprocess

from builtin_functions import Param_Type, Array_Type, Return_Placeholder, return_, Fn, builtin_function_definitions
from builtin_type import Builtin_Type, stringify_builtin_type
from builtin_operator import Builtin_Operator, builtin_operator_definitions


def get_static_type_builtin_id(name):
    return f"builtin_{name}"


def generate_alloc_identifier(value):
    return f"ast::Identifier{{\"{value}\"_sv, {{}}}}"


def generate_alloc_lt_integer(size):
    return f"VUSH_ALLOCATE(ast::Lt_Integer, allocator, ast::lt_integer_i32, {size}, Source_Info{{}})"


def generate_alloc_type(t):
    if isinstance(t, Array_Type):
        return f"VUSH_ALLOCATE(ast::Type_Array, allocator, Source_Info{{}}, {generate_alloc_type(t.base)}, {generate_alloc_lt_integer(t.size)})"
    elif isinstance(t, Builtin_Type):
        return f"&{get_static_type_builtin_id(stringify_builtin_type(t))}"
    else:
        raise TypeError("invalid type")


def generate_alloc_parameter(parameter):
    return f"ALLOC_PARAM(\"{parameter[0]}\"_sv, {generate_alloc_type(parameter[1])})"


def generate_parameter_list(parameters):
    parameter_list = [generate_alloc_parameter(p) for p in parameters]
    return f"construct_parameter_list({','.join(parameter_list)})"


def generate_alloc_function(identifier, return_type, parameters):
    fn_return_type = generate_alloc_type(return_type)
    fn_parameter_array = generate_parameter_list(parameters)
    return f"ALLOC_FUNCTION(\"{identifier}\"_sv, {fn_return_type}, {fn_parameter_array})"


def generate_functions(fn):
    def create_overload_generator(signature):
        # The enumerations within Param_Type are tuples containing one element,
        # therefore we have to unwrap them everywhere by accessing the first
        # element.

        # calculate_min_length
        # Calculate the minimum length of all Param_Type.
        #
        # Returns:
        # The least length of all Param_Type, If no Param_Type is present in
        # the signature, the length is 1.
        #
        def calculate_min_length(signature):
            min_length = 99
            has_param_type = False
            for p in signature:
                t = p[1]
                if isinstance(t, Param_Type):
                    min_length = min(len(t.value), min_length)
                    has_param_type = True
            if has_param_type == True:
                return min_length
            else:
                return 1

        def signature_generator(signature, index):
            for p in signature:
                if isinstance(p[1], Param_Type):
                    yield (p[0], p[1].value[index])
                elif isinstance(p[1], Builtin_Type) or isinstance(p[1], Array_Type):
                    yield p
                else:
                    raise TypeError("invalid parameter type")

        min_length = calculate_min_length(signature)
        for index in range(0, min_length):
            yield signature_generator(signature, index)

    for overload in fn.overloads:
        for g in create_overload_generator(overload):
            return_parameter = next(g)
            yield generate_alloc_function(fn.identifier, return_parameter[1], g)


def generate_operator(operator):
    if len(operator.signature) == 3:
        # Binary operator.
        signature = zip((return_, "lhs", "rhs"), operator.signature)
    elif len(operator.signature) == 2:
        # Unary operator.
        signature = zip((return_, "value"), operator.signature)
    else:
        raise Exception("invalid operator")

    return_parameter = next(signature)
    return generate_alloc_function(operator.identifier, return_parameter[1], signature)


def write_get_builtin_functions_declarations(file, functions):
    file.write(f"""\
template<typename... T>
[[nodiscard]] static anton::IList<ast::Fn_Parameter>
construct_parameter_list(T... parameters)
{{
  anton::IList<ast::Fn_Parameter> list;
  (..., list.insert_back(parameters));
  return list;
}}

anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*>
get_builtin_functions_declarations(Allocator* const allocator)
{{
  // Overallocate to lower pressure.
  anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*> groups(anton::reserve, {2 * len(functions)}, allocator);
""")

    for (name, group) in functions.items():
        identifier = f"\"{name}\"_sv"
        file.write("{\n")
        file.write(f"auto g = VUSH_ALLOCATE(ast::Overload_Group, allocator, allocator, {identifier});\n")
        for fn in group:
            file.write(f"g->overloads.push_back({fn});\n")
        file.write(f"groups.emplace({identifier}, g);\n")
        file.write("}\n")

    file.write(f"""\
  return groups;
}}
""")


def write_get_builtin_types(file):
    file.write(f"""\
  ast::Type_Builtin const* get_builtin_type(ast::Type_Builtin_Kind const type) {{
    switch(type) {{
""")

    for v in Builtin_Type:
        name = stringify_builtin_type(v)
        file.write(f"    case ast::Type_Builtin_Kind::e_{name}: return &{get_static_type_builtin_id(name)};\n")

    file.write("""    }
  }
""")


def write_preamble_builtin_functions(file):
    file.write("""\
// This file has been autogenerated.
// Do not modify manually.
//

#include <anton/flat_hash_map.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/memory.hpp>

#define BUILTIN_TYPE(identifier, value) \\
    static ast::Type_Builtin identifier(Source_Info{}, ast::Type_Builtin_Kind::value)
#define ALLOC_PARAM(name, type) VUSH_ALLOCATE(ast::Fn_Parameter, allocator, ast::Attr_List{}, ast::Identifier{name, {}}, type, ast::Identifier{""_sv, {}}, Source_Info{})
#define ALLOC_ARRAY_PARAM(...) VUSH_ALLOCATE(Array<ast::Fn_Parameter*>, allocator, allocator, anton::variadic_construct __VA_OPT__(,) __VA_ARGS__)
#define ALLOC_FUNCTION(identifier, return_type, parameter_array) \\
    VUSH_ALLOCATE(ast::Decl_Function, allocator, ast::Attr_List{}, ast::Identifier{identifier, {}}, \\
        parameter_array, return_type, {}, true, Source_Info{})

namespace vush {
  using namespace anton::literals;

""")


def write_preamble_builtin_types(file):
    file.write("""\
// This file has been autogenerated.
// Do not modify manually.
//

#include <vush_ast/ast.hpp>

#define BUILTIN_TYPE(identifier, value) \\
    static const ast::Type_Builtin identifier(Source_Info{}, ast::Type_Builtin_Kind::value)

namespace vush {
""")


def write_epilogue(file):
    file.write("""\
}
""")


def write_builtin_extensions(file):
    file.write("""\
// This file has been autogenerated.
// Do not modify manually.
//

#include <vush_ast/ast.hpp>
#include <vush_ir/ir.hpp>

namespace vush {
  ir::Instr_ext_call*
  select_ext(Allocator* const allocator, i64 const id, ir::Type* const type,
             ast::Expr_Call const* const expr) {
    anton::String_View const identifier = expr->identifier.value;
    bool const result_is_fp = ast::is_fp_based(*expr->evaluated_type);
    bool const result_is_sint =
      ast::is_signed_integer_based(*expr->evaluated_type);
    switch(anton::hash(identifier)) {
""")

    for function in builtin_function_definitions:
        if isinstance(function.exts, str):
            file.write(f"""\
    case anton::hash("{function.identifier}"):
      return ir::make_instr_ext_call(allocator, id,
                                     ir::Ext_Kind::e_{function.exts}, type,
                                     expr->source_info);
""")
        else:
            fp_ext = ""
            if function.exts.fp_ext:
                fp_ext = f"""\
return ir::make_instr_ext_call(allocator, id,
                            ir::Ext_Kind::e_{function.exts.fp_ext},
                            type, expr->source_info);"""
            else:
                fp_ext = "ANTON_UNREACHABLE(\"no fp ext\");"

            sint_ext = ""
            if function.exts.sint_ext:
                sint_ext = f"""\
return ir::make_instr_ext_call(allocator, id,
                            ir::Ext_Kind::e_{function.exts.sint_ext},
                            type, expr->source_info);"""
            else:
                sint_ext = "ANTON_UNREACHABLE(\"no sint ext\");"

            uint_ext = ""
            if function.exts.uint_ext:
                uint_ext = f"""\
return ir::make_instr_ext_call(allocator, id,
                            ir::Ext_Kind::e_{function.exts.uint_ext},
                            type, expr->source_info);"""
            else:
                uint_ext = "ANTON_UNREACHABLE(\"no uint ext\");"

            file.write(f"""\
    case anton::hash("{function.identifier}"):
      if(result_is_fp) {{
        {fp_ext}
      }} else if(result_is_sint) {{
        {sint_ext}
      }} else {{
        {uint_ext}
      }}
""")
    file.write("""\
    default:
      ANTON_UNREACHABLE("incorrect builtin name");
    }
  }
}
""")


def main():
    # TODO: --directory,-d option with default ./compiler/vush_autogen/
    # TODO: --filename,-f option with default builtin_symbols_autogen.cpp
    #       (reconsider as there are multiple files being generated)

    functions = {}
    for fn in builtin_function_definitions:
        if fn.identifier not in functions:
            functions[fn.identifier] = []
        for f in generate_functions(fn):
            functions[fn.identifier].append(f)

    for op in builtin_operator_definitions:
        if op.identifier not in functions:
            functions[op.identifier] = []
        f = generate_operator(op)
        functions[op.identifier].append(f)

    created_files = []

    builtin_functions_file = "./compiler/vush_autogen/builtin_functions.cpp"
    with open(builtin_functions_file, "w") as file:
        created_files.append(builtin_functions_file)
        write_preamble_builtin_functions(file)

        for t in Builtin_Type:
            identifier = stringify_builtin_type(t)
            file.write(f"BUILTIN_TYPE({get_static_type_builtin_id(identifier)}, e_{identifier});\n")

        write_get_builtin_functions_declarations(file, functions)

        write_epilogue(file)

    builtin_types_file = "./compiler/vush_autogen/builtin_types.cpp"
    with open(builtin_types_file, "w") as file:
        created_files.append(builtin_types_file)
        write_preamble_builtin_types(file)

        for t in Builtin_Type:
            identifier = stringify_builtin_type(t)
            file.write(f"BUILTIN_TYPE({get_static_type_builtin_id(identifier)}, e_{identifier});\n")

        file.write("\n")
        write_get_builtin_types(file)

        write_epilogue(file)

    builtin_extensions_file = "./compiler/vush_autogen/builtin_extensions.cpp"
    with open(builtin_extensions_file, "w") as file:
        created_files.append(builtin_extensions_file)
        write_builtin_extensions(file)

    process = subprocess.run(["clang-format", "-i"] + created_files)
    process.check_returncode()


main()
