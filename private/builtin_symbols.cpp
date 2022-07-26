#include <builtin_symbols.hpp>

#include <anton/flat_hash_map.hpp>
#include <anton/string_view.hpp>

#include <builtin_symbols_autogen.hpp>
#include <memory.hpp>

namespace vush {
    using namespace anton::literals;

    [[nodiscard]] static Array<Owning_Ptr<Variable_Declaration>> generate_builtin_variables(Context& ctx) {
        struct Builtin_Variable {
            anton::String_View name;
            Owning_Ptr<Type> type;
        };

        Source_Info const src_info{u8"<builtin>", 0, 0, 0};
        Builtin_Variable builtin_variables[] = {
            // Vertex Shader
            {"gl_VertexIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_InstanceIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_DrawID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_BaseVertex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_BaseInstance", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_Position", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec4, src_info)},
            {"gl_PointSize", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info)},
            {"gl_ClipDistance", allocate_owning<Array_Type>(
                                    ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info), nullptr, src_info)},
            {"gl_CullDistance", allocate_owning<Array_Type>(
                                    ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info), nullptr, src_info)},
            // TODO: Add tessellation shader variables and geometry shader variables
            // Fragment Shader
            {"gl_FragCoord", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec4, src_info)},
            {"gl_FrontFacing", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_bool, src_info)},
            // gl_ClipDistance, gl_CullDistance already declared above in the vertex shader section
            {"gl_PointCoord", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec2, src_info)},
            {"gl_PrimitiveID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_SampleID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_SamplePosition", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec2, src_info)},
            {"gl_SampleMaskIn", allocate_owning<Array_Type>(ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info),
                                                            nullptr, src_info)},
            {"gl_Layer", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_ViewportIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_HelperInvocation", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_FragDepth", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info)},
            {"gl_SampleMask", allocate_owning<Array_Type>(ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info),
                                                          nullptr, src_info)},
            // Compute Shader
            {"gl_NumWorkGroups", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_WorkgroupSize", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_WorkGroupID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_LocalInvocationID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_GlobalInvocationID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_LocalInvocationIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uint, src_info)}};

        // Populate storage.
        Array<Owning_Ptr<Variable_Declaration>> storage(anton::reserve, 256, ctx.allocator);
        constexpr i64 variable_count = sizeof(builtin_variables) / sizeof(Builtin_Variable);
        for(i64 i = 0; i < variable_count; ++i) {
            Builtin_Variable& var = builtin_variables[i];
            Owning_Ptr decl = allocate_owning<Variable_Declaration>(
                ctx.allocator, ANTON_MOV(var.type), allocate_owning<Identifier>(ctx.allocator, anton::String(var.name), src_info), nullptr, src_info);
            storage.emplace_back(ANTON_MOV(decl));
        }
        return storage;
    }

    Builtin_Declarations get_builtin_declarations(Context& ctx) {
        Array<Owning_Ptr<Function_Declaration>> builtin_functions = get_builtin_functions_declarations(ctx.allocator);
        anton::Flat_Hash_Map<anton::String_View, Owning_Ptr<Overloaded_Function_Declaration>> overloads_dictionary(anton::reserve, 1024, ctx.allocator);
        for(auto& fn_ptr: builtin_functions) {
            Owning_Ptr<Function_Declaration> fn{downcast, ANTON_MOV(fn_ptr)};
            auto iter = overloads_dictionary.find(fn->identifier->value);
            if(iter == overloads_dictionary.end()) {
                // We use the source information of the first overload to be able to provide diagnostics without
                // having to complicate the code with special cases for handling Overloaded_Function_Declaration.
                Owning_Ptr<Overloaded_Function_Declaration> overloaded_fn =
                    allocate_owning<Overloaded_Function_Declaration>(ctx.allocator, fn->identifier->clone(ctx.allocator), true, fn->source_info);
                overloaded_fn->overloads.emplace_back(ANTON_MOV(fn));
                overloads_dictionary.emplace(overloaded_fn->identifier->value, ANTON_MOV(overloaded_fn));
            } else {
                iter->value->overloads.emplace_back(ANTON_MOV(fn));
            }
        }

        Array<Owning_Ptr<Overloaded_Function_Declaration>> overloads(anton::reserve, 1024, ctx.allocator);
        for(auto& [key, overloaded_fn]: overloads_dictionary) {
            overloads.emplace_back(ANTON_MOV(overloaded_fn));
        }

        Array<Owning_Ptr<Variable_Declaration>> variables = generate_builtin_variables(ctx);
        return {ANTON_MOV(overloads), ANTON_MOV(variables)};
    }
} // namespace vush
