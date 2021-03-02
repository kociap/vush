#pragma once

#include <anton/expected.hpp>
#include <ast_fwd.hpp>
#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Sourced_Data_Buffers {
        anton::Array<Sourced_Function_Param const*> all;
        anton::Array<Sourced_Function_Param const*> variables;
        anton::Array<Sourced_Function_Param const*> opaque_variables;
    };

    struct Pass_Context {
        anton::String name;
        // Maps source name to sourced data (sourced parameters)
        anton::Flat_Hash_Map<anton::String, Sourced_Data_Buffers> sourced_data;
        Pass_Stage_Declaration* vertex_stage = nullptr;
        Pass_Stage_Declaration* fragment_stage = nullptr;
        Pass_Stage_Declaration* compute_stage = nullptr;
    };

    struct Codegen_Data {
        anton::Slice<Extension const> extensions;
        anton::Slice<Pass_Settings const> settings;
        anton::Slice<Pass_Context const> passes;
        anton::Slice<Function_Declaration const* const> functions;
        anton::Slice<Declaration const* const> structs_and_constants;
    };

    anton::Expected<anton::Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Codegen_Data const& data);
} // namespace vush
