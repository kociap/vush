#include <sources.hpp>

#include <anton/array.hpp>
#include <anton/fixed_array.hpp>
#include <ast.hpp>

namespace vush {
    struct Stage_Sourced_Data_Internal {
        anton::Array<Sourced_Variable> variables;
        anton::Array<Sourced_Opaque_Variable> opaque_variables;

        Stage_Sourced_Data to_stage_sourced_data() const {
            return {variables, opaque_variables};
        }
    };

    static void gather_stage_data(anton::Flat_Hash_Map<anton::String, anton::Fixed_Array<Stage_Sourced_Data_Internal, stage_type_count>>& sourced_data,
                                  Pass_Stage_Declaration& stage_declaration, i64 const stage_index) {
        Parameter_List& parameters = stage_declaration.parameters;
        for(i64 i = 0; i < parameters.size(); ++i) {
            Function_Parameter& p = static_cast<Function_Parameter&>(*parameters[i]);
            if(is_sourced_parameter(p) && !is_vertex_input_parameter(p)) {
                auto iter = sourced_data.find_or_emplace(p.source->value, anton::Fixed_Array<Stage_Sourced_Data_Internal, stage_type_count>{stage_type_count});
                Stage_Sourced_Data_Internal& ssdi = iter->value[stage_index];
                if(!is_opaque_type(*p.type)) {
                    anton::String name = p.identifier->value;
                    anton::String type = stringify_type(*p.type);
                    bool const unsized = is_unsized_array(*p.type);
                    Sourced_Variable variable{ANTON_MOV(name), ANTON_MOV(type), unsized};
                    ssdi.variables.push_back(ANTON_MOV(variable));
                } else {
                    anton::String name = p.identifier->value;
                    anton::String type = stringify_type(*p.type);
                    bool const unsized = is_unsized_array(*p.type);
                    anton::String image_layout;
                    if(p.image_layout) {
                        image_layout = anton::String{stringify(p.image_layout->type)};
                    }
                    Sourced_Opaque_Variable variable{ANTON_MOV(name), ANTON_MOV(type), ANTON_MOV(image_layout), unsized};
                    ssdi.opaque_variables.push_back(ANTON_MOV(variable));
                }
            }
        }
    }

    anton::Expected<void, anton::String> request_source_definitions(Context const& ctx, Pass_Context& pass,
                                                                    anton::Slice<Setting_Key_Value const> const settings) {
        anton::Flat_Hash_Map<anton::String, anton::Fixed_Array<Stage_Sourced_Data_Internal, stage_type_count>> sourced_data;

        if(pass.vertex_context) {
            gather_stage_data(sourced_data, *pass.vertex_context.declaration, (i64)Stage_Type::vertex);
        }

        if(pass.fragment_context) {
            gather_stage_data(sourced_data, *pass.fragment_context.declaration, (i64)Stage_Type::fragment);
        }

        if(pass.compute_context) {
            gather_stage_data(sourced_data, *pass.compute_context.declaration, (i64)Stage_Type::compute);
        }

        for(auto [source, data]: sourced_data) {
            anton::Fixed_Array<Stage_Sourced_Data, stage_type_count> per_stage_data;
            for(Stage_Sourced_Data_Internal const& ssdi: data) {
                per_stage_data.push_back(ssdi.to_stage_sourced_data());
            }

            Source_Definition_Context src_def_ctx{pass.name, source, settings, per_stage_data, ctx.source_definition_user_data};
            anton::Fixed_Array<Source_Definition, stage_type_count> definitions{stage_type_count};
            anton::Expected<void, anton::String> result = ctx.source_definition_cb(src_def_ctx, definitions);
            if(!result) {
                return {anton::expected_error, ANTON_MOV(result.error())};
            }

            pass.vertex_context.source_definitions.emplace(source, ANTON_MOV(definitions[(i64)Stage_Type::vertex]));
            pass.fragment_context.source_definitions.emplace(source, ANTON_MOV(definitions[(i64)Stage_Type::fragment]));
            pass.compute_context.source_definitions.emplace(source, ANTON_MOV(definitions[(i64)Stage_Type::compute]));
        }
        return {anton::expected_value};
    }
} // namespace vush
