#include <sources.hpp>

#include <anton/array.hpp>
#include <anton/fixed_array.hpp>

namespace vush {
    struct Stage_Sourced_Data_Internal {
        anton::Array<Sourced_Variable> variables;
        anton::Array<Sourced_Opaque_Variable> opaque_variables;

        Stage_Sourced_Data to_stage_sourced_data() const {
            return {variables, opaque_variables};
        }
    };

    static void gather_stage_data(Allocator* const allocator,
                                  anton::Flat_Hash_Map<anton::String_View, anton::Fixed_Array<Stage_Sourced_Data_Internal, stage_kind_count>>& sourced_data,
                                  Pass_Stage_Declaration& stage_declaration, i64 const stage_index) {
        Parameter_List& parameters = stage_declaration.parameters;
        for(i64 i = 0; i < parameters.size(); ++i) {
            Function_Parameter& p = static_cast<Function_Parameter&>(*parameters[i]);
            if(is_sourced_parameter(p) && !is_vertex_input_parameter(p)) {
                auto iter = sourced_data.find_or_emplace(p.source->value, anton::Fixed_Array<Stage_Sourced_Data_Internal, stage_kind_count>{stage_kind_count});
                Stage_Sourced_Data_Internal& ssdi = iter->value[stage_index];
                if(!is_opaque_type(*p.type)) {
                    anton::String name{p.identifier->value, allocator};
                    anton::String type = stringify_type(allocator, *p.type);
                    bool const unsized = is_unsized_array(*p.type);
                    Sourced_Variable variable{ANTON_MOV(name), ANTON_MOV(type), unsized};
                    ssdi.variables.push_back(ANTON_MOV(variable));
                } else {
                    anton::String name{p.identifier->value, allocator};
                    anton::String type = stringify_type(allocator, *p.type);
                    bool const unsized = is_unsized_array(*p.type);
                    anton::String image_layout;
                    if(p.image_layout) {
                        image_layout = anton::String{stringify(p.image_layout->type), allocator};
                    }
                    Sourced_Opaque_Variable variable{ANTON_MOV(name), ANTON_MOV(type), ANTON_MOV(image_layout), unsized};
                    ssdi.opaque_variables.push_back(ANTON_MOV(variable));
                }
            }
        }
    }

    anton::Expected<void, anton::String> request_source_definitions(Context const& ctx, Pass_Context& pass,
                                                                    anton::Slice<Setting_Key_Value const> const settings) {
        anton::Flat_Hash_Map<anton::String_View, anton::Fixed_Array<Stage_Sourced_Data_Internal, stage_kind_count>> sourced_data(ctx.allocator);

        if(pass.vertex_context) {
            gather_stage_data(ctx.allocator, sourced_data, *pass.vertex_context.declaration, (i64)Stage_Kind::vertex);
        }

        if(pass.fragment_context) {
            gather_stage_data(ctx.allocator, sourced_data, *pass.fragment_context.declaration, (i64)Stage_Kind::fragment);
        }

        if(pass.compute_context) {
            gather_stage_data(ctx.allocator, sourced_data, *pass.compute_context.declaration, (i64)Stage_Kind::compute);
        }

        for(auto [source, data]: sourced_data) {
            anton::Fixed_Array<Stage_Sourced_Data, stage_kind_count> per_stage_data;
            for(Stage_Sourced_Data_Internal const& ssdi: data) {
                per_stage_data.push_back(ssdi.to_stage_sourced_data());
            }

            Source_Definition_Context src_def_ctx{pass.name, source, settings, per_stage_data, ctx.source_definition_user_data};
            anton::Fixed_Array<Source_Definition, stage_kind_count> definitions{stage_kind_count};
            anton::Expected<void, anton::String> result = ctx.source_definition_cb(src_def_ctx, definitions);
            if(!result) {
                return {anton::expected_error, ANTON_MOV(result.error())};
            }

            pass.vertex_context.source_definitions.emplace(source, ANTON_MOV(definitions[(i64)Stage_Kind::vertex]));
            pass.fragment_context.source_definitions.emplace(source, ANTON_MOV(definitions[(i64)Stage_Kind::fragment]));
            pass.compute_context.source_definitions.emplace(source, ANTON_MOV(definitions[(i64)Stage_Kind::compute]));
        }
        return {anton::expected_value};
    }
} // namespace vush
