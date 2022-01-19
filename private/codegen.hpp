#pragma once

#include <anton/array.hpp>
#include <anton/expected.hpp>
#include <anton/slice.hpp>
#include <ast_fwd.hpp>
#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Stage_Context {
        // Maps source name to source definitions
        anton::Flat_Hash_Map<anton::String, Source_Definition> source_definitions;
        // If the declaration is nullptr, the stage is not present in the pass
        Pass_Stage_Declaration* declaration = nullptr;

        // operator bool
        // Checks whether the stage is defined within a pass.
        // The stage is not defined when declaration is nullptr.
        //
        // Returns:
        // true when the stage is defined within a pass. false otherwise.
        //
        [[nodiscard]] operator bool() const {
            return declaration != nullptr;
        }
    };

    struct Pass_Context {
        // The name of the pass
        anton::String name;
        Stage_Context vertex_context;
        Stage_Context fragment_context;
        Stage_Context compute_context;

        // TODO: Move the members below to Stage_Context
        //       once we implement symbol referencing.

        // Functions used by the pass
        Array<Function_Declaration const*> functions;
        // Structs and constants used by the pass
        Array<Declaration const*> structs_and_constants;

        Pass_Context(Allocator* allocator, anton::String_View const name): name(name, allocator), functions(allocator), structs_and_constants(allocator) {}
    };

    struct Codegen_Data {
        anton::Slice<Extension const> extensions;
        anton::Slice<Pass_Context const> passes;
    };

    anton::Expected<Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Codegen_Data const& data);
} // namespace vush
