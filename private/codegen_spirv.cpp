#include <codegen.hpp>

#include <spirv-headers/spirv.h>

namespace vush {
    // Version number
    //   0 | major | minor | 0
    constexpr u32 version_number = 0x00000100;
    // Generator's magic number.
    constexpr u32 generator_magic_number = 0x10000000;

    struct Codegen_Context {
        i64 bound;
    };

    [[nodiscard]] static anton::Expected<SPIRV_File, Error> generate_compute_stage(Context const& ctx) {
        Array<u32> out{ctx.allocator};
        // Word 0 - the SPIR-V magic number.
        out.push_back(SpvMagicNumber);
        // Word 1 - version number.
        out.push_back(version_number);
        // Word 2 - generator's magic number.
        out.push_back(generator_magic_number);
        // Word 3 - bound.
        out.push_back(0);
        // Word 4 - reserved.
        out.push_back(0);
        return {anton::expected_value, SPIRV_File{.data = out, .stage_type = Stage_Type::compute}};
    }

    anton::Expected<Array<SPIRV_Pass>, Error> generate_spirv(Context const& ctx, Codegen_Data const& data) {
        Array<SPIRV_Pass> passes{ctx.allocator};
        for(Pass_Context const& pass_ctx: data.passes) {
            Array<SPIRV_File> files{ctx.allocator};
            if(pass_ctx.compute_context) {
                anton::Expected<SPIRV_File, Error> res = generate_compute_stage(ctx);
                if(res) {
                    files.push_back(ANTON_MOV(res.value()));
                } else {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }
            }
            passes.push_back(SPIRV_Pass{.name = pass_ctx.name, .files = ANTON_MOV(files)});
        }
        return {anton::expected_value, ANTON_MOV(passes)};
    }
} // namespace vush
