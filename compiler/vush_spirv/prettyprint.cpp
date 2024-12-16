#include <vush_spirv/prettyprint.hpp>

#include <anton/format.hpp>

namespace vush::spirv {
  using namespace anton::literals;

  [[nodiscard]] static anton::String_View stringify(Addressing_Model const am)
  {
    switch(am) {
    case Addressing_Model::e_logical:
      return "Logical"_sv;
    case Addressing_Model::e_physical32:
      return "Physical32"_sv;
    case Addressing_Model::e_physical64:
      return "Physical64"_sv;
    case Addressing_Model::e_physical_storage_buffer64:
      return "PhysicalStorageBuffer64"_sv;
    }
  }

  [[nodiscard]] static anton::String_View stringify(Memory_Model const mm)
  {
    switch(mm) {
    case Memory_Model::e_glsl450:
      return "GLSL450"_sv;
    case Memory_Model::e_vulkan:
      return "Vulkan"_sv;
    }
  }

  [[nodiscard]] static anton::String_View stringify(Execution_Model const em)
  {
    switch(em) {
    case Execution_Model::e_vertex:
      return "Vertex"_sv;
    case Execution_Model::e_tessellation_control:
      return "TessellationControl"_sv;
    case Execution_Model::e_tessellation_evaluation:
      return "TessellationEvaluation"_sv;
    case Execution_Model::e_geometry:
      return "Geometry"_sv;
    case Execution_Model::e_fragment:
      return "Fragment"_sv;
    case Execution_Model::e_glcompute:
      return "GLCompute"_sv;
    case Execution_Model::e_kernel:
      return "Kernel"_sv;
    }
  }

  [[nodiscard]] static anton::String_View stringify(Decoration const decoration)
  {
    switch(decoration) {
    case Decoration::e_relaxed_precision:
      return "RelaxedPrecision"_sv;
    case Decoration::e_block:
      return "Block"_sv;
    case Decoration::e_row_major:
      return "RowMajor"_sv;
    case Decoration::e_col_major:
      return "ColMajor"_sv;
    case Decoration::e_array_stride:
      return "ArrayStride"_sv;
    case Decoration::e_matrix_stride:
      return "MatrixStride"_sv;
    case Decoration::e_builtin:
      return "BuiltIn"_sv;
    case Decoration::e_no_perspective:
      return "NoPerspective"_sv;
    case Decoration::e_flat:
      return "Flat"_sv;
    case Decoration::e_patch:
      return "Patch"_sv;
    case Decoration::e_centroid:
      return "Centroid"_sv;
    case Decoration::e_invariant:
      return "Invariant"_sv;
    case Decoration::e_location:
      return "Location"_sv;
    case Decoration::e_binding:
      return "Binding"_sv;
    case Decoration::e_descriptor_set:
      return "DescriptorSet"_sv;
    case Decoration::e_offset:
      return "Offset"_sv;
    }
  }

  [[nodiscard]] static anton::String_View stringify(Builtin const builtin)
  {
    switch(builtin) {
    case Builtin::e_position:
      return "Position"_sv;
    case Builtin::e_point_size:
      return "PointSize"_sv;
    case Builtin::e_clip_distance:
      return "ClipDistance"_sv;
    case Builtin::e_cull_distance:
      return "CullDistance"_sv;
    case Builtin::e_vertex_id:
      return "VertexId"_sv;
    case Builtin::e_instance_id:
      return "InstanceId"_sv;
    case Builtin::e_primitive_id:
      return "PrimitiveId"_sv;
    case Builtin::e_invocation_id:
      return "InvocationId"_sv;
    case Builtin::e_layer:
      return "Layer"_sv;
    case Builtin::e_viewport_index:
      return "ViewportIndex"_sv;
    case Builtin::e_tess_level_outer:
      return "TessLevelOuter"_sv;
    case Builtin::e_tess_level_inner:
      return "TessLevelInner"_sv;
    case Builtin::e_tess_coord:
      return "TessCoord"_sv;
    case Builtin::e_patch_vertices:
      return "PatchVertices"_sv;
    case Builtin::e_frag_coord:
      return "FragCoord"_sv;
    case Builtin::e_point_coord:
      return "PointCoord"_sv;
    case Builtin::e_front_facing:
      return "FrontFacing"_sv;
    case Builtin::e_sample_id:
      return "SampleId"_sv;
    case Builtin::e_sample_position:
      return "SamplePosition"_sv;
    case Builtin::e_sample_mask:
      return "SampleMask"_sv;
    case Builtin::e_frag_depth:
      return "FragDepth"_sv;
    case Builtin::e_helper_invocation:
      return "HelperInvocation"_sv;
    case Builtin::e_num_workgroups:
      return "NumWorkgroups"_sv;
    case Builtin::e_worgroup_size:
      return "WorgroupSize"_sv;
    case Builtin::e_workgroup_id:
      return "WorkgroupId"_sv;
    case Builtin::e_local_invocation_id:
      return "LocalInvocationId"_sv;
    case Builtin::e_global_invocation_id:
      return "GlobalInvocationId"_sv;
    case Builtin::e_local_invocation_index:
      return "LocalInvocationIndex"_sv;
    case Builtin::e_vertex_index:
      return "VertexIndex"_sv;
    case Builtin::e_instance_index:
      return "InstanceIndex"_sv;
    case Builtin::e_base_vertex:
      return "BaseVertex"_sv;
    case Builtin::e_base_instance:
      return "BaseInstance"_sv;
    case Builtin::e_draw_index:
      return "DrawIndex"_sv;
    case Builtin::e_device_index:
      return "DeviceIndex"_sv;
    case Builtin::e_view_index:
      return "ViewIndex"_sv;
    }
  }

  [[nodiscard]] static anton::String_View
  stringify(Storage_Class const storage_class)
  {
    switch(storage_class) {
    case Storage_Class::e_uniform_constant:
      return "UniformConstant"_sv;
    case Storage_Class::e_input:
      return "Input"_sv;
    case Storage_Class::e_uniform:
      return "Uniform"_sv;
    case Storage_Class::e_output:
      return "Output"_sv;
    case Storage_Class::e_workgroup:
      return "Workgroup"_sv;
    case Storage_Class::e_cross_workgroup:
      return "CrossWorkgroup"_sv;
    case Storage_Class::e_private:
      return "Private"_sv;
    case Storage_Class::e_function:
      return "Function"_sv;
    case Storage_Class::e_generic:
      return "Generic"_sv;
    case Storage_Class::e_push_constant:
      return "PushConstant"_sv;
    case Storage_Class::e_atomic_counter:
      return "AtomicCounter"_sv;
    case Storage_Class::e_image:
      return "Image"_sv;
    case Storage_Class::e_storage_buffer:
      return "StorageBuffer"_sv;
    case Storage_Class::e_physical_storage_buffer:
      return "PhysicalStorageBuffer"_sv;
    }
  }

  [[nodiscard]] static anton::String_View
  stringify(Execution_Mode const execution_mode)
  {
    switch(execution_mode) {
    case Execution_Mode::e_origin_lower_left:
      return "OriginLowerLeft"_sv;
    case Execution_Mode::e_origin_upper_left:
      return "OriginUpperLeft"_sv;
    }
  }

  [[nodiscard]] static anton::String_View stringify(Capability const capability)
  {
    switch(capability) {
    case Capability::e_matrix:
      return "Matrix"_sv;
    case Capability::e_shader:
      return "Shader"_sv;
    case Capability::e_geometry:
      return "Geometry"_sv;
    case Capability::e_tessellation:
      return "Tessellation"_sv;
    case Capability::e_addresses:
      return "Addresses"_sv;
    case Capability::e_linkage:
      return "Linkage"_sv;
    case Capability::e_float16:
      return "Float16"_sv;
    case Capability::e_float64:
      return "Float64"_sv;
    case Capability::e_int64:
      return "Int64"_sv;
    case Capability::e_int16:
      return "Int16"_sv;
    case Capability::e_clip_distance:
      return "ClipDistance"_sv;
    case Capability::e_cull_distance:
      return "CullDistance"_sv;
    case Capability::e_int8:
      return "Int8"_sv;
    case Capability::e_draw_parameters:
      return "DrawParameters"_sv;
    case Capability::e_vulkan_memory_model:
      return "VulkanMemoryModel"_sv;
    case Capability::e_physical_storage_buffer_addresses:
      return "PhysicalStorageBufferAddresses"_sv;
    }
  }

  void print_instruction(Allocator* allocator, anton::Output_Stream& stream,
                         Prettyprint_Options const& options,
                         Instr const* const ginstruction)
  {
#define CASE_BINARY_INSTR(ENUM, TYPE, STRING)                                 \
  case Instr_Kind::ENUM: {                                                    \
    auto const instruction = static_cast<TYPE const*>(ginstruction);          \
    stream.write(anton::format(allocator, "%{} = " STRING " %{} %{} %{}",     \
                               instruction->id, instruction->result_type->id, \
                               instruction->operand1->id,                     \
                               instruction->operand2->id));                   \
  } break;

#define CASE_UNARY_INSTR(ENUM, TYPE, STRING)                                  \
  case Instr_Kind::ENUM: {                                                    \
    auto const instruction = static_cast<TYPE const*>(ginstruction);          \
    stream.write(anton::format(allocator, "%{} = " STRING " %{} %{}",         \
                               instruction->id, instruction->result_type->id, \
                               instruction->operand->id));                    \
  } break;

#define CASE_TYPED_INSTR(ENUM, TYPE, STRING)                         \
  case Instr_Kind::ENUM: {                                           \
    auto const instruction = static_cast<TYPE const*>(ginstruction); \
    stream.write(anton::format(allocator, "%{} = " STRING " %{}",    \
                               instruction->id,                      \
                               instruction->result_type->id));       \
  } break;

#define CASE_ID_INSTR(ENUM, TYPE, STRING)                                     \
  case Instr_Kind::ENUM: {                                                    \
    auto const instruction = static_cast<TYPE const*>(ginstruction);          \
    stream.write(anton::format(allocator, "%{} = " STRING, instruction->id)); \
  } break;

#define CASE_NOTHING_INSTR(ENUM, TYPE, STRING) \
  case Instr_Kind::ENUM: {                     \
    stream.write(STRING);                      \
  } break;

#define CASE_GENERIC_INSTR(ENUM, TYPE, STRING, ...)                            \
  case Instr_Kind::ENUM: {                                                     \
    auto const instruction = static_cast<TYPE const*>(ginstruction);           \
    stream.write(anton::format(allocator, STRING __VA_OPT__(, ) __VA_ARGS__)); \
  } break;

    switch(ginstruction->instr_kind) {
      CASE_GENERIC_INSTR(e_string, Instr_string, "%{} = OpString \"%{}\"",
                         instruction->id, instruction->string)
      CASE_GENERIC_INSTR(e_line, Instr_line, "OpLine %{} %{} %{}",
                         instruction->file->id, instruction->line,
                         instruction->column)
      CASE_GENERIC_INSTR(e_extension, Instr_extension, "OpExtension \"%{}\"",
                         instruction->name)
      CASE_GENERIC_INSTR(e_ext_instr_import, Instr_ext_instr_import,
                         "%{} = OpExtInstImport \"%{}\"", instruction->id,
                         instruction->name)

    case Instr_Kind::e_ext_instr: {
      auto const instruction =
        static_cast<Instr_ext_instr const*>(ginstruction);
      stream.write(anton::format(allocator, "%{} = OpExtInst %{} %{} {}",
                                 instruction->id, instruction->result_type->id,
                                 instruction->set->id,
                                 instruction->instruction));
      for(auto const operand: instruction->operands) {
        stream.write(anton::format(allocator, " %{}", operand->id));
      }
    } break;

      CASE_GENERIC_INSTR(e_memory_model, Instr_memory_model,
                         "OpMemoryModel {} {}",
                         stringify(instruction->addressing_model),
                         stringify(instruction->memory_model))

    case Instr_Kind::e_entry_point: {
      auto const instruction =
        static_cast<Instr_entry_point const*>(ginstruction);
      stream.write(anton::format(allocator, "OpEntryPoint {} %{} \"{}\"",
                                 stringify(instruction->execution_model),
                                 instruction->entry_point->id,
                                 instruction->name));
      for(auto const interface: instruction->interface) {
        stream.write(anton::format(allocator, " %{}", interface->id));
      }
    } break;

      CASE_GENERIC_INSTR(e_execution_mode, Instr_execution_mode,
                         "OpExecutionMode %{} {}", instruction->entry_point->id,
                         stringify(instruction->execution_mode))
      CASE_GENERIC_INSTR(e_capability, Instr_capability, "OpCapability {}",
                         stringify(instruction->capability))

    case Instr_Kind::e_decorate: {
      auto const instruction = static_cast<Instr_decorate const*>(ginstruction);
      stream.write(anton::format(allocator, "OpDecorate %{} {}",
                                 instruction->target->id,
                                 stringify(instruction->decoration)));
      if(!instruction->argument.is_none()) {
        if(instruction->argument.is_string()) {
          stream.write(" ");
          stream.write(instruction->argument.get_string());
        } else {
          if(instruction->decoration == Decoration::e_builtin) {
            stream.write(" ");
            stream.write(
              stringify(static_cast<Builtin>(instruction->argument.get_u32())));
          } else {
            stream.write(" ");
            stream.write(
              anton::to_string(allocator, instruction->argument.get_u32()));
          }
        }
      }
    } break;

    case Instr_Kind::e_member_decorate: {
      auto const instruction =
        static_cast<Instr_member_decorate const*>(ginstruction);
      stream.write(anton::format(allocator, "OpMemberDecorate %{} {} {}",
                                 instruction->structure_type->id,
                                 instruction->member,
                                 stringify(instruction->decoration)));
      if(!instruction->argument.is_none()) {
        if(instruction->argument.is_string()) {
          stream.write(" ");
          stream.write(instruction->argument.get_string());
        } else {
          stream.write(" ");
          stream.write(
            anton::to_string(allocator, instruction->argument.get_u32()));
        }
      }
    } break;

      CASE_ID_INSTR(e_type_void, Instr_type_void, "OpTypeVoid")
      CASE_ID_INSTR(e_type_bool, Instr_type_bool, "OpTypeBool")
      CASE_GENERIC_INSTR(e_type_int, Instr_type_int, "%{} = OpTypeInt {} {}",
                         instruction->id, instruction->width,
                         static_cast<u32>(instruction->signedness))
      CASE_GENERIC_INSTR(e_type_float, Instr_type_float, "%{} = OpTypeFloat {}",
                         instruction->id, instruction->width)
      CASE_GENERIC_INSTR(e_type_vector, Instr_type_vector,
                         "%{} = OpTypeVector %{} {}", instruction->id,
                         instruction->component_type->id,
                         instruction->component_count)
      CASE_GENERIC_INSTR(e_type_matrix, Instr_type_matrix,
                         "%{} = OpTypeMatrix %{} {}", instruction->id,
                         instruction->column_type->id,
                         instruction->column_count)
      CASE_GENERIC_INSTR(e_type_image, Instr_type_image,
                         "%{} = OpTypeImage TODO", instruction->id)
      CASE_ID_INSTR(e_type_sampler, Instr_type_sampler, "OpTypeSamler")
      CASE_GENERIC_INSTR(e_type_sampled_image, Instr_type_sampled_image,
                         "%{} = OpTypeSampledImage %{}", instruction->id,
                         instruction->image_type->id)
      CASE_GENERIC_INSTR(e_type_array, Instr_type_array,
                         "%{} = OpTypeArray %{} %{}", instruction->id,
                         instruction->element_type->id, instruction->length->id)
      CASE_GENERIC_INSTR(e_type_runtime_array, Instr_type_runtime_array,
                         "%{} = OpTypeRuntimeArray %{}", instruction->id,
                         instruction->element_type->id)

    case Instr_Kind::e_type_struct: {
      auto const instruction =
        static_cast<Instr_type_struct const*>(ginstruction);
      stream.write(
        anton::format(allocator, "%{} = OpTypeStruct", instruction->id));
      for(auto const field: instruction->field_types) {
        stream.write(anton::format(allocator, " %{}", field->id));
      }
    } break;

      CASE_GENERIC_INSTR(e_type_pointer, Instr_type_pointer,
                         "%{} = OpTypePointer {} %{}", instruction->id,
                         stringify(instruction->storage_class),
                         instruction->type->id)

    case Instr_Kind::e_type_function: {
      auto const instruction =
        static_cast<Instr_type_function const*>(ginstruction);
      stream.write(anton::format(allocator, "%{} = OpTypeFunction %{}",
                                 instruction->id,
                                 instruction->return_type->id));
      for(auto const parameter: instruction->parameter_types) {
        stream.write(anton::format(allocator, " %{}", parameter->id));
      }
    } break;

      CASE_TYPED_INSTR(e_constant_true, Instr_constant_true, "OpConstantTrue")
      CASE_TYPED_INSTR(e_constant_false, Instr_constant_false,
                       "OpConstantFalse")

    case Instr_Kind::e_constant: {
      auto const instruction = static_cast<Instr_constant const*>(ginstruction);

      bool const is_fp = instanceof<Instr_type_float>(instruction->result_type);
      if(instruction->byte_length <= 4) {
        u32 const value = instruction->word1;
        if(is_fp) {
          stream.write(anton::format(allocator, "%{} = OpConstant %{} {}",
                                     instruction->id,
                                     instruction->result_type->id,
                                     *reinterpret_cast<f32 const*>(&value)));
        } else {
          stream.write(anton::format(allocator, "%{} = OpConstant %{} {}",
                                     instruction->id,
                                     instruction->result_type->id, value));
        }
      } else {
        u64 const value =
          ((u64)instruction->word2 << 32) | (u64)instruction->word1;
        if(is_fp) {
          stream.write(anton::format(allocator, "%{} = OpConstant %{} {}",
                                     instruction->id,
                                     instruction->result_type->id,
                                     *reinterpret_cast<f64 const*>(&value)));
        } else {
          stream.write(anton::format(allocator, "%{} = OpConstant %{} {}",
                                     instruction->id,
                                     instruction->result_type->id, value));
        }
      }
    } break;

    case Instr_Kind::e_constant_composite: {
      auto const instruction =
        static_cast<Instr_constant_composite const*>(ginstruction);
      stream.write(anton::format(allocator, "%{} = OpConstantComposite %{} %{}",
                                 instruction->id,
                                 instruction->result_type->id));
      for(auto const constituent: instruction->constituents) {
        stream.write(anton::format(allocator, " %{}", constituent->id));
      }
    } break;

      CASE_GENERIC_INSTR(e_variable, Instr_variable, "%{} = OpVariable %{} {}",
                         instruction->id, instruction->result_type->id,
                         stringify(instruction->storage_class))
      CASE_GENERIC_INSTR(e_load, Instr_load, "%{} = OpLoad %{} %{}",
                         instruction->id, instruction->result_type->id,
                         instruction->pointer->id)
      CASE_GENERIC_INSTR(e_store, Instr_store, "OpStore %{} %{}",
                         instruction->pointer->id, instruction->object->id)

    case Instr_Kind::e_access_chain: {
      auto const instruction =
        static_cast<Instr_access_chain const*>(ginstruction);
      stream.write(anton::format(allocator, "%{} = OpAccessChain %{} %{}",
                                 instruction->id, instruction->result_type->id,
                                 instruction->base->id));
      for(auto const index: instruction->indices) {
        stream.write(anton::format(allocator, " %{}", index->id));
      }
    } break;

      CASE_GENERIC_INSTR(e_function, Instr_function,
                         "%{} = OpFunction %{} None %{}", instruction->id,
                         instruction->function_type->return_type->id,
                         instruction->function_type->id)
      CASE_TYPED_INSTR(e_function_parameter, Instr_function_parameter,
                       "OpFunctionParameter")
      CASE_NOTHING_INSTR(e_function_end, Instr_function_end, "OpFunctionEnd")

    case Instr_Kind::e_function_call: {
      auto const instruction =
        static_cast<Instr_function_call const*>(ginstruction);
      stream.write(anton::format(
        allocator, "%{} = OpFunctionCall %{} %{}", instruction->id,
        instruction->function->function_type->return_type->id,
        instruction->function->id));
      for(auto const argument: instruction->arguments) {
        stream.write(anton::format(allocator, " %{}", argument->id));
      }
    } break;

      CASE_UNARY_INSTR(e_convert_f2u, Instr_convert_f2u, "OpConvertFToU")
      CASE_UNARY_INSTR(e_convert_f2s, Instr_convert_f2s, "OpConvertFToS")
      CASE_UNARY_INSTR(e_convert_s2f, Instr_convert_s2f, "OpConvertSToF")
      CASE_UNARY_INSTR(e_convert_u2f, Instr_convert_u2f, "OpConvertUToF")
      CASE_UNARY_INSTR(e_uconvert, Instr_uconvert, "OpUConvert")
      CASE_UNARY_INSTR(e_sconvert, Instr_sconvert, "OpSConvert")
      CASE_UNARY_INSTR(e_fconvert, Instr_fconvert, "OpFConvert")
      CASE_UNARY_INSTR(e_convert_ptr2u, Instr_convert_ptr2u, "OpConvertPtrToU")
      CASE_UNARY_INSTR(e_convert_u2ptr, Instr_convert_u2ptr, "OpConvertUToPtr")

    case Instr_Kind::e_composite_construct: {
      auto const instruction =
        static_cast<Instr_composite_construct const*>(ginstruction);
      stream.write(anton::format(allocator, "%{} = OpCompositeConstruct %{}",
                                 instruction->id,
                                 instruction->result_type->id));
      for(auto const constituent: instruction->constituents) {
        stream.write(anton::format(allocator, " %{}", constituent->id));
      }
    } break;

    case Instr_Kind::e_composite_extract: {
      auto const instruction =
        static_cast<Instr_composite_extract const*>(ginstruction);
      stream.write(anton::format(allocator, "%{} = OpCompositeExtract %{} %{}",
                                 instruction->id, instruction->result_type->id,
                                 instruction->composite->id));
      for(auto const index: instruction->indices) {
        stream.write(anton::format(allocator, " {}", index));
      }
    } break;

    case Instr_Kind::e_composite_insert: {
      auto const instruction =
        static_cast<Instr_composite_insert const*>(ginstruction);
      stream.write(
        anton::format(allocator, "%{} = OpCompositeInsert %{} %{} %{}",
                      instruction->id, instruction->result_type->id,
                      instruction->object->id, instruction->composite->id));
      for(auto const index: instruction->indices) {
        stream.write(anton::format(allocator, " {}", index));
      }
    } break;

      CASE_UNARY_INSTR(e_copy_object, Instr_copy_object, "OpCopyObject")
      CASE_UNARY_INSTR(e_transpose, Instr_transpose, "OpTranspose")

      CASE_UNARY_INSTR(e_snegate, Instr_snegate, "OpSNegate")
      CASE_UNARY_INSTR(e_fnegate, Instr_fnegate, "OpFNegate")

      CASE_BINARY_INSTR(e_iadd, Instr_iadd, "OpIAdd")
      CASE_BINARY_INSTR(e_fadd, Instr_fadd, "OpFAdd")
      CASE_BINARY_INSTR(e_isub, Instr_isub, "OpISub")
      CASE_BINARY_INSTR(e_fsub, Instr_fsub, "OpFSub")
      CASE_BINARY_INSTR(e_imul, Instr_imul, "OpIMul")
      CASE_BINARY_INSTR(e_fmul, Instr_fmul, "OpFMul")
      CASE_BINARY_INSTR(e_udiv, Instr_udiv, "OpUDiv")
      CASE_BINARY_INSTR(e_sdiv, Instr_sdiv, "OpSDiv")
      CASE_BINARY_INSTR(e_fdiv, Instr_fdiv, "OpFDiv")
      CASE_BINARY_INSTR(e_umod, Instr_umod, "OpUMod")
      CASE_BINARY_INSTR(e_srem, Instr_srem, "OpSRem")
      CASE_BINARY_INSTR(e_smod, Instr_smod, "OpSMod")
      CASE_BINARY_INSTR(e_frem, Instr_frem, "OpFRem")
      CASE_BINARY_INSTR(e_fmod, Instr_fmod, "OpFMod")
      CASE_BINARY_INSTR(e_vec_times_scalar, Instr_vec_times_scalar,
                        "OpVectorTimesScalar")
      CASE_BINARY_INSTR(e_mat_times_scalar, Instr_mat_times_scalar,
                        "OpMatrixTimesScalar")
      CASE_BINARY_INSTR(e_vec_times_mat, Instr_vec_times_mat,
                        "OpVectorTimesMatrix")
      CASE_BINARY_INSTR(e_mat_times_vec, Instr_mat_times_vec,
                        "OpMatrixTimesVector")
      CASE_BINARY_INSTR(e_mat_times_mat, Instr_mat_times_mat,
                        "OpMatrixTimesMatrix")
      CASE_BINARY_INSTR(e_outer_product, Instr_outer_product, "OpOuterProduct")
      CASE_BINARY_INSTR(e_dot, Instr_dot, "OpDot")
      CASE_BINARY_INSTR(e_shr_logical, Instr_shr_logical, "OpShiftRightLogical")
      CASE_BINARY_INSTR(e_shr_arithmetic, Instr_shr_arithmetic,
                        "OpShiftRightArithmetic")
      CASE_BINARY_INSTR(e_shl, Instr_shl, "OpShiftLeftLogical")
      CASE_BINARY_INSTR(e_bit_or, Instr_bit_or, "OpBitwiseOr")
      CASE_BINARY_INSTR(e_bit_xor, Instr_bit_xor, "OpBitwiseXor")
      CASE_BINARY_INSTR(e_bit_and, Instr_bit_and, "OpBitwiseAnd")
      CASE_UNARY_INSTR(e_bit_not, Instr_bit_not, "OpNot")
      CASE_BINARY_INSTR(e_logical_eq, Instr_logical_eq, "OpLogicalEqual")
      CASE_BINARY_INSTR(e_logical_neq, Instr_logical_neq, "OpLogicalNotEqual")
      CASE_BINARY_INSTR(e_logical_or, Instr_logical_or, "OpLogicalOr")
      CASE_BINARY_INSTR(e_logical_and, Instr_logical_and, "OpLogicalAnd")
      CASE_BINARY_INSTR(e_logical_not, Instr_logical_not, "OpLogicalNot")
      CASE_BINARY_INSTR(e_ieq, Instr_ieq, "OpIEqual")
      CASE_BINARY_INSTR(e_ineq, Instr_ineq, "OpINotEqual")
      CASE_BINARY_INSTR(e_ugt, Instr_ugt, "OpUGreaterThan")
      CASE_BINARY_INSTR(e_sgt, Instr_sgt, "OpSGreaterThan")
      CASE_BINARY_INSTR(e_uge, Instr_uge, "OpUGreaterThanEqual")
      CASE_BINARY_INSTR(e_sge, Instr_sge, "OpSGreaterThanEqual")
      CASE_BINARY_INSTR(e_ult, Instr_ult, "OpULessThan")
      CASE_BINARY_INSTR(e_slt, Instr_slt, "OpSLessThan")
      CASE_BINARY_INSTR(e_ule, Instr_ule, "OpULessThanEqual")
      CASE_BINARY_INSTR(e_sle, Instr_sle, "OpSLessThanEqual")
      CASE_BINARY_INSTR(e_foeq, Instr_foeq, "OpFOrdEqual")
      CASE_BINARY_INSTR(e_fueq, Instr_fueq, "OpFUnordEqual")
      CASE_BINARY_INSTR(e_foneq, Instr_foneq, "OpFOrdNotEqual")
      CASE_BINARY_INSTR(e_funeq, Instr_funeq, "OpFUnordNotEqual")
      CASE_BINARY_INSTR(e_folt, Instr_folt, "OpFOrdLessThan")
      CASE_BINARY_INSTR(e_fult, Instr_fult, "OpFUnordLessThan")
      CASE_BINARY_INSTR(e_fogt, Instr_fogt, "OpFOrdGreaterThan")
      CASE_BINARY_INSTR(e_fugt, Instr_fugt, "OpFUnordGreaterThan")
      CASE_BINARY_INSTR(e_fole, Instr_fole, "OpFOrdLessThanEqual")
      CASE_BINARY_INSTR(e_fule, Instr_fule, "OpFUnordLessThanEqual")
      CASE_BINARY_INSTR(e_foge, Instr_foge, "OpFOrdGreaterThanEqual")
      CASE_BINARY_INSTR(e_fuge, Instr_fuge, "OpFUnordGreaterThanEqual")

      CASE_GENERIC_INSTR(e_select, Instr_select,
                         "%{} = OpSelect %{} %{} %{} %{} %{}", instruction->id,
                         instruction->result_type->id,
                         instruction->condition->id, instruction->operand1->id,
                         instruction->operand2->id)

      CASE_UNARY_INSTR(e_dPdx, Instr_dPdx, "OpDPdx")
      CASE_UNARY_INSTR(e_dPdy, Instr_dPdy, "OpDPdy")
      CASE_UNARY_INSTR(e_fwidth, Instr_fwidth, "OpFwidth")
      CASE_UNARY_INSTR(e_dPdx_fine, Instr_dPdx_fine, "OpDPdxFine")
      CASE_UNARY_INSTR(e_dPdy_fine, Instr_dPdy_fine, "OpDPdyFine")
      CASE_UNARY_INSTR(e_fwidth_fine, Instr_fwidth_fine, "OpFwidthFine")
      CASE_UNARY_INSTR(e_dPdx_coarse, Instr_dPdx_coarse, "OpDPdxCoarse")
      CASE_UNARY_INSTR(e_dPdy_coarse, Instr_dPdy_coarse, "OpDPdyCoarse")
      CASE_UNARY_INSTR(e_fwidth_coarse, Instr_fwidth_coarse, "OpFwidthCoarse")

    case Instr_Kind::e_phi: {
      auto const instruction = static_cast<Instr_phi const*>(ginstruction);
      stream.write(anton::format(allocator, "%{} = OpPhi %{}", instruction->id,
                                 instruction->result_type->id));
      for(auto const variable: instruction->operands) {
        stream.write(anton::format(allocator, " %{} %{}", variable->id,
                                   variable->block->id));
      }
    } break;

      CASE_GENERIC_INSTR(e_selection_merge, Instr_selection_merge,
                         "OpSelectionMerge %{} None",
                         instruction->merge_block->id)
      CASE_ID_INSTR(e_label, Instr_label, "OpLabel")
      CASE_GENERIC_INSTR(e_branch, Instr_branch, "OpBranch %{}",
                         instruction->target->id)
      CASE_GENERIC_INSTR(
        e_brcond, Instr_brcond, "OpBranchConditional %{} %{} %{}",
        instruction->condition->id, instruction->true_label->id,
        instruction->false_label->id)

    case Instr_Kind::e_switch: {
      auto const instruction = static_cast<Instr_switch const*>(ginstruction);
      stream.write(anton::format(allocator, "OpSwitch %{} %{}",
                                 instruction->selector->id,
                                 instruction->default_label->id));
      for(auto const label: instruction->labels) {
        stream.write(
          anton::format(allocator, " {} %{}", label.literal, label.label->id));
      }
    } break;

      CASE_NOTHING_INSTR(e_return, Instr_return, "OpReturn")
      CASE_GENERIC_INSTR(e_return_value, Instr_return_value,
                         "OpReturnValue %{}", instruction->value->id)
      CASE_NOTHING_INSTR(e_terminate, Instr_terminate, "OpTerminateInvocation")
      CASE_NOTHING_INSTR(e_unreachable, Instr_unreachable, "OpUnreachable")
    }
    stream.write("\n");
  }

#undef CASE_BINARY_INSTR
#undef CASE_UNARY_INSTR
#undef CASE_TYPED_INSTR
#undef CASE_ID_INSTR
#undef CASE_NOTHING_INSTR

  void print_instructions(Allocator* allocator, anton::Output_Stream& stream,
                          Prettyprint_Options const& options,
                          anton::IList<Instr> const& instructions)
  {
    for(auto const& instruction: instructions) {
      print_instruction(allocator, stream, options, &instruction);
    }
  }

  void print_header(Allocator* allocator, anton::Output_Stream& stream,
                    Prettyprint_Options const& options, Module const& module)
  {
    u32 const bound = calculate_bound(module);
    stream.write("; SPIR-V\n"_sv);
    stream.write("; Version: 1.6\n"_sv);
    stream.write("; Generator: Vush; -1\n"_sv);
    stream.write(anton::format(allocator, "; Bound: {}\n"_sv, bound));
    stream.write("; Schema: 0\n"_sv);
  }

  void prettyprint(Allocator* allocator, anton::Output_Stream& stream,
                   Prettyprint_Options const& options, Module const& module)
  {
    print_header(allocator, stream, options, module);
    print_instructions(allocator, stream, options, module.capabilities);
    print_instructions(allocator, stream, options, module.extensions);
    print_instructions(allocator, stream, options, module.imports);
    print_instructions(allocator, stream, options, module.declarations);
    print_instructions(allocator, stream, options, module.debug);
    print_instructions(allocator, stream, options, module.annotations);
    print_instructions(allocator, stream, options, module.globals);
    print_instructions(allocator, stream, options, module.functions);
  }
} // namespace vush::spirv
