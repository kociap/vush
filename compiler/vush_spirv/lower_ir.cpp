#include <vush_spirv/lower_ir.hpp>

#include <anton/flat_hash_map.hpp>
#include <anton/memory/core.hpp>

#include <vush_core/running_hash.hpp>
#include <vush_core/utility.hpp>
#include <vush_ir/ir.hpp>
#include <vush_spirv/layout.hpp>
#include <vush_spirv/spirv.hpp>

namespace vush {
  using namespace anton::literals;

  namespace {
    struct Lowering_Context;
  }

  [[nodiscard]] static spirv::Instr*
  lower_constant(Lowering_Context& ctx, ir::Constant const* const constant);

  namespace {
    struct Lowering_Context {
    public:
      Allocator* allocator;
      anton::Flat_Hash_Map<ir::Value const*, spirv::Instr*> instr_map;
      anton::Flat_Hash_Map<ir::Basic_Block const*, spirv::Instr_label*> bb_map;
      anton::Flat_Hash_Map<u64, spirv::Instr*> type_map;
      anton::Flat_Hash_Map<ir::Buffer const*, spirv::Instr_variable*>
        buffer_map;
      Array<spirv::Instr_label*> pending_blocks;

      anton::IList<spirv::Instr> capabilities;
      anton::IList<spirv::Instr> extensions;
      anton::IList<spirv::Instr> imports;
      // Memory model, entry points, execution modes.
      anton::IList<spirv::Instr> declarations;
      anton::IList<spirv::Instr> debug;
      anton::IList<spirv::Instr> annotations;
      anton::IList<spirv::Instr> globals;
      anton::IList<spirv::Instr> functions;

    private:
      i64 current_id = 1;

    public:
      Lowering_Context(Allocator* allocator)
        : allocator(allocator), instr_map(allocator), bb_map(allocator),
          type_map(allocator), buffer_map(allocator), pending_blocks(allocator)
      {
      }

      i64 next_id()
      {
        return ++current_id;
      }

      spirv::Instr* get_instr(ir::Value const* const instr)
      {
        if(instanceof<ir::Constant>(instr)) {
          return lower_constant(*this, static_cast<ir::Constant const*>(instr));
        } else {
          auto const iterator = instr_map.find(instr);
          ANTON_ASSERT(iterator != instr_map.end(),
                       "no spirv::Instr for ir::Instr");
          return iterator->value;
        }
      }
    };

    struct Builder {
    private:
      spirv::Instr* current_instruction = nullptr;

    public:
      void set_current_instruction(spirv::Instr* instruction)
      {
        current_instruction = instruction;
      }

      void insert(spirv::Instr* const instruction)
      {
        ANTON_ASSERT(current_instruction != nullptr,
                     "current instruction not set");
        anton::ilist_insert_after(current_instruction, instruction);
        current_instruction = instruction;
      }

      void splice(spirv::Instr* const instruction)
      {
        anton::ilist_splice(current_instruction, instruction);
        current_instruction = anton::ilist_end(current_instruction);
      }
    };
  } // namespace

  void hash_type(Running_Hash& hash, ir::Type const* const type)
  {
    switch(type->kind) {
    case ir::Type_Kind::e_void:
      hash.feed("void");
      break;

    case ir::Type_Kind::e_bool:
      hash.feed("bool");
      break;

    case ir::Type_Kind::e_int8:
      hash.feed("int8");
      break;

    case ir::Type_Kind::e_int16:
      hash.feed("int16");
      break;

    case ir::Type_Kind::e_int32:
      hash.feed("int32");
      break;

    case ir::Type_Kind::e_uint8:
      hash.feed("uint8");
      break;

    case ir::Type_Kind::e_uint16:
      hash.feed("uint16");
      break;

    case ir::Type_Kind::e_uint32:
      hash.feed("uint32");
      break;

    case ir::Type_Kind::e_fp16:
      hash.feed("fp16");
      break;

    case ir::Type_Kind::e_fp32:
      hash.feed("fp32");
      break;

    case ir::Type_Kind::e_fp64:
      hash.feed("fp64");
      break;

    case ir::Type_Kind::e_ptr:
      hash.feed("ptr");
      break;

    case ir::Type_Kind::e_sampler: {
      hash.feed("sampler");
    } break;

    case ir::Type_Kind::e_image: {
      auto const t = static_cast<ir::Type_Image const*>(type);
      hash.feed("image");
      hash_type(hash, t->sampled_type);
      hash.feed(static_cast<u8>(t->format));
      hash.feed(static_cast<u8>(t->dimensions));
      hash.feed(static_cast<u8>(t->multisampled));
      hash.feed(static_cast<u8>(t->array));
      hash.feed(static_cast<u8>(t->shadow));
      hash.feed(static_cast<u8>(t->sampled));
    } break;

    case ir::Type_Kind::e_sampled_image: {
      auto const t = static_cast<ir::Type_Sampled_Image const*>(type);
      hash.feed("sampled_image");
      hash_type(hash, t->sampled_type);
      hash.feed(static_cast<u8>(t->dimensions));
      hash.feed(static_cast<u8>(t->multisampled));
      hash.feed(static_cast<u8>(t->array));
      hash.feed(static_cast<u8>(t->shadow));
    } break;

    case ir::Type_Kind::e_composite: {
      auto const t = static_cast<ir::Type_Composite const*>(type);
      hash.feed("composite");
      hash.feed(t->identifier);
      for(ir::Type const* const element: t->elements) {
        hash_type(hash, element);
      }
    } break;

    case ir::Type_Kind::e_array: {
      auto const t = static_cast<ir::Type_Array const*>(type);
      hash.feed("array");
      hash_type(hash, t->element_type);
      hash.feed(static_cast<u64>(t->size));
    } break;

    case ir::Type_Kind::e_vec: {
      auto const t = static_cast<ir::Type_Vec const*>(type);
      hash.feed("vec");
      hash_type(hash, t->element_type);
      hash.feed(static_cast<u64>(t->rows));
    } break;

    case ir::Type_Kind::e_mat: {
      auto const t = static_cast<ir::Type_Mat const*>(type);
      hash.feed("mat");
      hash_type(hash, t->column_type);
      hash.feed(static_cast<u64>(t->columns));
    } break;
    }
  }

  [[nodiscard]] static spirv::Instr* lower_type(Lowering_Context& ctx,
                                                ir::Type const* const type);

  [[nodiscard]] static spirv::Instr*
  make_new_type_instance(Lowering_Context& ctx, ir::Type const* const type)
  {
    switch(type->kind) {
    case ir::Type_Kind::e_void:
      return spirv::make_instr_type_void(ctx.allocator, ctx.next_id());

    case ir::Type_Kind::e_bool:
      return spirv::make_instr_type_bool(ctx.allocator, ctx.next_id());

    case ir::Type_Kind::e_int8:
      return spirv::make_instr_type_int(ctx.allocator, ctx.next_id(), 8, true);

    case ir::Type_Kind::e_int16:
      return spirv::make_instr_type_int(ctx.allocator, ctx.next_id(), 16, true);

    case ir::Type_Kind::e_int32:
      return spirv::make_instr_type_int(ctx.allocator, ctx.next_id(), 32, true);

    case ir::Type_Kind::e_uint8:
      return spirv::make_instr_type_int(ctx.allocator, ctx.next_id(), 8, false);

    case ir::Type_Kind::e_uint16:
      return spirv::make_instr_type_int(ctx.allocator, ctx.next_id(), 16,
                                        false);

    case ir::Type_Kind::e_uint32:
      return spirv::make_instr_type_int(ctx.allocator, ctx.next_id(), 32,
                                        false);

    case ir::Type_Kind::e_fp16:
      return spirv::make_instr_type_float(ctx.allocator, ctx.next_id(), 16);

    case ir::Type_Kind::e_fp32:
      return spirv::make_instr_type_float(ctx.allocator, ctx.next_id(), 32);

    case ir::Type_Kind::e_fp64:
      return spirv::make_instr_type_float(ctx.allocator, ctx.next_id(), 64);

    case ir::Type_Kind::e_ptr: {
      // TODO: lower pointer types.
      ANTON_UNREACHABLE("unimplemented");
    }

    case ir::Type_Kind::e_vec: {
      auto const t = static_cast<ir::Type_Vec const*>(type);
      spirv::Instr* component_type = lower_type(ctx, t->element_type);
      auto const instr = spirv::make_instr_type_vector(
        ctx.allocator, ctx.next_id(), component_type, t->rows);
      return instr;
    }

    case ir::Type_Kind::e_mat: {
      auto const t = static_cast<ir::Type_Mat const*>(type);
      auto const column_type = lower_type(ctx, t->column_type);
      auto const instr = spirv::make_instr_type_matrix(
        ctx.allocator, ctx.next_id(), column_type, t->columns);
      return instr;
    }

    case ir::Type_Kind::e_sampler:
      return spirv::make_instr_type_sampler(ctx.allocator, ctx.next_id());

    case ir::Type_Kind::e_image: {
      auto const t = static_cast<ir::Type_Image const*>(type);
      auto const sampled_type = lower_type(ctx, t->sampled_type);
      // Depth, array and multisampled map straightforwardly. Sampled == 1 for
      // sampled and sampled == 2 for non-sampled.
      u8 const sampled = t->sampled ? 1 : 2;
      return spirv::make_instr_type_image(
        ctx.allocator, ctx.next_id(), sampled_type,
        static_cast<spirv::Dimensionality>(t->dimensions), t->shadow, t->array,
        t->multisampled, sampled, static_cast<spirv::Image_Format>(t->format));
    }

    case ir::Type_Kind::e_sampled_image: {
      auto const t = static_cast<ir::Type_Sampled_Image const*>(type);
      // We manually hash the type as if it were an image type.
      Running_Hash hash;
      hash.start();
      hash.feed("image");
      hash_type(hash, t->sampled_type);
      hash.feed(static_cast<u8>(ir::Image_Format::e_unknown));
      hash.feed(static_cast<u8>(t->dimensions));
      hash.feed(static_cast<u8>(t->multisampled));
      hash.feed(static_cast<u8>(t->array));
      hash.feed(static_cast<u8>(t->shadow));
      hash.feed(static_cast<u8>(true));
      u64 const result = hash.finish();
      spirv::Instr_type_image* image = nullptr;
      {
        auto const iterator = ctx.type_map.find(result);
        if(iterator != ctx.type_map.end()) {
          image = safe_cast<spirv::Instr_type_image*>(iterator->value);
        } else {
          auto const sampled_type = lower_type(ctx, t->sampled_type);
          // Sampled == 1 for sampled.
          u8 const sampled = 1;
          image = spirv::make_instr_type_image(
            ctx.allocator, ctx.next_id(), sampled_type,
            static_cast<spirv::Dimensionality>(t->dimensions), t->shadow,
            t->array, t->multisampled, sampled, spirv::Image_Format::e_unknown);
          ctx.type_map.emplace(result, image);
          ctx.globals.insert_back(image);
        }
      }
      return spirv::make_instr_type_sampled_image(ctx.allocator, ctx.next_id(),
                                                  image);
    }

    case ir::Type_Kind::e_composite: {
      auto const t = static_cast<ir::Type_Composite const*>(type);
      auto const instr =
        spirv::make_instr_type_struct(ctx.allocator, ctx.next_id());
      for(ir::Type const* const element: t->elements) {
        auto const field_type = lower_type(ctx, element);
        instr->field_types.push_back(field_type);
      }
      return instr;
    }

    case ir::Type_Kind::e_array: {
      auto const t = static_cast<ir::Type_Array const*>(type);
      auto const element_type = lower_type(ctx, t->element_type);
      if(t->size > 0) {
        auto const u32_type = lower_type(ctx, ir::get_type_uint32());
        auto const length = make_instr_constant_u32(
          ctx.allocator, ctx.next_id(), u32_type, t->size);
        return spirv::make_instr_type_array(ctx.allocator, ctx.next_id(),
                                            element_type, length);
      } else {
        return spirv::make_instr_type_runtime_array(
          ctx.allocator, ctx.next_id(), element_type);
      }
    }
    }
  }

  spirv::Instr* lower_type(Lowering_Context& ctx, ir::Type const* const type)
  {
    Running_Hash hash;
    hash.start();
    hash_type(hash, type);
    u64 const value = hash.finish();
    auto iter = ctx.type_map.find(value);
    if(iter == ctx.type_map.end()) {
      auto const instr = make_new_type_instance(ctx, type);
      ctx.globals.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return iter->value;
  }

  [[nodiscard]] static spirv::Instr_type_function*
  lower_type(Lowering_Context& ctx, ir::Function const* const function)
  {
    Running_Hash hash;
    hash.start();
    hash.feed("function");
    for(ir::Argument const& argument: function->arguments) {
      hash_type(hash, argument.type);
    }
    hash_type(hash, function->return_type);
    u64 const value = hash.finish();
    auto iter = ctx.type_map.find(value);
    if(iter == ctx.type_map.end()) {
      auto const return_type = lower_type(ctx, function->return_type);
      auto const instr =
        make_instr_type_function(ctx.allocator, ctx.next_id(), return_type);
      ctx.globals.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return safe_cast<spirv::Instr_type_function*>(iter->value);
  }

  [[nodiscard]] static spirv::Instr_type_pointer*
  lower_type_as_pointer(Lowering_Context& ctx, ir::Type const* const type,
                        spirv::Storage_Class const storage_class)
  {
    Running_Hash hash;
    hash.start();
    hash.feed("ptr");
    hash.feed(static_cast<u32>(storage_class));
    hash_type(hash, type);
    u64 const value = hash.finish();
    auto iter = ctx.type_map.find(value);
    if(iter == ctx.type_map.end()) {
      auto const pointee_type = lower_type(ctx, type);
      auto const instr = make_instr_type_pointer(ctx.allocator, ctx.next_id(),
                                                 pointee_type, storage_class);
      ctx.globals.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return safe_cast<spirv::Instr_type_pointer*>(iter->value);
  }

  void hash_constant(Running_Hash& hash, ir::Constant const* const gconstant)
  {
    switch(gconstant->constant_kind) {
    case ir::Constant_Kind::e_constant_bool: {
      auto const constant = static_cast<ir::Constant_bool const*>(gconstant);
      hash.feed("constant_bool");
      hash.feed(constant->value ? "true" : "false");
    } break;

    case ir::Constant_Kind::e_constant_i32: {
      auto const constant = static_cast<ir::Constant_i32 const*>(gconstant);
      hash.feed("constant_i32");
      hash.feed((u32)constant->value);
    } break;

    case ir::Constant_Kind::e_constant_u32: {
      auto const constant = static_cast<ir::Constant_u32 const*>(gconstant);
      hash.feed("constant_u32");
      hash.feed(constant->value);
    } break;

    case ir::Constant_Kind::e_constant_f32: {
      auto const constant = static_cast<ir::Constant_f32 const*>(gconstant);
      hash.feed("constant_f32");
      hash.feed(*reinterpret_cast<u32 const*>(&constant->value));
    } break;

    case ir::Constant_Kind::e_constant_f64: {
      auto const constant = static_cast<ir::Constant_f64 const*>(gconstant);
      hash.feed("constant_f64");
      hash.feed(*reinterpret_cast<u64 const*>(&constant->value));
    } break;

    case ir::Constant_Kind::e_undef:
      hash.feed("constant_undef");
      break;
    }
  }

  [[nodiscard]] static spirv::Instr*
  make_new_constant_instance(Lowering_Context& ctx,
                             ir::Constant const* const gconstant)
  {
    auto const type = lower_type(ctx, gconstant->type);
    switch(gconstant->constant_kind) {
    case ir::Constant_Kind::e_constant_bool: {
      auto const constant = static_cast<ir::Constant_bool const*>(gconstant);
      if(constant->value == true) {
        return spirv::make_instr_constant_true(ctx.allocator, ctx.next_id(),
                                               type);
      } else {
        return spirv::make_instr_constant_false(ctx.allocator, ctx.next_id(),
                                                type);
      }
    } break;

    case ir::Constant_Kind::e_constant_i32: {
      auto const constant = static_cast<ir::Constant_i32 const*>(gconstant);
      return spirv::make_instr_constant_i32(ctx.allocator, ctx.next_id(), type,
                                            constant->value);
    } break;

    case ir::Constant_Kind::e_constant_u32: {
      auto const constant = static_cast<ir::Constant_u32 const*>(gconstant);
      return spirv::make_instr_constant_u32(ctx.allocator, ctx.next_id(), type,
                                            constant->value);
    } break;

    case ir::Constant_Kind::e_constant_f32: {
      auto const constant = static_cast<ir::Constant_f32 const*>(gconstant);
      return spirv::make_instr_constant_f32(ctx.allocator, ctx.next_id(), type,
                                            constant->value);
    } break;

    case ir::Constant_Kind::e_constant_f64: {
      auto const constant = static_cast<ir::Constant_f64 const*>(gconstant);
      return spirv::make_instr_constant_f64(ctx.allocator, ctx.next_id(), type,
                                            constant->value);
    } break;

    case ir::Constant_Kind::e_undef: {
      ANTON_UNREACHABLE("undef unimplemented");
    } break;
    }
  }

  [[nodiscard]] static spirv::Instr*
  lower_constant(Lowering_Context& ctx, ir::Constant const* const constant)
  {
    Running_Hash hash;
    hash.start();
    hash_constant(hash, constant);
    u64 const value = hash.finish();
    auto iter = ctx.type_map.find(value);
    if(iter == ctx.type_map.end()) {
      auto const instr = make_new_constant_instance(ctx, constant);
      ctx.globals.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return iter->value;
  }

  [[nodiscard]] static spirv::Instr*
  lower_u32_to_constant(Lowering_Context& ctx, u32 const constant)
  {
    Running_Hash hash;
    hash.start();
    hash.feed("constant_u32");
    hash.feed(constant);
    u64 const value = hash.finish();
    auto iter = ctx.type_map.find(value);
    if(iter == ctx.type_map.end()) {
      auto const type = lower_type(ctx, ir::get_type_uint32());
      auto const instr = spirv::make_instr_constant_u32(
        ctx.allocator, ctx.next_id(), type, constant);
      ctx.globals.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return iter->value;
  }

  // make_annotated_buffer_type
  //
  // Decorate the members with layout annotations.
  //
  [[nodiscard]] static spirv::Instr*
  make_annotated_buffer_type(Lowering_Context& ctx, ir::Type const* const type)
  {
    Running_Hash hash;
    hash.start();
    hash_type(hash, type);
    u64 const value = hash.finish();
    {
      auto iter = ctx.type_map.find(value);
      if(iter != ctx.type_map.end()) {
        return iter->value;
      }
    }

    switch(type->kind) {
    case ir::Type_Kind::e_array: {
      auto const array = static_cast<ir::Type_Array const*>(type);
      auto const element_type =
        make_annotated_buffer_type(ctx, array->element_type);
      spirv::Instr* instr;
      if(array->size > 0) {
        auto const u32_type = lower_type(ctx, ir::get_type_uint32());
        auto const length = make_instr_constant_u32(
          ctx.allocator, ctx.next_id(), u32_type, array->size);
        instr = spirv::make_instr_type_array(ctx.allocator, ctx.next_id(),
                                             element_type, length);
      } else {
        instr = spirv::make_instr_type_runtime_array(
          ctx.allocator, ctx.next_id(), element_type);
      }
      ctx.globals.insert_back(instr);
      ctx.type_map.emplace(value, instr);

      // SPIR-V 2.16.2: Arrays require the ArrayStride decoration.
      {
        auto const layout = calculate_base_layout(array->element_type);
        auto const decoration = spirv::make_instr_decorate(
          ctx.allocator, instr, spirv::Decoration::e_array_stride,
          spirv::Decoration_Argument(layout.size));
        ctx.annotations.insert_back(*decoration);
      }

      return instr;
    }
    case ir::Type_Kind::e_composite: {
      auto const composite = static_cast<ir::Type_Composite const*>(type);
      auto const instr =
        spirv::make_instr_type_struct(ctx.allocator, ctx.next_id());

      u32 offset = 0;
      u32 index = 0;
      for(auto const element: composite->elements) {
        auto const element_instr = make_annotated_buffer_type(ctx, element);
        instr->field_types.push_back(element_instr);
        auto const layout = calculate_base_layout(element);
        // SPIR-V 2.16.2: Members must be decorated with offset.
        {
          offset = anton::align_address(offset, layout.alignment);
          auto const decoration = spirv::make_instr_member_decorate(
            ctx.allocator, instr, index, spirv::Decoration::e_offset,
            spirv::Decoration_Argument(offset));
          ctx.annotations.insert_back(*decoration);
        }
        // SPIR-V 2.16.2: Matrices or arrays of matrices must be decorated
        // with MatrixStride and ColMajor.
        {
          auto type = element;
          if(instanceof<ir::Type_Array>(type)) {
            auto const array = static_cast<ir::Type_Array const*>(type);
            type = array->element_type;
          }

          if(instanceof<ir::Type_Mat>(type)) {
            auto const matrix = static_cast<ir::Type_Mat const*>(type);
            auto const layout = calculate_base_layout(matrix->column_type);

            auto const decoration_matrix_stride =
              spirv::make_instr_member_decorate(
                ctx.allocator, instr, index, spirv::Decoration::e_matrix_stride,
                spirv::Decoration_Argument(layout.size));
            ctx.annotations.insert_back(*decoration_matrix_stride);
            auto const decoration_col_major = spirv::make_instr_member_decorate(
              ctx.allocator, instr, index, spirv::Decoration::e_col_major,
              spirv::Decoration_Argument{});
            ctx.annotations.insert_back(*decoration_col_major);
          }
        }

        offset += layout.size;
        index += 1;
      }
      // We must insert after all constituent types have been created.
      ctx.globals.insert_back(instr);
      ctx.type_map.emplace(value, instr);
      return instr;
    }

    default:
      return lower_type(ctx, type);
    }
  }

  [[nodiscard]] static spirv::Instr_variable*
  lower_buffer(Lowering_Context& ctx, ir::Buffer const* const buffer)
  {
    {
      auto const iterator = ctx.buffer_map.find(buffer);
      if(iterator != ctx.buffer_map.end()) {
        return iterator->value;
      }
    }

    // TODO: Always lowers as storage buffer.
    auto const buffer_type = make_annotated_buffer_type(ctx, buffer->type);
    // Mandatory Block decoration for the buffer type.
    auto const decoration_block = spirv::make_instr_decorate(
      ctx.allocator, buffer_type, spirv::Decoration::e_block);
    ctx.annotations.insert_back(*decoration_block);
    // Make the variable.
    auto const pointer_type =
      spirv::make_instr_type_pointer(ctx.allocator, ctx.next_id(), buffer_type,
                                     spirv::Storage_Class::e_storage_buffer);
    ctx.globals.insert_back(*pointer_type);
    auto const variable =
      spirv::make_instr_variable(ctx.allocator, ctx.next_id(), pointer_type,
                                 spirv::Storage_Class::e_storage_buffer);
    ctx.globals.insert_back(*variable);
    ctx.buffer_map.emplace(buffer, variable);
    // Binding and DescriptorSet decorations.
    auto const decoration_set = spirv::make_instr_decorate(
      ctx.allocator, variable, spirv::Decoration::e_descriptor_set,
      spirv::Decoration_Argument{buffer->descriptor_set});
    ctx.annotations.insert_back(*decoration_set);
    auto const decoration_binding = spirv::make_instr_decorate(
      ctx.allocator, variable, spirv::Decoration::e_binding,
      spirv::Decoration_Argument{buffer->binding});
    ctx.annotations.insert_back(*decoration_binding);
    return variable;
  }

  [[nodiscard]] static i64
  get_integer_constant_as_i64(ir::Constant const* const constant)
  {
    if(instanceof<ir::Constant_i32>(constant)) {
      return static_cast<ir::Constant_i32 const*>(constant)->value;
    } else if(instanceof<ir::Constant_u32>(constant)) {
      return static_cast<ir::Constant_u32 const*>(constant)->value;
    } else {
      ANTON_UNREACHABLE("non-integer constant cannot be converted to integer");
    }
  }

  [[nodiscard]] static spirv::Instr_label*
  lower_block(Lowering_Context& ctx, ir::Basic_Block const* const block)
  {
    {
      auto const iterator = ctx.bb_map.find(block);
      if(iterator != ctx.bb_map.end()) {
        return iterator->value;
      }
    }

    auto const label = spirv::make_instr_label(ctx.allocator, ctx.next_id());
    ctx.bb_map.emplace(block, label);
    ctx.pending_blocks.push_back(label);

    Builder builder;
    builder.set_current_instruction(label);
    for(auto const& instruction: block->instructions) {
      switch(instruction.instr_kind) {
      case ir::Instr_Kind::e_intrinsic: {
        auto const instr_intrinsic =
          static_cast<ir::Instr_intrinsic const*>(&instruction);
        switch(instr_intrinsic->intrinsic_kind) {
        case ir::Intrinsic_Kind::e_scf_branch_head: {
          auto const intrinsic =
            static_cast<ir::Intrinsic_scf_branch_head const*>(instr_intrinsic);
          auto const converge_block =
            lower_block(ctx, intrinsic->converge_block);
          // We do not add this instruction to the instruction map - it is not
          // referenced by anything.
          auto const selection_merge =
            spirv::make_instr_selection_merge(ctx.allocator, converge_block);
          builder.insert(selection_merge);
          selection_merge->block = label;
        } break;
        }
      } break;

      case ir::Instr_Kind::e_alloc: {
        auto const instr_alloc =
          static_cast<ir::Instr_alloc const*>(&instruction);
        auto const result_type = lower_type_as_pointer(
          ctx, instr_alloc->alloc_type, spirv::Storage_Class::e_function);
        auto const instr =
          spirv::make_instr_variable(ctx.allocator, ctx.next_id(), result_type,
                                     spirv::Storage_Class::e_function);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_load: {
        auto const instr_load =
          static_cast<ir::Instr_load const*>(&instruction);
        auto const address = ctx.get_instr(instr_load->address);
        auto const type = lower_type(ctx, instr_load->type);
        auto const instr =
          spirv::make_instr_load(ctx.allocator, ctx.next_id(), type, address);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_store: {
        auto const instr_store =
          static_cast<ir::Instr_store const*>(&instruction);
        auto const pointer = ctx.get_instr(instr_store->dst);
        auto const object = ctx.get_instr(instr_store->src);
        auto const instr =
          spirv::make_instr_store(ctx.allocator, pointer, object);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_getptr: {
        auto const instr_getptr =
          static_cast<ir::Instr_getptr const*>(&instruction);
        auto const base = ctx.get_instr(instr_getptr->address);
        // Base must have a pointer type.
        auto const base_type =
          safe_cast<spirv::Instr_type_pointer*>(spirv::get_result_type(base));
        spirv::Storage_Class const storage_class = base_type->storage_class;
        ir::Type const* const type = instr_getptr->addressed_type;
        // When the type is a composite, the indices must be constant integers.
        // We can use those indices to get the type of the member.
        // Otherwise, the type is an array, vector or matrix and all elements
        // are the same regardless of the index.
        spirv::Instr_type_pointer* result_type = nullptr;
        switch(type->kind) {
        case ir::Type_Kind::e_composite: {
          auto const type_composite =
            static_cast<ir::Type_Composite const*>(type);
          i64 const index = get_integer_constant_as_i64(
            safe_cast<ir::Constant const*>(instr_getptr->index));
          auto const element_type = type_composite->elements[index];
          result_type = lower_type_as_pointer(ctx, element_type, storage_class);
        } break;

        case ir::Type_Kind::e_array: {
          auto const type_array = static_cast<ir::Type_Array const*>(type);
          result_type =
            lower_type_as_pointer(ctx, type_array->element_type, storage_class);
        } break;

        case ir::Type_Kind::e_mat: {
          auto const type_mat = static_cast<ir::Type_Mat const*>(type);
          result_type =
            lower_type_as_pointer(ctx, type_mat->column_type, storage_class);
        } break;

        case ir::Type_Kind::e_vec: {
          auto const type_vec = static_cast<ir::Type_Vec const*>(type);
          result_type =
            lower_type_as_pointer(ctx, type_vec->element_type, storage_class);
        } break;

        default:
          ANTON_UNREACHABLE("invalid getptr base type");
        }

        auto const index = ctx.get_instr(instr_getptr->index);
        auto const instr = spirv::make_instr_access_chain(
          ctx.allocator, ctx.next_id(), result_type, base, index);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_alu: {
        auto const instr_alu = static_cast<ir::Instr_ALU const*>(&instruction);
        auto const result_type = lower_type(ctx, instr_alu->type);
        spirv::Instr* instr;

#define ALU_UNARY_CASE(OPCODE, INSTR)                               \
  case ir::ALU_Opcode::OPCODE: {                                    \
    auto const operand = ctx.get_instr(instr_alu->src1);            \
    instr = spirv::make_instr_##INSTR(ctx.allocator, ctx.next_id(), \
                                      result_type, operand);        \
  } break;

#define ALU_BINARY_CASE(OPCODE, INSTR)                                  \
  case ir::ALU_Opcode::OPCODE: {                                        \
    auto const operand1 = ctx.get_instr(instr_alu->src1);               \
    auto const operand2 = ctx.get_instr(instr_alu->src2);               \
    instr = spirv::make_instr_##INSTR(ctx.allocator, ctx.next_id(),     \
                                      result_type, operand1, operand2); \
  } break;

#define ALU_CASE_UNREACHABLE(OPCODE)                                    \
  case ir::ALU_Opcode::OPCODE:                                          \
    ANTON_UNREACHABLE("ALU instruction " #OPCODE " cannot be lowered"); \
    break;

        switch(instr_alu->op) {
          ALU_UNARY_CASE(e_inv, bit_not)
          ALU_BINARY_CASE(e_and, bit_and)
          ALU_BINARY_CASE(e_or, bit_or)
          ALU_BINARY_CASE(e_xor, bit_xor)
          ALU_BINARY_CASE(e_shl, shl)
          // All SHR operations are logical for now.
          ALU_BINARY_CASE(e_shr, shr_logical)
          ALU_UNARY_CASE(e_neg, snegate)
          ALU_BINARY_CASE(e_iadd, iadd)
          ALU_BINARY_CASE(e_imul, imul)
          ALU_BINARY_CASE(e_uadd, iadd)
          ALU_BINARY_CASE(e_umul, imul)
          ALU_BINARY_CASE(e_idiv, sdiv)
          ALU_BINARY_CASE(e_udiv, udiv)
          ALU_BINARY_CASE(e_irem, srem)
          ALU_BINARY_CASE(e_urem, umod)
          ALU_UNARY_CASE(e_fneg, fnegate)
          ALU_BINARY_CASE(e_fadd, fadd)
          ALU_BINARY_CASE(e_fmul, fmul)
          ALU_BINARY_CASE(e_fdiv, fdiv)
          ALU_BINARY_CASE(e_icmp_eq, ieq)
          ALU_BINARY_CASE(e_icmp_neq, ineq)
          ALU_BINARY_CASE(e_icmp_ugt, ugt)
          ALU_BINARY_CASE(e_icmp_ult, ult)
          ALU_BINARY_CASE(e_icmp_uge, uge)
          ALU_BINARY_CASE(e_icmp_ule, ule)
          ALU_BINARY_CASE(e_icmp_sgt, sgt)
          ALU_BINARY_CASE(e_icmp_slt, slt)
          ALU_BINARY_CASE(e_icmp_sge, sge)
          ALU_BINARY_CASE(e_icmp_sle, sle)
          ALU_BINARY_CASE(e_fcmp_eq, foeq)
          ALU_BINARY_CASE(e_fcmp_neq, foneq)
          ALU_BINARY_CASE(e_fcmp_gt, fogt)
          ALU_BINARY_CASE(e_fcmp_lt, folt)
          ALU_BINARY_CASE(e_fcmp_ge, foge)
          ALU_BINARY_CASE(e_fcmp_le, fole)
          // No FMA instruction in SPIR-V. There's FMA in the extended
          // instructions for GLSL.
          ALU_CASE_UNREACHABLE(e_fma)
        }
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_vector_extract: {
        auto const instr_extract =
          static_cast<ir::Instr_vector_extract const*>(&instruction);
        auto const result_type = lower_type(ctx, instr_extract->type);
        auto const vector = ctx.get_instr(instr_extract->value);
        auto const instr = spirv::make_instr_composite_extract(
          ctx.allocator, ctx.next_id(), result_type, vector);
        // TODO: Verify that the indices fit within 32 bits.
        instr->indices.push_back(instr_extract->index);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_vector_insert: {
        auto const instr_insert =
          static_cast<ir::Instr_vector_insert const*>(&instruction);
        auto const result_type = lower_type(ctx, instr_insert->type);
        auto const vector = ctx.get_instr(instr_insert->dst);
        auto const value = ctx.get_instr(instr_insert->value);
        auto const instr = spirv::make_instr_composite_insert(
          ctx.allocator, ctx.next_id(), result_type, vector, value);
        // TODO: Verify that the indices fit within 32 bits.
        instr->indices.push_back(instr_insert->index);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_composite_extract: {
        auto const instr_extract =
          static_cast<ir::Instr_composite_extract const*>(&instruction);
        auto const result_type = lower_type(ctx, instr_extract->type);
        auto const composite = ctx.get_instr(instr_extract->value);
        auto const instr = spirv::make_instr_composite_extract(
          ctx.allocator, ctx.next_id(), result_type, composite);
        // TODO: Verify that the indices fit within 32 bits.
        for(auto const index: instr_extract->indices) {
          instr->indices.push_back(index);
        }
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_composite_construct: {
        auto const instr_construct =
          static_cast<ir::Instr_composite_construct const*>(&instruction);
        auto const result_type = lower_type(ctx, instr_construct->type);
        auto const instr = spirv::make_instr_composite_construct(
          ctx.allocator, ctx.next_id(), result_type);
        for(auto const element: instr_construct->elements) {
          auto const constituent = ctx.get_instr(element);
          instr->constituents.push_back(constituent);
        }
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

        // TODO: Lower the remaining instructions.
      case ir::Instr_Kind::e_cvt_sext:
      case ir::Instr_Kind::e_cvt_zext:
      case ir::Instr_Kind::e_cvt_trunc:
      case ir::Instr_Kind::e_cvt_fpext:
      case ir::Instr_Kind::e_cvt_fptrunc:
      case ir::Instr_Kind::e_cvt_si2fp:
      case ir::Instr_Kind::e_cvt_fp2si:
      case ir::Instr_Kind::e_cvt_ui2fp:
      case ir::Instr_Kind::e_cvt_fp2ui:
      case ir::Instr_Kind::e_call:
      case ir::Instr_Kind::e_ext_call:
      case ir::Instr_Kind::e_phi:
        break;

      case ir::Instr_Kind::e_return: {
        auto const instr_return =
          static_cast<ir::Instr_return const*>(&instruction);
        if(instr_return->value != nullptr) {
          auto const value = ctx.get_instr(instr_return->value);
          auto const instr =
            spirv::make_instr_return_value(ctx.allocator, value);
          builder.insert(instr);
          instr->block = label;
          ctx.instr_map.emplace(&instruction, instr);
        } else {
          auto const instr = spirv::make_instr_return(ctx.allocator);
          builder.insert(instr);
          instr->block = label;
          ctx.instr_map.emplace(&instruction, instr);
        }
      } break;

      case ir::Instr_Kind::e_die: {
        auto const instr = spirv::make_instr_terminate(ctx.allocator);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_branch: {
        auto const instr_branch =
          static_cast<ir::Instr_branch const*>(&instruction);
        auto const label = lower_block(ctx, instr_branch->target);
        auto const instr = spirv::make_instr_branch(ctx.allocator, label);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_brcond: {
        auto const instr_brcond =
          static_cast<ir::Instr_brcond const*>(&instruction);
        auto const condition = ctx.get_instr(instr_brcond->condition);
        auto const then_label = lower_block(ctx, instr_brcond->then_target);
        auto const else_label = lower_block(ctx, instr_brcond->else_target);
        auto const instr = spirv::make_instr_brcond(ctx.allocator, condition,
                                                    then_label, else_label);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_switch: {
        auto const instr_switch =
          static_cast<ir::Instr_switch const*>(&instruction);
        auto const selector = ctx.get_instr(instr_switch->selector);
        auto const default_label =
          lower_block(ctx, instr_switch->default_label);
        auto const instr =
          spirv::make_instr_switch(ctx.allocator, selector, default_label);
        builder.insert(instr);
        instr->block = label;
        ctx.instr_map.emplace(&instruction, instr);
        for(auto const label: instr_switch->labels) {
          auto const target = lower_block(ctx, label.target);
          instr->labels.push_back({(u64)label.value, target});
        }
      } break;
      }
    }
    return label;
  }

  static void hoist_variables(spirv::Instr* instruction)
  {
    spirv::Instr* label = nullptr;
    // Find the first label instruction.
    while(!instanceof<spirv::Instr_function_end>(instruction)) {
      if(instanceof<spirv::Instr_label>(instruction)) {
        label = instruction;
        break;
      }
      instruction = ilist_next(instruction);
    }

    while(!instanceof<spirv::Instr_function_end>(instruction)) {
      if(!instanceof<spirv::Instr_variable>(instruction)) {
        instruction = ilist_next(instruction);
        continue;
      }

      auto const next_instruction = ilist_next(instruction);
      anton::ilist_erase(instruction);
      anton::ilist_insert_after(label, instruction);
      instruction = next_instruction;
    }
  }

  [[nodiscard]] static anton::IList<spirv::Instr>
  lower_function(Lowering_Context& ctx, ir::Function const* const function)
  {
    auto const function_type = lower_type(ctx, function);
    auto const instr_function =
      spirv::make_instr_function(ctx.allocator, ctx.next_id(), function_type);
    Builder builder;
    builder.set_current_instruction(instr_function);
    // Lower parameters.
    for(auto const& argument: function->arguments) {
      // TODO: What about storage class?
      auto const result_type = lower_type(ctx, argument.type);
      auto const parameter = spirv::make_instr_function_parameter(
        ctx.allocator, ctx.next_id(), result_type);
      builder.insert(parameter);
      ctx.instr_map.emplace(&argument, parameter);
    }
    auto const entry_label = lower_block(ctx, function->entry_block);
    // We intentionally ignore the entry_label as it is automatically added to
    // the list of pending blocks at the first position.
    ANTON_UNUSED(entry_label);
    for(auto const label: ctx.pending_blocks) {
      builder.splice(label);
    }
    ctx.pending_blocks.clear();

    auto const instr_end = spirv::make_instr_function_end(ctx.allocator);
    builder.insert(instr_end);

    hoist_variables(instr_function);

    return {instr_function, instr_end};
  }

  [[nodiscard]] static spirv::Instr_type_function*
  make_entry_function_type(Lowering_Context& ctx)
  {
    auto const ir_return_type = ir::get_type_void();
    Running_Hash hash;
    hash.start();
    hash.feed("function");
    hash_type(hash, ir_return_type);
    u64 const value = hash.finish();
    auto iter = ctx.type_map.find(value);
    if(iter == ctx.type_map.end()) {
      auto const return_type = lower_type(ctx, ir_return_type);
      auto const instr =
        make_instr_type_function(ctx.allocator, ctx.next_id(), return_type);
      ctx.globals.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return safe_cast<spirv::Instr_type_function*>(iter->value);
  }

  struct Module_Entry {
    Array<spirv::Instr_variable*> interface;
    spirv::Instr_function* entry_begin;
    spirv::Instr_function_end* entry_end;
  };

  static void decorate_interface(Lowering_Context& ctx,
                                 ir::Decoration const* decoration,
                                 spirv::Instr_variable* const variable)
  {
    while(decoration != nullptr) {
      switch(anton::hash(decoration->identifier)) {
      case anton::hash("builtin"): {
        spirv::Builtin value;
        switch(anton::hash(decoration->argument.get_string())) {
        case anton::hash("position"):
          value = spirv::Builtin::e_position;
          break;
        case anton::hash("vertex_index"):
          value = spirv::Builtin::e_vertex_index;
          break;
        default:
          ANTON_UNREACHABLE("unimplemented");
        }

        auto const instr = spirv::make_instr_decorate(
          ctx.allocator, variable, spirv::Decoration::e_builtin,
          spirv::Decoration_Argument(static_cast<u32>(value)));
        ctx.annotations.insert_back(*instr);
      } break;

      case anton::hash("location"): {
        auto const instr = spirv::make_instr_decorate(
          ctx.allocator, variable, spirv::Decoration::e_location,
          spirv::Decoration_Argument(decoration->argument.get_u64()));
        ctx.annotations.insert_back(*instr);
      } break;

      default:
        break;
      }

      decoration = ilist_next<anton::IList_Node<ir::Decoration>>(decoration);
    }
  }

  [[nodiscard]] static Module_Entry
  lower_module_entry(Lowering_Context& ctx, ir::Function const* const function)
  {
    auto const function_type = make_entry_function_type(ctx);
    auto const instr_function =
      spirv::make_instr_function(ctx.allocator, ctx.next_id(), function_type);
    Builder builder;
    builder.set_current_instruction(instr_function);
    Array<spirv::Instr_variable*> interface(ctx.allocator);
    // Module has no parameters. All function inputs are sourced, hence they are
    // global OpVariable. We have to construct pointers via OpAccessChain to
    // the members of the buffers.
    Array<spirv::Instr*> parameter_pointers(ctx.allocator);
    for(auto const& argument: function->arguments) {
      if(argument.buffer == nullptr) {
        ANTON_ASSERT(argument.storage_class == ir::Storage_Class::e_output ||
                       argument.storage_class == ir::Storage_Class::e_input ||
                       argument.storage_class == ir::Storage_Class::e_uniform,
                     "argument without buffer is not input/output/uniform");
        ANTON_ASSERT(argument.storage_class != ir::Storage_Class::e_uniform ||
                       ir::is_image(*argument.pointee_type),
                     "uniform argument must be an image");
        // We always lower the input/output/uniform parameters as variables.
        // Images lower as UniformConstant.
        auto const storage_class =
          argument.storage_class == ir::Storage_Class::e_uniform
            ? spirv::Storage_Class::e_uniform_constant
            : (argument.storage_class == ir::Storage_Class::e_output
                 ? spirv::Storage_Class::e_output
                 : spirv::Storage_Class::e_input);
        // TODO: Requires new type instance due to decorations.
        auto const value_type = lower_type(ctx, argument.pointee_type);
        if(instanceof<spirv::Instr_type_struct>(value_type)) {
          // The block decoration is only required on structure types.
          auto const decoration_block = spirv::make_instr_decorate(
            ctx.allocator, value_type, spirv::Decoration::e_block);
          ctx.annotations.insert_back(*decoration_block);
        }
        // Make the variable.
        auto const pointer_type =
          lower_type_as_pointer(ctx, argument.pointee_type, storage_class);
        auto const variable = spirv::make_instr_variable(
          ctx.allocator, ctx.next_id(), pointer_type, storage_class);
        decorate_interface(ctx, argument.decorations, variable);
        interface.push_back(variable);
        ctx.globals.insert_back(*variable);
        ctx.instr_map.emplace(&argument, variable);
      } else {
        auto const buffer = lower_buffer(ctx, argument.buffer);
        interface.push_back(buffer);
        // TODO: We might be decorating an interface multiple times.
        decorate_interface(ctx, argument.decorations, buffer);
        // Make pointer type to the buffer field.
        auto const pointer_type = lower_type_as_pointer(
          ctx, argument.pointee_type, buffer->storage_class);
        // Make AccessChain to the field.
        auto const index = lower_u32_to_constant(ctx, argument.buffer_index);
        auto const field_pointer = spirv::make_instr_access_chain(
          ctx.allocator, ctx.next_id(), pointer_type, buffer, index);
        // Store the AccessChains to insert them after the entry label.
        parameter_pointers.push_back(field_pointer);
        ctx.instr_map.emplace(&argument, field_pointer);
      }
    }

    auto const entry_label = lower_block(ctx, function->entry_block);
    // We intentionally ignore the entry_label as it is automatically added to
    // the list of pending blocks at the first position.
    for(auto const label: ctx.pending_blocks) {
      builder.splice(label);
    }
    ctx.pending_blocks.clear();

    // Insert the missing parameter pointers.
    for(auto const pointer: parameter_pointers) {
      ilist_insert_after(entry_label, pointer);
    }
    parameter_pointers.clear();

    auto const instr_end = spirv::make_instr_function_end(ctx.allocator);
    builder.insert(instr_end);

    hoist_variables(instr_function);

    return {ANTON_MOV(interface), instr_function, instr_end};
  }

  [[nodiscard]] static spirv::Execution_Model
  stage_to_execution(Stage_Kind const kind)
  {
    switch(kind) {
    case Stage_Kind::vertex:
      return spirv::Execution_Model::e_vertex;
    case Stage_Kind::fragment:
      return spirv::Execution_Model::e_fragment;
    case Stage_Kind::compute:
      return spirv::Execution_Model::e_glcompute;
    }
  }

  spirv::Module lower_ir_module(Allocator* const allocator,
                                ir::Module const* const module)
  {
    Lowering_Context ctx(allocator);
    // Always add Shader, Matrix, DrawParameters, PSBA capabilities. We will
    // dynamically select which capabilities to add later on.
    {
      auto const capability_shader =
        spirv::make_instr_capability(allocator, spirv::Capability::e_shader);
      auto const capability_matrix =
        spirv::make_instr_capability(allocator, spirv::Capability::e_matrix);
      auto const capability_draw_parameters = spirv::make_instr_capability(
        allocator, spirv::Capability::e_draw_parameters);
      auto const capability_PSBA = spirv::make_instr_capability(
        allocator, spirv::Capability::e_physical_storage_buffer_addresses);
      // auto const capability_VMM = spirv::make_instr_capability(
      //   allocator, spirv::Capability::e_vulkan_memory_model);
      ctx.capabilities.insert_back(*capability_shader);
      ctx.capabilities.insert_back(*capability_matrix);
      ctx.capabilities.insert_back(*capability_draw_parameters);
      ctx.capabilities.insert_back(*capability_PSBA);
      // ctx.capabilities.insert_back(*capability_VMM);
    }
    // The required memory model. We always use PhysicalStorageBuffer64 and
    // GLSL450.
    {
      spirv::Instr_memory_model* const memory_model =
        spirv::make_instr_memory_model(
          allocator, spirv::Addressing_Model::e_physical_storage_buffer64,
          spirv::Memory_Model::e_glsl450);
      ctx.declarations.insert_back(*memory_model);
    }
    // Lower the entry function and create the entry point.
    {
      Module_Entry entry = lower_module_entry(ctx, module->entry);
      anton::IList<spirv::Instr> instructions{entry.entry_begin,
                                              entry.entry_end};
      ctx.functions.splice(instructions);
      spirv::Execution_Model const execution_model =
        stage_to_execution(module->stage);
      // Always call the entry point "main".
      auto const entry_point = spirv::make_instr_entry_point(
        allocator, entry.entry_begin, anton::String("main"_sv, allocator),
        execution_model);
      entry_point->interface = ANTON_MOV(entry.interface);
      ctx.declarations.insert_back(*entry_point);
      // Add execution modes.
      if(module->stage == Stage_Kind::fragment) {
        auto const origin = spirv::make_instr_execution_mode(
          ctx.allocator, entry.entry_begin,
          spirv::Execution_Mode::e_origin_upper_left);
        ctx.declarations.insert_back(*origin);
      }
    }
    spirv::Module spirv_module{
      .capabilities = ANTON_MOV(ctx.capabilities),
      .extensions = ANTON_MOV(ctx.extensions),
      .imports = ANTON_MOV(ctx.imports),
      .declarations = ANTON_MOV(ctx.declarations),
      .debug = ANTON_MOV(ctx.debug),
      .annotations = ANTON_MOV(ctx.annotations),
      .globals = ANTON_MOV(ctx.globals),
      .functions = ANTON_MOV(ctx.functions),
    };
    return spirv_module;
  }
} // namespace vush
