#include <anton/flat_hash_map.hpp>

#include <vush_ir/ir.hpp>
#include <vush_spirv/spirv.hpp>

namespace vush {
  struct Running_Hash {
  private:
    static constexpr u64 m = 0xc6a4a7935bd1e995;
    static constexpr i64 r = 47;

    u64 h = 0;
    u64 buffer = 0;
    i64 buffer_length = 0;

  public:
    void start(u64 seed = 0x1F0D3804)
    {
      h = seed;
      buffer = 0;
      buffer_length = 0;
    }

    u64 finish()
    {
      switch(buffer_length) {
      case 7:
        h ^= buffer & 0xFF000000000000;
      case 6:
        h ^= buffer & 0xFF0000000000;
      case 5:
        h ^= buffer & 0xFF00000000;
      case 4:
        h ^= buffer & 0xFF000000;
      case 3:
        h ^= buffer & 0xFF0000;
      case 2:
        h ^= buffer & 0xFF00;
      case 1:
        h ^= buffer & 0xFF;
        h *= m;
      }

      h ^= h >> r;
      h *= m;
      h ^= h >> r;

      return h;
    }

    void feed(anton::String_View const data)
    {
      for(char8 const c: data.bytes()) {
        feed(c);
      }
    }

    void feed(char8 const data)
    {
      feed((u8)data);
    }

    void feed(u8 const data)
    {
      buffer = buffer << 8 | data;
      buffer_length += 1;

      if(buffer_length == 8) {
        flush_buffer();
      }
    }

    void feed(u32 const data)
    {
      if(buffer_length + 4 >= 8) {
        i32 const rem = 8 * ((buffer_length + 4) % 8);
        i32 const size = 32 - rem;
        buffer = buffer << size | data >> (32 - size);
        flush_buffer();
        buffer = data & (((u32)0x1 << rem) - 1);
      } else {
        buffer = buffer << 32 | data;
      }
      buffer_length = (buffer_length + 4) % 8;
    }

    void feed(u64 const data)
    {
      // buffer_length remains unchanged.
      // v + 8 mod 8 = v
      i32 const rem = 64 * buffer_length;
      i32 const size = 64 - rem;
      buffer = buffer << size | data >> (64 - size);
      flush_buffer();
      buffer = data & (((u32)0x1 << rem) - 1);
    }

  private:
    void flush_buffer()
    {
      buffer *= m;
      buffer ^= buffer >> r;
      buffer *= m;

      h ^= buffer;
      h *= m;

      buffer = 0;
      buffer_length = 0;
    }
  };

  struct Lowering_Context {
  public:
    Allocator* allocator;
    anton::Flat_Hash_Map<ir::Instr*, spirv::Instr*> instr_map;
    anton::Flat_Hash_Map<ir::Basic_Block*, spirv::Instr_label*> bb_map;
    anton::Flat_Hash_Map<u64, spirv::Instr*> type_map;

    anton::IList<spirv::Instr> capabilities;
    anton::IList<spirv::Instr> extensions;
    anton::IList<spirv::Instr> imports;
    // Memory model, entry points, execution modes.
    anton::IList<spirv::Instr> declarations;
    anton::IList<spirv::Instr> debug;
    anton::IList<spirv::Instr> types;
    anton::IList<spirv::Instr> functions;

  private:
    i64 current_id = 0;

  public:
    Lowering_Context(Allocator* allocator)
      : allocator(allocator), instr_map(allocator), bb_map(allocator),
        type_map(allocator)
    {
    }

    i64 next_id()
    {
      return ++current_id;
    }
  };

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
      auto const t = static_cast<ir::Type_Sampler const*>(type);
      hash.feed("sampler");
      hash.feed(static_cast<u8>(t->sampled_type));
      hash.feed(static_cast<u8>(t->dimensions));
      hash.feed(static_cast<u8>(t->array));
      hash.feed(static_cast<u8>(t->shadow));
    } break;

    case ir::Type_Kind::e_image: {
      auto const t = static_cast<ir::Type_Image const*>(type);
      hash.feed("image");
      hash.feed(static_cast<u8>(t->sampled_type));
      hash.feed(static_cast<u8>(t->dimensions));
      hash.feed(static_cast<u8>(t->array));
      hash.feed(static_cast<u8>(t->shadow));
    } break;

    case ir::Type_Kind::e_texture: {
      auto const t = static_cast<ir::Type_Texture const*>(type);
      hash.feed("texture");
      hash.feed(static_cast<u8>(t->sampled_type));
      hash.feed(static_cast<u8>(t->dimensions));
      hash.feed(static_cast<u8>(t->array));
      hash.feed(static_cast<u8>(t->shadow));
    } break;

    case ir::Type_Kind::e_composite: {
      auto const t = static_cast<ir::Type_Composite const*>(type);
      hash.feed("composite");
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

    case ir::Type_Kind::e_sampler: {
      // TODO: lower sampler types.
      ANTON_UNREACHABLE("unimplemented");
    }

    case ir::Type_Kind::e_image: {
      // TODO: lower sampler types.
      ANTON_UNREACHABLE("unimplemented");
    }

    case ir::Type_Kind::e_texture: {
      // TODO: lower sampler types.
      ANTON_UNREACHABLE("unimplemented");
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
      ctx.types.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return iter->value;
  }

  [[nodiscard]] static spirv::Instr*
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
      ctx.types.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return iter->value;
  }

  [[nodiscard]] static spirv::Instr_function*
  lower_function(Lowering_Context& ctx, ir::Function const* const function)
  {
    // auto const instr_function =
    //   make_instr_function(ctx.allocator, ctx.next_id(), function_type);
    return nullptr;
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

  spirv::Instr* lower_ir_module(Allocator* const allocator,
                                ir::Module const* const module)
  {
    Lowering_Context ctx(allocator);
    spirv::Instr_function* const entry = lower_function(ctx, module->entry);
    spirv::Execution_Model const model = stage_to_execution(module->stage);
    auto const entry_point = spirv::make_instr_entry_point(
      allocator, entry, anton::String(module->pass_identifier, allocator),
      model);
    return nullptr;
  }
} // namespace vush
