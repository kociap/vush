#include <anton/flat_hash_map.hpp>

#include <vush_core/utility.hpp>
#include <vush_ir/ir.hpp>
#include <vush_spirv/spirv.hpp>

namespace vush {
  namespace {
    // Running_Hash
    // Implements the murmurhash.
    //
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
  } // namespace

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
      Array<spirv::Instr_label*> pending_blocks;

      anton::IList<spirv::Instr> capabilities;
      anton::IList<spirv::Instr> extensions;
      anton::IList<spirv::Instr> imports;
      // Memory model, entry points, execution modes.
      anton::IList<spirv::Instr> declarations;
      anton::IList<spirv::Instr> debug;
      anton::IList<spirv::Instr> types;
      anton::IList<spirv::Instr> functions;

    private:
      i64 current_id = 1;

    public:
      Lowering_Context(Allocator* allocator)
        : allocator(allocator), instr_map(allocator), bb_map(allocator),
          type_map(allocator), pending_blocks(allocator)
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
      ctx.types.insert_back(*instr);
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
    hash_type(hash, type);
    u64 const value = hash.finish();
    auto iter = ctx.type_map.find(value);
    if(iter == ctx.type_map.end()) {
      auto const pointee_type = lower_type(ctx, type);
      auto const instr = make_instr_type_pointer(ctx.allocator, ctx.next_id(),
                                                 pointee_type, storage_class);
      ctx.types.insert_back(*instr);
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
      ctx.types.insert_back(*instr);
      iter = ctx.type_map.emplace(value, instr);
    }
    return iter->value;
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
      case ir::Instr_Kind::e_alloc: {
        auto const instr_alloc =
          static_cast<ir::Instr_alloc const*>(&instruction);
        auto const result_type = lower_type_as_pointer(
          ctx, instr_alloc->alloc_type, spirv::Storage_Class::e_function);
        auto const instr =
          spirv::make_instr_variable(ctx.allocator, ctx.next_id(), result_type,
                                     spirv::Storage_Class::e_function);
        builder.insert(instr);
        instr->label = label;
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
        instr->label = label;
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
        instr->label = label;
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
        instr->label = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_alu: {
        auto const instr_alu = static_cast<ir::Instr_ALU const*>(&instruction);
        auto const operand1 = ctx.get_instr(instr_alu->src1);
        auto const operand2 = ctx.get_instr(instr_alu->src2);
        auto const result_type = lower_type(ctx, instr_alu->type);
        spirv::Instr* instr;

#define ALU_CASE(OPCODE, INSTR)                                         \
  case ir::ALU_Opcode::OPCODE:                                          \
    instr = spirv::make_instr_##INSTR(ctx.allocator, ctx.next_id(),     \
                                      result_type, operand1, operand2); \
    break;

#define ALU_CASE_UNREACHABLE(OPCODE)                                    \
  case ir::ALU_Opcode::OPCODE:                                          \
    ANTON_UNREACHABLE("ALU instruction " #OPCODE " cannot be lowered"); \
    break;

        switch(instr_alu->op) {
          ALU_CASE(e_inv, bit_not)
          ALU_CASE(e_and, bit_and)
          ALU_CASE(e_or, bit_or)
          ALU_CASE(e_xor, bit_xor)
          ALU_CASE(e_shl, shl)
          // All SHR operations are logical for now.
          ALU_CASE(e_shr, shr_logical)
          ALU_CASE(e_neg, snegate)
          ALU_CASE(e_iadd, iadd)
          ALU_CASE(e_imul, imul)
          ALU_CASE(e_uadd, iadd)
          ALU_CASE(e_umul, imul)
          ALU_CASE(e_idiv, sdiv)
          ALU_CASE(e_udiv, udiv)
          ALU_CASE(e_irem, srem)
          ALU_CASE(e_urem, umod)
          ALU_CASE(e_fneg, fnegate)
          ALU_CASE(e_fadd, fadd)
          ALU_CASE(e_fmul, fmul)
          ALU_CASE(e_fdiv, fdiv)
          ALU_CASE(e_icmp_eq, ieq)
          ALU_CASE(e_icmp_neq, ineq)
          ALU_CASE(e_icmp_ugt, ugt)
          ALU_CASE(e_icmp_ult, ult)
          ALU_CASE(e_icmp_uge, uge)
          ALU_CASE(e_icmp_ule, ule)
          ALU_CASE(e_icmp_sgt, sgt)
          ALU_CASE(e_icmp_slt, slt)
          ALU_CASE(e_icmp_sge, sge)
          ALU_CASE(e_icmp_sle, sle)
          ALU_CASE(e_fcmp_eq, foeq)
          ALU_CASE(e_fcmp_neq, foneq)
          ALU_CASE(e_fcmp_gt, fogt)
          ALU_CASE(e_fcmp_lt, folt)
          ALU_CASE(e_fcmp_ge, foge)
          ALU_CASE(e_fcmp_le, fole)
          // No FMA instruction in SPIR-V. There's FMA in the extended
          // instructions for GLSL.
          ALU_CASE_UNREACHABLE(e_fma)
        }
        builder.insert(instr);
        instr->label = label;
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
        instr->label = label;
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
        instr->label = label;
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
        instr->label = label;
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
        instr->label = label;
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
          instr->label = label;
          ctx.instr_map.emplace(&instruction, instr);
        } else {
          auto const instr = spirv::make_instr_return(ctx.allocator);
          builder.insert(instr);
          instr->label = label;
          ctx.instr_map.emplace(&instruction, instr);
        }
      } break;

      case ir::Instr_Kind::e_die: {
        auto const instr = spirv::make_instr_terminate(ctx.allocator);
        builder.insert(instr);
        instr->label = label;
        ctx.instr_map.emplace(&instruction, instr);
      } break;

      case ir::Instr_Kind::e_branch: {
        auto const instr_branch =
          static_cast<ir::Instr_branch const*>(&instruction);
        auto const label = lower_block(ctx, instr_branch->target);
        auto const instr = spirv::make_instr_branch(ctx.allocator, label);
        builder.insert(instr);
        instr->label = label;
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
        instr->label = label;
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
        instr->label = label;
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

    return {instr_function, instr_end};
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
    spirv::Instr_function* entry = nullptr;
    {
      anton::IList<spirv::Instr> list = lower_function(ctx, module->entry);
      entry = static_cast<spirv::Instr_function*>(list.begin().node);
      ctx.functions.splice(list);
    }
    // The required memory model. We always use PhysicalStorageBuffer64 and
    // Vulkan.
    spirv::Instr_memory_model* const memory_model =
      spirv::make_instr_memory_model(
        allocator, spirv::Addressing_Model::e_physical_storage_buffer64,
        spirv::Memory_Model::e_vulkan);
    ctx.declarations.insert_back(*memory_model);
    // The single entry point.
    spirv::Execution_Model const execution_model =
      stage_to_execution(module->stage);
    auto const entry_point = spirv::make_instr_entry_point(
      allocator, entry, anton::String(module->pass_identifier, allocator),
      execution_model);
    ctx.declarations.insert_back(*entry_point);
    spirv::Module spirv_module{
      .capabilities = ANTON_MOV(ctx.capabilities),
      .extensions = ANTON_MOV(ctx.extensions),
      .imports = ANTON_MOV(ctx.imports),
      .declarations = ANTON_MOV(ctx.declarations),
      .debug = ANTON_MOV(ctx.debug),
      .types = ANTON_MOV(ctx.types),
      .functions = ANTON_MOV(ctx.functions),
    };
    return spirv_module;
  }
} // namespace vush
