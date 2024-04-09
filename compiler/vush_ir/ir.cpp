#include <vush_ir/ir.hpp>

#include <vush_core/memory.hpp>
#include <vush_ir/opcodes.hpp>

namespace vush::ir {
  void Basic_Block::insert(Instr* instruction)
  {
    instruction->block = this;
    instructions.insert_back(*instruction);
  }

  template<>
  bool instanceof <Constant>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const;
  }

  template<>
  bool instanceof <Instr>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const;
  }

  template<>
  bool instanceof <Constant_bool>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_bool;
  }

  template<>
  bool instanceof <Constant_i32>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_i32;
  }

  template<>
  bool instanceof <Constant_f32>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_f32;
  }

  template<>
  bool instanceof <Constant_f64>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_f64;
  }

  template<>
  bool instanceof <Constant_undef>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_undef;
  }

  template<>
  bool instanceof <Instr_alloc>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_alloc;
  }

  template<>
  bool instanceof <Instr_load>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_load;
  }

  template<>
  bool instanceof <Instr_store>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_store;
  }

  template<>
  bool instanceof <Instr_getptr>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_getptr;
  }

  template<>
  bool instanceof <Instr_ALU>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_alu;
  }

  template<>
  bool instanceof <Instr_setvalue>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_setvalue;
  }

  template<>
  bool instanceof <Instr_call>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_call;
  }

  template<>
  bool instanceof <Instr_branch>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_branch;
  }

  template<>
  bool instanceof <Instr_brcond>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_brcond;
  }

  template<>
  bool instanceof <Instr_switch>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_switch;
  }

  template<>
  bool instanceof <Instr_phi>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_phi;
  }

  template<>
  bool instanceof <Instr_return>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_return;
  }

  template<>
  bool instanceof <Instr_die>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_die;
  }

  Constant_bool* make_constant_bool(Allocator* const allocator,
                                    bool const value)
  {
    auto const instr =
      VUSH_ALLOCATE(Constant_bool, allocator, value, allocator);
    return instr;
  }

  Constant_i32* make_constant_i32(Allocator* const allocator, i32 const value)
  {
    auto const instr = VUSH_ALLOCATE(Constant_i32, allocator, value, allocator);
    return instr;
  }

  Constant_f32* make_constant_f32(Allocator* const allocator, f32 const value)
  {
    auto const instr = VUSH_ALLOCATE(Constant_f32, allocator, value, allocator);
    return instr;
  }

  Constant_f64* make_constant_f64(Allocator* const allocator, f64 const value)
  {
    auto const instr = VUSH_ALLOCATE(Constant_f64, allocator, value, allocator);
    return instr;
  }

  Constant_undef* make_constant_undef(Allocator* allocator, Type* const type)
  {
    auto const value =
      VUSH_ALLOCATE(Constant_undef, allocator, type, allocator);
    return value;
  }

  Instr_alloc* make_instr_alloc(Allocator* const allocator, i64 const id,
                                Type* const alloc_type,
                                Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_alloc, allocator, id, alloc_type,
                                     allocator, source_info);
    return instr;
  }

  Instr_load* make_instr_load(Allocator* const allocator, i64 const id,
                              Type* const type, Instr* const address,
                              Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_load, allocator, id, type, address,
                                     allocator, source_info);
    address->add_referrer(instr);
    return instr;
  }

  Instr_store* make_instr_store(Allocator* const allocator, i64 const id,
                                Instr* const dst, Value* const src,
                                Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_store, allocator, id, dst, src,
                                     allocator, source_info);
    dst->add_referrer(instr);
    src->add_referrer(instr);
    return instr;
  }

  // Instr_memcpy* make_instr_memcpy(Allocator* const allocator, i64 const id,
  //                                 Instr* const dst, Instr* const src,
  //                                 i64 const count,
  //                                 Source_Info const& source_info)
  // {
  //   auto const instr = VUSH_ALLOCATE(Instr_memcpy, allocator, id, dst, src,
  //                                    count, allocator, source_info);
  //   dst->add_referrer(instr);
  //   src->add_referrer(instr);
  //   return instr;
  // }

  // Instr_memset* make_instr_memset(Allocator* const allocator, i64 const id,
  //                                 Instr* const dst, i64 const count,
  //                                 u8 const value,
  //                                 Source_Info const& source_info)
  // {
  //   auto const instr = VUSH_ALLOCATE(Instr_memset, allocator, id, dst, count,
  //                                    value, allocator, source_info);
  //   dst->add_referrer(instr);
  //   return instr;
  // }

  Instr_getptr* make_instr_getptr(Allocator* const allocator, i64 const id,
                                  Type* addressed_type, Instr* const address,
                                  Value* const index,
                                  Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_getptr, allocator, id, addressed_type, address, index,
                    allocator, source_info);
    address->add_referrer(instr);
    return instr;
  }

  Instr_ALU* make_instr_alu(Allocator* const allocator, i64 const id,
                            Type* const type, ALU_Opcode const op,
                            Value* const src1, Value* const src2,
                            Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_ALU, allocator, id, type, op, src1,
                                     src2, allocator, source_info);
    src1->add_referrer(instr);
    src2->add_referrer(instr);
    return instr;
  }

  Instr_call* make_instr_call(Allocator* allocator, i64 id, Function* function,
                              Type* type, Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_call, allocator, id, function, type,
                                     allocator, source_info);
    return instr;
  }

  Instr_call* make_instr_call(Allocator* allocator, i64 id, Function* function,
                              Type* type, anton::Slice<Value* const> args,
                              Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_call, allocator, id, function, type,
                                     allocator, source_info);
    for(Value* const value: args) {
      instr->add_argument(value);
    }
    return instr;
  }

  Instr_branch* make_instr_branch(Allocator* const allocator, i64 const id,
                                  Basic_Block* const target,
                                  Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(ir::Instr_branch, allocator, id, target,
                                     allocator, source_info);
    return instr;
  }

  Instr_brcond* make_instr_brcond(Allocator* const allocator, i64 const id,
                                  Value* const condition,
                                  Basic_Block* const then_target,
                                  Basic_Block* const else_target,
                                  Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(ir::Instr_brcond, allocator, id, condition, then_target,
                    else_target, allocator, source_info);
    condition->add_referrer(instr);
    return instr;
  }

  ir::Instr_switch* make_instr_switch(Allocator* allocator, i64 id,
                                      Value* selector,
                                      Basic_Block* default_label,
                                      Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_switch, allocator, id, selector,
                                     default_label, allocator, source_info);
    selector->add_referrer(instr);
    return instr;
  }

  ir::Instr_switch* make_instr_switch(Allocator* allocator, i64 id,
                                      Value* selector,
                                      Basic_Block* default_label,
                                      anton::Slice<Switch_Label const> labels,
                                      Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_switch, allocator, id, selector,
                                     default_label, allocator, source_info);
    selector->add_referrer(instr);
    instr->labels.assign(labels.begin(), labels.end());
    return instr;
  }

  Instr_phi* make_instr_phi(Allocator* const allocator, i64 const id,
                            Type* const type, Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_phi, allocator, id, type, allocator, source_info);
    return instr;
  }

  Instr_phi* make_instr_phi(Allocator* const allocator, i64 const id,
                            Type* const type,
                            anton::Slice<Instr* const> const srcs,
                            Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_phi, allocator, id, type, allocator, source_info);
    for(Instr* src: srcs) {
      instr->add_source(src);
    }
    return instr;
  }

  ir::Instr_return* make_instr_return(Allocator* allocator, i64 id,
                                      Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_return, allocator, id, allocator, source_info);
    return instr;
  }

  ir::Instr_return* make_instr_return(Allocator* allocator, i64 id,
                                      ir::Value* value,
                                      Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_return, allocator, id, value, allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  ir::Instr_die* make_instr_die(Allocator* allocator, i64 id,
                                Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_die, allocator, id, allocator, source_info);
    return instr;
  }
} // namespace vush::ir
