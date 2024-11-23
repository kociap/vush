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
  bool instanceof<Argument>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_argument;
  }

  template<>
  bool instanceof<Constant>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const;
  }

  template<>
  bool instanceof<Instr>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr;
  }

  template<>
  bool instanceof<Constant_bool>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_bool;
  }

  template<>
  bool instanceof<Constant_i32>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_i32;
  }

  template<>
  bool instanceof<Constant_u32>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_u32;
  }

  template<>
  bool instanceof<Constant_f32>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_f32;
  }

  template<>
  bool instanceof<Constant_f64>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_constant_f64;
  }

  template<>
  bool instanceof<Constant_undef>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_const &&
           static_cast<Constant const*>(value)->constant_kind ==
             Constant_Kind::e_undef;
  }

  template<>
  bool instanceof<Instr_alloc>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_alloc;
  }

  template<>
  bool instanceof<Instr_load>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_load;
  }

  template<>
  bool instanceof<Instr_store>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_store;
  }

  template<>
  bool instanceof<Instr_getptr>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_getptr;
  }

  template<>
  bool instanceof<Instr_ALU>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_alu;
  }

  template<>
  bool instanceof<Instr_vector_extract>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_vector_extract;
  }

  template<>
  bool instanceof<Instr_vector_insert>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_vector_insert;
  }

  template<>
  bool instanceof<Instr_composite_extract>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_composite_extract;
  }

  template<>
  bool instanceof<Instr_composite_construct>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_composite_construct;
  }

  template<>
  bool instanceof<Instr_cvt_sext>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_sext;
  }

  template<>
  bool instanceof<Instr_cvt_zext>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_zext;
  }

  template<>
  bool instanceof<Instr_cvt_trunc>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_trunc;
  }

  template<>
  bool instanceof<Instr_cvt_fpext>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_fpext;
  }

  template<>
  bool instanceof<Instr_cvt_fptrunc>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_fptrunc;
  }

  template<>
  bool instanceof<Instr_cvt_si2fp>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_si2fp;
  }

  template<>
  bool instanceof<Instr_cvt_ui2fp>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_ui2fp;
  }

  template<>
  bool instanceof<Instr_cvt_fp2si>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_fp2si;
  }

  template<>
  bool instanceof<Instr_cvt_fp2ui>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_cvt_fp2ui;
  }

  template<>
  bool instanceof<Instr_call>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_call;
  }

  template<>
  bool instanceof<Instr_ext_call>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind ==
             Instr_Kind::e_ext_call;
  }

  template<>
  bool instanceof<Instr_branch>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_branch;
  }

  template<>
  bool instanceof<Instr_brcond>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_brcond;
  }

  template<>
  bool instanceof<Instr_switch>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_switch;
  }

  template<>
  bool instanceof<Instr_phi>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_phi;
  }

  template<>
  bool instanceof<Instr_return>(Value const* value)
  {
    return value->value_kind == Value_Kind::e_instr &&
           static_cast<Instr const*>(value)->instr_kind == Instr_Kind::e_return;
  }

  template<>
  bool instanceof<Instr_die>(Value const* value)
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

  Constant_u32* make_constant_u32(Allocator* allocator, u32 value)
  {
    auto const instr = VUSH_ALLOCATE(Constant_u32, allocator, value, allocator);
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
    if(src2 != nullptr) {
      src2->add_referrer(instr);
    }
    return instr;
  }

  Instr_vector_extract*
  make_instr_vector_extract(Allocator* allocator, i64 id, Type* type,
                            Value* value, i64 index,
                            Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_vector_extract, allocator, id, type,
                                     value, index, allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_vector_insert* make_instr_vector_insert(Allocator* allocator, i64 id,
                                                Type* type, Value* dst,
                                                Value* value, i64 index,
                                                Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_vector_insert, allocator, id, type,
                                     dst, value, index, allocator, source_info);
    dst->add_referrer(instr);
    value->add_referrer(instr);
    return instr;
  }

  Instr_composite_extract*
  make_instr_composite_extract(Allocator* allocator, i64 id, Type* type,
                               Value* value, i64 index,
                               Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_composite_extract, allocator, id, type, value, index,
                    allocator, source_info);
    instr->indices.push_back(index);
    value->add_referrer(instr);
    return instr;
  }

  Instr_composite_extract*
  make_instr_composite_extract(Allocator* allocator, i64 id, Type* type,
                               Value* value, anton::Slice<i64 const> indices,
                               Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_composite_extract, allocator, id, type, value,
                    indices, allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_composite_construct*
  make_instr_composite_construct(Allocator* allocator, i64 id, Type* type,
                                 Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_composite_construct, allocator, id,
                                     type, allocator, source_info);
    return instr;
  }

  Instr_composite_construct*
  make_instr_composite_construct(Allocator* allocator, i64 id, Type* type,
                                 anton::Slice<Value* const> elements,
                                 Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_composite_construct, allocator, id,
                                     type, allocator, source_info);
    for(Value* const e: elements) {
      instr->add_element(e);
    }
    return instr;
  }

  Instr_cvt_sext* make_instr_cvt_sext(Allocator* allocator, i64 id,
                                      Type* target_type, Value* value,
                                      Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_cvt_sext, allocator, id, target_type,
                                     value, allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_zext* make_instr_cvt_zext(Allocator* allocator, i64 id,
                                      Type* target_type, Value* value,
                                      Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_cvt_zext, allocator, id, target_type,
                                     value, allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_trunc* make_instr_cvt_trunc(Allocator* allocator, i64 id,
                                        Type* target_type, Value* value,
                                        Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_cvt_trunc, allocator, id, target_type, value,
                    allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_fpext* make_instr_cvt_fpext(Allocator* allocator, i64 id,
                                        Type* target_type, Value* value,
                                        Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_cvt_fpext, allocator, id, target_type, value,
                    allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_fptrunc* make_instr_cvt_fptrunc(Allocator* allocator, i64 id,
                                            Type* target_type, Value* value,
                                            Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_cvt_fptrunc, allocator, id, target_type, value,
                    allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_si2fp* make_instr_cvt_si2fp(Allocator* allocator, i64 id,
                                        Type* target_type, Value* value,
                                        Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_cvt_si2fp, allocator, id, target_type, value,
                    allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_ui2fp* make_instr_cvt_ui2fp(Allocator* allocator, i64 id,
                                        Type* target_type, Value* value,
                                        Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_cvt_ui2fp, allocator, id, target_type, value,
                    allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_fp2si* make_instr_cvt_fp2si(Allocator* allocator, i64 id,
                                        Type* target_type, Value* value,
                                        Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_cvt_fp2si, allocator, id, target_type, value,
                    allocator, source_info);
    value->add_referrer(instr);
    return instr;
  }

  Instr_cvt_fp2ui* make_instr_cvt_fp2ui(Allocator* allocator, i64 id,
                                        Type* target_type, Value* value,
                                        Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_cvt_fp2ui, allocator, id, target_type, value,
                    allocator, source_info);
    value->add_referrer(instr);
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

  Instr_ext_call* make_instr_ext_call(Allocator* allocator, i64 id,
                                      Ext_Kind ext, Type* type,
                                      Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_ext_call, allocator, id, ext, type,
                                     allocator, source_info);
    return instr;
  }

  Instr_ext_call* make_instr_ext_call(Allocator* allocator, i64 id,
                                      Ext_Kind ext, Type* type,
                                      anton::Slice<Value* const> args,
                                      Source_Info const& source_info)
  {
    auto const instr = VUSH_ALLOCATE(Instr_ext_call, allocator, id, ext, type,
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
                            anton::Slice<Value* const> const srcs,
                            Source_Info const& source_info)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_phi, allocator, id, type, allocator, source_info);
    for(Value* src: srcs) {
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
