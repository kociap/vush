#pragma once

#include <anton/expected.hpp>
#include <anton/ilist.hpp>
#include <anton/optional.hpp>

#include <vush.hpp>
#include <vush_ast/ast_fwd.hpp>
#include <vush_core/source_info.hpp>
#include <vush_core/types.hpp>

#include <vush_ir/ext.hpp>
#include <vush_ir/ir_fwd.hpp>
#include <vush_ir/opcodes.hpp>
#include <vush_ir/types.hpp>

// Types:
// Aggregate - a struct or an array type.
// Composite - an aggregate, a matrix or a vector.

namespace vush::ir {
  struct Basic_Block {
    anton::IList<Instr, Basic_Block> instructions;
    i64 id;

    Basic_Block(i64 id): id(id) {}

    void insert(Instr* node);

    [[nodiscard]] bool empty() const;
    [[nodiscard]] Instr* get_first();
    [[nodiscard]] Instr const* get_first() const;
    [[nodiscard]] Instr* get_last();
    [[nodiscard]] Instr const* get_last() const;
  };

  // split_block
  //
  // Split the block into two blocks at the specified instruction. An
  // unconditional branch is added to the original block and the remaining
  // instructions are moved to the new block.
  //
  [[nodiscard]] Basic_Block* split_block(i64 label, Instr* instruction,
                                         Instr* end, bool before = false);

  struct Module: Decorable {
    anton::String pass_identifier;
    Stage_Kind stage;

    Function* entry;

    Module(anton::String&& pass_identifier, Stage_Kind stage, Function* entry)
      : pass_identifier(ANTON_MOV(pass_identifier)), stage(stage), entry(entry)
    {
    }
  };

  struct Function {
    anton::IList<Argument, Argument> arguments;
    Type* return_type;
    Basic_Block* entry_block;
    // IR identifier of the function.
    i64 id;

    // Source code string identifier of the function.
    anton::String identifier;
    Source_Info source_info;

    Function(i64 id, Type* return_type, Basic_Block* entry_block,
             anton::String&& identifier, Source_Info const& source_info)
      : return_type(return_type), entry_block(entry_block), id(id),
        identifier(ANTON_MOV(identifier)), source_info(source_info)
    {
    }
  };

  struct Buffer {
    Type* type;
    u32 descriptor_set = -1;
    u32 binding = -1;

    // Source code string identifier of the buffer.
    anton::String identifier;
    Source_Info source_info;

    Buffer(Type* type, anton::String&& identifier,
           Source_Info const& source_info)
      : type(type), identifier(ANTON_MOV(identifier)), source_info(source_info)
    {
    }
  };

  enum struct Value_Kind {
    e_argument,
    e_const,
    e_instr,
  };

  struct Value {
    anton::Array<Value*> referrers;
    Type* type;
    Value_Kind value_kind;

    Value(Value_Kind value_kind, Type* type, Allocator* allocator)
      : referrers(allocator), type(type), value_kind(value_kind)
    {
    }

    void add_referrer(Value* instruction)
    {
      referrers.push_back(instruction);
    }

    void remove_referrer(Value* instruction)
    {
      auto begin = referrers.begin();
      auto const end = referrers.end();
      for(; begin != end; ++begin) {
        if(*begin == instruction) {
          referrers.erase_unsorted(begin);
        }
      }
    }
  };

  template<typename T>
  [[nodiscard]] bool instanceof(Value const* value);

  void replace_uses_with(Value* value, Value* replacement);

  enum struct Storage_Class {
    e_automatic,
    e_input,
    e_output,
    e_uniform,
    e_push_constant,
    e_buffer,
  };

  // Argument
  // Represents the formal parameter of a function and, when used within the
  // body, the value passed to the function.
  // When used in a module's parameter list, represents the pointer to the
  // location of a member of the buffer.
  //
  // Parameters:
  // type - when sourced, always a pointer type.
  // pointee_type - when sourced, the pointed to type. Otherwise, nullptr.
  //
  struct Argument: public Value, anton::IList_Node<Argument>, Decorable {
    i64 id = -1;
    Function* function;
    Buffer* buffer = nullptr;
    i64 buffer_index = -1;
    Type* pointee_type = nullptr;
    Storage_Class storage_class = Storage_Class::e_automatic;

    Argument(i64 id, Type* type, Function* function, Allocator* allocator)
      : Value(Value_Kind::e_argument, type, allocator), id(id),
        function(function)
    {
    }
  };

  enum struct Constant_Kind {
    e_constant_bool,
    // constant_i8,
    // constant_i16,
    e_constant_i32,
    e_constant_u32,
    e_constant_f32,
    e_constant_f64,
    e_undef,
  };

  struct Constant: public Value {
    Constant_Kind constant_kind;

    Constant(Constant_Kind constant_kind, Type* type, Allocator* allocator)
      : Value(Value_Kind::e_const, type, allocator),
        constant_kind(constant_kind)
    {
    }
  };

  struct Constant_bool: public Constant {
    bool value;

    Constant_bool(bool value, Allocator* allocator)
      : Constant(Constant_Kind::e_constant_bool, get_type_bool(), allocator),
        value(value)
    {
    }
  };

  [[nodiscard]] Constant_bool* make_constant_bool(Allocator* allocator,
                                                  bool value);

  struct Constant_i32: public Constant {
    i32 value;

    Constant_i32(i32 value, Allocator* allocator)
      : Constant(Constant_Kind::e_constant_i32, get_type_int32(), allocator),
        value(value)
    {
    }
  };

  [[nodiscard]] Constant_i32* make_constant_i32(Allocator* allocator,
                                                i32 value);

  struct Constant_u32: public Constant {
    u32 value;

    Constant_u32(u32 value, Allocator* allocator)
      : Constant(Constant_Kind::e_constant_u32, get_type_uint32(), allocator),
        value(value)
    {
    }
  };

  [[nodiscard]] Constant_u32* make_constant_u32(Allocator* allocator,
                                                u32 value);

  struct Constant_f32: public Constant {
    f32 value;

    Constant_f32(f32 value, Allocator* allocator)
      : Constant(Constant_Kind::e_constant_f32, get_type_fp32(), allocator),
        value(value)
    {
    }
  };

  [[nodiscard]] Constant_f32* make_constant_f32(Allocator* allocator,
                                                f32 value);

  struct Constant_f64: public Constant {
    f64 value;

    Constant_f64(f64 value, Allocator* allocator)
      : Constant(Constant_Kind::e_constant_f64, get_type_fp64(), allocator),
        value(value)
    {
    }
  };

  [[nodiscard]] Constant_f64* make_constant_f64(Allocator* allocator,
                                                f64 value);

  struct Constant_undef: public Constant {
    Constant_undef(Type* type, Allocator* allocator)
      : Constant(Constant_Kind::e_undef, type, allocator)
    {
    }
  };

  [[nodiscard]] Constant_undef* make_constant_undef(Allocator* allocator,
                                                    Type* type);

  enum struct Instr_Kind : u8 {
    e_alloc,
    e_load,
    e_store,
    e_getptr,
    e_alu,
    e_vector_extract,
    e_vector_insert,
    e_composite_extract,
    e_composite_construct,
    e_cvt_sext,
    e_cvt_zext,
    e_cvt_trunc,
    e_cvt_fpext,
    e_cvt_fptrunc,
    e_cvt_si2fp,
    e_cvt_fp2si,
    e_cvt_ui2fp,
    e_cvt_fp2ui,
    e_call,
    e_ext_call,
    e_branch,
    e_brcond,
    e_switch,
    e_phi,
    e_return,
    e_die,
  };

  struct Instr: public anton::IList_Node<Basic_Block>, public Value {
    Basic_Block* block = nullptr;
    i64 id = -1;
    Instr_Kind instr_kind;
    Source_Info source_info;

    Instr(i64 id, Instr_Kind instr_kind, Type* type, Allocator* allocator,
          Source_Info const& source_info)
      : Value(Value_Kind::e_instr, type, allocator), id(id),
        instr_kind(instr_kind), source_info(source_info)
    {
    }
  };

  [[nodiscard]] bool is_control_flow_instruction(Instr const* instruction);

  // Instr_alloc
  // Produces an address to the allocated stack memory.
  //
  struct Instr_alloc: public Instr {
    Type* alloc_type;

    Instr_alloc(i64 id, Type* alloc_type, Allocator* allocator,
                Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_alloc, get_type_ptr(), allocator, source_info),
        alloc_type(alloc_type)
    {
    }
  };

  [[nodiscard]] Instr_alloc* make_instr_alloc(Allocator* allocator, i64 id,
                                              Type* alloc_type,
                                              Source_Info const& source_info);

  struct Instr_load: public Instr {
    Value* address;

    Instr_load(i64 id, Type* type, Value* address, Allocator* allocator,
               Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_load, type, allocator, source_info),
        address(address)
    {
    }
  };

  [[nodiscard]] Instr_load* make_instr_load(Allocator* allocator, i64 id,
                                            Type* type, Value* address,
                                            Source_Info const& source_info);

  // Instr_store
  // Store value from src in memory pointed to by dst.
  //
  struct Instr_store: public Instr {
    // Address to store to.
    Value* dst;
    // Value to be stored.
    Value* src;

    Instr_store(i64 id, Value* dst, Value* src, Allocator* allocator,
                Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_store, get_type_void(), allocator, source_info),
        dst(dst), src(src)
    {
    }
  };

  [[nodiscard]] Instr_store* make_instr_store(Allocator* allocator, i64 id,
                                              Value* dst, Value* src,
                                              Source_Info const& source_info);

  // getptr
  // The instruction is used to calculate the address of a field of an aggregate
  // data structure.
  //
  struct Instr_getptr: public Instr {
    Type* addressed_type;
    Value* address;
    // TODO: Verify that the index is constant integer and fits in i64 when
    // type is composite. AND is positive.
    Value* index;

    Instr_getptr(i64 id, Type* addressed_type, Value* address, Value* index,
                 Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_getptr, get_type_ptr(), allocator, source_info),
        addressed_type(addressed_type), address(address), index(index)
    {
    }
  };

  [[nodiscard]] Instr_getptr* make_instr_getptr(Allocator* allocator, i64 id,
                                                Type* addressed_type,
                                                Value* address, Value* index,
                                                Source_Info const& source_info);

  struct Instr_ALU: public Instr {
    Value* src1;
    Value* src2;
    ALU_Opcode op;

    Instr_ALU(i64 id, Type* type, ALU_Opcode op, Value* src1, Value* src2,
              Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_alu, type, allocator, source_info), src1(src1),
        src2(src2), op(op)
    {
    }
  };

  [[nodiscard]] Instr_ALU* make_instr_alu(Allocator* allocator, i64 id,
                                          Type* type, ALU_Opcode op,
                                          Value* src1, Value* src2,
                                          Source_Info const& source_info);

  struct Instr_vector_extract: public Instr {
    Value* value;
    i64 index;

    Instr_vector_extract(i64 id, Type* type, Value* value, i64 index,
                         Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_vector_extract, type, allocator, source_info),
        value(value), index(index)
    {
    }
  };

  [[nodiscard]] Instr_vector_extract*
  make_instr_vector_extract(Allocator* allocator, i64 id, Type* type,
                            Value* value, i64 index,
                            Source_Info const& source_info);

  struct Instr_vector_insert: public Instr {
    Value* dst;
    Value* value;
    i64 index;

    Instr_vector_insert(i64 id, Type* type, Value* dst, Value* value, i64 index,
                        Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_vector_insert, type, allocator, source_info),
        dst(dst), value(value), index(index)
    {
    }
  };

  [[nodiscard]] Instr_vector_insert*
  make_instr_vector_insert(Allocator* allocator, i64 id, Type* type, Value* dst,
                           Value* value, i64 index,
                           Source_Info const& source_info);

  struct Instr_composite_extract: public Instr {
    Value* value;
    Array<i64> indices;

    Instr_composite_extract(i64 id, Type* type, Value* value, i64 index,
                            Allocator* allocator,
                            Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_composite_extract, type, allocator,
              source_info),
        value(value)
    {
      indices.push_back(index);
    }

    Instr_composite_extract(i64 id, Type* type, Value* value,
                            anton::Slice<i64 const> indices,
                            Allocator* allocator,
                            Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_composite_extract, type, allocator,
              source_info),
        value(value)
    {
      this->indices.assign(indices.begin(), indices.end());
    }
  };

  [[nodiscard]] Instr_composite_extract*
  make_instr_composite_extract(Allocator* allocator, i64 id, Type* type,
                               Value* value, i64 index,
                               Source_Info const& source_info);

  [[nodiscard]] Instr_composite_extract*
  make_instr_composite_extract(Allocator* allocator, i64 id, Type* type,
                               Value* value, anton::Slice<i64 const> indices,
                               Source_Info const& source_info);

  struct Instr_composite_construct: public Instr {
    Array<Value*> elements;

    Instr_composite_construct(i64 id, Type* type, Allocator* allocator,
                              Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_composite_construct, type, allocator,
              source_info),
        elements(allocator)
    {
    }

    void add_element(Value* value)
    {
      elements.push_back(value);
      value->add_referrer(this);
    }
  };

  [[nodiscard]] Instr_composite_construct*
  make_instr_composite_construct(Allocator* allocator, i64 id, Type* type,
                                 Source_Info const& source_info);

  [[nodiscard]] Instr_composite_construct*
  make_instr_composite_construct(Allocator* allocator, i64 id, Type* type,
                                 anton::Slice<Value* const> elements,
                                 Source_Info const& source_info);

  struct Instr_cvt_sext: public Instr {
    Value* value;

    Instr_cvt_sext(i64 id, Type* target_type, Value* value,
                   Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_sext, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_sext*
  make_instr_cvt_sext(Allocator* allocator, i64 id, Type* target_type,
                      Value* value, Source_Info const& source_info);

  struct Instr_cvt_zext: public Instr {
    Value* value;

    Instr_cvt_zext(i64 id, Type* target_type, Value* value,
                   Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_zext, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_zext*
  make_instr_cvt_zext(Allocator* allocator, i64 id, Type* target_type,
                      Value* value, Source_Info const& source_info);

  struct Instr_cvt_trunc: public Instr {
    Value* value;

    Instr_cvt_trunc(i64 id, Type* target_type, Value* value,
                    Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_trunc, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_trunc*
  make_instr_cvt_trunc(Allocator* allocator, i64 id, Type* target_type,
                       Value* value, Source_Info const& source_info);

  struct Instr_cvt_fpext: public Instr {
    Value* value;

    Instr_cvt_fpext(i64 id, Type* target_type, Value* value,
                    Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_fpext, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_fpext*
  make_instr_cvt_fpext(Allocator* allocator, i64 id, Type* target_type,
                       Value* value, Source_Info const& source_info);

  struct Instr_cvt_fptrunc: public Instr {
    Value* value;

    Instr_cvt_fptrunc(i64 id, Type* target_type, Value* value,
                      Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_fptrunc, target_type, allocator,
              source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_fptrunc*
  make_instr_cvt_fptrunc(Allocator* allocator, i64 id, Type* target_type,
                         Value* value, Source_Info const& source_info);

  struct Instr_cvt_si2fp: public Instr {
    Value* value;

    Instr_cvt_si2fp(i64 id, Type* target_type, Value* value,
                    Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_si2fp, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_si2fp*
  make_instr_cvt_si2fp(Allocator* allocator, i64 id, Type* target_type,
                       Value* value, Source_Info const& source_info);

  struct Instr_cvt_ui2fp: public Instr {
    Value* value;

    Instr_cvt_ui2fp(i64 id, Type* target_type, Value* value,
                    Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_ui2fp, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_ui2fp*
  make_instr_cvt_ui2fp(Allocator* allocator, i64 id, Type* target_type,
                       Value* value, Source_Info const& source_info);

  struct Instr_cvt_fp2si: public Instr {
    Value* value;

    Instr_cvt_fp2si(i64 id, Type* target_type, Value* value,
                    Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_fp2si, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_fp2si*
  make_instr_cvt_fp2si(Allocator* allocator, i64 id, Type* target_type,
                       Value* value, Source_Info const& source_info);

  struct Instr_cvt_fp2ui: public Instr {
    Value* value;

    Instr_cvt_fp2ui(i64 id, Type* target_type, Value* value,
                    Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_cvt_fp2ui, target_type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_cvt_fp2ui*
  make_instr_cvt_fp2ui(Allocator* allocator, i64 id, Type* target_type,
                       Value* value, Source_Info const& source_info);

  struct Instr_call: public Instr {
    Array<Value*> args;
    Function* function;

    Instr_call(i64 id, Function* function, Type* type, Allocator* allocator,
               Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_call, type, allocator, source_info),
        args(allocator), function(function)
    {
    }

    void add_argument(Value* const value)
    {
      args.push_back(value);
      value->add_referrer(this);
    }
  };

  [[nodiscard]] Instr_call* make_instr_call(Allocator* allocator, i64 id,
                                            Function* function, Type* type,
                                            Source_Info const& source_info);

  [[nodiscard]] Instr_call* make_instr_call(Allocator* allocator, i64 id,
                                            Function* function, Type* type,
                                            anton::Slice<Value* const> args,
                                            Source_Info const& source_info);

  struct Instr_ext_call: public Instr {
    Array<Value*> args;
    Ext_Kind ext;

    Instr_ext_call(i64 id, Ext_Kind ext, Type* type, Allocator* allocator,
                   Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_ext_call, type, allocator, source_info),
        args(allocator), ext(ext)
    {
    }

    void add_argument(Value* const value)
    {
      args.push_back(value);
      value->add_referrer(this);
    }
  };

  [[nodiscard]] Instr_ext_call*
  make_instr_ext_call(Allocator* allocator, i64 id, Ext_Kind ext, Type* type,
                      Source_Info const& source_info);

  [[nodiscard]] Instr_ext_call*
  make_instr_ext_call(Allocator* allocator, i64 id, Ext_Kind ext, Type* type,
                      anton::Slice<Value* const> args,
                      Source_Info const& source_info);

  struct Instr_branch: public Instr {
    Basic_Block* target;

    Instr_branch(i64 id, Basic_Block* target, Allocator* allocator,
                 Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_branch, get_type_void(), allocator,
              source_info),
        target(target)
    {
    }
  };

  [[nodiscard]] Instr_branch* make_instr_branch(Allocator* allocator, i64 id,
                                                Basic_Block* target,
                                                Source_Info const& source_info);

  struct Instr_brcond: public Instr {
    Value* condition;
    Basic_Block* then_target;
    Basic_Block* else_target;

    Instr_brcond(i64 id, Value* condition, Basic_Block* then_target,
                 Basic_Block* else_target, Allocator* allocator,
                 Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_brcond, get_type_void(), allocator,
              source_info),
        condition(condition), then_target(then_target), else_target(else_target)
    {
    }
  };

  [[nodiscard]] Instr_brcond* make_instr_brcond(Allocator* allocator, i64 id,
                                                Value* condition,
                                                Basic_Block* then_target,
                                                Basic_Block* else_target,
                                                Source_Info const& source_info);

  struct Switch_Label {
    i64 value;
    Basic_Block* target;
  };

  struct Instr_switch: public Instr {
    Value* selector;
    Basic_Block* default_label;
    Array<Switch_Label> labels;

    Instr_switch(i64 id, Value* selector, Basic_Block* default_label,
                 Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_switch, get_type_void(), allocator,
              source_info),
        selector(selector), default_label(default_label), labels(allocator)
    {
    }

    void add_label(Switch_Label label)
    {
      labels.push_back(label);
    }
  };

  [[nodiscard]] Instr_switch* make_instr_switch(Allocator* allocator, i64 id,
                                                Value* selector,
                                                Basic_Block* default_label,
                                                Source_Info const& source_info);

  [[nodiscard]] Instr_switch* make_instr_switch(
    Allocator* allocator, i64 id, Value* selector, Basic_Block* default_label,
    anton::Slice<Switch_Label const> labels, Source_Info const& source_info);

  struct Instr_phi: public Instr {
    Array<Value*> srcs;

    Instr_phi(i64 id, Type* type, Allocator* allocator,
              Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_phi, type, allocator, source_info),
        srcs(allocator)
    {
    }

    void add_source(Value* const value)
    {
      srcs.push_back(value);
      value->add_referrer(this);
    }
  };

  [[nodiscard]] Instr_phi* make_instr_phi(Allocator* allocator, i64 id,
                                          Type* type,
                                          Source_Info const& source_info);

  [[nodiscard]] Instr_phi* make_instr_phi(Allocator* allocator, i64 id,
                                          Type* type,
                                          anton::Slice<Value* const> srcs,
                                          Source_Info const& source_info);

  struct Instr_return: public Instr {
    Value* value = nullptr;

    Instr_return(i64 id, Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_return, get_type_void(), allocator, source_info)
    {
    }

    Instr_return(i64 id, Value* value, Allocator* allocator,
                 Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_return, value->type, allocator, source_info),
        value(value)
    {
    }
  };

  [[nodiscard]] Instr_return* make_instr_return(Allocator* allocator, i64 id,
                                                Source_Info const& source_info);
  [[nodiscard]] Instr_return* make_instr_return(Allocator* allocator, i64 id,
                                                Value* value,
                                                Source_Info const& source_info);

  // Instr_die
  // Terminate (die) current invocation.
  //
  struct Instr_die: public Instr {
    Instr_die(i64 id, Allocator* allocator, Source_Info const& source_info)
      : Instr(id, Instr_Kind::e_die, get_type_void(), allocator, source_info)
    {
    }
  };

  [[nodiscard]] Instr_die* make_instr_die(Allocator* allocator, i64 id,
                                          Source_Info const& source_info);
} // namespace vush::ir
