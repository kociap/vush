#include <vush_ast_lowering/ast_lowering.hpp>

#include <anton/iterators/zip.hpp>
#include <anton/math/math.hpp>
#include <anton/string7_view.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/memory.hpp>
#include <vush_core/scoped_map.hpp>
#include <vush_ir/ir.hpp>

namespace vush {
  using namespace anton::literals;

  using Fn_Table = Scoped_Map<anton::String_View, ir::Function*>;
  using Symbol_Table = Scoped_Map<anton::String_View, ir::Instr*>;

  struct Lowering_Context {
  public:
    Allocator* allocator;
    Fn_Table fntable;
    Symbol_Table symtable;

    ir::Basic_Block* nearest_converge_block = nullptr;
    ir::Basic_Block* nearest_continuation_block = nullptr;

  private:
    i64 id = 0;

  public:
    Lowering_Context(Allocator* allocator)
      : allocator(allocator), fntable(allocator), symtable(allocator)
    {
    }

    [[nodiscard]] i64 get_next_id()
    {
      i64 const value = id;
      id += 1;
      return value;
    }
  };

  struct Builder {
  private:
    ir::Basic_Block* insert_block = nullptr;

  public:
    void set_insert_block(ir::Basic_Block* const bb)
    {
      insert_block = bb;
    }

    void insert(ir::Instr* const node)
    {
      ANTON_ASSERT(insert_block != nullptr, "insert_block has not been set");
      insert_block->insert(node);
    }
  };

  [[nodiscard]] static ir::Type* convert_ast_to_ir_type(ast::Type const* const)
  {
    return nullptr;
  }

  [[nodiscard]] static ir::Instr* generate_conversion(Lowering_Context& ctx,
                                                      Builder& builder,
                                                      ir::Value* value,
                                                      ast::Type* target_type)
  {
    return nullptr;
  }

  [[nodiscard]] static i32 get_field_index(ast::Decl_Struct* const decl,
                                           anton::String_View const identifier)
  {
    i32 index = 0;
    for(ast::Struct_Field* const field: decl->fields) {
      if(field->identifier.value == identifier) {
        return index;
      }
      index += 1;
    }

    return -1;
  }

  [[nodiscard]] static ir::Instr*
  get_address(Lowering_Context& ctx, Builder& builder, ast::Expr const* expr);

  [[nodiscard]] static ir::Value*
  lower_expression(Lowering_Context& ctx, Builder& builder,
                   ast::Expr const* generic_expr);

  [[nodiscard]] static ir::Function* lower_function(Lowering_Context& ctx,
                                                    ast::Decl_Function* fn);

  [[nodiscard]] static ir::Instr*
  address_expr_identifier(Lowering_Context& ctx, Builder& builder,
                          ast::Expr_Identifier const* const expr)
  {
    ANTON_UNUSED(builder);
    ir::Instr* const* const address = ctx.symtable.find_entry(expr->value);
    ANTON_ASSERT(address != nullptr, "missing entry in symbol table");
    return *address;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_field(Lowering_Context& ctx, Builder& builder,
                     ast::Expr_Field const* const expr)
  {
    ANTON_ASSERT(expr->base->evaluated_type->type_kind ==
                   ast::Type_Kind::type_struct,
                 "cannot address fields of a non-struct type");
    ast::Type_Struct const* const base_type =
      static_cast<ast::Type_Struct const*>(expr->base->evaluated_type);
    ir::Type* const addressed_type = convert_ast_to_ir_type(base_type);
    ir::Instr* const address = get_address(ctx, builder, expr->base);
    i32 const index = get_field_index(base_type->definition, expr->field.value);
    ir::Value* const index_value = ir::make_constant_i32(ctx.allocator, index);
    ir::Instr* const getptr =
      ir::make_instr_getptr(ctx.allocator, ctx.get_next_id(), addressed_type,
                            address, index_value, expr->source_info);
    builder.insert(getptr);
    return getptr;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_index(Lowering_Context& ctx, Builder& builder,
                     ast::Expr_Index const* const expr)
  {
    ir::Type* const addressed_type =
      convert_ast_to_ir_type(expr->base->evaluated_type);
    ir::Instr* const address = get_address(ctx, builder, expr->base);
    ir::Value* const index = lower_expression(ctx, builder, expr->index);
    ir::Instr* const getptr =
      ir::make_instr_getptr(ctx.allocator, ctx.get_next_id(), addressed_type,
                            address, index, expr->source_info);
    builder.insert(getptr);
    return getptr;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_if(Lowering_Context& ctx, Builder& builder,
                  ast::Expr_If const* const expr)
  {
    ir::Value* const condition =
      lower_expression(ctx, builder, expr->condition);
    auto const then_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const else_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            then_block, else_block, expr->source_info);
    builder.insert(brcond);

    // Lower then branch.
    builder.set_insert_block(then_block);
    ir::Instr* const then_result = get_address(ctx, builder, expr->then_branch);
    auto const branch_from_then = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(branch_from_then);

    // Lower else branch.
    builder.set_insert_block(else_block);
    ir::Instr* const else_result = get_address(ctx, builder, expr->else_branch);
    auto const branch_from_else = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(branch_from_else);

    ANTON_ASSERT(compare_types_equal(*then_result->type, *else_result->type),
                 "expression types of then and else must be equal");

    // Insert the phi node at the start of the converge block.
    builder.set_insert_block(converge_block);
    auto const phi = ir::make_instr_phi(ctx.allocator, ctx.get_next_id(),
                                        ir::get_type_ptr(), expr->source_info);
    phi->srcs.push_back(then_result);
    then_result->add_referrer(phi);
    phi->srcs.push_back(else_result);
    else_result->add_referrer(phi);
    builder.insert(phi);
    return phi;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_call(Lowering_Context& ctx, Builder& builder,
                    ast::Expr_Call const* const expr)
  {
    // TODO: Implement.
    ANTON_UNREACHABLE("unimplemented");
    return nullptr;
  }

  ir::Instr* get_address(Lowering_Context& ctx, Builder& builder,
                         ast::Expr const* const generic_expr)
  {
    switch(generic_expr->node_kind) {
    case ast::Node_Kind::expr_identifier: {
      auto const expr = static_cast<ast::Expr_Identifier const*>(generic_expr);
      return address_expr_identifier(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_field: {
      auto const expr = static_cast<ast::Expr_Field const*>(generic_expr);
      return address_expr_field(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_index: {
      auto const expr = static_cast<ast::Expr_Index const*>(generic_expr);
      return address_expr_index(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_if: {
      auto const expr = static_cast<ast::Expr_If const*>(generic_expr);
      return address_expr_if(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_call: {
      auto const expr = static_cast<ast::Expr_Call const*>(generic_expr);
      return address_expr_call(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_reinterpret:
      ANTON_UNREACHABLE("unimplemented");

    case ast::Node_Kind::expr_init:
      ANTON_UNREACHABLE("expr_init does not produce an addressable result");

    case ast::Node_Kind::expr_default:
      ANTON_UNREACHABLE("expr_default is not addressable");

    case ast::Node_Kind::lt_bool:
      ANTON_UNREACHABLE("lt_bool is not addressable");

    case ast::Node_Kind::lt_integer:
      ANTON_UNREACHABLE("lt_integer is not addressable");

    case ast::Node_Kind::lt_float:
      ANTON_UNREACHABLE("lt_float is not addressable");

    default:
      ANTON_UNREACHABLE("unhandled expression kind");
    }
  }

  // vector_swizzle_char_to_index
  // Convert swizzle character to index. Allowed swizzle characters are
  //   x, y, z, w, r, g, b, a, s, t, u, v
  // Other chars result in undefined behaviour.
  //
  [[nodiscard]] static i32 vector_swizzle_char_to_index(char8 const c)
  {
    // Swizzles:
    // xyzw
    // rgba
    // stuv
    switch(c) {
    case 'x':
    case 'r':
    case 's':
      return 0;

    case 'y':
    case 'g':
    case 't':
      return 1;

    case 'z':
    case 'b':
    case 'u':
      return 2;

    case 'w':
    case 'a':
    case 'v':
      return 3;

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_identifier(Lowering_Context& ctx, Builder& builder,
                        ast::Expr_Identifier const* const expr)
  {
    ir::Type* const type = convert_ast_to_ir_type(expr->evaluated_type);
    ir::Instr* const address = get_address(ctx, builder, expr);
    ir::Instr* const load = ir::make_instr_load(
      ctx.allocator, ctx.get_next_id(), type, address, expr->source_info);
    builder.insert(load);
    return load;
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_field(Lowering_Context& ctx, Builder& builder,
                   ast::Expr_Field const* const expr)
  {
    ir::Instr* const address = get_address(ctx, builder, expr);
    ir::Type* const type = convert_ast_to_ir_type(expr->evaluated_type);
    ir::Instr* const load = ir::make_instr_load(
      ctx.allocator, ctx.get_next_id(), type, address, expr->source_info);
    builder.insert(load);
    return load;
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_index(Lowering_Context& ctx, Builder& builder,
                   ast::Expr_Index const* const expr)
  {
    ir::Instr* const address = get_address(ctx, builder, expr);
    ir::Type* const type = convert_ast_to_ir_type(expr->evaluated_type);
    ir::Instr* const load = ir::make_instr_load(
      ctx.allocator, ctx.get_next_id(), type, address, expr->source_info);
    builder.insert(load);
    return load;
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_init(Lowering_Context& ctx, Builder& builder,
                  ast::Expr_Init const* const expr)
  {
    return nullptr;
  }

  [[nodiscard]] static ir::Instr*
  lower_builtin_operator(Lowering_Context& ctx, Builder& builder,
                         ir::Type* const type, ast::Expr_Call const* const expr)
  {
    ANTON_ASSERT(expr->is_unary() || expr->is_binary(),
                 "call to operator is not unary or binary");
    auto const lhs = lower_expression(ctx, builder, expr->arguments[0]);
    anton::String_View const identifier = expr->function->identifier.value;
    if(identifier == "operator+"_sv) {
      // operator+ is always binary.
      ANTON_ASSERT(expr->is_binary(), "operator+ not binary");
      auto const rhs = lower_expression(ctx, builder, expr->arguments[1]);
      // TODO: uint
      if(ir::is_int_based_type(*type)) {
        auto const instr =
          ir::make_instr_alu(ctx.allocator, ctx.get_next_id(), type,
                             ir::ALU_Opcode::iadd, lhs, rhs, expr->source_info);
        builder.insert(instr);
        return instr;
      } else {
        auto const instr =
          ir::make_instr_alu(ctx.allocator, ctx.get_next_id(), type,
                             ir::ALU_Opcode::fadd, lhs, rhs, expr->source_info);
        builder.insert(instr);
        return instr;
      }
    }

    return nullptr;
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_call(Lowering_Context& ctx, Builder& builder,
                  ast::Expr_Call const* const expr)
  {
    ast::Decl_Function* const fn = expr->function;
    ir::Type* const type = convert_ast_to_ir_type(expr->evaluated_type);
    if(fn->builtin) {
      if(anton::begins_with(fn->identifier.value, "operator"_sv)) {
        return lower_builtin_operator(ctx, builder, type, expr);
      } else {
        // lower_builtin_function_call();
        return nullptr;
      }
    } else {
      ir::Function* const* const function =
        ctx.fntable.find_entry(expr->identifier.value);
      ANTON_ASSERT(function != nullptr, "call has no function");
      auto const call = ir::make_instr_call(ctx.allocator, ctx.get_next_id(),
                                            *function, type, expr->source_info);
      for(ast::Expr const* const arg: expr->arguments) {
        auto const value = lower_expression(ctx, builder, arg);
        call->add_argument(value);
      }
      builder.insert(call);
      return call;
    }
  }

  [[nodiscard]] static ir::Instr* lower_expr_if(Lowering_Context& ctx,
                                                Builder& builder,
                                                ast::Expr_If const* const expr)
  {
    ir::Value* const condition =
      lower_expression(ctx, builder, expr->condition);
    auto const then_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const else_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            then_block, else_block, expr->source_info);
    builder.insert(brcond);

    // Lower then branch.
    builder.set_insert_block(then_block);
    ir::Value* const then_result =
      lower_expression(ctx, builder, expr->then_branch);
    auto const jmp_from_then = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(jmp_from_then);

    // Lower else branch.
    builder.set_insert_block(else_block);
    ir::Value* const else_result =
      lower_expression(ctx, builder, expr->else_branch);
    auto const jmp_from_else = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(jmp_from_else);

    ANTON_ASSERT(compare_types_equal(*then_result->type, *else_result->type),
                 "expression types of then and else must be equal");

    // Insert phi nodes at the start of the converge block.
    builder.set_insert_block(converge_block);
    auto const phi = ir::make_instr_phi(ctx.allocator, ctx.get_next_id(),
                                        then_result->type, expr->source_info);
    ANTON_ASSERT(instanceof <ir::Instr>(then_result),
                            "phi argument is not an instruction");
    phi->srcs.push_back(static_cast<ir::Instr*>(then_result));
    then_result->add_referrer(phi);
    ANTON_ASSERT(instanceof <ir::Instr>(else_result),
                            "phi argument is not an instruction");
    phi->srcs.push_back(static_cast<ir::Instr*>(else_result));
    else_result->add_referrer(phi);
    builder.insert(phi);
    return phi;
  }

  // lower_expression
  // Lowers an ast::Expr node into a series of IR instructions.
  //
  ir::Value* lower_expression(Lowering_Context& ctx, Builder& builder,
                              ast::Expr const* const generic_expr)
  {
    switch(generic_expr->node_kind) {
    case ast::Node_Kind::lt_bool: {
      auto const expr = static_cast<ast::Lt_Bool const*>(generic_expr);
      auto const value = ir::make_constant_bool(ctx.allocator, expr->value);
      return value;
    } break;

    case ast::Node_Kind::lt_integer: {
      auto const expr = static_cast<ast::Lt_Integer const*>(generic_expr);
      switch(expr->kind) {
      case ast::Lt_Integer_Kind::i32: {
        auto const value =
          ir::make_constant_i32(ctx.allocator, expr->i32_value);
        return value;
      }

      case ast::Lt_Integer_Kind::u32: {
        return nullptr;
      }
      }
    } break;

    case ast::Node_Kind::lt_float: {
      auto const expr = static_cast<ast::Lt_Float const*>(generic_expr);
      switch(expr->kind) {
      case ast::Lt_Float_Kind::f32: {
        auto const value =
          ir::make_constant_f32(ctx.allocator, expr->f32_value);
        return value;
      }

      case ast::Lt_Float_Kind::f64: {
        auto const value =
          ir::make_constant_f64(ctx.allocator, expr->f64_value);
        return value;
      }
      }
    } break;

    case ast::Node_Kind::expr_identifier: {
      auto const expr = static_cast<ast::Expr_Identifier const*>(generic_expr);
      return lower_expr_identifier(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_field: {
      auto const expr = static_cast<ast::Expr_Field const*>(generic_expr);
      return lower_expr_field(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_index: {
      auto const expr = static_cast<ast::Expr_Index const*>(generic_expr);
      return lower_expr_index(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_init: {
      auto const expr = static_cast<ast::Expr_Init const*>(generic_expr);
      return lower_expr_init(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_call: {
      auto const expr = static_cast<ast::Expr_Call const*>(generic_expr);
      return lower_expr_call(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_if: {
      auto const expr = static_cast<ast::Expr_If const*>(generic_expr);
      return lower_expr_if(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_reinterpret:
      ANTON_UNREACHABLE("unimplemented");

    case ast::Node_Kind::expr_default:
      ANTON_UNREACHABLE("expr_default is not lowerable");

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  static void lower_statement(Lowering_Context& ctx, Builder& builder,
                              ast::Node const* const generic_stmt);

  static void lower_statement_block(Lowering_Context& ctx, Builder& builder,
                                    ast::Node_List const stmts)
  {
    ctx.symtable.push_scope();
    for(ast::Node* const stmt: stmts) {
      lower_statement(ctx, builder, stmt);
    }
    ctx.symtable.pop_scope();
  }

  static void lower_variable(Lowering_Context& ctx, Builder& builder,
                             ast::Variable const* const variable)
  {
    ir::Type* const type = convert_ast_to_ir_type(variable->type);
    ir::Instr* const instr = ir::make_instr_alloc(
      ctx.allocator, ctx.get_next_id(), type, variable->source_info);
    builder.insert(instr);
    ctx.symtable.add_entry(variable->identifier.value, instr);
  }

  static void lower_stmt_assignment(Lowering_Context& ctx, Builder& builder,
                                    ast::Stmt_Assignment const* const stmt)
  {
    // TODO FIX: does not account for assignment kind.
    ir::Instr* const address = get_address(ctx, builder, stmt->lhs);
    ANTON_ASSERT(address->type->kind == ir::Type_Kind::e_ptr,
                 "address is not pointer");
    ir::Value* const rhs = lower_expression(ctx, builder, stmt->rhs);
    // TODO: target conversion type.
    ir::Instr* const result = generate_conversion(ctx, builder, rhs, nullptr);
    ir::Instr* const store = ir::make_instr_store(
      ctx.allocator, ctx.get_next_id(), address, result, stmt->source_info);
    builder.insert(store);
  }

  static void lower_stmt_return(Lowering_Context& ctx, Builder& builder,
                                ast::Stmt_Return const* const stmt)
  {
    if(stmt->expression != nullptr) {
      ir::Value* const ret_expr =
        lower_expression(ctx, builder, stmt->expression);
      // TODO: target conversion type.
      ir::Instr* const cvt_ret_expr =
        generate_conversion(ctx, builder, ret_expr, nullptr);
      ir::Instr* const ret = ir::make_instr_return(
        ctx.allocator, ctx.get_next_id(), cvt_ret_expr, stmt->source_info);
      builder.insert(ret);
    } else {
      ir::Instr* const ret = ir::make_instr_return(
        ctx.allocator, ctx.get_next_id(), stmt->source_info);
      builder.insert(ret);
    }
  }

  static void lower_stmt_if(Lowering_Context& ctx, Builder& builder,
                            ast::Stmt_If const* const stmt)
  {
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const then_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const else_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            then_block, else_block, stmt->source_info);
    builder.insert(brcond);

    // Lower then branch.
    builder.set_insert_block(then_block);
    lower_statement_block(ctx, builder, stmt->then_branch);
    auto const branch_from_then = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, stmt->source_info);
    builder.insert(branch_from_then);

    // Lower else branch.
    builder.set_insert_block(else_block);
    lower_statement_block(ctx, builder, stmt->else_branch);
    auto const branch_from_else = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, stmt->source_info);
    builder.insert(branch_from_else);

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_for(Lowering_Context& ctx, Builder& builder,
                             ast::Stmt_For const* const stmt)
  {
    auto const condition_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const loop_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const continuation_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());

    ctx.nearest_continuation_block = continuation_block;
    ctx.nearest_converge_block = converge_block;

    // Lower variables in the previous block.
    for(ast::Variable const* const variable: stmt->declarations) {
      lower_variable(ctx, builder, variable);
    }

    // Branch to the condition block from wherever we are.
    ir::Instr* branch = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch);

    // Lower the condition.
    builder.set_insert_block(condition_block);
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            loop_block, converge_block, stmt->source_info);
    builder.insert(brcond);

    // Lower the loop block and branch to continuation.
    builder.set_insert_block(loop_block);
    lower_statement_block(ctx, builder, stmt->statements);
    auto const branch_to_continuation = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), continuation_block, stmt->source_info);
    builder.insert(branch_to_continuation);

    // Lower the actions in the continuation block.
    builder.set_insert_block(continuation_block);
    for(ast::Expr const* const expr: stmt->actions) {
      ir::Value* const instr = lower_expression(ctx, builder, expr);
      // Result is discarded.
      ANTON_UNUSED(instr);
    }

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_while(Lowering_Context& ctx, Builder& builder,
                               ast::Stmt_While const* const stmt)
  {
    auto const condition_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const loop_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());

    ctx.nearest_continuation_block = condition_block;
    ctx.nearest_converge_block = converge_block;

    // Branch to the condition block from wherever we are.
    ir::Instr* branch = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch);

    // Lower the condition.
    builder.set_insert_block(condition_block);
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            loop_block, converge_block, stmt->source_info);
    builder.insert(brcond);

    // Lower the loop block.
    builder.set_insert_block(loop_block);
    lower_statement_block(ctx, builder, stmt->statements);
    auto const branch_to_condition = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch_to_condition);

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_do_while(Lowering_Context& ctx, Builder& builder,
                                  ast::Stmt_Do_While const* const stmt)
  {
    auto const condition_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const loop_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());

    ctx.nearest_continuation_block = condition_block;
    ctx.nearest_converge_block = converge_block;

    // Branch to the loop block from wherever we are.
    ir::Instr* branch = ir::make_instr_branch(ctx.allocator, ctx.get_next_id(),
                                              loop_block, stmt->source_info);
    builder.insert(branch);

    // Lower the loop block.
    builder.set_insert_block(loop_block);
    lower_statement_block(ctx, builder, stmt->statements);
    auto const branch_to_condition = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch_to_condition);

    // Lower the condition.
    builder.set_insert_block(condition_block);
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            loop_block, converge_block, stmt->source_info);
    builder.insert(brcond);

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_switch(Lowering_Context& ctx, Builder& builder,
                                ast::Stmt_Switch const* const stmt)
  {
    ir::Value* const selector =
      lower_expression(ctx, builder, stmt->expression);
    auto const default_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    ir::Instr_switch* const instr_switch =
      ir::make_instr_switch(ctx.allocator, ctx.get_next_id(), selector,
                            default_block, stmt->source_info);
    builder.insert(instr_switch);

    for(ast::Switch_Arm const* const arm: stmt->arms) {
      ir::Basic_Block* current_block = default_block;
      if(!arm->has_default) {
        current_block =
          VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
      }

      builder.set_insert_block(current_block);
      lower_statement_block(ctx, builder, arm->statements);
      for(ast::Expr const* const label: arm->labels) {
        ANTON_ASSERT(label->node_kind == ast::Node_Kind::lt_integer ||
                       label->node_kind == ast::Node_Kind::expr_default,
                     "label is not an integer");
        if(label->node_kind == ast::Node_Kind::lt_integer) {
          auto const node = static_cast<ast::Lt_Integer const*>(label);
          i64 const value =
            (node->kind == ast::Lt_Integer_Kind::i32 ? node->i32_value
                                                     : node->u32_value);
          instr_switch->add_label(ir::Switch_Label{value, current_block});
        }
      }
    }
  }

  static void lower_statement(Lowering_Context& ctx, Builder& builder,
                              ast::Node const* const generic_stmt)
  {
    switch(generic_stmt->node_kind) {
    case ast::Node_Kind::stmt_block: {
      auto const stmt = static_cast<ast::Stmt_Block const*>(generic_stmt);
      lower_statement_block(ctx, builder, stmt->statements);
    } break;

    case ast::Node_Kind::stmt_expression: {
      auto const stmt = static_cast<ast::Stmt_Expression const*>(generic_stmt);
      ir::Value* const value = lower_expression(ctx, builder, stmt->expression);
      // The result is discarded.
      ANTON_UNUSED(value);
    } break;

    case ast::Node_Kind::stmt_discard: {
      auto const instr = ir::make_instr_die(ctx.allocator, ctx.get_next_id(),
                                            generic_stmt->source_info);
      builder.insert(instr);
    } break;

    case ast::Node_Kind::stmt_break: {
      ANTON_ASSERT(ctx.nearest_converge_block != nullptr,
                   "missing loop converge block");
      auto const instr = ir::make_instr_branch(ctx.allocator, ctx.get_next_id(),
                                               ctx.nearest_converge_block,
                                               generic_stmt->source_info);
      builder.insert(instr);
    } break;

    case ast::Node_Kind::stmt_continue: {
      ANTON_ASSERT(ctx.nearest_continuation_block != nullptr,
                   "missing loop continuation block");
      auto const instr = ir::make_instr_branch(ctx.allocator, ctx.get_next_id(),
                                               ctx.nearest_continuation_block,
                                               generic_stmt->source_info);
      builder.insert(instr);
    } break;

    case ast::Node_Kind::stmt_return: {
      auto const stmt = static_cast<ast::Stmt_Return const*>(generic_stmt);
      lower_stmt_return(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::variable: {
      auto const stmt = static_cast<ast::Variable const*>(generic_stmt);
      lower_variable(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_assignment: {
      auto const stmt = static_cast<ast::Stmt_Assignment const*>(generic_stmt);
      lower_stmt_assignment(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_if: {
      auto const stmt = static_cast<ast::Stmt_If const*>(generic_stmt);
      lower_stmt_if(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_for: {
      auto const stmt = static_cast<ast::Stmt_For const*>(generic_stmt);
      lower_stmt_for(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_while: {
      auto const stmt = static_cast<ast::Stmt_While const*>(generic_stmt);
      lower_stmt_while(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_do_while: {
      auto const stmt = static_cast<ast::Stmt_Do_While const*>(generic_stmt);
      lower_stmt_do_while(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_switch: {
      auto const stmt = static_cast<ast::Stmt_Switch const*>(generic_stmt);
      lower_stmt_switch(ctx, builder, stmt);
    } break;

    default:
      ANTON_UNREACHABLE("unhandled statement node kind");
    }
  }

  // [[nodiscard]] static ir::Function* lower_function(Lowering_Context& ctx,
  //                                                   ast::Decl_Function* const fn)
  // {
  //   return nullptr;
  // }

  [[nodiscard]] static ir::Module
  lower_module(Lowering_Context& ctx,
               ast::Decl_Stage_Function const* const stage)
  {
    ir::Basic_Block entry_block(ctx.get_next_id());
    ir::Function* const fn = VUSH_ALLOCATE(
      ir::Function, ctx.allocator, ctx.get_next_id(), ANTON_MOV(entry_block),
      anton::String("main"_sv, ctx.allocator), stage->source_info);
    Builder builder;
    builder.set_insert_block(&fn->entry_block);
    lower_statement_block(ctx, builder, stage->body);
    return ir::Module(anton::String(stage->pass.value, ctx.allocator),
                      stage->stage.value, fn);
  }

  Array<ir::Module> lower_ast_to_ir(Allocator* const allocator,
                                    ast::Node_List const ast)
  {
    Array<ir::Module> modules{allocator};
    Lowering_Context ctx{allocator};
    for(ast::Node const* const node: ast) {
      if(node->node_kind == ast::Node_Kind::decl_function) {
        auto const ast_fn = static_cast<ast::Decl_Function const*>(node);
        ir::Basic_Block entry_block{ctx.get_next_id()};
        anton::String identifier{ast_fn->identifier.value, ctx.allocator};
        auto const ir_fn = VUSH_ALLOCATE(
          ir::Function, allocator, ctx.get_next_id(), ANTON_MOV(entry_block),
          ANTON_MOV(identifier), ast_fn->source_info);
        ctx.fntable.add_entry(ir_fn->identifier, ir_fn);
      }
    }

    for(ast::Node const* const node: ast) {
      if(node->node_kind == ast::Node_Kind::decl_stage_function) {
        auto const stage = static_cast<ast::Decl_Stage_Function const*>(node);
        ir::Module module = lower_module(ctx, stage);
        modules.push_back(ANTON_MOV(module));
      }
    }
    return modules;
  }
} // namespace vush
