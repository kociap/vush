#include <vush_sema/sema.hpp>

#include <anton/algorithm.hpp>
#include <anton/ranges.hpp>

#include <vush_ast/ast.hpp>
#include <vush_autogen/builtin_symbols.hpp>
#include <vush_core/context.hpp>
#include <vush_core/memory.hpp>
#include <vush_core/scoped_map.hpp>
#include <vush_diagnostics/diagnostics.hpp>
#include <vush_sema/diagnostics.hpp>
#include <vush_sema/typeconv.hpp>

namespace vush {
  using namespace anton::literals;

  enum struct Stmt_Ctx {
    e_none,
    e_loop,
    e_switch,
    e_continuation,
  };

  struct Sema_Context {
    Stmt_Ctx stmt;
  };

  struct Namespace;

  enum struct Symbol_Kind {
    e_variable,
    e_parameter,
    e_struct,
    e_buffer,
    e_overload_group,
    e_namespace,
  };

  struct Symbol {
    anton::String_View identifier;
    union {
      ast::Variable* value_variable;
      ast::Fn_Parameter* value_parameter;
      ast::Decl_Struct* value_struct;
      ast::Decl_Buffer* value_buffer;
      ast::Overload_Group* value_overload_group;
      Namespace* value_namespace;
    };
    Symbol_Kind kind;

    Symbol(anton::String_View identifier, ast::Variable* value)
      : identifier(identifier), value_variable(value),
        kind(Symbol_Kind::e_variable)
    {
    }

    Symbol(anton::String_View identifier, ast::Fn_Parameter* value)
      : identifier(identifier), value_parameter(value),
        kind(Symbol_Kind::e_parameter)
    {
    }

    Symbol(anton::String_View identifier, ast::Decl_Struct* value)
      : identifier(identifier), value_struct(value), kind(Symbol_Kind::e_struct)
    {
    }

    Symbol(anton::String_View identifier, ast::Decl_Buffer* value)
      : identifier(identifier), value_buffer(value), kind(Symbol_Kind::e_buffer)
    {
    }

    Symbol(anton::String_View identifier, ast::Overload_Group* value)
      : identifier(identifier), value_overload_group(value),
        kind(Symbol_Kind::e_overload_group)
    {
    }

    Symbol(anton::String_View identifier, Namespace* value)
      : identifier(identifier), value_namespace(value),
        kind(Symbol_Kind::e_namespace)
    {
    }
  };

  using Symbol_Table = Scoped_Map<anton::String_View, Symbol>;

  struct Namespace {
    ast::Identifier identifier;
    Symbol_Table symtable;

    Namespace(Allocator* allocator, ast::Identifier identifier)
      : identifier(identifier), symtable(allocator)
    {
    }
  };

  [[nodiscard]] static ast::Type* copy(Allocator* const allocator,
                                       ast::Type const* const gtype)
  {
    switch(gtype->type_kind) {
    case ast::Type_Kind::type_builtin: {
      auto const type = static_cast<ast::Type_Builtin const*>(gtype);
      return VUSH_ALLOCATE(ast::Type_Builtin, allocator, type->source_info,
                           type->qualifiers, type->value);
    }

    case ast::Type_Kind::type_struct: {
      auto const type = static_cast<ast::Type_Struct const*>(gtype);
      return VUSH_ALLOCATE(ast::Type_Struct, allocator, type->source_info,
                           type->qualifiers,
                           anton::String(type->value, allocator));
    }

    case ast::Type_Kind::type_array: {
      auto const type = static_cast<ast::Type_Array const*>(gtype);
      auto const base = copy(allocator, type->base);
      auto const size =
        type->size != nullptr
          ? VUSH_ALLOCATE(ast::Lt_Integer, allocator, *type->size)
          : nullptr;
      return VUSH_ALLOCATE(ast::Type_Array, allocator, type->source_info,
                           type->qualifiers, base, size);
    }
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  check_array_is_sized(Context const& ctx, ast::Type const* const type)
  {
    if(ast::is_unsized_array(*type)) {
      return {anton::expected_error,
              err_unsized_array_not_allowed(ctx, type->source_info)};
    }

    if(type->type_kind == ast::Type_Kind::type_array) {
      auto const t = static_cast<ast::Type_Array const*>(type);
      return check_array_is_sized(ctx, t->base);
    }

    return anton::expected_value;
  }

#define RETURN_ON_FAIL(fn, ...)                                            \
  {                                                                        \
    anton::Expected<void, Error> _RETURN_ON_FAIL_result = fn(__VA_ARGS__); \
    if(!_RETURN_ON_FAIL_result) {                                          \
      return ANTON_MOV(_RETURN_ON_FAIL_result);                            \
    }                                                                      \
  }

#define RETURN_ON_FAIL_VAR(variable, fn, ...)                    \
  auto variable = fn(__VA_ARGS__);                               \
  if(!variable) {                                                \
    return {anton::expected_error, ANTON_MOV(variable.error())}; \
  }

  // add_symbol
  // Checks whether a symbol already exists and if not, adds it to the symbol
  // table. Otherwise returns an error diagnostic.
  //
  [[nodiscard]] static anton::Expected<void, Error>
  add_symbol(Context& ctx, Symbol_Table& symtable, Symbol const& symbol)
  {
    Symbol const* const original_symbol =
      symtable.find_entry(symbol.identifier);
    if(original_symbol != nullptr) {
      auto get_symbol_identifier_source =
        [](Symbol const& symbol) -> Source_Info {
        switch(symbol.kind) {
        case Symbol_Kind::e_variable:
          return symbol.value_variable->identifier.source_info;

        case Symbol_Kind::e_parameter:
          return symbol.value_parameter->identifier.source_info;

        case Symbol_Kind::e_struct:
          return symbol.value_struct->identifier.source_info;

        case Symbol_Kind::e_buffer:
          return symbol.value_buffer->identifier.source_info;

        case Symbol_Kind::e_overload_group: {
          // Use the first overload as the source location.
          ast::Decl_Function* const overload =
            symbol.value_overload_group->overloads[0];
          return overload->identifier.source_info;
        }

        case Symbol_Kind::e_namespace:
          return symbol.value_namespace->identifier.source_info;
        }
      };

      Source_Info const old_name =
        get_symbol_identifier_source(*original_symbol);
      Source_Info const new_name = get_symbol_identifier_source(symbol);
      return {anton::expected_error,
              err_symbol_redefinition(ctx, old_name, new_name)};
    }

    symtable.add_entry(symbol.identifier, symbol);
    return anton::expected_value;
  }

  // select_overload
  //
  [[nodiscard]] static anton::Expected<ast::Decl_Function*, Error>
  select_overload(Context& ctx, ast::Expr_Call* const call,
                  ast::Overload_Group const* const group)
  {
    Array<ast::Decl_Function*> candidates(ctx.allocator);
    i64 best_score = anton::limits::maximum_i64;
    for(ast::Decl_Function* const fn: group->overloads) {
      if(fn->parameters.size() != call->arguments.size()) {
        continue;
      }

      bool viable = true;
      i64 overload_rank = 0;
      for(auto const [argument, parameter]:
          anton::zip(call->arguments, fn->parameters)) {
        ANTON_ASSERT(argument->evaluated_type != nullptr,
                     "argument has unevaluated type");
        anton::Optional<i64> rank_result =
          rank_conversion(parameter->type, argument->evaluated_type);
        if(!rank_result) {
          viable = false;
          break;
        }

        overload_rank += rank_result.value();
      }

      if(viable) {
        if(overload_rank < best_score) {
          best_score = overload_rank;
          candidates.clear();
        }

        if(overload_rank == best_score) {
          candidates.push_back(fn);
        }
      }
    }

    if(candidates.size() == 0) {
      return {anton::expected_error,
              err_no_matching_overload(ctx, call, group->overloads)};
    }

    if(candidates.size() > 1) {
      return {anton::expected_error,
              err_ambiguous_overload(ctx, call, candidates)};
    }

    return {anton::expected_value, candidates[0]};
  }

  [[nodiscard]] static anton::Expected<void, Error>
  namebind_type(Context& ctx, Symbol_Table& symtable, ast::Type* const type)
  {
    switch(type->type_kind) {
    case ast::Type_Kind::type_builtin: {
      return anton::expected_value;
    }

    case ast::Type_Kind::type_struct: {
      auto const type_struct = static_cast<ast::Type_Struct*>(type);
      Symbol const* const symbol = symtable.find_entry(type_struct->value);
      if(symbol == nullptr) {
        return {anton::expected_error,
                err_undefined_symbol(ctx, type->source_info)};
      }

      switch(symbol->kind) {
      case Symbol_Kind::e_struct: {
        type_struct->definition = symbol->value_struct;
        return anton::expected_value;
      }

      default:
        // TODO: Error.
        return {anton::expected_error,
                err_unimplemented(ctx, type->source_info, __FILE__, __LINE__)};
      }

      type_struct->definition = symbol->value_struct;
      return anton::expected_value;
    }

    case ast::Type_Kind::type_array: {
      auto const array = static_cast<ast::Type_Array*>(type);
      return namebind_type(ctx, symtable, array->base);
    }
    }
  }

  [[nodiscard]] static anton::Expected<ast::Type*, Error>
  evaluate_vector_field(Context const& ctx, ast::Type_Builtin const& type,
                        ast::Identifier const& field)
  {
    ANTON_ASSERT(is_vector(type), "type is not vector");
    // Vectors have swizzle members of sizes 1 (scalar), 2 (vec2), 3 (vec3), 4
    // (vec4). The set of allowed characters is { x, y, z, w, s, t, u, v, r, g,
    // b, a }.
    anton::String_View const value = field.value;
    i64 const size = value.size_bytes();
    if(size > 4 || size < 1) {
      // Less than 1 check for sanity.
      return {anton::expected_error,
              err_vector_swizzle_overlong(ctx, &type, field)};
    }

    for(char8 const c: value.bytes()) {
      bool const is_allowed = c == 'x' | c == 'y' | c == 'z' | c == 'w' |
                              c == 's' | c == 't' | c == 'u' | c == 'v' |
                              c == 'r' | c == 'g' | c == 'b' | c == 'a';
      if(!is_allowed) {
        return {anton::expected_error, err_vector_swizzle_invalid(ctx, field)};
      }
    }

    if(is_bool_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_bool)};
      case 2:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_bvec2)};
      case 3:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_bvec3)};
      case 4:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_bvec4)};
      }
    }

    if(is_i32_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_int)};
      case 2:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_ivec2)};
      case 3:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_ivec3)};
      case 4:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_ivec4)};
      }
    }

    if(is_u32_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_uint)};
      case 2:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_uvec2)};
      case 3:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_uvec3)};
      case 4:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_uvec4)};
      }
    }

    if(is_f32_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_float)};
      case 2:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_vec2)};
      case 3:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_vec3)};
      case 4:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_vec4)};
      }
    }

    if(is_f64_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_double)};
      case 2:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_dvec2)};
      case 3:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_dvec3)};
      case 4:
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator, Source_Info{},
                              ast::Type_Builtin_Kind::e_dvec4)};
      }
    }

    return {anton::expected_error,
            err_unimplemented(ctx, field.source_info, __FILE__, __LINE__)};
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expression(Context& ctx, Symbol_Table& symtable,
                     ast::Expr* const expression);

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_identifier(Context& ctx, Symbol_Table& symtable,
                          ast::Expr_Identifier* const expr)
  {
    Symbol const* const symbol = symtable.find_entry(expr->value);
    if(!symbol) {
      return {anton::expected_error,
              err_undefined_symbol(ctx, expr->source_info)};
    }

    switch(symbol->kind) {
    case Symbol_Kind::e_variable: {
      expr->definition = symbol->value_variable;
      ast::Variable const* const variable =
        static_cast<ast::Variable const*>(expr->definition);
      ast::Type const* const type = variable->type;
      expr->evaluated_type = type;
      return anton::expected_value;
    }

    case Symbol_Kind::e_parameter: {
      expr->definition = symbol->value_parameter;
      ast::Fn_Parameter const* const parameter =
        static_cast<ast::Fn_Parameter const*>(expr->definition);
      ast::Type const* const type = parameter->type;
      expr->evaluated_type = type;
      return anton::expected_value;
    }

    case Symbol_Kind::e_overload_group: {
      return {anton::expected_error,
              err_identifier_names_a_function_but_is_not_called(
                ctx, expr->source_info)};
    } break;

    case Symbol_Kind::e_buffer:
    case Symbol_Kind::e_struct:
    case Symbol_Kind::e_namespace:
      // TODO: Error.
      return {anton::expected_error,
              err_unimplemented(ctx, expr->source_info, __FILE__, __LINE__)};
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_call(Context& ctx, Symbol_Table& symtable,
                    ast::Expr_Call* const expr)
  {
    // Namebind the identifier.
    Symbol const* const symbol = symtable.find_entry(expr->identifier.value);
    if(!symbol) {
      return {anton::expected_error,
              err_undefined_symbol(ctx, expr->source_info)};
    }

    if(symbol->kind == Symbol_Kind::e_overload_group) {
      expr->overload_group = symbol->value_overload_group;
    } else {
      // TODO: Error.
      return {anton::expected_error,
              err_unimplemented(ctx, expr->identifier.source_info, __FILE__,
                                __LINE__)};
    }

    // Always evaluate all arguments before doing overload resolution. The
    // types are cached, so there is no risk of reevaluation or a major
    // performance penalty, but this way we ensure that all expressions have
    // their types evaluated.
    for(ast::Expr* const argument: expr->arguments) {
      RETURN_ON_FAIL(analyse_expression, ctx, symtable, argument);
    }

    ast::Overload_Group const* const group = expr->overload_group;
    anton::Expected<ast::Decl_Function*, Error> result =
      select_overload(ctx, expr, group);
    if(!result) {
      return {anton::expected_error, ANTON_MOV(result.error())};
    }

    auto const fn = result.value();
    expr->function = fn;
    ast::Type const* const type = fn->return_type;
    expr->evaluated_type = type;
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_index(Context& ctx, Symbol_Table& symtable,
                     ast::Expr_Index* const expr)
  {
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, expr->base);
    ast::Type const* const gbase_type = expr->base->evaluated_type;
    if(!is_array(*gbase_type)) {
      return {anton::expected_error,
              err_expression_is_not_indexable(ctx, gbase_type, expr->base)};
    }

    RETURN_ON_FAIL(analyse_expression, ctx, symtable, expr->index);
    ast::Type const* const index_type = expr->index->evaluated_type;
    if(!is_integer(*index_type)) {
      return {anton::expected_error,
              err_array_index_is_not_integer(ctx, index_type, expr->index)};
    }

    auto const base_type = static_cast<ast::Type_Array const*>(gbase_type);
    expr->evaluated_type = base_type->base;
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_if(Context& ctx, Symbol_Table& symtable,
                  ast::Expr_If* const expr)
  {
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, expr->condition);
    ast::Type const* const condition_type = expr->condition->evaluated_type;
    ast::Type const* const bool_type =
      get_builtin_type(ast::Type_Builtin_Kind::e_bool);
    if(!is_convertible(bool_type, condition_type)) {
      return {anton::expected_error, err_condition_not_of_bool_type(
                                       ctx, expr->condition, condition_type)};
    }

    RETURN_ON_FAIL(analyse_expression, ctx, symtable, expr->then_branch);
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, expr->else_branch);
    ast::Type const* const then_type = expr->then_branch->evaluated_type;
    ast::Type const* const else_type = expr->else_branch->evaluated_type;
    // 'then' and 'else' types must always be equal. Otherwise, there is no
    // reliable way for us to convert the types.
    if(!compare_types_equal(*then_type, *else_type)) {
      return {anton::expected_error, err_incompatible_if_expression_types(
                                       ctx, then_type, expr->then_branch,
                                       else_type, expr->else_branch)};
    }

    expr->evaluated_type = then_type;
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_field(Context& ctx, Symbol_Table& symtable,
                     ast::Expr_Field* const expr)
  {
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, expr->base);
    // Arrays have no members. Builtin types with the exception of vectors do
    // not have members.
    //
    // TODO: Separate diagnostics for each type kind.
    ast::Type const* generic_type = expr->base->evaluated_type;
    switch(generic_type->type_kind) {
    case ast::Type_Kind::type_builtin: {
      auto const type = static_cast<ast::Type_Builtin const*>(generic_type);
      if(is_vector(*type)) {
        RETURN_ON_FAIL_VAR(field_result, evaluate_vector_field, ctx, *type,
                           expr->field);
        // We must propagate the qualifiers.
        auto const evaluated_type = field_result.value();
        evaluated_type->qualifiers = type->qualifiers;
        expr->evaluated_type = evaluated_type;
        return anton::expected_value;
      } else {
        return {anton::expected_error, err_builtin_type_has_no_member_named(
                                         ctx, generic_type, expr->field)};
      }
    } break;

    case ast::Type_Kind::type_struct: {
      ast::Type_Struct const* const type =
        static_cast<ast::Type_Struct const*>(generic_type);
      ast::Decl_Struct const* const decl = type->definition;
      for(ast::Struct_Field const* const field: decl->fields) {
        if(field->identifier.value != expr->field.value) {
          continue;
        }

        // We must propagate the qualifiers.
        auto const evaluated_type = copy(ctx.allocator, field->type);
        evaluated_type->qualifiers = type->qualifiers;
        expr->evaluated_type = evaluated_type;
        return anton::expected_value;
      }

      return {anton::expected_error,
              err_type_has_no_field_named(ctx, generic_type, expr->field)};
    } break;

    case ast::Type_Kind::type_array: {
      return {anton::expected_error,
              err_type_has_no_field_named(ctx, generic_type, expr->field)};
    } break;
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_init_type_struct(Context& ctx, Symbol_Table& symtable,
                                ast::Type_Struct const* const type,
                                ast::Initializer* const ginitializer)
  {
    if(ginitializer->node_kind != ast::Node_Kind::field_initializer) {
      return {anton::expected_error,
              err_init_invalid_struct_initializer_kind(ctx, ginitializer)};
    }

    auto const initializer = static_cast<ast::Field_Initializer*>(ginitializer);

    // Find the field within the struct and report that first.
    ast::Decl_Struct const* const decl = type->definition;
    ast::Struct_Field_List::iterator const fields_begin = decl->fields.begin();
    ast::Struct_Field_List::iterator const fields_end = decl->fields.end();
    anton::String_View const identifier = initializer->identifier.value;
    ast::Struct_Field_List::iterator const field =
      anton::find_if(fields_begin, fields_end,
                     [identifier](ast::Struct_Field const* const field) {
                       return field->identifier.value == identifier;
                     });
    if(field == fields_end) {
      return {anton::expected_error,
              err_field_initializer_no_field_named(ctx, decl, initializer)};
    }

    // Perform validation of the expression and report any errors there.
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, initializer->expression);

    // Ensure type compatibility as the last step.
    ast::Type const* const field_type = (*field)->type;
    ast::Type const* const initializer_type =
      initializer->expression->evaluated_type;
    if(!is_convertible(field_type, initializer_type)) {
      return {anton::expected_error,
              err_cannot_convert_type(ctx, initializer->expression->source_info,
                                      field_type, initializer_type)};
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_init_type_array(Context& ctx, Symbol_Table& symtable,
                               ast::Type_Array const* const type,
                               ast::Initializer* const ginitializer)
  {
    if(ginitializer->node_kind != ast::Node_Kind::index_initializer &&
       ginitializer->node_kind != ast::Node_Kind::basic_initializer) {
      return {anton::expected_error,
              err_init_invalid_array_initializer_kind(ctx, ginitializer)};
    }

    return {
      anton::expected_error,
      err_unimplemented(ctx, ginitializer->source_info, __FILE__, __LINE__)};
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_expr_init(Context& ctx, Symbol_Table& symtable,
                    ast::Expr_Init* const expr)
  {
    // Initialization rules:
    // - Builtin types: we do not allow initialization of builtin types with the
    //   exception of vectors and matrices - basic initializers in both cases.
    // - Struct types: we allow initialization of struct types using only field
    //   initializers.
    // - Array types: we allow initialization of array types using only range or
    //   basic initializers. Usage of both kinds of initializers in one
    //   initialization is disallowed.
    //   TODO: We might lift this restriction at a later time.
    // Duplicate or overlapping initializers are not allowed. Initializers are
    // evaluated in the order of appearance.
    RETURN_ON_FAIL(namebind_type, ctx, symtable, expr->type);
    ast::Type_Kind const type_kind = expr->type->type_kind;
    switch(type_kind) {
    case ast::Type_Kind::type_struct: {
      auto const type = static_cast<ast::Type_Struct*>(expr->type);
      for(ast::Initializer* const ginitializer: expr->initializers) {
        RETURN_ON_FAIL(analyse_expr_init_type_struct, ctx, symtable, type,
                       ginitializer);
      }
      // TODO: Deduplicate initializers.
    } break;

    case ast::Type_Kind::type_builtin: {
      auto const type = static_cast<ast::Type_Builtin*>(expr->type);
      for(ast::Initializer* const ginitializer: expr->initializers) {
        if(is_vector(*type)) {
          if(ginitializer->node_kind != ast::Node_Kind::basic_initializer) {
            return {
              anton::expected_error,
              err_init_invalid_vector_initializer_kind(ctx, ginitializer)};
          }
        } else if(is_matrix(*type)) {
          if(ginitializer->node_kind != ast::Node_Kind::basic_initializer) {
            return {
              anton::expected_error,
              err_init_invalid_matrix_initializer_kind(ctx, ginitializer)};
          }
        } else {
          // TODO: This must be lifted to allow conversions.
          return {anton::expected_error, err_init_type_is_builtin(ctx, type)};
        }

        auto const initializer =
          static_cast<ast::Basic_Initializer const*>(ginitializer);
        RETURN_ON_FAIL(analyse_expression, ctx, symtable,
                       initializer->expression);
      }

      // Do a hand-wavy "overload" resolution of the init expression.
      // TODO: This ^.
      //
      // Vector construction
      // ===================
      // Fields without an initializer provided will be defaulted to 0.
      //
      // 1. vecX() - construct vector with all elements set to 0.
      // 2. vecX(x), x is scalar - construct vector with all elements set to x.
      // 3. vecX(vecY), X < Y
      // 4. vecX(yn...), total size of all yi <= X, yi may be vectors or
      //     scalars.
      //
      // Matrix construction
      // ==================
      // Missing columns/elements will be initialized from an identity matrix
      // of the target size.
      //
      // 1. matX() - construct identity matrix.
      // 2. matX(x), x is scalar - construct matrix with diagonal elements set
      //      to x.
      // 3. matX(matY)
      // 4. matYxZ(xn...), xi are scalars, n == X * Z - construct with
      //      elements set to xi in column order.
      // 5. matYxZ(vn...), vi are vectors of size Z, n == Y - construct with
      //      column vectors vi.
    } break;

    case ast::Type_Kind::type_array: {
      return {anton::expected_error,
              err_unimplemented(ctx, expr->source_info, __FILE__, __LINE__)};

      // auto const type = static_cast<ast::Type_Array*>(expr->type);
      // for(ast::Initializer* const ginitializer: expr->initializers) {
      //   RETURN_ON_FAIL(analyse_expr_init_type_array, ctx, symtable, type,
      //                  ginitializer);
      // }

      // TODO: Deduplicate initializers.
    } break;
    }

    expr->evaluated_type = expr->type;

    return anton::expected_value;
  }

  anton::Expected<void, Error> analyse_expression(Context& ctx,
                                                  Symbol_Table& symtable,
                                                  ast::Expr* const expression)
  {
    switch(expression->node_kind) {
    case ast::Node_Kind::expr_default:
      // Nothing.
      return anton::expected_value;

    case ast::Node_Kind::lt_bool: {
      auto const type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
      expression->evaluated_type = type;
      return anton::expected_value;
    }

    case ast::Node_Kind::lt_integer: {
      auto const lt = static_cast<ast::Lt_Integer*>(expression);
      switch(lt->kind) {
      case ast::Lt_Integer_Kind::i32: {
        auto const type = get_builtin_type(ast::Type_Builtin_Kind::e_int);
        expression->evaluated_type = type;
        return anton::expected_value;
      }

      case ast::Lt_Integer_Kind::u32: {
        auto const type = get_builtin_type(ast::Type_Builtin_Kind::e_uint);
        expression->evaluated_type = type;
        return anton::expected_value;
      }
      }
    }

    case ast::Node_Kind::lt_float: {
      auto const lt = static_cast<ast::Lt_Float const*>(expression);
      switch(lt->kind) {
      case ast::Lt_Float_Kind::f32: {
        auto const type = get_builtin_type(ast::Type_Builtin_Kind::e_float);
        expression->evaluated_type = type;
        return anton::expected_value;
      }

      case ast::Lt_Float_Kind::f64: {
        auto const type = get_builtin_type(ast::Type_Builtin_Kind::e_double);
        expression->evaluated_type = type;
        return anton::expected_value;
      }
      }
    }

    case ast::Node_Kind::expr_identifier: {
      auto const expr = static_cast<ast::Expr_Identifier*>(expression);
      return analyse_expr_identifier(ctx, symtable, expr);
    }

    case ast::Node_Kind::expr_call: {
      auto const expr = static_cast<ast::Expr_Call*>(expression);
      return analyse_expr_call(ctx, symtable, expr);
    }

    case ast::Node_Kind::expr_init: {
      auto const expr = static_cast<ast::Expr_Init*>(expression);
      return analyse_expr_init(ctx, symtable, expr);
    }

    case ast::Node_Kind::expr_field: {
      auto const expr = static_cast<ast::Expr_Field*>(expression);
      return analyse_expr_field(ctx, symtable, expr);
    }

    case ast::Node_Kind::expr_index: {
      auto const expr = static_cast<ast::Expr_Index*>(expression);
      return analyse_expr_index(ctx, symtable, expr);
    }

    case ast::Node_Kind::expr_if: {
      auto const expr = static_cast<ast::Expr_If*>(expression);
      return analyse_expr_if(ctx, symtable, expr);
    }

    case ast::Node_Kind::expr_reinterpret: {
      // TODO: Implement once we implement transform for reinterpret.
      return anton::expected_value;
    }

    default:
      ANTON_UNREACHABLE("invalid node kind");
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_statements(Context& ctx, Symbol_Table& symtable,
                     ast::Node_List const statements, Sema_Context semactx);

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_variable(Context& ctx, Symbol_Table& symtable,
                   ast::Variable* const node)
  {
    RETURN_ON_FAIL(namebind_type, ctx, symtable, node->type);

    if(ast::is_unsized_array(*node->type)) {
      return {anton::expected_error,
              err_variable_type_unsized_array(ctx, node)};
    }

    if(ast::is_opaque_type(*node->type)) {
      return {anton::expected_error, err_variable_type_opaque(ctx, node)};
    }

    // We first check the initializer, then add the symbol for the variable,
    // so that if the initializer uses the variable, an error is reported.
    if(node->initializer != nullptr) {
      RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->initializer);
      auto const initializer_type = node->initializer->evaluated_type;
      if(!is_convertible(node->type, initializer_type)) {
        return {anton::expected_error,
                err_cannot_convert_type(ctx, node->initializer->source_info,
                                        node->type, initializer_type)};
      }
    } else {
      // Immutable variables must have an initializer.
      bool const immutable = !node->type->qualifiers.mut;
      if(immutable) {
        return {
          anton::expected_error,
          err_immutable_variable_missing_initializer(ctx, node->source_info)};
      }
    }

    RETURN_ON_FAIL(add_symbol, ctx, symtable,
                   Symbol(node->identifier.value, node));

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_lvalue_vector_field(Context& ctx, ast::Expr_Field const* const expr)
  {
    if(instanceof<ast::Expr_Field>(expr->base)) {
      auto const base = static_cast<ast::Expr_Field const*>(expr->base);
      RETURN_ON_FAIL(analyse_lvalue_vector_field, ctx, base);
    }

    auto const base_type = expr->base->evaluated_type;
    if(is_vector(*base_type)) {
      anton::String_View const field = expr->field.value;
      i64 const vector_size = ast::get_vector_size(*base_type);
      if(field.size_bytes() > vector_size) {
        return {anton::expected_error,
                err_vector_lvalue_swizzle_overlong(ctx, expr)};
      }

      for(auto b = field.bytes_begin(), e = field.bytes_end(); b != e; ++b) {
        for(auto i = b + 1; i != e; ++i) {
          if(*b == *i) {
            return {anton::expected_error,
                    err_vector_lvalue_swizzle_duplicate_components(ctx, expr)};
          }
        }
      }
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_assignment(Context& ctx, Symbol_Table& symtable,
                          ast::Stmt_Assignment* const node)
  {
    // Analyse the LHS as an lvalue.
    switch(node->lhs->node_kind) {
    case ast::Node_Kind::expr_identifier: {
      auto const expr = static_cast<ast::Expr_Identifier*>(node->lhs);
      RETURN_ON_FAIL(analyse_expr_identifier, ctx, symtable, expr);
    } break;

    case ast::Node_Kind::expr_field: {
      auto const expr = static_cast<ast::Expr_Field*>(node->lhs);
      RETURN_ON_FAIL(analyse_expr_field, ctx, symtable, expr);
      RETURN_ON_FAIL(analyse_lvalue_vector_field, ctx, expr);
    } break;

    case ast::Node_Kind::expr_index: {
      auto const expr = static_cast<ast::Expr_Index*>(node->lhs);
      RETURN_ON_FAIL(analyse_expr_index, ctx, symtable, expr);
    } break;

    case ast::Node_Kind::expr_default:
      return {anton::expected_error,
              err_expr_default_not_lvalue(
                ctx, static_cast<ast::Expr_Default*>(node->lhs))};

    case ast::Node_Kind::lt_bool:
      return {
        anton::expected_error,
        err_lt_bool_not_lvalue(ctx, static_cast<ast::Lt_Bool*>(node->lhs))};

    case ast::Node_Kind::lt_integer:
      return {anton::expected_error,
              err_lt_integer_not_lvalue(
                ctx, static_cast<ast::Lt_Integer*>(node->lhs))};

    case ast::Node_Kind::lt_float:
      return {
        anton::expected_error,
        err_lt_float_not_lvalue(ctx, static_cast<ast::Lt_Float*>(node->lhs))};

    case ast::Node_Kind::expr_call:
      return {
        anton::expected_error,
        err_expr_call_not_lvalue(ctx, static_cast<ast::Expr_Call*>(node->lhs))};

    case ast::Node_Kind::expr_init:
      return {
        anton::expected_error,
        err_expr_init_not_lvalue(ctx, static_cast<ast::Expr_Init*>(node->lhs))};

    case ast::Node_Kind::expr_if:
      return {
        anton::expected_error,
        err_expr_if_not_lvalue(ctx, static_cast<ast::Expr_If*>(node->lhs))};

    case ast::Node_Kind::expr_reinterpret:
    default:
      ANTON_UNREACHABLE("invalid node kind");
    }

    ast::Type const* const lhs_type = node->lhs->evaluated_type;
    if(ast::is_opaque_type(*lhs_type)) {
      return {anton::expected_error, err_opaque_type_non_assignable(ctx, node)};
    }

    bool const lhs_immutable = !lhs_type->qualifiers.mut;
    if(lhs_immutable) {
      return {anton::expected_error, err_assignment_to_immutable(ctx, node)};
    }

    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->rhs);

    bool const arithmetic_assignment =
      node->kind != ast::Assignment_Kind::e_assign;
    if(arithmetic_assignment && !ast::is_arithmetic_type(*lhs_type)) {
      return {anton::expected_error,
              err_arithmetic_assignment_to_non_arithmetic_type(ctx, node)};
    }

    // TODO: Not all arithmetic operations are allowed on all types. Do some
    //       sort of overload resolution?

    ast::Type const* const rhs_type = node->rhs->evaluated_type;
    if(is_convertible(lhs_type, rhs_type)) {
      return anton::expected_value;
    } else {
      return {anton::expected_error,
              err_no_assignment_operator(ctx, rhs_type, lhs_type, node)};
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_if(Context& ctx, Symbol_Table& symtable,
                  ast::Stmt_If* const node, Sema_Context semactx)
  {
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->condition);
    auto const condition_type = node->condition->evaluated_type;
    auto const bool_type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
    if(!is_convertible(bool_type, condition_type)) {
      return {anton::expected_error, err_condition_not_of_bool_type(
                                       ctx, node->condition, condition_type)};
    }

    RETURN_ON_FAIL(analyse_statements, ctx, symtable, node->then_branch,
                   semactx);
    RETURN_ON_FAIL(analyse_statements, ctx, symtable, node->else_branch,
                   semactx);
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_for(Context& ctx, Symbol_Table& symtable,
                   ast::Stmt_For* const node, Sema_Context semactx)
  {
    for(ast::Node* const declaration: node->declarations) {
      auto const variable = static_cast<ast::Variable*>(declaration);
      RETURN_ON_FAIL(analyse_variable, ctx, symtable, variable);
    }

    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->condition);
    auto const condition_type = node->condition->evaluated_type;
    auto const bool_type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
    if(!is_convertible(bool_type, condition_type)) {
      return {anton::expected_error, err_condition_not_of_bool_type(
                                       ctx, node->condition, condition_type)};
    }

    for(ast::Expr* const action: node->actions) {
      RETURN_ON_FAIL(analyse_expression, ctx, symtable, action);
    }

    semactx.stmt = Stmt_Ctx::e_loop;
    RETURN_ON_FAIL(analyse_statements, ctx, symtable, node->statements,
                   semactx);
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->condition);
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_while(Context& ctx, Symbol_Table& symtable,
                     ast::Stmt_While* const node, Sema_Context semactx)
  {
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->condition);
    auto const condition_type = node->condition->evaluated_type;
    auto const bool_type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
    if(!is_convertible(bool_type, condition_type)) {
      return {anton::expected_error, err_condition_not_of_bool_type(
                                       ctx, node->condition, condition_type)};
    }

    semactx.stmt = Stmt_Ctx::e_loop;
    RETURN_ON_FAIL(analyse_statements, ctx, symtable, node->statements,
                   semactx);
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_do_while(Context& ctx, Symbol_Table& symtable,
                        ast::Stmt_Do_While* const node, Sema_Context semactx)
  {
    semactx.stmt = Stmt_Ctx::e_loop;
    RETURN_ON_FAIL(analyse_statements, ctx, symtable, node->statements,
                   semactx);

    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->condition);
    auto const condition_type = node->condition->evaluated_type;
    auto const bool_type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
    if(!is_convertible(bool_type, condition_type)) {
      return {anton::expected_error, err_condition_not_of_bool_type(
                                       ctx, node->condition, condition_type)};
    }
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_switch(Context& ctx, Symbol_Table& symtable,
                      ast::Stmt_Switch* const node, Sema_Context semactx)
  {
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->expression);
    auto const expression_type = node->expression->evaluated_type;
    auto const i32_type = get_builtin_type(ast::Type_Builtin_Kind::e_int);
    auto const u32_type = get_builtin_type(ast::Type_Builtin_Kind::e_uint);
    if(!compare_types_equal(*expression_type, *i32_type) &&
       !compare_types_equal(*expression_type, *u32_type)) {
      return {anton::expected_error, err_condition_not_of_bool_type(
                                       ctx, node->expression, expression_type)};
    }

    semactx.stmt = Stmt_Ctx::e_switch;
    ast::Expr const* default_label = nullptr;
    anton::Flat_Hash_Map<u32, ast::Lt_Integer*> labels{ctx.allocator};
    for(ast::Switch_Arm* const arm: node->arms) {
      for(ast::Expr* const label: arm->labels) {
        RETURN_ON_FAIL(analyse_expression, ctx, symtable, label);
        if(label->node_kind == ast::Node_Kind::expr_default) {
          arm->has_default = true;
          // Ensure the default label is unique.
          if(default_label == nullptr) {
            default_label = label;
          } else {
            return {anton::expected_error,
                    err_duplicate_default_label(ctx, default_label->source_info,
                                                label->source_info)};
          }
        } else if(label->node_kind == ast::Node_Kind::lt_integer) {
          auto const lt = static_cast<ast::Lt_Integer*>(label);
          u32 const value = ast::get_lt_integer_value_as_u32(*lt);
          auto const it = labels.find(value);
          if(it == labels.end()) {
            labels.emplace(value, lt);
          } else {
            Source_Info const& src1 = it->value->source_info;
            Source_Info const& src2 = lt->source_info;
            return {anton::expected_error,
                    err_duplicate_label(ctx, src1, src2)};
          }
        } else {
          Source_Info const& src = label->source_info;
          return {anton::expected_error,
                  err_invalid_switch_arm_expression(ctx, src)};
        }
      }

      RETURN_ON_FAIL(analyse_statements, ctx, symtable, arm->statements,
                     semactx);
    }
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_return(Context& ctx, Symbol_Table& symtable,
                      ast::Stmt_Return* const node)
  {
    if(node->expression) {
      RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->expression);
    }
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stmt_expression(Context& ctx, Symbol_Table& symtable,
                          ast::Stmt_Expression* const node)
  {
    RETURN_ON_FAIL(analyse_expression, ctx, symtable, node->expression);
    return anton::expected_value;
  }

  anton::Expected<void, Error>
  analyse_statements(Context& ctx, Symbol_Table& symtable,
                     ast::Node_List const statements, Sema_Context semactx)
  {
    // We push a new scope and pop it only at the end of the function. We do not
    // pop the scope when we fail because an error always leads to termination.
    symtable.push_scope();

    for(ast::Node* const stmt: statements) {
      switch(stmt->node_kind) {
      case ast::Node_Kind::variable: {
        auto const node = static_cast<ast::Variable*>(stmt);
        RETURN_ON_FAIL(analyse_variable, ctx, symtable, node);
      } break;

      case ast::Node_Kind::stmt_block: {
        auto const node = static_cast<ast::Stmt_Block*>(stmt);
        RETURN_ON_FAIL(analyse_statements, ctx, symtable, node->statements,
                       semactx);
      } break;

      case ast::Node_Kind::stmt_assignment: {
        auto const node = static_cast<ast::Stmt_Assignment*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_assignment, ctx, symtable, node);
      } break;

      case ast::Node_Kind::stmt_if: {
        auto const node = static_cast<ast::Stmt_If*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_if, ctx, symtable, node, semactx);
      } break;

      case ast::Node_Kind::stmt_for: {
        auto const node = static_cast<ast::Stmt_For*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_for, ctx, symtable, node, semactx);
      } break;

      case ast::Node_Kind::stmt_while: {
        auto const node = static_cast<ast::Stmt_While*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_while, ctx, symtable, node, semactx);
      } break;

      case ast::Node_Kind::stmt_do_while: {
        auto const node = static_cast<ast::Stmt_Do_While*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_do_while, ctx, symtable, node, semactx);
      } break;

      case ast::Node_Kind::stmt_switch: {
        auto const node = static_cast<ast::Stmt_Switch*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_switch, ctx, symtable, node, semactx);
      } break;

      case ast::Node_Kind::stmt_return: {
        auto const node = static_cast<ast::Stmt_Return*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_return, ctx, symtable, node);
      } break;

      case ast::Node_Kind::stmt_expression: {
        auto const node = static_cast<ast::Stmt_Expression*>(stmt);
        RETURN_ON_FAIL(analyse_stmt_expression, ctx, symtable, node);
      } break;

      case ast::Node_Kind::stmt_break: {
        if(semactx.stmt != Stmt_Ctx::e_loop) {
          return {anton::expected_error,
                  err_break_used_outside_loop(ctx, stmt->source_info)};
        }
      } break;

      case ast::Node_Kind::stmt_continue: {
        if(semactx.stmt != Stmt_Ctx::e_loop) {
          return {anton::expected_error,
                  err_continue_used_outside_loop(ctx, stmt->source_info)};
        }
      } break;

      case ast::Node_Kind::stmt_discard: {
        // TODO: We need to validate that the only reaching path of any discard
        //       is through a fragment shader.

        // if(semactx.stage != Stage_Kind::fragment) {
        //   return {anton::expected_error,
        //           err_discard_outside_fragment(ctx, stmt->source_info)};
        // }
      } break;

      default:
        break;
      }
    }

    symtable.pop_scope();
    return anton::expected_value;
  }

  // analyse_struct_field_type
  // Validate that a type is a complete type, i.e. not opaque, and is not a
  // recursive definition.
  //
  [[nodiscard]] static anton::Expected<void, Error>
  analyse_struct_field_type(Context const& ctx, ast::Type const& type,
                            ast::Identifier const& struct_identifier)
  {
    switch(type.type_kind) {
    case ast::Type_Kind::type_builtin: {
      if(ast::is_opaque_type(type)) {
        return {anton::expected_error,
                err_opaque_type_in_struct(ctx, type.source_info)};
      }

      return anton::expected_value;
    }

    case ast::Type_Kind::type_struct: {
      auto t = static_cast<ast::Type_Struct const&>(type);
      if(t.value == struct_identifier.value) {
        return {anton::expected_error,
                err_recursive_type_definition(
                  ctx, struct_identifier.source_info, t.source_info)};
      }

      return anton::expected_value;
    }

    case ast::Type_Kind::type_array: {
      auto t = static_cast<ast::Type_Array const&>(type);
      return analyse_struct_field_type(ctx, *t.base, struct_identifier);
    }
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_struct(Context& ctx, Symbol_Table& symtable,
                 ast::Decl_Struct* const dstruct)
  {
    if(dstruct->fields.size() == 0) {
      return {anton::expected_error,
              err_empty_struct(ctx, dstruct->identifier.source_info)};
    }

    anton::Flat_Hash_Map<anton::String_View, ast::Identifier>
      member_identifiers;
    for(ast::Struct_Field* const field: dstruct->fields) {
      RETURN_ON_FAIL(namebind_type, ctx, symtable, field->type);
      RETURN_ON_FAIL(analyse_struct_field_type, ctx, *field->type,
                     dstruct->identifier);
      // Validate out duplicate names.
      auto iter = member_identifiers.find(field->identifier.value);
      if(iter != member_identifiers.end()) {
        return {anton::expected_error,
                err_duplicate_struct_field(ctx, iter->value.source_info,
                                           field->identifier.source_info)};
      } else {
        member_identifiers.emplace(field->identifier.value, field->identifier);
      }
    }

    // TODO: Attributes and initializers.
    // noperspective, flat, smooth, invariant

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_buffer(Context& ctx, Symbol_Table& symtable,
                 ast::Decl_Buffer* const buffer)
  {
    // TODO: Validate out duplicate buffer attributes.

    if(buffer->fields.size() == 0) {
      // TODO: Error.
      return {anton::expected_error,
              err_unimplemented(ctx, buffer->identifier.source_info, __FILE__,
                                __LINE__)};
    }

    anton::Flat_Hash_Map<anton::String_View, ast::Identifier> field_identifiers;
    for(ast::Buffer_Field* const field: buffer->fields) {
      RETURN_ON_FAIL(namebind_type, ctx, symtable, field->type);
      // Validate out duplicate names.
      auto iter = field_identifiers.find(field->identifier.value);
      if(iter != field_identifiers.end()) {
        return {
          anton::expected_error,
          err_unimplemented(ctx, iter->value.source_info, __FILE__, __LINE__)};
      } else {
        field_identifiers.emplace(field->identifier.value, field->identifier);
      }
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_function(Context& ctx, Symbol_Table& symtable,
                   ast::Decl_Function* const fn)
  {
    // Validate attributes. Currently there are no attributes that are not allowed on ordinary functions.
    for(ast::Attribute const* const attribute: fn->attributes) {
      return {anton::expected_error,
              err_illegal_attribute(ctx, attribute->identifier.source_info)};
    }

    // We do not defcheck the identifier of the function because it has already
    // been added as an overloaded function.

    // Push a new scope for the function body and parameters.
    symtable.push_scope();
    // Validate parameters:
    // - only ordinary parameters are allowed.
    for(ast::Fn_Parameter* const parameter: fn->parameters) {
      RETURN_ON_FAIL(namebind_type, ctx, symtable, parameter->type);
      RETURN_ON_FAIL(add_symbol, ctx, symtable,
                     Symbol(parameter->identifier.value, parameter));
      if(ast::is_sourced_parameter(*parameter)) {
        return {anton::expected_error, err_fn_sourced_parameter_not_allowed(
                                         ctx, parameter->source_info)};
      }
    }

    // Validate the return type:
    // - if the type is an array, it must be sized.
    RETURN_ON_FAIL(namebind_type, ctx, symtable, fn->return_type);
    RETURN_ON_FAIL(check_array_is_sized, ctx, fn->return_type);

    Sema_Context semactx{Stmt_Ctx::e_none};
    RETURN_ON_FAIL(analyse_statements, ctx, symtable, fn->body, semactx);

    symtable.pop_scope();
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_parameter_source(Context& ctx, Symbol_Table& symtable,
                           ast::Fn_Parameter* const p)
  {
    if(!ast::is_sourced_parameter(*p)) {
      return anton::expected_value;
    }

    if(ast::is_input_parameter(*p)) {
      return anton::expected_value;
    }

    if(ast::is_output_parameter(*p)) {
      return anton::expected_value;
    }

    if(ast::is_image_parameter(*p)) {
      return anton::expected_value;
    }

    anton::String_View const source = p->source.value;
    Symbol const* const symbol = symtable.find_entry(source);
    if(!symbol) {
      return {anton::expected_error,
              err_undefined_symbol(ctx, p->source.source_info)};
    }

    if(symbol->kind != Symbol_Kind::e_buffer) {
      switch(symbol->kind) {
      case Symbol_Kind::e_buffer:
        // The above condition ensures that this case is unreachable, however,
        // clang (and possibly others) cannot figure that out and complain about
        // unhandled enumeration.
        ANTON_UNREACHABLE("unreachable");

      case Symbol_Kind::e_variable:
      case Symbol_Kind::e_parameter:
      case Symbol_Kind::e_overload_group:
      case Symbol_Kind::e_struct:
      case Symbol_Kind::e_namespace:
        // TODO: Error.
        return {
          anton::expected_error,
          err_unimplemented(ctx, p->source.source_info, __FILE__, __LINE__)};
      }
    }

    p->buffer = symbol->value_buffer;

    // Verify that a field with the given name and type exists.
    auto const field_iter = anton::find_if(
      p->buffer->fields.begin(), p->buffer->fields.end(),
      [identifier = p->identifier.value](ast::Buffer_Field const* const field) {
        return field->identifier.value == identifier;
      });
    if(field_iter == p->buffer->fields.end()) {
      // TODO: Error.
      return {
        anton::expected_error,
        err_unimplemented(ctx, p->identifier.source_info, __FILE__, __LINE__)};
    }

    ast::Buffer_Field const* const field = *field_iter;
    if(!ast::compare_types_equal(*field->type, *p->type)) {
      // TODO: Error.
      return {anton::expected_error,
              err_unimplemented(ctx, p->type->source_info, __FILE__, __LINE__)};
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  analyse_stage_function(Context& ctx, Symbol_Table& symtable,
                         ast::Decl_Stage_Function* const fn)
  {
    // Validate attributes:
    // - compute stage might have the workgroup attribute (at most 1).
    // - other stages must not have any attributes.
    switch(fn->stage.value) {
    case Stage_Kind::compute: {
      ast::Attribute const* workgroup = nullptr;
      for(ast::Attribute const* const attribute: fn->attributes) {
        if(attribute->identifier.value == "workgroup"_sv) {
          if(!workgroup) {
            workgroup = attribute;
          } else {
            return {anton::expected_error,
                    err_duplicate_attribute(ctx,
                                            workgroup->identifier.source_info,
                                            attribute->identifier.source_info)};
          }
        } else {
          return {
            anton::expected_error,
            err_illegal_attribute(ctx, attribute->identifier.source_info)};
        }
      }
    } break;

    default: {
      for(ast::Attribute const* const attribute: fn->attributes) {
        return {anton::expected_error,
                err_illegal_attribute(ctx, attribute->source_info)};
      }
    } break;
    }

    // Push a new scope for the function body and parameters.
    symtable.push_scope();
    // We use the namespace's symtable for sources of the parameters.
    Symbol const* const namespace_symbol = symtable.find_entry(fn->pass.value);
    ANTON_ASSERT(namespace_symbol != nullptr,
                 "pass namespace not in symbol table");
    ANTON_ASSERT(namespace_symbol->kind == Symbol_Kind::e_namespace,
                 "symbol for namespace is not namespace");
    Symbol_Table& ns_symtable = namespace_symbol->value_namespace->symtable;
    // Validate parameters:
    // - vertex: input, output and sourced parameters.
    // - fragment: input, output and sourced parameters.
    // - compute: only sourced parameters are allowed.
    for(ast::Fn_Parameter* const parameter: fn->parameters) {
      RETURN_ON_FAIL(namebind_type, ctx, symtable, parameter->type);
      RETURN_ON_FAIL(add_symbol, ctx, symtable,
                     Symbol(parameter->identifier.value, parameter));
      switch(fn->stage.value) {
      case Stage_Kind::vertex: {
        if(!ast::is_sourced_parameter(*parameter)) {
          return {anton::expected_error,
                  err_vertex_ordinary_parameter_not_allowed(
                    ctx, parameter->source_info)};
        }

      } break;

      case Stage_Kind::fragment: {
        if(!ast::is_sourced_parameter(*parameter)) {
          return {anton::expected_error,
                  err_fragment_ordinary_parameter_not_allowed(
                    ctx, parameter->source_info)};
        }
      } break;

      case Stage_Kind::compute: {
        if(!ast::is_sourced_parameter(*parameter)) {
          return {anton::expected_error,
                  err_compute_ordinary_parameter_not_allowed(
                    ctx, parameter->source_info)};
        }

        if(ast::is_input_parameter(*parameter)) {
          return {anton::expected_error,
                  err_compute_input_not_allowed(ctx, parameter->source_info)};
        } else if(ast::is_output_parameter(*parameter)) {
          return {anton::expected_error,
                  err_compute_output_not_allowed(ctx, parameter->source_info)};
        }
      } break;
      }

      if(is_input_parameter(*parameter)) {
        if(is_opaque_type(*parameter->type)) {
          return {anton::expected_error, err_input_must_not_be_opaque(
                                           ctx, parameter->type->source_info)};
        }

        if(parameter->type->type_kind == ast::Type_Kind::type_array) {
          return {anton::expected_error, err_input_must_not_be_array(
                                           ctx, parameter->type->source_info)};
        }
      } else if(is_output_parameter(*parameter)) {
        if(is_opaque_type(*parameter->type)) {
          return {anton::expected_error, err_output_must_not_be_opaque(
                                           ctx, parameter->type->source_info)};
        }

        if(parameter->type->type_kind == ast::Type_Kind::type_array) {
          return {anton::expected_error, err_output_must_not_be_array(
                                           ctx, parameter->type->source_info)};
        }
      } else if(is_image_parameter(*parameter)) {
        if(!is_image_type(*parameter->type)) {
          return {anton::expected_error,
                  err_image_parameter_not_image(ctx, *parameter)};
        }
      }

      RETURN_ON_FAIL(analyse_parameter_source, ctx, ns_symtable, parameter);
    }

    Sema_Context semactx{Stmt_Ctx::e_none};
    RETURN_ON_FAIL(analyse_statements, ctx, symtable, fn->body, semactx);

    symtable.pop_scope();
    return anton::expected_value;
  }

  // analyse_new_overload
  // Verify that a function, if added to an overload group, will not cause
  // errors, that is it complies with the overload rules.
  //
  [[nodiscard]] static anton::Expected<void, Error>
  analyse_new_overload(Context& ctx, ast::Overload_Group const* const group,
                       ast::Decl_Function const* const fn)
  {
    for(ast::Decl_Function const* const overload: group->overloads) {
      if(overload->parameters.size() != fn->parameters.size()) {
        continue;
      }

      bool identical = true;
      for(auto const [p1, p2]:
          anton::zip(overload->parameters, fn->parameters)) {
        if(!ast::compare_types_equal(*p1->type, *p2->type)) {
          identical = false;
          break;
        }
      }

      if(identical) {
        if(!ast::compare_types_equal(*overload->return_type,
                                     *fn->return_type)) {
          return {anton::expected_error,
                  err_overload_on_return_type(
                    ctx, overload->identifier.source_info,
                    overload->return_type->source_info,
                    fn->identifier.source_info, fn->return_type->source_info)};
        } else {
          return {anton::expected_error,
                  err_symbol_redefinition(ctx, overload->identifier.source_info,
                                          fn->identifier.source_info)};
        }
      }
    }
    return anton::expected_value;
  }

  anton::Expected<void, Error> run_sema(Context& ctx, ast::Node_List const ast)
  {
    // There is yet no support for struct member initializers, however, for
    // future considerations, validating structs and constants separately
    // prevents us from properly validating both. Currently we validate structs
    // first as that seems to be the best solution giving us the option to
    // conduct the most thorough analysis.

    // Add the builtin overload groups, then traverse the AST in order to report
    // errors correctly. As we traverse we will encounter functions. We either
    // add them to an existing overload group or create a new overload group and
    // check the symbol.

    anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*> groups =
      get_builtin_functions_declarations(ctx.allocator);

    Symbol_Table symtable(ctx.allocator);
    symtable.push_scope();
    // There are no symbols in the symbol table at this point and the builtin
    // function groups are unique, hence we do not have to check for symbol
    // redefinitions.
    for(auto [key, group]: groups) {
      symtable.add_entry(key, Symbol(key, group));
    }

    // Populate the symbol table with global symbols.
    for(ast::Node* const decl: ast) {
      switch(decl->node_kind) {
      case ast::Node_Kind::variable: {
        ast::Variable* const node = static_cast<ast::Variable*>(decl);
        RETURN_ON_FAIL(add_symbol, ctx, symtable,
                       Symbol(node->identifier.value, node));
      } break;

      case ast::Node_Kind::decl_struct: {
        ast::Decl_Struct* const node = static_cast<ast::Decl_Struct*>(decl);
        RETURN_ON_FAIL(add_symbol, ctx, symtable,
                       Symbol(node->identifier.value, node));
      } break;

      case ast::Node_Kind::decl_buffer: {
        auto const node = static_cast<ast::Decl_Buffer*>(decl);
        ast::Identifier const pass = node->pass;
        Symbol const* ns = symtable.find_entry(pass.value);
        if(!ns) {
          auto const pns =
            VUSH_ALLOCATE(Namespace, ctx.allocator, ctx.allocator, pass);
          ns = symtable.add_entry(pass.value, Symbol(pass.value, pns));
        } else {
          if(ns->kind != Symbol_Kind::e_namespace) {
            // TODO: Error.
            return {
              anton::expected_error,
              err_unimplemented(ctx, pass.source_info, __FILE__, __LINE__)};
          }
        }
        RETURN_ON_FAIL(add_symbol, ctx, ns->value_namespace->symtable,
                       Symbol(node->identifier.value, node));
      } break;

      case ast::Node_Kind::decl_function: {
        auto const node = static_cast<ast::Decl_Function*>(decl);
        auto const i = groups.find(node->identifier.value);
        if(i != groups.end()) {
          // Group exists, add our function to it.
          RETURN_ON_FAIL(analyse_new_overload, ctx, i->value, node);
          i->value->overloads.push_back(node);
        } else {
          // A group does not exist, hence we have to create it and check its
          // symbol for redefinition.
          auto const group =
            VUSH_ALLOCATE(ast::Overload_Group, ctx.allocator, ctx.allocator,
                          node->identifier.value);
          group->overloads.push_back(node);
          groups.emplace(node->identifier.value, group);
          RETURN_ON_FAIL(add_symbol, ctx, symtable,
                         Symbol(group->identifier, group));
        }
      } break;

      case ast::Node_Kind::decl_stage_function: {
        auto const node = static_cast<ast::Decl_Stage_Function*>(decl);
        ast::Identifier const pass = node->pass;
        // Create namespace if does not exist. We want to ensure that each stage
        // has its namespace's symbol table.
        Symbol const* ns = symtable.find_entry(pass.value);
        if(!ns) {
          auto const pns =
            VUSH_ALLOCATE(Namespace, ctx.allocator, ctx.allocator, pass);
          ns = symtable.add_entry(pass.value, Symbol(pass.value, pns));
        }
      } break;

      default:
        // Nothing.
        break;
      }
    }

    // Run the analysis.
    for(ast::Node* const node: ast) {
      switch(node->node_kind) {
      case ast::Node_Kind::variable:
        return {anton::expected_error,
                err_unimplemented(ctx, node->source_info, __FILE__, __LINE__)};
        break;

      case ast::Node_Kind::decl_struct: {
        auto const decl = static_cast<ast::Decl_Struct*>(node);
        RETURN_ON_FAIL(analyse_struct, ctx, symtable, decl);
      } break;

      case ast::Node_Kind::decl_buffer: {
        auto const decl = static_cast<ast::Decl_Buffer*>(node);
        RETURN_ON_FAIL(analyse_buffer, ctx, symtable, decl);
      } break;

      case ast::Node_Kind::decl_function: {
        auto const decl = static_cast<ast::Decl_Function*>(node);
        RETURN_ON_FAIL(analyse_function, ctx, symtable, decl);
      } break;

      case ast::Node_Kind::decl_stage_function: {
        auto const decl = static_cast<ast::Decl_Stage_Function*>(node);
        RETURN_ON_FAIL(analyse_stage_function, ctx, symtable, decl);
      } break;

      default:
        // Nothing.
        break;
      }
    }

    return anton::expected_value;
  }
} // namespace vush
