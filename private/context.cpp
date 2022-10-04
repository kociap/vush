#include <context.hpp>

#include <ast2.hpp>
#include <vush/types.hpp>

namespace vush {
    Symbol::Symbol(anton::String_View name, ast::Node const* node): name(name), node(node) {}

    anton::String_View Symbol::get_name() const {
        return name;
    }

    ast::Node const* Symbol::get_node() const {
        return node;
    }

    Symbol_Kind Symbol::get_kind() const {
        return node->node_kind;
    }

    Context::Context() {
        symbols.emplace_back();
    }

    Symbol const* Context::find_symbol(anton::String_View const name) const {
        for(i64 i = symbols.size() - 1; i >= 0; --i) {
            auto result = symbols[i].find(name);
            if(result != symbols[i].end()) {
                return &result->value;
            }
        }
        return nullptr;
    }

    void Context::add_symbol(Symbol const symbol) {
        auto& symbol_map = symbols.back();
        symbol_map.emplace(symbol.get_name(), symbol);
    }

    void Context::push_scope() {
        symbols.emplace_back();
    }

    void Context::pop_scope() {
        if(symbols.size() > 1) {
            symbols.pop_back();
        }
    }

    Source_Data const* Context::find_source(anton::String_View const name) const {
        auto result = source_registry.find(name);
        if(result != source_registry.end()) {
            return &result->value;
        } else {
            return nullptr;
        }
    }

    void Context::add_source(Source_Data source) {
        source_registry.emplace(source.name, ANTON_MOV(source));
    }

    ast::Node const* Context::find_definition(ast::Node const* const node) const {
        auto result = definitions.find(node);
        if(result != definitions.end()) {
            return result->value;
        } else {
            return nullptr;
        }
    }

    void Context::add_definition(ast::Node const* const node, ast::Node const* const definition) {
        definitions.emplace(node, definition);
    }
} // namespace vush
