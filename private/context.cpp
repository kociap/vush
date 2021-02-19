#include <context.hpp>

#include <vush/types.hpp>

namespace vush {
    Symbol* find_symbol(Context& ctx, anton::String_View name) {
        for(i64 i = ctx.symbols.size() - 1; i >= 0; --i) {
            auto& symbol_map = ctx.symbols[i];
            auto iter = symbol_map.find(name);
            if(iter != symbol_map.end()) {
                return iter->value;
            }
        }
        return nullptr;
    }

    Symbol const* find_symbol(Context const& ctx, anton::String_View name) {
        for(i64 i = ctx.symbols.size() - 1; i >= 0; --i) {
            auto& symbol_map = ctx.symbols[i];
            auto iter = symbol_map.find(name);
            if(iter != symbol_map.end()) {
                return iter->value;
            }
        }
        return nullptr;
    }

    void add_symbol(Context& ctx, anton::String_View name, Symbol* symbol) {
        auto& symbol_map = ctx.symbols[ctx.symbols.size() - 1];
        symbol_map.emplace(name, symbol);
    }

    void push_scope(Context& ctx) {
        ctx.symbols.emplace_back();
    }

    void pop_scope(Context& ctx) {
        if(ctx.symbols.size() > 1) {
            ctx.symbols.pop_back();
        }
    }
} // namespace vush
