#include <context.hpp>

#include <ast2.hpp>
#include <vush/types.hpp>

namespace vush {
    Context::Context(Allocator* const allocator): allocator(allocator), source_registry(allocator), definitions(allocator) {}

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

    ast::Node const* Context::find_node_definition(ast::Node const* const node) const {
        auto result = definitions.find(node);
        if(result != definitions.end()) {
            return result->value;
        } else {
            return nullptr;
        }
    }

    void Context::add_node_definition(ast::Node const* const node, ast::Node const* const definition) {
        definitions.emplace(node, definition);
    }
} // namespace vush
