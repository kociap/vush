#include <filesystem.hpp>

#include <filesystem>

namespace vush::fs {
    std::string concat_paths(std::string_view const lhs, std::string_view const rhs) {
        std::filesystem::path a(lhs);
        std::filesystem::path b(rhs);
        a /= b;
        return a.generic_string();
    }

    bool exists(std::string_view const path) {
        std::filesystem::path a(path);
        return std::filesystem::exists(a);
    }
} // namespace vush