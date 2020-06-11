#include <filesystem.hpp>
#include <vush/types.hpp>

#include <filesystem>

namespace vush::fs {
    static anton::String fs_path_to_string(std::filesystem::path const& path) {
        std::string gen_str = path.generic_string();
        return {gen_str.data(), (i64)gen_str.size()};
    }

    anton::String concat_paths(anton::String_View const lhs, anton::String_View const rhs) {
        std::filesystem::path a(std::string_view(lhs.data(), lhs.size_bytes()));
        std::filesystem::path b(std::string_view(rhs.data(), rhs.size_bytes()));
        a /= b;
        return fs_path_to_string(a);
    }

    bool exists(anton::String_View const path) {
        std::filesystem::path a(std::string_view(path.data(), path.size_bytes()));
        return std::filesystem::exists(a);
    }
} // namespace vush