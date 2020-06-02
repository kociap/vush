#include <vush/types.hpp>

#include <new>

namespace vush {
    String::~String() {
        if(_data != nullptr) {
            ::operator delete((void*)_data, _size + 1);
        }
    }
} // namespace vush
