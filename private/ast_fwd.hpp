#pragma once

#include <anton/array.hpp>
#include <owning_ptr.hpp>

namespace vush {
    struct Declaration;

    using Declaration_List = anton::Array<Owning_Ptr<Declaration>>;
} // namespace vush
