#pragma once

#include <vush/types.hpp>

namespace vush {
  // Arena_Allocator
  //
  struct Arena_Allocator: public Allocator {
    Arena_Allocator(i64 default_block_size = 65536, i64 default_block_alignment = 8);
    Arena_Allocator(Arena_Allocator const& allocator) = delete;
    Arena_Allocator(Arena_Allocator&& allocator);
    ~Arena_Allocator();
    Arena_Allocator& operator=(Arena_Allocator const& allocator) = delete;
    Arena_Allocator& operator=(Arena_Allocator&& allocator);

    // allocate
    //
    [[nodiscard]] ANTON_DECLSPEC_ALLOCATOR virtual void* allocate(i64 size, i64 alignment) override;

    // deallocate
    // Does nothing.
    //
    virtual void deallocate(void* memory, i64 size, i64 alignment) override;

    // is_equal
    // Compares two allocators. Two arena allocators are equal if and only if they are the same
    // object.
    //
    // Returns:
    // true if allocator is the same object as *this.
    //
    [[nodiscard]] virtual bool is_equal(Memory_Allocator const& allocator) const override;

    // reset
    // Frees all memory owned by the allocator without calling destructors and restores the
    // allocator to the default state.
    //
    void reset();

    // owned_memory
    // Obtains the total amount of memory owned by the allocator.
    //
    // Returns:
    // The amount of memory owned by the allocator in bytes.
    //
    [[nodiscard]] i64 owned_memory() const;

  private:
    struct Block {
      Block* next = nullptr;
      // Pointer to the first free location in the block.
      void* free = nullptr;
      // Pointer to the end of the block.
      void* end = nullptr;
    };

    Block* first = nullptr;
    Block* last = nullptr;
    i64 default_block_size;
    i64 default_block_alignment;
    i64 owned_memory_amount = 0;

    Block* allocate_block(i64 size, i64 alignment);
  };
} // namespace vush
