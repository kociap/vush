#pragma once

#include <anton/assert.hpp>
#include <anton/memory.hpp>
#include <anton/swap.hpp>
#include <anton/type_traits.hpp>

namespace vush {
  struct Either_Left_Tag {
    explicit Either_Left_Tag() = default;
  };

  constexpr Either_Left_Tag either_left;

  struct Either_Right_Tag {
    explicit Either_Right_Tag() = default;
  };

  constexpr Either_Right_Tag either_right;

  template<typename Left, typename Right>
  struct Either {
    static_assert(!anton::is_same<Left, Right>,
                  "Left and Right must not be the same types");

  public:
    using left_type = Left;
    using right_type = Right;

    template<typename... Args>
    Either(Either_Left_Tag, Args&&... args)
      : left_value{ANTON_FWD(args)...}, holds_left(true)
    {
    }

    template<typename... Args>
    Either(Either_Right_Tag, Args&&... args)
      : right_value{ANTON_FWD(args)...}, holds_left(false)
    {
    }

    Either(Left const& v): left_value(v), holds_left(true) {}
    Either(Left&& v): left_value(ANTON_MOV(v)), holds_left(true) {}
    Either(Right const& v): right_value(v), holds_left(false) {}
    Either(Right&& v): right_value(ANTON_MOV(v)), holds_left(false) {}

    Either(Either const& other): null_state(), holds_left(other.holds_left)
    {
      if(other.holds_left) {
        anton::construct(anton::addressof(left_value), other.left_value);
      } else {
        anton::construct(anton::addressof(right_value), other.right_value);
      }
    }

    Either(Either&& other): null_state(), holds_left(other.holds_left)
    {
      if(other.holds_left) {
        anton::construct(anton::addressof(left_value),
                         ANTON_MOV(other.left_value));
      } else {
        anton::construct(anton::addressof(right_value),
                         ANTON_MOV(other.right_value));
      }
    }

    Either& operator=(Either const& other)
    {
      if(holds_left) {
        if(other.holds_left) {
          left_value = other.left_value;
        } else {
          anton::destruct(anton::addressof(left_value));
          anton::construct(anton::addressof(right_value), other.right_value);
          holds_left = false;
        }
      } else {
        if(other.holds_left) {
          anton::destruct(anton::addressof(right_value));
          anton::construct(anton::addressof(left_value), other.left_value);
          holds_left = true;
        } else {
          right_value = other.right_value;
        }
      }

      return *this;
    }

    Either& operator=(Either&& other)
    {
      if(holds_left) {
        if(other.holds_left) {
          left_value = ANTON_MOV(other.left_value);
        } else {
          anton::destruct(anton::addressof(left_value));
          anton::construct(anton::addressof(right_value),
                           ANTON_MOV(other.right_value));
          holds_left = false;
        }
      } else {
        if(other.holds_left) {
          anton::destruct(anton::addressof(right_value));
          anton::construct(anton::addressof(left_value),
                           ANTON_MOV(other.left_value));
          holds_left = true;
        } else {
          right_value = ANTON_MOV(other.right_value);
        }
      }

      return *this;
    }

    ~Either()
    {
      if(holds_left) {
        left_value.~Left();
      } else {
        right_value.~Right();
      }
    }

    [[nodiscard]] bool is_left() const
    {
      return holds_left;
    }

    [[nodiscard]] bool is_right() const
    {
      return !holds_left;
    }

    [[nodiscard]] Left& left() &
    {
      ANTON_ASSERT(
        holds_left,
        u8"cannot call left() on Either that does not hold a left value");
      return left_value;
    }

    [[nodiscard]] Left const& left() const&
    {
      ANTON_ASSERT(
        holds_left,
        u8"cannot call left() on Either that does not hold a left value");
      return left_value;
    }

    [[nodiscard]] Left&& left() &&
    {
      ANTON_ASSERT(
        holds_left,
        u8"cannot call left() on Either that does not hold a left value");
      return left_value;
    }

    [[nodiscard]] Left const&& left() const&&
    {
      ANTON_ASSERT(
        holds_left,
        u8"cannot call left() on Either that does not hold a left value");
      return left_value;
    }

    [[nodiscard]] Right& right() &
    {
      ANTON_ASSERT(
        !holds_left,
        u8"cannot call right() on Either that does not hold a right value");
      return right_value;
    }

    [[nodiscard]] Right const& right() const&
    {
      ANTON_ASSERT(
        !holds_left,
        u8"cannot call right() on Either that does not hold a right value");
      return right_value;
    }

    [[nodiscard]] Right&& right() &&
    {
      ANTON_ASSERT(
        !holds_left,
        u8"cannot call right() on Either that does not hold a right value");
      return right_value;
    }

    [[nodiscard]] Right const&& right() const&&
    {
      ANTON_ASSERT(
        !holds_left,
        u8"cannot call right() on Either that does not hold a right value");
      return right_value;
    }

    friend void swap(Either& lhs, Either& rhs)
    {
      using anton::swap;
      if(lhs.holds_left) {
        if(rhs.holds_left) {
          swap(lhs.left_value, rhs.left_value);
        } else {
          anton::construct(anton::addressof(rhs.left_value),
                           ANTON_MOV(lhs.left_value));
          anton::destruct(anton::addressof(lhs.left_value));
          anton::construct(anton::addressof(lhs.right_value),
                           ANTON_MOV(rhs.right_value));
          anton::destruct(anton::addressof(rhs.right_value));
          anton::swap(lhs.holds_left, rhs.holds_left);
        }
      } else {
        if(rhs.holds_left) {
          anton::construct(anton::addressof(rhs.right_value),
                           ANTON_MOV(lhs.right_value));
          anton::destruct(anton::addressof(lhs.right_value));
          anton::construct(anton::addressof(lhs.left_value),
                           ANTON_MOV(rhs.left_value));
          anton::destruct(anton::addressof(rhs.left_value));
          anton::swap(lhs.holds_left, rhs.holds_left);
        } else {
          swap(lhs.right_value, rhs.right_value);
        }
      }
    }

  private:
    union {
      Left left_value;
      Right right_value;
      bool null_state;
    };
    bool holds_left;
  };
} // namespace vush
