#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>

namespace gekko_code_gen {

//
// nth_char
//
template <std::size_t N, char... c>
struct nth_char {};

template <std::size_t N, char c0, char c1, char... c>
struct nth_char<N, c0, c1, c...> {
   static_assert(N < 2 + sizeof...(c), "Index out of bounds");
   constexpr static char val = nth_char<N - 1, c1, c...>::val;
};

template <std::size_t N, char c0>
struct nth_char<N, c0> {
   static_assert(N == 0, "Index out of bounds");
   constexpr static char val = c0;
};

template <char c0, char c1, char... c>
struct nth_char<0, c0, c1, c...> {
   constexpr static char val = c0;
};

template <char c0>
struct nth_char<0, c0> {
   constexpr static char val = c0;
};


//
// cts - compile time string
//
template <std::size_t count, char... c>
struct string_trim {};

template <std::size_t len, char... c>
struct string_include {};

template <char... c>
struct cts {
   constexpr static const char as_arr[] = { c..., '\0' };
   constexpr static bool is_empty = sizeof...(c) == 0;

   template <std::size_t off>
   using trim = typename string_trim<off, c...>::type;

   template <std::size_t off, std::size_t len>
   using substr = typename trim<off>::include<len>;

   template <std::size_t len>
   using include = typename string_include<len, c...>::type;

   template <char c0>
   using prepend = cts<c0, c...>;

   template <std::size_t idx, char... c1>
   static constexpr bool streq_impl() {
      if constexpr (idx == sizeof...(c)) {
         return true;
      } else {
         return (nth_char<idx, c...>::val == nth_char<idx, c1...>::val) &&
                (streq_impl<idx + 1, c1...>());
      }
   }

   template <char... c1>
   static constexpr bool streq(cts<c1...>) {
      if constexpr (sizeof...(c) == sizeof...(c1)) {
         return streq_impl<0, c1...>();
      } else {
         return false;
      }
   }
};

template <std::size_t i, char... c>
constexpr char get_char(cts<c...>) {
   return nth_char<i, c...>::val;
}


//
// string_trim
//
template <std::size_t off, char c0, char c1, char... c>
struct string_trim<off, c0, c1, c...> {
   using type = typename string_trim<off - 1, c1, c...>::type;
};

template <char c0, char c1, char... c>
struct string_trim<0, c0, c1, c...> {
   using type = cts<c0, c1, c...>;
};

template <std::size_t off, char c0>
struct string_trim<off, c0> {
   using type = cts<>;
};

template <char c0>
struct string_trim<0, c0> {
   using type = cts<c0>;
};

template <std::size_t off>
struct string_trim<off> {
   using type = cts<>;
};


//
// string_include
//
template <std::size_t len, char c0, char c1, char... c>
struct string_include<len, c0, c1, c...> {
   using type = typename string_include<len - 1, c1, c...>::type::prepend<c0>;
};

template <char c0, char c1, char... c>
struct string_include<0, c0, c1, c...> {
   using type = cts<>;
};

template <std::size_t len, char c0>
struct string_include<len, c0> {
   using type = cts<c0>;
};

template <char c0>
struct string_include<0, c0> {
   using type = cts<>;
};

template <std::size_t len>
struct string_include<len> {
   using type = cts<>;
};

template <typename String, size_t i = 0>
constexpr auto from_cstr() {
   if constexpr (String::str()[i] == '\0') {
      return cts<>{};
   } else {
      return typename decltype(from_cstr<String, i + 1>())::prepend<String::str()[i]>{};
   }
}


//
// atoi
//
template <uint32_t init = 0, std::size_t idx = 0, char... c>
constexpr uint32_t decimal_atoi(cts<c...>) {
   if constexpr (sizeof...(c) == idx) {
      return init;
   } else {
      constexpr char c0 = nth_char<idx, c...>::val;
      static_assert(c0 >= '0' && c0 <= '9', "Invalid decimal literal!");
      if constexpr (c0 >= '0' && c0 <= '9') {
         constexpr uint32_t cur_shifted = init * 10 + (c0 - '0');
         return decimal_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else {
         return 0;
      }
   }
}

template <uint32_t init = 0, std::size_t idx = 0, char... c>
constexpr uint32_t hex_atoi(cts<c...>) {
   if constexpr (sizeof...(c) == idx) {
      return init;
   } else {
      constexpr char c0 = nth_char<idx, c...>::val;
      static_assert((c0 >= '0' && c0 <= '9') ||
                    (c0 >= 'a' && c0 <= 'f') ||
                    (c0 >= 'A' && c0 <= 'F'), "Invalid hex literal!");
      if constexpr (c0 >= '0' && c0 <= '9') {
         constexpr uint32_t cur_shifted = init * 16 + (c0 - '0');
         return hex_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else if constexpr (c0 >= 'a' && c0 <= 'f') {
         constexpr uint32_t cur_shifted = init * 16 + (c0 - 'a' + 10);
         return hex_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else if constexpr (c0 >= 'A' && c0 <= 'F') {
         constexpr uint32_t cur_shifted = init * 16 + (c0 - 'A' + 10);
         return hex_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else {
         return 0;
      }
   }
}

template <char... c>
constexpr uint32_t atoi_full(cts<c...>) {
   constexpr bool negate = nth_char<0, c...>::val == '-';
   constexpr uint32_t initial_trim = negate ? 1 : 0;
   if constexpr (sizeof...(c) > 2) {
      constexpr auto prefix = typename cts<c...>::substr<initial_trim, 2> {};
      if constexpr (prefix.streq(cts<'0', 'x'>{})) {
         constexpr uint32_t val = hex_atoi((typename cts<c...>::trim<initial_trim + 2>) {});
         return (negate ? -val : val);
      } else {
         constexpr uint32_t val = decimal_atoi(typename cts<c...>::trim<initial_trim> {});
         return (negate ? -val : val);
      }
   } else {
      constexpr uint32_t val = decimal_atoi(typename cts<c...>::trim<initial_trim> {});
      return (negate ? -val : val);
   }
}

template <uint32_t init = 0, std::size_t idx = 0, char... c>
constexpr std::optional<uint32_t> try_decimal_atoi(cts<c...>) {
   if constexpr (sizeof...(c) == idx) {
      return init;
   } else {
      constexpr char c0 = nth_char<idx, c...>::val;
      if constexpr (c0 >= '0' && c0 <= '9') {
         constexpr uint32_t cur_shifted = init * 10 + (c0 - '0');
         return decimal_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else {
         return std::nullopt;
      }
   }
}

template <uint32_t init = 0, std::size_t idx = 0, char... c>
constexpr std::optional<uint32_t> try_hex_atoi(cts<c...>) {
   if constexpr (sizeof...(c) == idx) {
      return init;
   } else {
      constexpr char c0 = nth_char<idx, c...>::val;
      if constexpr (c0 >= '0' && c0 <= '9') {
         constexpr uint32_t cur_shifted = init * 16 + (c0 - '0');
         return hex_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else if constexpr (c0 >= 'a' && c0 <= 'f') {
         constexpr uint32_t cur_shifted = init * 16 + (c0 - 'a' + 10);
         return hex_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else if constexpr (c0 >= 'A' && c0 <= 'F') {
         constexpr uint32_t cur_shifted = init * 16 + (c0 - 'A' + 10);
         return hex_atoi<cur_shifted, idx + 1, c...>(cts<c...>{});
      } else {
         return std::nullopt;
      }
   }
}

template <char... c>
constexpr std::optional<uint32_t> try_atoi_full(cts<c...>) {
   constexpr bool negate = nth_char<0, c...>::val == '-';
   constexpr uint32_t initial_trim = negate ? 1 : 0;
   if constexpr (sizeof...(c) > 2) {
      constexpr auto prefix = typename cts<c...>::substr<initial_trim, 2> {};
      if constexpr (prefix.streq(cts<'0', 'x'>{})) {
         constexpr auto val = try_hex_atoi((typename cts<c...>::trim<initial_trim + 2>) {});
         if constexpr (val) {
            return (negate ? -(val.value()) : (val.value()));
         } else {
            return std::nullopt;
         }
      } else {
         constexpr auto val = try_decimal_atoi(typename cts<c...>::trim<initial_trim> {});
         return (negate ? -(val.value()) : (val.value()));
      }
   } else {
      constexpr auto val = try_decimal_atoi(typename cts<c...>::trim<initial_trim> {});
      return (negate ? -(val.value()) : (val.value()));
   }
}
}

#define GEN_STR(gs) \
   []() { \
      struct type_gen { static constexpr const char* str() { return gs; } }; \
      return gekko_code_gen::from_cstr<type_gen>(); \
   }()
