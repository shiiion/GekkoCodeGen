#pragma once

#include "cts.hh"
#include "ctrie.hh"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <tuple>
#include <type_traits>

namespace gekko_code_gen {
enum class ParseGroupType {
   SEPARATOR,
   PARSER,
   ARGUMENT,
   TERMINATOR,
};


//
// Separators
//
struct WHITESPACE_SEPARATOR {
   constexpr static ParseGroupType p_type = ParseGroupType::SEPARATOR;

   template <char... c>
   constexpr static auto eat(cts<c...>) {
      if constexpr (sizeof...(c) == 0) {
         return cts<> {};
      } else {
         constexpr char c0 = nth_char<0, c...>::val;
         if constexpr (c0 == ' ' || c0 == '\t') {
            return eat(typename cts<c...>::trim<1> {});
         } else {
            return cts<c...> {};
         }
      }
   }

   template <std::size_t idx = 0, char... c>
   constexpr static auto clipto(cts<c...>) {
      if constexpr (sizeof...(c) == idx) {
         return cts<c...> {};
      } else {
         constexpr char chI = nth_char<idx, c...>::val;
         if constexpr (chI == ' ' || chI == '\t') {
            return typename cts<c...>::substr<0, idx> {};
         } else {
            return clipto<idx + 1>(cts<c...> {});
         }
      }
   }

   template <std::size_t idx = 0, char... c>
   constexpr static auto seekto(cts<c...>) {
      if constexpr (sizeof...(c) == idx) {
         return cts<> {};
      } else {
         constexpr char chI = nth_char<idx, c...>::val;
         if constexpr (chI == ' ' || chI == '\t') {
            return typename cts<c...>::trim<idx> {};
         } else {
            return seekto<idx + 1>(cts<c...> {});
         }
      }
   }
};

// always surrounded in 0+ whitespace chars
template <char Ec>
struct CHARACTER_SEPARATOR {
   constexpr static ParseGroupType p_type = ParseGroupType::SEPARATOR;

   template <char... c>
   constexpr static auto eat(cts<c...>) {
      constexpr auto skip0 = WHITESPACE_SEPARATOR::eat(cts<c...>{});
      static_assert(!decltype(skip0)::is_empty, "Failed to parse expected character");
      if constexpr (!decltype(skip0)::is_empty) {
         static_assert(get_char<0>(skip0) == Ec, "Failed to parse expected character");
         return WHITESPACE_SEPARATOR::eat(typename decltype(skip0)::trim<1> {});
      } else {
         return cts<> {};
      }
   }

   template <std::size_t idx = 0, char... c>
   constexpr static auto clipto(cts<c...>) {
      if constexpr (sizeof...(c) == idx) {
         return cts<c...> {};
      } else {
         constexpr char chI = nth_char<idx, c...>::val;
         if constexpr (chI == ' ' || chI == '\t' || chI == Ec) {
            return typename cts<c...>::substr<0, idx> {};
         } else {
            return clipto<idx + 1>(cts<c...> {});
         }
      }
   }

   template <std::size_t idx = 0, char... c>
   constexpr static auto seekto(cts<c...>) {
      if constexpr (sizeof...(c) == idx) {
         return cts<> {};
      } else {
         constexpr char chI = nth_char<idx, c...>::val;
         if constexpr (chI == ' ' || chI == '\t' || chI == Ec) {
            return typename cts<c...>::trim<idx> {};
         } else {
            return seekto<idx + 1>(cts<c...> {});
         }
      }
   }
};


//
// Parsers
//
template <uint32_t width>
constexpr uint32_t gen_mask() {
   if constexpr (width == 0) {
      return 0;
   } else {
      return (gen_mask<width - 1>() << 1) | 1;
   }
}

template <uint32_t val, uint32_t off, uint32_t width>
constexpr static uint32_t shift() {
   return ((val & gen_mask<width>()) << (32 - off - width));
}

template <uint32_t off, uint32_t width, char... header>
struct REG_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      if constexpr (sizeof...(c) > sizeof...(header)) {
         if constexpr (cts<header...>::streq(typename cts<c...>::substr<0, sizeof...(header)> {})) {
            constexpr auto regnum = try_decimal_atoi((typename cts<c...>::trim<sizeof...(header)>) {});
            if constexpr (regnum) {
               if constexpr (regnum.value() < (1 << width)) {
                  return shift<regnum.value(), off, width>();
               } else {
                  return std::nullopt;
               }
            } else {
               return std::nullopt;
            }
         } else {
            return std::nullopt;
         }
      } else {
         return std::nullopt;
      }
   }
};

template <uint32_t off>
struct GPR_PARSE : REG_PARSE<off, 5, 'r'> {};

template <uint32_t off>
struct FPR_PARSE : REG_PARSE<off, 5, 'f'> {};

template <uint32_t off>
struct CR_PARSE : REG_PARSE<off, 3, 'c', 'r'> {};

template <uint32_t off, uint32_t width, uint32_t align = 0>
struct UIMM_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      constexpr auto res = try_atoi_full(cts<c...> {});
      if constexpr (res) {
         constexpr uint32_t val = res.value();
         constexpr bool valid = (1 << width) > val;
         if constexpr (valid) {
            constexpr uint32_t align_mask = ~((1 << align) - 1);
            return shift<val & align_mask, off, width>();
         } else {
            return std::nullopt;
         }
      } else { 
         return std::nullopt;
      }
   }
};

template <uint32_t off, uint32_t width, uint32_t align = 0>
struct SIMM_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      constexpr auto res = try_atoi_full(cts<c...> {});
      if constexpr (res) {
         constexpr int32_t val = static_cast<int32_t>(res.value());
         constexpr int32_t max = static_cast<int32_t>(1 << (width - 1)) - 1;
         constexpr int32_t min = -(max + 1);
         constexpr bool valid = (val >= min) && (val <= max);
         if constexpr (valid) {
            constexpr uint32_t align_mask = ~((1 << align) - 1);
            return shift<static_cast<uint32_t>(val) & align_mask, off, width>();
         } else {
            return std::nullopt;
         }
      } else {
         return std::nullopt;
      }
   }
};

template <uint32_t off, uint32_t width>
struct CR0_BIT_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;
   
   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      if constexpr (cts<c...>::streq(cts<'l', 't'> {})) {
         return shift<0, off, width>();
      } else if constexpr (cts<c...>::streq(cts<'g', 't'> {})) {
         return shift<1, off, width>();
      } else if constexpr (cts<c...>::streq(cts<'e', 'q'> {})) {
         return shift<2, off, width>();
      } else if constexpr (cts<c...>::streq(cts<'s', 'o'> {})) {
         return shift<3, off, width>();
      } else {
         return std::nullopt;
      }
   }
};

template <typename... Parsers>
struct PARSE_ANY {};

template <typename Parser0, typename Parser1, typename... Parsers>
struct PARSE_ANY<Parser0, Parser1, Parsers...> {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      constexpr auto res0 = Parser0::parse(cts<c...> {});
      if constexpr (res0) {
         return res0;
      } else {
         return PARSE_ANY<Parser1, Parsers...>::parse(cts<c...> {});
      }
   }
};

template <typename Parser0>
struct PARSE_ANY<Parser0> {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      return Parser0::parse(cts<c...> {});
   }
};

struct TERMINATION_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::TERMINATOR;

   template <char... c>
   constexpr static uint32_t parse(cts<c...>) {
      static_assert(sizeof...(c) == 0, "Failed to find end-of-line.");
      return 0;
   }
};


//
// Argument
//
template <bool optional, typename Parser, typename Separator>
struct ARGUMENT {
   constexpr static ParseGroupType p_type = ParseGroupType::ARGUMENT;
   static_assert(Parser::p_type == ParseGroupType::PARSER,
                 "Invalid argument definition, Parser is not of type PARSER");
   static_assert(Separator::p_type == ParseGroupType::SEPARATOR,
                 "Invalid argument definition, Separator is not of type SEPARATOR");

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      constexpr auto parse_str = Separator::clipto(cts<c...> {});
      return Parser::parse(parse_str);
   }

   template <char... c>
   constexpr static auto eat(cts<c...>) {
      return Separator::eat(Separator::seekto(cts<c...>{}));
   }

   constexpr static bool is_optional = optional;
};

template <typename Parser, typename Separator>
struct REQUIRED_ARG : ARGUMENT<false, Parser, Separator> {};

template <uint32_t def, typename Parser, typename Separator>
struct OPTIONAL_ARG : ARGUMENT<true, Parser, Separator> {
   constexpr static uint32_t default_val = def;
};


//
// Matchers
//
template <uint32_t on_match, char Ec>
struct CHARACTER_MATCH {
   template <char... c>
   constexpr static bool match(cts<c...>) {
      if constexpr (sizeof...(c) > 0) {
         return nth_char<0, c...>::val == Ec;
      } else {
         return false;
      }
   }

   template <char... c>
   constexpr static auto eat(cts<c...>) {
      if constexpr (match(cts<c...> {})) {
         return typename cts<c...>::trim<1> {};
      } else {
         return cts<c...> {};
      }
   }

   constexpr static uint32_t val = on_match;
};

template <uint32_t on_match, char... c>
struct STRING_MATCH {
   template <char... c1>
   constexpr static bool match(cts<c1...>) {
      return cts<c...>::streq(typename cts<c1...>::substr<0, sizeof...(c)> {});
   }

   template <char... c1>
   constexpr static auto eat(cts<c1...>) {
      if constexpr (match(cts<c1...> {})) {
         return typename cts<c1...>::trim<sizeof...(c)> {};
      } else {
         return cts<c1...> {};
      }
   }

   constexpr static uint32_t val = on_match;
};


//
// Reg-Reg-IMM instructions
//
template <template <uint32_t off, uint32_t width> typename imm_parse>
struct RRUI_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<GPR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<imm_parse<16, 16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct ADDI : RRUI_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<14, 0, 6>(),
                                                'a', 'd', 'd', 'i'>>;
};

struct ADDIC : RRUI_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<12, 0, 6>(),
                                                'a', 'd', 'd', 'i', 'c'>>;
};

struct ADDIC_DOT : RRUI_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i', 'c', '.'>;
   using match_groups = std::tuple<STRING_MATCH<shift<13, 0, 6>(),
                                                'a', 'd', 'd', 'i', 'c', '.'>>;
};

struct ADDIS : RRUI_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<15, 0, 6>(),
                                                'a', 'd', 'd', 'i', 's'>>;
};

struct ANDI_DOT : RRUI_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'a', 'n', 'd', 'i', '.'>;
   using match_groups = std::tuple<STRING_MATCH<shift<28, 0, 6>(),
                                                'a', 'n', 'd', 'i', '.'>>;
};

struct ANDIS_DOT : RRUI_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'a', 'n', 'd', 'i', 's', '.'>;
   using match_groups = std::tuple<STRING_MATCH<shift<29, 0, 6>(),
                                                'a', 'n', 'd', 'i', 's', '.'>>;
};


//
// Reg-Reg-Reg instructions
//
struct RRR_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<GPR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct ADD : RRR_FAMILY {
   using lookup_key = cts<'a', 'd', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<266, 22, 9>(),
                                                'a', 'd', 'd'>,
                                   CHARACTER_MATCH<shift<1, 21, 1>(), 'o'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};

struct ADDC : RRR_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<10, 22, 9>(),
                                                'a', 'd', 'd', 'c'>,
                                   CHARACTER_MATCH<shift<1, 21, 1>(), 'o'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};

struct ADDE : RRR_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<138, 22, 9>(),
                                                'a', 'd', 'd', 'e'>,
                                   CHARACTER_MATCH<shift<1, 21, 1>(), 'o'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};

struct AND : RRR_FAMILY {
   using lookup_key = cts<'a', 'n', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<28, 21, 10>(),
                                                'a', 'n', 'd'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};

struct ANDC : RRR_FAMILY {
   using lookup_key = cts<'a', 'n', 'd', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<60, 21, 10>(),
                                                'a', 'n', 'd', 'c'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};

struct OR : RRR_FAMILY {
   using lookup_key = cts<'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<444, 21, 10>(),
                                                'o', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};


//
// Reg-Reg-0 instructions
//
struct RR0_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<GPR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct ADDME : RR0_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'm', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<234, 22, 9>(),
                                                'a', 'd', 'd', 'm', 'e'>,
                                   CHARACTER_MATCH<shift<1, 21, 1>(), 'o'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};

struct ADDZE : RR0_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'z', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<202, 22, 9>(),
                                                'a', 'd', 'd', 'z', 'e'>,
                                   CHARACTER_MATCH<shift<1, 21, 1>(), 'o'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;
};


//
// Branch instructions
//
struct BRANCH {
   using lookup_key = cts<'b'>;
   using match_groups = std::tuple<CHARACTER_MATCH<shift<18, 0, 6>(), 'b'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>,
                                   CHARACTER_MATCH<shift<1, 30, 1>(), 'a'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<SIMM_PARSE<6, 26, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BRANCH_COND {
   using lookup_key = cts<'b', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>(), 'b', 'c'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>,
                                   CHARACTER_MATCH<shift<1, 30, 1>(), 'a'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<UIMM_PARSE<6, 5>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<UIMM_PARSE<11, 5>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BEQ {
   using lookup_key = cts<'b', 'e', 'q'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'e', 'q'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BNE {
   using lookup_key = cts<'b', 'n', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'n', 'e'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BLT {
   using lookup_key = cts<'b', 'l', 't'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'l', 't'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BGE {
   using lookup_key = cts<'b', 'g', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'g', 'e'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BGT {
   using lookup_key = cts<'b', 'g', 't'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'g', 't'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BLE {
   using lookup_key = cts<'b', 'l', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'l', 'e'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BRANCH_CTR_COND {
   using lookup_key = cts<'b', 'c', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>(),
                                                'b', 'c', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<UIMM_PARSE<6, 5>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<PARSE_ANY<UIMM_PARSE<11, 5>, CR0_BIT_PARSE<11, 5>>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BCTR {
   using lookup_key = cts<'b', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() | shift<20, 6, 5>(),
                                                'b', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BEQCTR {
   using lookup_key = cts<'b', 'e', 'q', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<12, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'e', 'q', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BNECTR {
   using lookup_key = cts<'b', 'n', 'e', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<4, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'n', 'e', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BLTCTR {
   using lookup_key = cts<'b', 'l', 't', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<12, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'l', 't', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BGECTR {
   using lookup_key = cts<'b', 'g', 'e', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<4, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'g', 'e', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BGTCTR {
   using lookup_key = cts<'b', 'g', 't', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<12, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'g', 't', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BLECTR {
   using lookup_key = cts<'b', 'l', 'e', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<4, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'l', 'e', 'c', 't', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BRANCH_LR_COND {
   using lookup_key = cts<'b', 'c', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>(),
                                                'b', 'c', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<UIMM_PARSE<6, 5>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<PARSE_ANY<UIMM_PARSE<11, 5>, CR0_BIT_PARSE<11, 5>>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16, 2>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct BLR {
   using lookup_key = cts<'b', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() | shift<20, 6, 5>(),
                                                'b', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BEQLR {
   using lookup_key = cts<'b', 'e', 'q', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<12, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'e', 'q', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BNELR {
   using lookup_key = cts<'b', 'n', 'e', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<4, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'n', 'e', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BLTLR {
   using lookup_key = cts<'b', 'l', 't', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<12, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'l', 't', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BGELR {
   using lookup_key = cts<'b', 'g', 'e', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<4, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'g', 'e', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BGTLR {
   using lookup_key = cts<'b', 'g', 't', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<12, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'g', 't', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct BLELR {
   using lookup_key = cts<'b', 'l', 'e', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<4, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'l', 'e', 'l', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};


//
// CMP instructions
//
struct CMP {
   using lookup_key = cts<'c', 'm', 'p'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>(), 'c', 'm', 'p'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct CMPI {
   using lookup_key = cts<'c', 'm', 'p', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct CMPL {
   using lookup_key = cts<'c', 'm', 'p', 'l'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<32, 21, 11>(),
                                                'c', 'm', 'p', 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct CMPLI {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQUIRED_ARG<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<UIMM_PARSE<16, 16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct CMPW {
   using lookup_key = cts<'c', 'm', 'p', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>(), 'c', 'm', 'p', 'w'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct CMPWI {
   using lookup_key = cts<'c', 'm', 'p', 'w', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'w', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct CMPLW {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<32, 21, 11>(),
                                                'c', 'm', 'p', 'l', 'w'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

struct CMPLWI {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'w', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'l', 'w', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<GPR_PARSE<11>, CHARACTER_SEPARATOR<','>>,
                                   REQUIRED_ARG<UIMM_PARSE<16, 16>, WHITESPACE_SEPARATOR>,
                                   TERMINATION_PARSE>;
};

template <typename MG0, typename... MG, char... c>
constexpr bool run_matchgroups(std::tuple<MG0, MG...>, cts<c...>) {
   if constexpr (sizeof...(MG) == 0) {
      constexpr auto final_str = MG0::eat(cts<c...> {});
      return decltype(final_str)::is_empty;
   } else {
      return run_matchgroups(std::tuple<MG...> {}, MG0::eat(cts<c...> {}));
   }
}

template <typename Inst, char... c>
constexpr bool run_match(cts<c...>) {
   return run_matchgroups(typename std::decay_t<Inst>::match_groups {}, cts<c...> {});
}

template <typename MG0, typename... MG, char... c>
constexpr uint32_t get_matchgroup_data(std::tuple<MG0, MG...>, cts<c...>) {
   constexpr uint32_t MG0_val = MG0::match(cts<c...> {}) ? MG0::val : 0;
   if constexpr (sizeof...(MG) == 0) {
      return MG0_val;
   } else {
      return MG0_val | get_matchgroup_data(std::tuple<MG...> {}, MG0::eat(cts<c...> {}));
   }
}

template <typename Inst, char... c>
constexpr uint32_t get_match_data(cts<c...>) {
   return get_matchgroup_data(typename std::decay_t<Inst>::match_groups {}, cts<c...> {});
}

template <typename MG0, typename... MG, char... c>
constexpr auto eat_matchgroup(std::tuple<MG0, MG...>, cts<c...>) {
   constexpr uint32_t MG0_val = MG0::match(cts<c...> {}) ? MG0::val : 0;
   if constexpr (sizeof...(MG) == 0) {
      return MG0::eat(cts<c...> {});
   } else {
      return eat_matchgroup(std::tuple<MG...> {}, MG0::eat(cts<c...> {}));
   }
}

template <typename Inst, char... c>
constexpr auto eat_match(cts<c...>) {
   return eat_matchgroup(typename std::decay_t<Inst>::match_groups {}, cts<c...> {});
}

template <typename PG0, typename... PG, char... c>
constexpr uint32_t parse_pg_recurse(std::tuple<PG0, PG...>, cts<c...>) {
   if constexpr (PG0::p_type == ParseGroupType::SEPARATOR) {
      return parse_pg_recurse(std::tuple<PG...> {}, PG0::eat(cts<c...> {}));
   } else if constexpr (PG0::p_type == ParseGroupType::ARGUMENT) {
      constexpr auto parsed_value = PG0::parse(cts<c...> {});
      static_assert(parsed_value || PG0::is_optional, "Failed to parse required argument PG0");
      if constexpr (parsed_value) {
         return parsed_value.value() |
                parse_pg_recurse(std::tuple<PG...> {}, PG0::eat(cts<c...> {}));
      } else if constexpr (PG0::is_optional) {
         return PG0::default_val | parse_pg_recurse(std::tuple<PG...> {}, cts<c...> {});
      } else {
         return 0;
      }
   } else if constexpr (PG0::p_type == ParseGroupType::PARSER) {
      static_assert(sizeof...(PG) > 0, "Instruction ends in bad parser type.");
      using next_sep = std::decay_t<decltype(std::get<0>(std::tuple<PG...> {}))>;
      constexpr auto clipped_str = next_sep::clipto(cts<c...> {});
      constexpr auto parsed_value = PG0::parse(clipped_str);
      static_assert(parsed_value, "Failed to parse on PG0");
      if constexpr (parsed_value) {
         return parsed_value.value() |
                parse_pg_recurse(std::tuple<PG...> {}, next_sep::seekto(cts<c...> {}));
      } else {
         return 0;
      }
   } else {
      PG0::parse(cts<c...> {});
      return 0;
   }   
}

template <typename Inst, char... c>
constexpr uint32_t parse_inst_body(cts<c...>) {
   return parse_pg_recurse(typename std::decay_t<Inst>::parse_groups {}, cts<c...> {});
}

template <typename trie_root, char... c>
constexpr uint32_t parse_instr_trie(cts<c...>, trie_root) {
   constexpr auto instr_guess =
      lookup_nearest_trie(cts<c...> {}, trie_root {});
   constexpr bool found_match = !std::is_same_v<decltype(instr_guess), lookup_failure>;
   static_assert(found_match, "Failed to parse instruction head");
   if constexpr (found_match) {
      constexpr auto header = WHITESPACE_SEPARATOR::clipto(cts<c...> {});
      constexpr bool match_result = run_match<decltype(instr_guess)>(header);
      static_assert(match_result, "Failed to parse instruction head");
      if constexpr (match_result) {
         constexpr uint32_t base = get_match_data<decltype(instr_guess)>(header);
         return base | parse_inst_body<decltype(instr_guess)>(eat_match<decltype(instr_guess)>(cts<c...> {}));
      } else {
         return 0;
      }
   } else {
      return 0;
   }
}

// Linear search, performance is pitiful
template <typename Inst0, typename... InstList, char... c>
constexpr uint32_t parse_instr_tuple(std::tuple<Inst0, InstList...>, cts<c...>) {
   constexpr auto header = WHITESPACE_SEPARATOR::clipto(cts<c...> {});
   constexpr bool match_result = run_match<Inst0>(header);
   static_assert(match_result || sizeof...(InstList) > 0, "Failed to parse instruction head");
   if constexpr (match_result) {
      constexpr uint32_t base = get_match_data<Inst0>(header);
      return base | parse_inst_body<Inst0>(eat_match<Inst0>(cts<c...> {}));
   } else if constexpr (sizeof...(InstList) > 0) {
      return parse_instr_tuple(std::tuple<InstList...> {}, cts<c...>{});
   } else {
      return 0;
   }
}

template <char... c>
constexpr uint32_t parse(cts<c...>) {
//   using instr_list = std::tuple<ADDI, ADDIC, ADDIC_DOT, ADDIS, ANDI_DOT,
//                                 ANDIS_DOT, ADD, ADDC, ADDE, AND, ANDC, OR,
//                                 ADDME, ADDZE, BRANCH, BRANCH_COND, BRANCH_CTR_COND,
//                                 BRANCH_LR_COND, BLR, BEQLR, BNELR, BLTLR, BGELR,
//                                 BGTLR, BLELR, CMP, CMPI, CMPL, CMPLI, CMPW>;
//   return parse_instr_tuple(instr_list {}, cts<c...> {});
   constexpr auto instr_list_trie =
      create_trie<ADDI, ADDIC, ADDIC_DOT, ADDIS, ANDI_DOT, ANDIS_DOT,
                  ADD, ADDC, ADDE, AND, ANDC, OR,
                  ADDME, ADDZE,
                  BRANCH, BRANCH_COND, BEQ, BNE, BLT, BGE, BGT, BLE,
                  BRANCH_CTR_COND, BCTR, BEQCTR, BNECTR, BLTCTR, BGECTR, BGTCTR, BLECTR,
                  BRANCH_LR_COND, BLR, BEQLR, BNELR, BLTLR, BGELR, BGTLR, BLELR,
                  CMP, CMPI, CMPL, CMPLI, CMPW, CMPWI, CMPLW, CMPLWI>();
   return parse_instr_trie(cts<c...> {}, instr_list_trie);
}
}
