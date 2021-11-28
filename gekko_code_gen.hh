#pragma once

#include "cts.hh"
#include "ctrie.hh"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <tuple>
#include <type_traits>

namespace gekko_code_gen {
template <typename T>
constexpr void force_failure(T) {
   static_assert(std::is_same_v<std::decay_t<T>, T&>, "See error for T");
}

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

using RA_PARSE = GPR_PARSE<11>;
using RB_PARSE = GPR_PARSE<16>;
using RD_PARSE = GPR_PARSE<6>;
using RS_PARSE = GPR_PARSE<6>;

struct RS_RB_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      constexpr auto reg_val = RS_PARSE::parse(cts<c...> {});
      if constexpr (reg_val) {
         return reg_val.value() | (reg_val.value() >> 10);
      } else {
         return std::nullopt;
      }
   }
};

template <uint32_t off>
struct FPR_PARSE : REG_PARSE<off, 5, 'f'> {};

using FA_PARSE = FPR_PARSE<11>;
using FB_PARSE = FPR_PARSE<16>;
using FC_PARSE = FPR_PARSE<21>;
using FD_PARSE = FPR_PARSE<6>;
using FS_PARSE = FPR_PARSE<6>;

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
         constexpr uint32_t align_mask = ~((1 << align) - 1);
         if constexpr (width == 32) {
            return shift<val & align_mask, off, width>();
         } else if constexpr ((1 << width) > val) {
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
         if constexpr ((val >= min) && (val <= max)) {
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
struct BRANCH_REL_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      using from = decltype(CHARACTER_SEPARATOR<'~'>::clipto(cts<c...> {}));
      using to = typename decltype(CHARACTER_SEPARATOR<'~'>::seekto(cts<c...> {}))::trim<1>;
      constexpr auto val_from = UIMM_PARSE<0, 32, 2>::parse(from {});
      constexpr auto val_to = UIMM_PARSE<0, 32, 2>::parse(to {});
      if constexpr (val_from && val_to) {
         constexpr int32_t dist = static_cast<int32_t>(val_to.value() - val_from.value());
         constexpr int32_t max = static_cast<int32_t>(1 << (width - 1)) - 1;
         constexpr int32_t min = -(max + 1);
         if constexpr ((dist >= min) && (dist <= max)) {
            return shift<static_cast<uint32_t>(dist), off, width>();
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

template <uint32_t off>
struct SPR_IMM_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      // I don't like how this is set up, but it's too late to fix
      constexpr auto val_parse = UIMM_PARSE<22, 10>::parse(cts<c...> {});
      if constexpr (val_parse) {
         return shift<((val_parse.value() << 5) & 0b1111100000) |
                      ((val_parse.value() >> 5) & 0b0000011111), off, 10>();
      } else {
         return std::nullopt;
      }
   }
};

template <uint32_t off>
struct SPR_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      if constexpr (cts<c...>::streq(cts<'x', 'e', 'r'> {})) {
         return shift<0b0000100000, off, 10>();
      } else if constexpr (cts<c...>::streq(cts<'l', 'r'> {})) {
         return shift<0b0100000000, off, 10>();
      } else if constexpr (cts<c...>::streq(cts<'c', 't', 'r'> {})) {
         return shift<0b0100100000, off, 10>();
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
using REQUIRED_ARG = ARGUMENT<false, Parser, Separator>;

template <typename Parser>
using REQ_ARG_COMMA = REQUIRED_ARG<Parser, CHARACTER_SEPARATOR<','>>;
template <typename Parser>
using REQ_ARG_FINAL = REQUIRED_ARG<Parser, WHITESPACE_SEPARATOR>;

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

template <uint32_t bit_off, char Ec>
using CHARACTER_BIT_MATCH = CHARACTER_MATCH<shift<1, bit_off, 1>(), Ec>;

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
// Reg-Reg-IMM instructions (S-A, A destination)
//
template <template <uint32_t off, uint32_t width> typename imm_parse>
struct RRIA_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_COMMA<RS_PARSE>,
                                   REQ_ARG_FINAL<imm_parse<16, 16>>,
                                   TERMINATION_PARSE>;
};

struct ANDI_DOT : RRIA_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'a', 'n', 'd', 'i', '.'>;
   using match_groups = std::tuple<STRING_MATCH<shift<28, 0, 6>(),
                                                'a', 'n', 'd', 'i', '.'>>;
};

struct ANDIS_DOT : RRIA_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'a', 'n', 'd', 'i', 's', '.'>;
   using match_groups = std::tuple<STRING_MATCH<shift<29, 0, 6>(),
                                                'a', 'n', 'd', 'i', 's', '.'>>;
};

struct ORI : RRIA_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'o', 'r', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<24, 0, 6>(),
                                                'o', 'r', 'i'>>;
};

struct ORIS : RRIA_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'o', 'r', 'i', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<25, 0, 6>(),
                                                'o', 'r', 'i', 's'>>;
};

struct XORI : RRIA_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'x', 'o', 'r', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<26, 0, 6>(),
                                                'x', 'o', 'r', 'i'>>;
};

struct XORIS : RRIA_FAMILY<UIMM_PARSE> {
   using lookup_key = cts<'x', 'o', 'r', 'i', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<27, 0, 6>(),
                                                'x', 'o', 'r', 'i', 's'>>;
};

struct NOP {
   using lookup_key = cts<'n', 'o', 'p'>;
   using match_groups = std::tuple<STRING_MATCH<shift<24, 0, 6>(),
                                                'n', 'o', 'p'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR, TERMINATION_PARSE>;
};

//
// Reg-Reg-IMM instructions (D-A, D destination)
//
template <template <uint32_t off, uint32_t width> typename imm_parse>
struct RRID_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RD_PARSE>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<imm_parse<16, 16>>,
                                   TERMINATION_PARSE>;
};

struct ADDI : RRID_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<14, 0, 6>(),
                                                'a', 'd', 'd', 'i'>>;
};

struct ADDIC : RRID_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<12, 0, 6>(),
                                                'a', 'd', 'd', 'i', 'c'>>;
};

struct ADDIC_DOT : RRID_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i', 'c', '.'>;
   using match_groups = std::tuple<STRING_MATCH<shift<13, 0, 6>(),
                                                'a', 'd', 'd', 'i', 'c', '.'>>;
};

struct ADDIS : RRID_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'a', 'd', 'd', 'i', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<15, 0, 6>(),
                                                'a', 'd', 'd', 'i', 's'>>;
};

struct LI {
   using lookup_key = cts<'l', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<14, 0, 6>(),
                                                'l', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RD_PARSE>,
                                   REQ_ARG_FINAL<SIMM_PARSE<16, 16>>,
                                   TERMINATION_PARSE>;
};

struct LIS {
   using lookup_key = cts<'l', 'i', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<15, 0, 6>(),
                                                'l', 'i', 's'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RD_PARSE>,
                                   REQ_ARG_FINAL<PARSE_ANY<SIMM_PARSE<16, 16>, UIMM_PARSE<16, 16>>>,
                                   TERMINATION_PARSE>;
};

struct MULLI : RRID_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'m', 'u', 'l', 'l', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<7, 0, 6>(),
                                                'm', 'u', 'l', 'l', 'i'>>;
};

struct SUBFIC : RRID_FAMILY<SIMM_PARSE> {
   using lookup_key = cts<'s', 'u', 'b', 'f', 'i', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<8, 0, 6>(),
                                                's', 'u', 'b', 'f', 'i', 'c'>>;
};


//
// Reg-Reg-Reg instructions (S-A-B, A destination)
//
struct RRRA_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_COMMA<RS_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct AND : RRRA_FAMILY {
   using lookup_key = cts<'a', 'n', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<28, 21, 10>(),
                                                'a', 'n', 'd'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct ANDC : RRRA_FAMILY {
   using lookup_key = cts<'a', 'n', 'd', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<60, 21, 10>(),
                                                'a', 'n', 'd', 'c'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

// Typo in manual, rB is not 6 bits? Probably function code should be 10.
struct EQV : RRRA_FAMILY {
   using lookup_key = cts<'e', 'q', 'v'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<284, 22, 9>(),
                                                'e', 'q', 'v'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct NAND : RRRA_FAMILY {
   using lookup_key = cts<'n', 'a', 'n', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<476, 21, 10>(),
                                                'n', 'a', 'n', 'd'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct NOR : RRRA_FAMILY {
   using lookup_key = cts<'n', 'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<124, 21, 10>(),
                                                'n', 'o', 'r'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct OR : RRRA_FAMILY {
   using lookup_key = cts<'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<444, 21, 10>(),
                                                'o', 'r'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct MR {
   using lookup_key = cts<'m', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<444, 21, 10>(),
                                                'm', 'r'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RS_RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct ORC : RRRA_FAMILY {
   using lookup_key = cts<'o', 'r', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<412, 21, 10>(),
                                                'o', 'r', 'c'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SLW : RRRA_FAMILY {
   using lookup_key = cts<'s', 'l', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<24, 21, 10>(),
                                                's', 'l', 'w'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SRAW : RRRA_FAMILY {
   using lookup_key = cts<'s', 'r', 'a', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<792, 21, 10>(),
                                                's', 'r', 'a', 'w'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SRW : RRRA_FAMILY {
   using lookup_key = cts<'s', 'r', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<536, 21, 10>(),
                                                's', 'r', 'w'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct XOR : RRRA_FAMILY {
   using lookup_key = cts<'x', 'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<316, 21, 10>(),
                                                'x', 'o', 'r'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};


//
// Reg-Reg-Reg instructions (D-A-B, D destination)
//
struct RRRD_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RD_PARSE>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct ADD : RRRD_FAMILY {
   using lookup_key = cts<'a', 'd', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<266, 22, 9>(),
                                                'a', 'd', 'd'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct ADDC : RRRD_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<10, 22, 9>(),
                                                'a', 'd', 'd', 'c'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct ADDE : RRRD_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<138, 22, 9>(),
                                                'a', 'd', 'd', 'e'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct DIVW : RRRD_FAMILY {
   using lookup_key = cts<'d', 'i', 'v', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<491, 22, 9>(),
                                                'd', 'i', 'v', 'w'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct DIVWU : RRRD_FAMILY {
   using lookup_key = cts<'d', 'i', 'v', 'w', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<459, 22, 9>(),
                                                'd', 'i', 'v', 'w', 'u'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct LBZUX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'b', 'z', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<119, 21, 10>(),
                                                'l', 'b', 'z', 'u', 'x'>>;
};

struct LBZX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'b', 'z', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<87, 21, 10>(),
                                                'l', 'b', 'z', 'x'>>;
};

struct LHAUX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'h', 'a', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<375, 21, 10>(),
                                                'l', 'h', 'a', 'u', 'x'>>;
};

struct LHAX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'h', 'a', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<343, 21, 10>(),
                                                'l', 'h', 'a', 'x'>>;
};

struct LHBRX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'h', 'b', 'r', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<790, 21, 10>(),
                                                'l', 'h', 'b', 'r', 'x'>>;
};

struct LHZUX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'h', 'z', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<311, 21, 10>(),
                                                'l', 'h', 'z', 'u', 'x'>>;
};

struct LHZX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'h', 'z', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<279, 21, 10>(),
                                                'l', 'h', 'z', 'x'>>;
};

struct LWBRX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'w', 'b', 'r', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<534, 21, 10>(),
                                                'l', 'w', 'b', 'r', 'x'>>;
};

struct LWZUX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'w', 'z', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<55, 21, 10>(),
                                                'l', 'w', 'z', 'u', 'x'>>;
};

struct LWZX : RRRD_FAMILY {
   using lookup_key = cts<'l', 'w', 'z', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<23, 21, 10>(),
                                                'l', 'w', 'z', 'x'>>;
};

struct MULHW : RRRD_FAMILY {
   using lookup_key = cts<'m', 'u', 'l', 'h', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<75, 22, 9>(),
                                                'm', 'u', 'l', 'h', 'w'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct MULHWU : RRRD_FAMILY {
   using lookup_key = cts<'m', 'u', 'l', 'h', 'w', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<11, 22, 9>(),
                                                'm', 'u', 'l', 'h', 'w', 'u'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct MULLW : RRRD_FAMILY {
   using lookup_key = cts<'m', 'u', 'l', 'l', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<235, 22, 9>(),
                                                'm', 'u', 'l', 'l', 'w'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SUBF : RRRD_FAMILY {
   using lookup_key = cts<'s', 'u', 'b', 'f'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<40, 22, 9>(),
                                                's', 'u', 'b', 'f'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SUBFC : RRRD_FAMILY {
   using lookup_key = cts<'s', 'u', 'b', 'f', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<8, 22, 9>(),
                                                's', 'u', 'b', 'f', 'c'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SUBFE : RRRD_FAMILY {
   using lookup_key = cts<'s', 'u', 'b', 'f', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<136, 22, 9>(),
                                                's', 'u', 'b', 'f', 'e'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SUB {
   using lookup_key = cts<'s', 'u', 'b'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<40, 22, 9>(),
                                                's', 'u', 'b'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RD_PARSE>,
                                   REQ_ARG_COMMA<RB_PARSE>,
                                   REQ_ARG_FINAL<RA_PARSE>,
                                   TERMINATION_PARSE>;
};

struct SUBC {
   using lookup_key = cts<'s', 'u', 'b', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<8, 22, 9>(),
                                                's', 'u', 'b', 'c'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = SUB::parse_groups;
};


//
// Reg-Reg-0 instructions (S-A, A destination)
//
struct RR0A_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RS_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CNTLZW : RR0A_FAMILY {
   using lookup_key = cts<'c', 'n', 't', 'l', 'z', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<26, 21, 10>(),
                                                'c', 'n', 't', 'l', 'z', 'w'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct EXTSB : RR0A_FAMILY {
   using lookup_key = cts<'e', 'x', 't', 's', 'b'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<954, 21, 10>(),
                                                'e', 'x', 't', 's', 'b'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct EXTSH : RR0A_FAMILY {
   using lookup_key = cts<'e', 'x', 't', 's', 'h'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<922, 21, 10>(),
                                                'e', 'x', 't', 's', 'h'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SRAWI {
   using lookup_key = cts<'s', 'r', 'a', 'w', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<824, 21, 10>(),
                                                's', 'r', 'a', 'w', 'i'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_COMMA<RS_PARSE>,
                                   REQ_ARG_FINAL<UIMM_PARSE<16, 5>>,
                                   TERMINATION_PARSE>;
};


//
// Reg-Reg-0 instructions (D-A, D destination)
//
struct RR0D_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RD_PARSE>,
                                   REQ_ARG_FINAL<RA_PARSE>,
                                   TERMINATION_PARSE>;
};

struct ADDME : RR0D_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'm', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<234, 22, 9>(),
                                                'a', 'd', 'd', 'm', 'e'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct ADDZE : RR0D_FAMILY {
   using lookup_key = cts<'a', 'd', 'd', 'z', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<202, 22, 9>(),
                                                'a', 'd', 'd', 'z', 'e'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct NEG : RR0D_FAMILY {
   using lookup_key = cts<'n', 'e', 'g'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<104, 22, 9>(),
                                                'n', 'e', 'g'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SUBFME : RR0D_FAMILY {
   using lookup_key = cts<'s', 'u', 'b', 'f', 'm', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<232, 22, 9>(),
                                                's', 'u', 'b', 'f', 'm', 'e'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct SUBFZE : RR0D_FAMILY {
   using lookup_key = cts<'s', 'u', 'b', 'f', 'z', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<200, 22, 9>(),
                                                's', 'u', 'b', 'f', 'z', 'e'>,
                                   CHARACTER_BIT_MATCH<21, 'o'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};


//
// CRB-CRB-CRB instructions
//
struct CCC_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 5>, CR0_BIT_PARSE<6, 5>>>,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<11, 5>, CR0_BIT_PARSE<11, 5>>>,
                                   REQ_ARG_FINAL<PARSE_ANY<UIMM_PARSE<16, 5>, CR0_BIT_PARSE<16, 5>>>,
                                   TERMINATION_PARSE>;
};

struct CRAND : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'a', 'n', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<257, 21, 10>(),
                                                'c', 'r', 'a', 'n', 'd'>>;
};

struct CRANDC : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'a', 'n', 'd', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<129, 21, 10>(),
                                                'c', 'r', 'a', 'n', 'd', 'c'>>;
};

struct CREQV : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'e', 'q', 'v'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<289, 21, 10>(),
                                                'c', 'r', 'e', 'q', 'v'>>;
};

struct CRNAND : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'n', 'a', 'n', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<225, 21, 10>(),
                                                'c', 'r', 'n', 'a', 'n', 'd'>>;
};

struct CRNOR : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'n', 'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<33, 21, 10>(),
                                                'c', 'r', 'n', 'o', 'r'>>;
};

struct CROR : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<449, 21, 10>(),
                                                'c', 'r', 'o', 'r'>>;
};

struct CRORC : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'o', 'r', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<417, 21, 10>(),
                                                'c', 'r', 'o', 'r', 'c'>>;
};

struct CRXOR : CCC_FAMILY {
   using lookup_key = cts<'c', 'r', 'x', 'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<193, 21, 10>(),
                                                'c', 'r', 'x', 'o', 'r'>>;
};


//
// FReg-FReg instructions (D-B, D destination)
//
struct F0FD_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FD_PARSE>,
                                   REQ_ARG_FINAL<FB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct FABS : F0FD_FAMILY {
   using lookup_key = cts<'f', 'a', 'b', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<264, 21, 10>(),
                                                'f', 'a', 'b', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FCTIW : F0FD_FAMILY { 
   using lookup_key = cts<'f', 'c', 't', 'i', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<14, 21, 10>(),
                                                'f', 'c', 't', 'i', 'w'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FCTIWZ : F0FD_FAMILY {
   using lookup_key = cts<'f', 'c', 't', 'i', 'w', 'z'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<15, 21, 10>(),
                                                'f', 'c', 't', 'i', 'w', 'z'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FMR : F0FD_FAMILY {
   using lookup_key = cts<'f', 'm', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<72, 21, 10>(),
                                                'f', 'm', 'r'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FNABS : F0FD_FAMILY {
   using lookup_key = cts<'f', 'n', 'a', 'b', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<136, 21, 10>(),
                                                'f', 'n', 'a', 'b', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FNEG : F0FD_FAMILY {
   using lookup_key = cts<'f', 'n', 'e', 'g'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<40, 21, 10>(),
                                                'f', 'n', 'e', 'g'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FRES : F0FD_FAMILY {
   using lookup_key = cts<'f', 'r', 'e', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<24, 26, 5>(),
                                                'f', 'r', 'e', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FRSP : F0FD_FAMILY {
   using lookup_key = cts<'f', 'r', 's', 'p'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<12, 21, 10>(),
                                                'f', 'r', 's', 'p'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FRSQRTE : F0FD_FAMILY {
   using lookup_key = cts<'f', 'r', 's', 'q', 'r', 't', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<26, 26, 5>(),
                                                'f', 'r', 's', 'q', 'r', 't', 'e'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};


//
// FReg-FReg-FReg instructions (D-A-B, D destination)
//
struct FFF0D_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FD_PARSE>,
                                   REQ_ARG_COMMA<FA_PARSE>,
                                   REQ_ARG_FINAL<FB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct FADD : FFF0D_FAMILY {
   using lookup_key = cts<'f', 'a', 'd', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<21, 26, 5>(),
                                                'f', 'a', 'd', 'd'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FADDS : FFF0D_FAMILY {
   using lookup_key = cts<'f', 'a', 'd', 'd', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<21, 26, 5>(),
                                                'f', 'a', 'd', 'd', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FDIV : FFF0D_FAMILY {
   using lookup_key = cts<'f', 'd', 'i', 'v'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<18, 26, 5>(),
                                                'f', 'd', 'i', 'v'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FDIVS : FFF0D_FAMILY {
   using lookup_key = cts<'f', 'd', 'i', 'v', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<18, 26, 5>(),
                                                'f', 'd', 'i', 'v', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FSUB : FFF0D_FAMILY {
   using lookup_key = cts<'f', 's', 'u', 'b'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<20, 26, 5>(),
                                                'f', 's', 'u', 'b'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FSUBS : FFF0D_FAMILY {
   using lookup_key = cts<'f', 's', 'u', 'b', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<20, 26, 5>(),
                                                'f', 's', 'u', 'b', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};


//
// FReg-FReg-FReg instructions (D-A-C, D destination)
//
struct FF0FD_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FD_PARSE>,
                                   REQ_ARG_COMMA<FA_PARSE>,
                                   REQ_ARG_FINAL<FC_PARSE>,
                                   TERMINATION_PARSE>;
};

struct FMUL : FF0FD_FAMILY {
   using lookup_key = cts<'f', 'm', 'u', 'l'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<25, 26, 5>(),
                                                'f', 'm', 'u', 'l'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FMULS : FF0FD_FAMILY {
   using lookup_key = cts<'f', 'm', 'u', 'l', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<25, 26, 5>(),
                                                'f', 'm', 'u', 'l', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};


//
// FReg-FReg-FReg-FReg instructions (D-A-C-B, D destination)
//
struct FFFFD_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FD_PARSE>,
                                   REQ_ARG_COMMA<FA_PARSE>,
                                   REQ_ARG_COMMA<FC_PARSE>,
                                   REQ_ARG_FINAL<FB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct FMADD : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'm', 'a', 'd', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<29, 26, 5>(),
                                                'f', 'm', 'a', 'd', 'd'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FMADDS : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'm', 'a', 'd', 'd', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<29, 26, 5>(),
                                                'f', 'm', 'a', 'd', 'd', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FMSUB : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'm', 's', 'u', 'b'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<28, 26, 5>(),
                                                'f', 'm', 's', 'u', 'b'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FMSUBS : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'm', 's', 'u', 'b', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<28, 26, 5>(),
                                                'f', 'm', 's', 'u', 'b', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FNMADD : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'n', 'm', 'a', 'd', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<31, 26, 5>(),
                                                'f', 'n', 'm', 'a', 'd', 'd'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FNMADDS : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'n', 'm', 'a', 'd', 'd', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<31, 26, 5>(),
                                                'f', 'n', 'm', 'a', 'd', 'd', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FNMSUB : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'n', 'm', 's', 'u', 'b'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<30, 26, 5>(),
                                                'f', 'n', 'm', 's', 'u', 'b'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FNMSUBS : FFFFD_FAMILY {
   using lookup_key = cts<'f', 'n', 'm', 's', 'u', 'b', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<59, 0, 6>() | shift<30, 26, 5>(),
                                                'f', 'n', 'm', 's', 'u', 'b', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FSEL : FFFFD_FAMILY {
   using lookup_key = cts<'f', 's', 'e', 'l'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<23, 26, 5>(),
                                                'f', 's', 'e', 'l'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};


//
// Float compare
//
struct FCMPO {
   using lookup_key = cts<'f', 'c', 'm', 'p', 'o'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<32, 21, 10>(),
                                                'f', 'c', 'm', 'p', 'o'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<CR_PARSE<6>>,
                                   REQ_ARG_COMMA<FA_PARSE>,
                                   REQ_ARG_FINAL<FB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct FCMPU {
   using lookup_key = cts<'f', 'c', 'm', 'p', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<0, 21, 10>(),
                                                'f', 'c', 'm', 'p', 'u'>>;
   // Same parse
   using parse_groups = FCMPO::parse_groups;
};


//
// Branch instructions
//
struct BRANCH {
   using lookup_key = cts<'b'>;
   using match_groups = std::tuple<CHARACTER_MATCH<shift<18, 0, 6>(), 'b'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<PARSE_ANY<SIMM_PARSE<6, 26, 2>, BRANCH_REL_PARSE<6, 26>>>,
                                   TERMINATION_PARSE>;
};

struct BRANCH_COND {
   using lookup_key = cts<'b', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>(), 'b', 'c'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<UIMM_PARSE<6, 5>>,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<11, 5>, CR0_BIT_PARSE<11, 5>>>,
                                   REQ_ARG_FINAL<PARSE_ANY<SIMM_PARSE<16, 16, 2>, BRANCH_REL_PARSE<16, 16>>>,
                                   TERMINATION_PARSE>;
};

struct Bxx_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<PARSE_ANY<SIMM_PARSE<16, 16, 2>, BRANCH_REL_PARSE<16, 16>>>,
                                   TERMINATION_PARSE>;
};

struct BEQ : Bxx_FAMILY {
   using lookup_key = cts<'b', 'e', 'q'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'e', 'q'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
};

struct BNE : Bxx_FAMILY {
   using lookup_key = cts<'b', 'n', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'n', 'e'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
};

struct BLT : Bxx_FAMILY {
   using lookup_key = cts<'b', 'l', 't'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'l', 't'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
};

struct BGE : Bxx_FAMILY {
   using lookup_key = cts<'b', 'g', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'g', 'e'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
};

struct BGT : Bxx_FAMILY {
   using lookup_key = cts<'b', 'g', 't'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'g', 't'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
};

struct BLE : Bxx_FAMILY {
   using lookup_key = cts<'b', 'l', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'l', 'e'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
};

struct BRANCH_CTR_COND {
   using lookup_key = cts<'b', 'c', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>(),
                                                'b', 'c', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<UIMM_PARSE<6, 5>>,
                                   REQ_ARG_FINAL<PARSE_ANY<UIMM_PARSE<11, 5>, CR0_BIT_PARSE<11, 5>>>,
                                   TERMINATION_PARSE>;
};

struct EMPTY_PARSE {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR, TERMINATION_PARSE>;
};

struct BCTR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() | shift<20, 6, 5>(),
                                                'b', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BEQCTR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'e', 'q', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<12, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'e', 'q', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BNECTR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'n', 'e', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<4, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'n', 'e', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BLTCTR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'l', 't', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<12, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'l', 't', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BGECTR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'g', 'e', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<4, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'g', 'e', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BGTCTR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'g', 't', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<12, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'g', 't', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BLECTR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'l', 'e', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<528, 21, 10>() |
                                                shift<4, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'l', 'e', 'c', 't', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BRANCH_LR_COND {
   using lookup_key = cts<'b', 'c', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>(),
                                                'b', 'c', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<UIMM_PARSE<6, 5>>,
                                   REQ_ARG_FINAL<PARSE_ANY<UIMM_PARSE<11, 5>, CR0_BIT_PARSE<11, 5>>>,
                                   TERMINATION_PARSE>;
};

struct BLR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() | shift<20, 6, 5>(),
                                                'b', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BEQLR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'e', 'q', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<12, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'e', 'q', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BNELR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'n', 'e', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<4, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'n', 'e', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BLTLR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'l', 't', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<12, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'l', 't', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BGELR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'g', 'e', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<4, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'g', 'e', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BGTLR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'g', 't', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<12, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'g', 't', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};

struct BLELR : EMPTY_PARSE {
   using lookup_key = cts<'b', 'l', 'e', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<16, 21, 10>() |
                                                shift<4, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'l', 'e', 'l', 'r'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>>;
};


//
// CMP instructions
//
struct CMP {
   using lookup_key = cts<'c', 'm', 'p'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>(), 'c', 'm', 'p'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>>,
                                   OPTIONAL_ARG<0, UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CMPI {
   using lookup_key = cts<'c', 'm', 'p', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>>,
                                   OPTIONAL_ARG<0, UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<SIMM_PARSE<16, 16>>,
                                   TERMINATION_PARSE>;
};

struct CMPL {
   using lookup_key = cts<'c', 'm', 'p', 'l'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<32, 21, 10>(),
                                                'c', 'm', 'p', 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>>,
                                   OPTIONAL_ARG<0, UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CMPLI {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<10, 0, 6>(), 'c', 'm', 'p', 'l', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>>,
                                   OPTIONAL_ARG<0, UIMM_PARSE<10, 1>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<UIMM_PARSE<16, 16>>,
                                   TERMINATION_PARSE>;
};

struct CMPW {
   using lookup_key = cts<'c', 'm', 'p', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>(), 'c', 'm', 'p', 'w'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CMPWI {
   using lookup_key = cts<'c', 'm', 'p', 'w', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'w', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<SIMM_PARSE<16, 16>>,
                                   TERMINATION_PARSE>;
};

struct CMPLW {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<32, 21, 10>(),
                                                'c', 'm', 'p', 'l', 'w'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CMPLWI {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'w', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<10, 0, 6>(), 'c', 'm', 'p', 'l', 'w', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<UIMM_PARSE<16, 16>>,
                                   TERMINATION_PARSE>;
};


//
// Memory load-store
//
struct LOAD_INTEGER_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RD_PARSE>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16>, CHARACTER_SEPARATOR<'('>>,
                                   REQUIRED_ARG<RA_PARSE, CHARACTER_SEPARATOR<')'>>,
                                   TERMINATION_PARSE>;
};

struct LBZ : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'b', 'z'>;
   using match_groups = std::tuple<STRING_MATCH<shift<34, 0, 6>(),
                                                'l', 'b', 'z'>>;
};

struct LBZU : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'b', 'z', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<35, 0, 6>(),
                                                'l', 'b', 'z', 'u'>>;
};

struct LHA : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'h', 'a'>;
   using match_groups = std::tuple<STRING_MATCH<shift<42, 0, 6>(),
                                                'l', 'h', 'a'>>;
};

struct LHAU : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'h', 'a', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<43, 0, 6>(),
                                                'l', 'h', 'a', 'u'>>;
};

struct LHZ : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'h', 'z'>;
   using match_groups = std::tuple<STRING_MATCH<shift<40, 0, 6>(),
                                                'l', 'h', 'z'>>;
};

struct LHZU : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'h', 'z', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<41, 0, 6>(),
                                                'l', 'h', 'z', 'u'>>;
};

struct LMW : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'm', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<46, 0, 6>(),
                                                'l', 'm', 'w'>>;
};

struct LWZ : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'w', 'z'>;
   using match_groups = std::tuple<STRING_MATCH<shift<32, 0, 6>(),
                                                'l', 'w', 'z'>>;
};

struct LWZU : LOAD_INTEGER_FAMILY {
   using lookup_key = cts<'l', 'w', 'z', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<33, 0, 6>(),
                                                'l', 'w', 'z', 'u'>>;
};

struct STORE_INTEGER_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RS_PARSE>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16>, CHARACTER_SEPARATOR<'('>>,
                                   REQUIRED_ARG<RA_PARSE, CHARACTER_SEPARATOR<')'>>,
                                   TERMINATION_PARSE>;
};

struct STB : STORE_INTEGER_FAMILY {
   using lookup_key = cts<'s', 't', 'b'>;
   using match_groups = std::tuple<STRING_MATCH<shift<38, 0, 6>(),
                                                's', 't', 'b'>>;
};

struct STBU : STORE_INTEGER_FAMILY {
   using lookup_key = cts<'s', 't', 'b', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<39, 0, 6>(),
                                                's', 't', 'b', 'u'>>;
};

struct STH : STORE_INTEGER_FAMILY {
   using lookup_key = cts<'s', 't', 'h'>;
   using match_groups = std::tuple<STRING_MATCH<shift<44, 0, 6>(),
                                                's', 't', 'h'>>;
};

struct STHU : STORE_INTEGER_FAMILY {
   using lookup_key = cts<'s', 't', 'h', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<45, 0, 6>(),
                                                's', 't', 'h', 'u'>>;
};

struct STMW : STORE_INTEGER_FAMILY {
   using lookup_key = cts<'s', 't', 'm', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<47, 0, 6>(),
                                                's', 't', 'm', 'w'>>;
};

struct STW : STORE_INTEGER_FAMILY {
   using lookup_key = cts<'s', 't', 'w'>;
   using match_groups = std::tuple<STRING_MATCH<shift<36, 0, 6>(),
                                                's', 't', 'w'>>;
};

struct STWU : STORE_INTEGER_FAMILY {
   using lookup_key = cts<'s', 't', 'w', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<37, 0, 6>(),
                                                's', 't', 'w', 'u'>>;
};

struct STORE_INTEGER_INDEXED_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RS_PARSE>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct STBUX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'b', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<247, 22, 9>(),
                                                's', 't', 'b', 'u', 'x'>>;
};

struct STBX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'b', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<215, 22, 9>(),
                                                's', 't', 'b', 'x'>>;
};

struct STHBRX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'h', 'b', 'r', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<918, 21, 10>(),
                                                's', 't', 'h', 'b', 'r', 'x'>>;
};

struct STHUX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'h', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<439, 21, 10>(),
                                                's', 't', 'h', 'u', 'x'>>;
};

struct STHX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'h', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<407, 21, 10>(),
                                                's', 't', 'h', 'x'>>;
};

struct STWBRX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'w', 'b', 'r', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<662, 21, 10>(),
                                                's', 't', 'w', 'b', 'r', 'x'>>;
};

struct STWUX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'w', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<183, 21, 10>(),
                                                's', 't', 'w', 'u', 'x'>>;
};

struct STWX : STORE_INTEGER_INDEXED_FAMILY {
   using lookup_key = cts<'s', 't', 'w', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<151, 21, 10>(),
                                                's', 't', 'w', 'x'>>;
};


struct LOAD_FLOAT_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FD_PARSE>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16>, CHARACTER_SEPARATOR<'('>>,
                                   REQUIRED_ARG<RA_PARSE, CHARACTER_SEPARATOR<')'>>,
                                   TERMINATION_PARSE>;
};

struct LFD : LOAD_FLOAT_FAMILY {
   using lookup_key = cts<'l', 'f', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<50, 0, 6>(),
                                                'l', 'f', 'd'>>;
};

struct LFDU : LOAD_FLOAT_FAMILY {
   using lookup_key = cts<'l', 'f', 'd', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<51, 0, 6>(),
                                                'l', 'f', 'd', 'u'>>;
};

struct LFDUX {
   using lookup_key = cts<'l', 'f', 'd', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<631, 21, 10>(),
                                                'l', 'f', 'd', 'u', 'x'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FD_PARSE>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct LFDX {
   using lookup_key = cts<'l', 'f', 'd', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<599, 21, 10>(),
                                                'l', 'f', 'd', 'x'>>;
   using parse_groups = LFDUX::parse_groups;
};

struct LFS : LOAD_FLOAT_FAMILY {
   using lookup_key = cts<'l', 'f', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<48, 0, 6>(),
                                                'l', 'f', 's'>>;
};

struct LFSU : LOAD_FLOAT_FAMILY {
   using lookup_key = cts<'l', 'f', 's', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<49, 0, 6>(),
                                                'l', 'f', 's', 'u'>>;
};

struct LFSUX {
   using lookup_key = cts<'l', 'f', 's', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<567, 21, 10>(),
                                                'l', 'f', 's', 'u', 'x'>>;
   using parse_groups = LFDUX::parse_groups;
};

struct LFSX {
   using lookup_key = cts<'l', 'f', 's', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<535, 21, 10>(),
                                                'l', 'f', 's', 'x'>>;
   using parse_groups = LFDUX::parse_groups;
};

struct STORE_FLOAT_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FS_PARSE>,
                                   REQUIRED_ARG<SIMM_PARSE<16, 16>, CHARACTER_SEPARATOR<'('>>,
                                   REQUIRED_ARG<RA_PARSE, CHARACTER_SEPARATOR<')'>>,
                                   TERMINATION_PARSE>;
};

struct STFD : STORE_FLOAT_FAMILY {
   using lookup_key = cts<'s', 't', 'f', 'd'>;
   using match_groups = std::tuple<STRING_MATCH<shift<54, 0, 6>(),
                                                's', 't', 'f', 'd'>>;
};

struct STFDU : STORE_FLOAT_FAMILY {
   using lookup_key = cts<'s', 't', 'f', 'd', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<55, 0, 6>(),
                                                's', 't', 'f', 'd', 'u'>>;
};

struct STFDUX {
   using lookup_key = cts<'s', 't', 'f', 'd', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<759, 21, 10>(),
                                                's', 't', 'f', 'd', 'u', 'x'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<FS_PARSE>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct STFDX {
   using lookup_key = cts<'s', 't', 'f', 'd', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<727, 21, 10>(),
                                                's', 't', 'f', 'd', 'x'>>;
   using parse_groups = STFDUX::parse_groups;
};

struct STFS : STORE_FLOAT_FAMILY {
   using lookup_key = cts<'s', 't', 'f', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<52, 0, 6>(),
                                                's', 't', 'f', 's'>>;
};

struct STFSU : STORE_FLOAT_FAMILY {
   using lookup_key = cts<'s', 't', 'f', 's', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<53, 0, 6>(),
                                                's', 't', 'f', 's', 'u'>>;
};

struct STFSUX {
   using lookup_key = cts<'s', 't', 'f', 's', 'u', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<695, 21, 10>(),
                                                's', 't', 'f', 's', 'u', 'x'>>;
   using parse_groups = STFDUX::parse_groups;
};

struct STFSX {
   using lookup_key = cts<'s', 't', 'f', 's', 'x'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<663, 21, 10>(),
                                                's', 't', 'f', 's', 'x'>>;
   using parse_groups = STFDUX::parse_groups;
};


//
// Move to/from SPR
//
struct MCRF {
   using lookup_key = cts<'m', 'c', 'r', 'f'>;
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>(),
                                                'm', 'c', 'r', 'f'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<CR_PARSE<6>>,
                                   REQ_ARG_FINAL<CR_PARSE<11>>,
                                   TERMINATION_PARSE>;
};

struct MCRFS {
   using lookup_key = cts<'m', 'c', 'r', 'f', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<64, 21, 10>(),
                                                'm', 'c', 'r', 'f', 's'>>;
   using parse_groups = MCRF::parse_groups;
};

struct MCRXR {
   using lookup_key = cts<'m', 'c', 'r', 'x', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<512, 21, 10>(),
                                                'm', 'c', 'r', 'x', 'r'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<CR_PARSE<6>>,
                                   TERMINATION_PARSE>;
};

struct MFCR {
   using lookup_key = cts<'m', 'f', 'c', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<19, 21, 10>(),
                                                'm', 'f', 'c', 'r'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<RD_PARSE>,
                                   TERMINATION_PARSE>;
};

struct MFFS {
   using lookup_key = cts<'m', 'f', 'f', 's'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<583, 21, 10>(),
                                                'm', 'f', 'f', 's'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<FD_PARSE>,
                                   TERMINATION_PARSE>;
};

struct MFMSR {
   using lookup_key = cts<'m', 'f', 'm', 's', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<83, 21, 10>(),
                                                'm', 'f', 'm', 's', 'r'>>;
   using parse_groups = MFCR::parse_groups;
};

struct MFSPR {
   using lookup_key = cts<'m', 'f', 's', 'p', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<339, 21, 10>(),
                                                'm', 'f', 's', 'p', 'r'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<PARSE_ANY<SPR_IMM_PARSE<11>, SPR_PARSE<11>>>,
                                   TERMINATION_PARSE>;
};

struct MFLR {
   using lookup_key = cts<'m', 'f', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<339, 21, 10>() |
                                                shift<0b0100000000, 11, 10>(),
                                                'm', 'f', 'l', 'r'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<RD_PARSE>,
                                   TERMINATION_PARSE>;
};

struct MFCTR {
   using lookup_key = cts<'m', 'f', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<339, 21, 10>() |
                                                shift<0b0100100000, 11, 10>(),
                                                'm', 'f', 'c', 't', 'r'>>;
   using parse_groups = MFLR::parse_groups;
};

struct MFXER {
   using lookup_key = cts<'m', 'f', 'x', 'e', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<339, 21, 10>() |
                                                shift<0b0000100000, 11, 10>(),
                                                'm', 'f', 'x', 'e', 'r'>>;
   using parse_groups = MFLR::parse_groups;
};

struct MTCRF {
   using lookup_key = cts<'m', 't', 'c', 'r', 'f'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<144, 21, 10>(),
                                                'm', 't', 'c', 'r', 'f'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<UIMM_PARSE<12, 8>>,
                                   REQ_ARG_FINAL<RS_PARSE>,
                                   TERMINATION_PARSE>;
};

struct MTCR {
   using lookup_key = cts<'m', 't', 'c', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<144, 21, 10>() |
                                                shift<0xff, 12, 8>(),
                                                'm', 't', 'c', 'r'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<RS_PARSE>,
                                   TERMINATION_PARSE>;
};

struct MTMSR {
   using lookup_key = cts<'m', 't', 'm', 's', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<146, 21, 10>(),
                                                'm', 't', 'm', 's', 'r'>>;

   using parse_groups = MTCR::parse_groups;
};

struct MTSPR {
   using lookup_key = cts<'m', 't', 's', 'p', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<467, 21, 10>(),
                                                'm', 't', 's', 'p', 'r'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<SPR_IMM_PARSE<11>, SPR_PARSE<11>>>,
                                   REQ_ARG_FINAL<RS_PARSE>,
                                   TERMINATION_PARSE>;
};

struct MTLR {
   using lookup_key = cts<'m', 't', 'l', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<467, 21, 10>() |
                                                shift<0b0100000000, 11, 10>(),
                                                'm', 't', 'l', 'r'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<RS_PARSE>,
                                   TERMINATION_PARSE>;
};

struct MTCTR {
   using lookup_key = cts<'m', 't', 'c', 't', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<467, 21, 10>() |
                                                shift<0b0100100000, 11, 10>(),
                                                'm', 't', 'c', 't', 'r'>>;
   using parse_groups = MTLR::parse_groups;
};

struct MTXER {
   using lookup_key = cts<'m', 't', 'x', 'e', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<467, 21, 10>() |
                                                shift<0b0000100000, 11, 10>(),
                                                'm', 't', 'x', 'e', 'r'>>;
   using parse_groups = MTLR::parse_groups;
};


//
// Bit manipulation instructions
//
struct RLWIMI {
   using lookup_key = cts<'r', 'l', 'w', 'i', 'm', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<20, 0, 6>(),
                                                'r', 'l', 'w', 'i', 'm', 'i'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_COMMA<RS_PARSE>,
                                   REQ_ARG_COMMA<UIMM_PARSE<16, 5>>,
                                   REQ_ARG_COMMA<UIMM_PARSE<21, 5>>,
                                   REQ_ARG_FINAL<UIMM_PARSE<26, 5>>,
                                   TERMINATION_PARSE>;
};

struct RLWINM {
   using lookup_key = cts<'r', 'l', 'w', 'i', 'n', 'm'>;
   using match_groups = std::tuple<STRING_MATCH<shift<21, 0, 6>(),
                                                'r', 'l', 'w', 'i', 'n', 'm'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = RLWIMI::parse_groups;
};

struct RLWNM {
   using lookup_key = cts<'r', 'l', 'w', 'n', 'm'>;
   using match_groups = std::tuple<STRING_MATCH<shift<23, 0, 6>(),
                                                'r', 'l', 'w', 'n', 'm'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_COMMA<RS_PARSE>,
                                   REQ_ARG_COMMA<RB_PARSE>,
                                   REQ_ARG_COMMA<UIMM_PARSE<21, 5>>,
                                   REQ_ARG_FINAL<UIMM_PARSE<26, 5>>,
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
#ifdef _USE_LINEAR_SEARCH_PARSE
   using instr_list =
      std::tuple<
                 // RRIA
                 ANDI_DOT, ANDIS_DOT, ORI, ORIS, XORI, XORIS, NOP,
                 // RRID
                 ADDI, ADDIC, ADDIC_DOT, ADDIS, MULLI, SUBFIC,
                 // RRRA
                 AND, ANDC, EQV, OR, NAND, NOR, ORC, SLW, SRAW, SRW, STBUX, STBX,
                 STHBRX, STHUX, STHX, STWBRX, STWUX, STWX, XOR,
                 // RRRD
                 ADD, ADDC, ADDE, DIVW, DIVWU, LBZUX, LBZX, LHAUX, LHAX, LHBRX,
                 LHZUX, LHZX, LWBRX, LWZUX, LWZX, MULHW, MULLW, SUBF, SUBFC, SUBFE,
                 SUB, SUBC,
                 // RR0A
                 CNTLZW, EXTSB, EXTSH, SRAWI,
                 // RR0D
                 ADDME, ADDZE, NEG, SUBFME, SUBFZE,
                 // CCC
                 CRAND, CRANDC, CREQV, CRNAND, CRNOR, CROR, CRORC, CRXOR,
                 // F0FD
                 FABS, FCTIW, FCTIWZ, FMR, FNABS, FNEG, FRES, FRSP, FRSQRTE,
                 // FFF0D
                 FADD, FADDS, FDIV, FDIVS, FSUB, FSUBS,
                 // FF0FD
                 FMUL, FMULS,
                 // FFFFD
                 FMADD, FMADDS, FMSUB, FMSUBS, FNMADD, FNMADDS, FNMSUB, FNMSUBS, FSEL,
                 // Float compare
                 FCMPO, FCMPU,
                 // Branch
                 BRANCH, BRANCH_COND, BEQ, BNE, BLT, BGE, BGT, BLE,
                 // Branch CTR
                 BRANCH_CTR_COND, BCTR, BEQCTR, BNECTR, BLTCTR, BGECTR, BGTCTR, BLECTR,
                 // Branch LR
                 BRANCH_LR_COND, BLR, BEQLR, BNELR, BLTLR, BGELR, BGTLR, BLELR,
                 // Compare
                 CMP, CMPI, CMPL, CMPLI, CMPW, CMPWI, CMPLW, CMPLWI,
                 // Memory
                 LBZ, LBZU, LHA, LHAU, LHZ, LHZU, LMW, LWZ, LWZU, STB, STBU, STH,
                 STHU, STMW, STW, STWU, LFD, LFDU, LFDUX, LFDX, LFS, LFSU, LFSUX,
                 LFSX, STFD, STFDU, STFDUX, STFDX, STFS, STFSU, STFSUX, STFSX,
                 // Move to/from SPR
                 MCRF, MCRFS, MCRXR, MFCR, MFFS, MFMSR, MFSPR, MFLR, MFCTR, MFXER,
                 MTCRF, MTCR, MTMSR, MTSPR, MTLR, MTCTR, MTXER,
                 // Bit manipulation
                 RLWIMI, RLWINM, RLWNM
                 >;
   return parse_instr_tuple(instr_list {}, cts<c...> {});
#else 
   /*
   constexpr auto instr_list_trie =
      create_trie<
                  // RRIA
                  ANDI_DOT, ANDIS_DOT, ORI, ORIS, XORI, XORIS, NOP,
                  // RRID
                  ADDI, ADDIC, ADDIC_DOT, ADDIS, MULLI, SUBFIC, LI, LIS,
                  // RRRA
                  AND, ANDC, EQV, OR, NAND, NOR, ORC, SLW, SRAW, SRW, STBUX, STBX,
                  STHBRX, STHUX, STHX, STWBRX, STWUX, STWX, XOR, MR,
                  // RRRD
                  ADD, ADDC, ADDE, DIVW, DIVWU, LBZUX, LBZX, LHAUX, LHAX, LHBRX,
                  LHZUX, LHZX, LWBRX, LWZUX, LWZX, MULHW, MULLW, SUBF, SUBFC, SUBFE,
                  SUB, SUBC,
                  // RR0A
                  CNTLZW, EXTSB, EXTSH, SRAWI,
                  // RR0D
                  ADDME, ADDZE, NEG, SUBFME, SUBFZE,
                  // CCC
                  CRAND, CRANDC, CREQV, CRNAND, CRNOR, CROR, CRORC, CRXOR,
                  // F0FD
                  FABS, FCTIW, FCTIWZ, FMR, FNABS, FNEG, FRES, FRSP, FRSQRTE,
                  // FFF0D
                  FADD, FADDS, FDIV, FDIVS, FSUB, FSUBS,
                  // FF0FD
                  FMUL, FMULS,
                  // FFFFD
                  FMADD, FMADDS, FMSUB, FMSUBS, FNMADD, FNMADDS, FNMSUB, FNMSUBS, FSEL,
                  // Float compare
                  FCMPO, FCMPU,
                  // Branch
                  BRANCH, BRANCH_COND, BEQ, BNE, BLT, BGE, BGT, BLE,
                  // Branch CTR
                  BRANCH_CTR_COND, BCTR, BEQCTR, BNECTR, BLTCTR, BGECTR, BGTCTR, BLECTR,
                  // Branch LR
                  BRANCH_LR_COND, BLR, BEQLR, BNELR, BLTLR, BGELR, BGTLR, BLELR,
                  // Compare
                  CMP, CMPI, CMPL, CMPLI, CMPW, CMPWI, CMPLW, CMPLWI,
                  // Memory
                  LBZ, LBZU, LHA, LHAU, LHZ, LHZU, LMW, LWZ, LWZU, STB, STBU, STH,
                  STHU, STMW, STW, STWU, LFD, LFDU, LFDUX, LFDX, LFS, LFSU, LFSUX,
                  LFSX, STFD, STFDU, STFDUX, STFDX, STFS, STFSU, STFSUX, STFSX,
                  // Move to/from SPR
                  MCRF, MCRFS, MCRXR, MFCR, MFFS, MFMSR, MFSPR, MFLR, MFCTR, MFXER,
                  MTCRF, MTCR, MTMSR, MTSPR, MTLR, MTCTR, MTXER,
                  // Bit manipulation
                  RLWIMI, RLWINM, RLWNM
                  >();
   force_failure(instr_list_trie);
   return parse_instr_trie(cts<c...> {}, instr_list_trie);
   */

   using cached_trie = trie_node<'\000', lookup_failure, trie_node<'o', lookup_failure, trie_node<'r', OR, trie_node<'i', ORI, trie_node<'s', ORIS>>, trie_node<'c', ORC>>>, trie_node<'x', lookup_failure, trie_node<'o', lookup_failure, trie_node<'r', XOR, trie_node<'i', XORI, trie_node<'s', XORIS>>>>>, trie_node<'d', lookup_failure, trie_node<'i', lookup_failure, trie_node<'v', lookup_failure, trie_node<'w', DIVW, trie_node<'u', DIVWU>>>>>, trie_node<'e', lookup_failure, trie_node<'q', lookup_failure, trie_node<'v', EQV>>, trie_node<'x', lookup_failure, trie_node<'t', lookup_failure, trie_node<'s', lookup_failure, trie_node<'b', EXTSB>, trie_node<'h', EXTSH>>>>>, trie_node<'a', lookup_failure, trie_node<'n', lookup_failure, trie_node<'d', AND, trie_node<'i', lookup_failure, trie_node<'.', ANDI_DOT>, trie_node<'s', lookup_failure, trie_node<'.', ANDIS_DOT>>>, trie_node<'c', ANDC>>>, trie_node<'d', lookup_failure, trie_node<'d', ADD, trie_node<'i', ADDI, trie_node<'c', ADDIC, trie_node<'.', ADDIC_DOT>>, trie_node<'s', ADDIS>>, trie_node<'c', ADDC>, trie_node<'e', ADDE>, trie_node<'m', lookup_failure, trie_node<'e', ADDME>>, trie_node<'z', lookup_failure, trie_node<'e', ADDZE>>>>>, trie_node<'n', lookup_failure, trie_node<'a', lookup_failure, trie_node<'n', lookup_failure, trie_node<'d', NAND>>>, trie_node<'o', lookup_failure, trie_node<'p', NOP>, trie_node<'r', NOR>>, trie_node<'e', lookup_failure, trie_node<'g', NEG>>>, trie_node<'f', lookup_failure, trie_node<'r', lookup_failure, trie_node<'e', lookup_failure, trie_node<'s', FRES>>, trie_node<'s', lookup_failure, trie_node<'p', FRSP>, trie_node<'q', lookup_failure, trie_node<'r', lookup_failure, trie_node<'t', lookup_failure, trie_node<'e', FRSQRTE>>>>>>, trie_node<'a', lookup_failure, trie_node<'b', lookup_failure, trie_node<'s', FABS>>, trie_node<'d', lookup_failure, trie_node<'d', FADD, trie_node<'s', FADDS>>>>, trie_node<'d', lookup_failure, trie_node<'i', lookup_failure, trie_node<'v', FDIV, trie_node<'s', FDIVS>>>>, trie_node<'m', lookup_failure, trie_node<'r', FMR>, trie_node<'u', lookup_failure, trie_node<'l', FMUL, trie_node<'s', FMULS>>>, trie_node<'a', lookup_failure, trie_node<'d', lookup_failure, trie_node<'d', FMADD, trie_node<'s', FMADDS>>>>, trie_node<'s', lookup_failure, trie_node<'u', lookup_failure, trie_node<'b', FMSUB, trie_node<'s', FMSUBS>>>>>, trie_node<'n', lookup_failure, trie_node<'a', lookup_failure, trie_node<'b', lookup_failure, trie_node<'s', FNABS>>>, trie_node<'e', lookup_failure, trie_node<'g', FNEG>>, trie_node<'m', lookup_failure, trie_node<'a', lookup_failure, trie_node<'d', lookup_failure, trie_node<'d', FNMADD, trie_node<'s', FNMADDS>>>>, trie_node<'s', lookup_failure, trie_node<'u', lookup_failure, trie_node<'b', FNMSUB, trie_node<'s', FNMSUBS>>>>>>, trie_node<'s', lookup_failure, trie_node<'u', lookup_failure, trie_node<'b', FSUB, trie_node<'s', FSUBS>>>, trie_node<'e', lookup_failure, trie_node<'l', FSEL>>>, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'i', lookup_failure, trie_node<'w', FCTIW, trie_node<'z', FCTIWZ>>>>, trie_node<'m', lookup_failure, trie_node<'p', lookup_failure, trie_node<'o', FCMPO>, trie_node<'u', FCMPU>>>>>, trie_node<'b', BRANCH, trie_node<'c', BRANCH_COND, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'r', BRANCH_CTR_COND>>>, trie_node<'t', lookup_failure, trie_node<'r', BCTR>>, trie_node<'l', lookup_failure, trie_node<'r', BRANCH_LR_COND>>>, trie_node<'e', lookup_failure, trie_node<'q', BEQ, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'r', BEQCTR>>>, trie_node<'l', lookup_failure, trie_node<'r', BEQLR>>>>, trie_node<'n', lookup_failure, trie_node<'e', BNE, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'r', BNECTR>>>, trie_node<'l', lookup_failure, trie_node<'r', BNELR>>>>, trie_node<'g', lookup_failure, trie_node<'e', BGE, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'r', BGECTR>>>, trie_node<'l', lookup_failure, trie_node<'r', BGELR>>>, trie_node<'t', BGT, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'r', BGTCTR>>>, trie_node<'l', lookup_failure, trie_node<'r', BGTLR>>>>, trie_node<'l', lookup_failure, trie_node<'r', BLR>, trie_node<'t', BLT, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'r', BLTCTR>>>, trie_node<'l', lookup_failure, trie_node<'r', BLTLR>>>, trie_node<'e', BLE, trie_node<'c', lookup_failure, trie_node<'t', lookup_failure, trie_node<'r', BLECTR>>>, trie_node<'l', lookup_failure, trie_node<'r', BLELR>>>>>, trie_node<'c', lookup_failure, trie_node<'n', lookup_failure, trie_node<'t', lookup_failure, trie_node<'l', lookup_failure, trie_node<'z', lookup_failure, trie_node<'w', CNTLZW>>>>>, trie_node<'r', lookup_failure, trie_node<'a', lookup_failure, trie_node<'n', lookup_failure, trie_node<'d', CRAND, trie_node<'c', CRANDC>>>>, trie_node<'e', lookup_failure, trie_node<'q', lookup_failure, trie_node<'v', CREQV>>>, trie_node<'n', lookup_failure, trie_node<'a', lookup_failure, trie_node<'n', lookup_failure, trie_node<'d', CRNAND>>>, trie_node<'o', lookup_failure, trie_node<'r', CRNOR>>>, trie_node<'o', lookup_failure, trie_node<'r', CROR, trie_node<'c', CRORC>>>, trie_node<'x', lookup_failure, trie_node<'o', lookup_failure, trie_node<'r', CRXOR>>>>, trie_node<'m', lookup_failure, trie_node<'p', CMP, trie_node<'i', CMPI>, trie_node<'w', CMPW, trie_node<'i', CMPWI>>, trie_node<'l', CMPL, trie_node<'i', CMPLI>, trie_node<'w', CMPLW, trie_node<'i', CMPLWI>>>>>>, trie_node<'l', lookup_failure, trie_node<'i', LI, trie_node<'s', LIS>>, trie_node<'b', lookup_failure, trie_node<'z', LBZ, trie_node<'x', LBZX>, trie_node<'u', LBZU, trie_node<'x', LBZUX>>>>, trie_node<'h', lookup_failure, trie_node<'b', lookup_failure, trie_node<'r', lookup_failure, trie_node<'x', LHBRX>>>, trie_node<'a', LHA, trie_node<'x', LHAX>, trie_node<'u', LHAU, trie_node<'x', LHAUX>>>, trie_node<'z', LHZ, trie_node<'x', LHZX>, trie_node<'u', LHZU, trie_node<'x', LHZUX>>>>, trie_node<'m', lookup_failure, trie_node<'w', LMW>>, trie_node<'w', lookup_failure, trie_node<'b', lookup_failure, trie_node<'r', lookup_failure, trie_node<'x', LWBRX>>>, trie_node<'z', LWZ, trie_node<'x', LWZX>, trie_node<'u', LWZU, trie_node<'x', LWZUX>>>>, trie_node<'f', lookup_failure, trie_node<'d', LFD, trie_node<'u', LFDU, trie_node<'x', LFDUX>>, trie_node<'x', LFDX>>, trie_node<'s', LFS, trie_node<'u', LFSU, trie_node<'x', LFSUX>>, trie_node<'x', LFSX>>>>, trie_node<'s', lookup_failure, trie_node<'l', lookup_failure, trie_node<'w', SLW>>, trie_node<'r', lookup_failure, trie_node<'w', SRW>, trie_node<'a', lookup_failure, trie_node<'w', SRAW, trie_node<'i', SRAWI>>>>, trie_node<'u', lookup_failure, trie_node<'b', SUB, trie_node<'c', SUBC>, trie_node<'f', SUBF, trie_node<'i', lookup_failure, trie_node<'c', SUBFIC>>, trie_node<'c', SUBFC>, trie_node<'e', SUBFE>, trie_node<'m', lookup_failure, trie_node<'e', SUBFME>>, trie_node<'z', lookup_failure, trie_node<'e', SUBFZE>>>>>, trie_node<'t', lookup_failure, trie_node<'b', STB, trie_node<'x', STBX>, trie_node<'u', STBU, trie_node<'x', STBUX>>>, trie_node<'h', STH, trie_node<'b', lookup_failure, trie_node<'r', lookup_failure, trie_node<'x', STHBRX>>>, trie_node<'x', STHX>, trie_node<'u', STHU, trie_node<'x', STHUX>>>, trie_node<'m', lookup_failure, trie_node<'w', STMW>>, trie_node<'w', STW, trie_node<'b', lookup_failure, trie_node<'r', lookup_failure, trie_node<'x', STWBRX>>>, trie_node<'x', STWX>, trie_node<'u', STWU, trie_node<'x', STWUX>>>, trie_node<'f', lookup_failure, trie_node<'d', STFD, trie_node<'u', STFDU, trie_node<'x', STFDUX>>, trie_node<'x', STFDX>>, trie_node<'s', STFS, trie_node<'u', STFSU, trie_node<'x', STFSUX>>, trie_node<'x', STFSX>>>>>, trie_node<'m', lookup_failure, trie_node<'r', MR>, trie_node<'u', lookup_failure, trie_node<'l', lookup_failure, trie_node<'h', lookup_failure, trie_node<'w', MULHW>>, trie_node<'l', lookup_failure, trie_node<'i', MULLI>, trie_node<'w', MULLW>>>>, trie_node<'c', lookup_failure, trie_node<'r', lookup_failure, trie_node<'f', MCRF, trie_node<'s', MCRFS>>, trie_node<'x', lookup_failure, trie_node<'r', MCRXR>>>>, trie_node<'f', lookup_failure, trie_node<'f', lookup_failure, trie_node<'s', MFFS>>, trie_node<'m', lookup_failure, trie_node<'s', lookup_failure, trie_node<'r', MFMSR>>>, trie_node<'s', lookup_failure, trie_node<'p', lookup_failure, trie_node<'r', MFSPR>>>, trie_node<'l', lookup_failure, trie_node<'r', MFLR>>, trie_node<'c', lookup_failure, trie_node<'r', MFCR>, trie_node<'t', lookup_failure, trie_node<'r', MFCTR>>>, trie_node<'x', lookup_failure, trie_node<'e', lookup_failure, trie_node<'r', MFXER>>>>, trie_node<'t', lookup_failure, trie_node<'m', lookup_failure, trie_node<'s', lookup_failure, trie_node<'r', MTMSR>>>, trie_node<'s', lookup_failure, trie_node<'p', lookup_failure, trie_node<'r', MTSPR>>>, trie_node<'l', lookup_failure, trie_node<'r', MTLR>>, trie_node<'c', lookup_failure, trie_node<'r', MTCR, trie_node<'f', MTCRF>>, trie_node<'t', lookup_failure, trie_node<'r', MTCTR>>>, trie_node<'x', lookup_failure, trie_node<'e', lookup_failure, trie_node<'r', MTXER>>>>>, trie_node<'r', lookup_failure, trie_node<'l', lookup_failure, trie_node<'w', lookup_failure, trie_node<'i', lookup_failure, trie_node<'m', lookup_failure, trie_node<'i', RLWIMI>>, trie_node<'n', lookup_failure, trie_node<'m', RLWINM>>>, trie_node<'n', lookup_failure, trie_node<'m', RLWNM>>>>>>;
   return parse_instr_trie(cts<c...> {}, cached_trie {});

#endif
}
}

#define GEKKO_ASM(s) gekko_code_gen::parse(GEN_STR(s))
