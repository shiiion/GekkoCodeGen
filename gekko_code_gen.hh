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

using RA_PARSE = GPR_PARSE<11>;
using RB_PARSE = GPR_PARSE<16>;
using RD_PARSE = GPR_PARSE<6>;
using RS_PARSE = GPR_PARSE<6>;

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

struct OR : RRRA_FAMILY {
   using lookup_key = cts<'o', 'r'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<444, 21, 10>(),
                                                'o', 'r'>,
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


//
// CRB-CRB-CRB instructions
//
struct CCC_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<PARSE_ANY<UIMM_PARSE<6, 5>, CR0_BIT_PARSE<6, 5>>>,
                                   REQ_ARG_FINAL<PARSE_ANY<UIMM_PARSE<11, 5>, CR0_BIT_PARSE<11, 5>>>,
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
   using match_groups = std::tuple<STRING_MATCH<shift<19, 0, 6>() | shift<193, 21, 10>(),
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

struct FMUL {
   using lookup_key = cts<'f', 'm', 'u', 'l'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<25, 26, 5>(),
                                                'f', 'm', 'u', 'l'>,
                                   CHARACTER_BIT_MATCH<31, '.'>>;
};

struct FMULS {
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
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<0, 21, 10>(),
                                                'f', 'c', 'm', 'p', 'o'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<CR_PARSE<6>>,
                                   REQ_ARG_COMMA<FA_PARSE>,
                                   REQ_ARG_FINAL<FB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct FCMPU {
   using lookup_key = cts<'f', 'c', 'm', 'p', 'u'>;
   using match_groups = std::tuple<STRING_MATCH<shift<63, 0, 6>() | shift<32, 21, 10>(),
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
                                   REQ_ARG_FINAL<SIMM_PARSE<6, 26, 2>>,
                                   TERMINATION_PARSE>;
};

struct BRANCH_COND {
   using lookup_key = cts<'b', 'c'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>(), 'b', 'c'>,
                                   CHARACTER_BIT_MATCH<31, 'l'>,
                                   CHARACTER_BIT_MATCH<30, 'a'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<UIMM_PARSE<6, 5>>,
                                   REQ_ARG_COMMA<UIMM_PARSE<11, 5>>,
                                   REQ_ARG_FINAL<SIMM_PARSE<16, 16, 2>>,
                                   TERMINATION_PARSE>;
};

struct Bxx_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_FINAL<SIMM_PARSE<16, 16, 2>>,
                                   TERMINATION_PARSE>;
};

struct BEQ : Bxx_FAMILY {
   using lookup_key = cts<'b', 'e', 'q'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'e', 'q'>>;
};

struct BNE : Bxx_FAMILY {
   using lookup_key = cts<'b', 'n', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<2, 11, 5>(),
                                                'b', 'n', 'e'>>;
};

struct BLT : Bxx_FAMILY {
   using lookup_key = cts<'b', 'l', 't'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'l', 't'>>;
};

struct BGE : Bxx_FAMILY {
   using lookup_key = cts<'b', 'g', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<0, 11, 5>(),
                                                'b', 'g', 'e'>>;
};

struct BGT : Bxx_FAMILY {
   using lookup_key = cts<'b', 'g', 't'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<12, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'g', 't'>>;
};

struct BLE : Bxx_FAMILY {
   using lookup_key = cts<'b', 'l', 'e'>;
   using match_groups = std::tuple<STRING_MATCH<shift<16, 0, 6>() |
                                                shift<4, 6, 5>() | shift<1, 11, 5>(),
                                                'b', 'l', 'e'>>;
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
                                   REQ_ARG_COMMA<UIMM_PARSE<10, 1>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CMPI {
   using lookup_key = cts<'c', 'm', 'p', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>>,
                                   REQ_ARG_COMMA<UIMM_PARSE<10, 1>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<SIMM_PARSE<16, 16>>,
                                   TERMINATION_PARSE>;
};

struct CMPL {
   using lookup_key = cts<'c', 'm', 'p', 'l'>;
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<32, 21, 11>(),
                                                'c', 'm', 'p', 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>>,
                                   REQ_ARG_COMMA<UIMM_PARSE<10, 1>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CMPLI {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'l'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   REQ_ARG_COMMA<PARSE_ANY<UIMM_PARSE<6, 3>, CR_PARSE<6>>>,
                                   REQ_ARG_COMMA<UIMM_PARSE<10, 1>>,
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
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<32, 21, 11>(),
                                                'c', 'm', 'p', 'l', 'w'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<RB_PARSE>,
                                   TERMINATION_PARSE>;
};

struct CMPLWI {
   using lookup_key = cts<'c', 'm', 'p', 'l', 'w', 'i'>;
   using match_groups = std::tuple<STRING_MATCH<shift<11, 0, 6>(), 'c', 'm', 'p', 'l', 'w', 'i'>>;
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   OPTIONAL_ARG<0, CR_PARSE<6>, CHARACTER_SEPARATOR<','>>,
                                   REQ_ARG_COMMA<RA_PARSE>,
                                   REQ_ARG_FINAL<UIMM_PARSE<16, 16>>,
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
//   using instr_list =
//      std::tuple<
//                 // RRIA
//                 ANDI_DOT, ANDIS_DOT,
//                 // RRID
//                 ADDI, ADDIC, ADDIC_DOT, ADDIS,
//                 // RRRA
//                 AND, ANDC, EQV, OR,
//                 // RRRD
//                 ADD, ADDC, ADDE, DIVW, DIVWU,
//                 // RR0A
//                 CNTLZW, EXTSB, EXTSH,
//                 // RR0D
//                 ADDME, ADDZE,
//                 // CCC
//                 CRAND, CRANDC, CREQV, CRNAND, CRNOR, CROR, CRORC, CRXOR,
//                 // F0FD
//                 FABS, FCTIW, FCTIWZ, FMR, FNABS, FNEG, FRES, FRSP, FRSQRTE,
//                 // FFF0D
//                 FADD, FADDS, FDIV, FDIVS, FSUB, FSUBS,
//                 // FF0FD
//                 FMUL, FMULS,
//                 // FFFFD
//                 FMADD, FMADDS, FMSUB, FMSUBS, FNMADD, FNMADDS, FNMSUB, FNMSUBS, FSEL,
//                 // Float compare
//                 FCMPO, FCMPU,
//                 // Branch
//                 BRANCH, BRANCH_COND, BEQ, BNE, BLT, BGE, BGT, BLE,
//                 // Branch CTR
//                 BRANCH_CTR_COND, BCTR, BEQCTR, BNECTR, BLTCTR, BGECTR, BGTCTR, BLECTR,
//                 // Branch LR
//                 BRANCH_LR_COND, BLR, BEQLR, BNELR, BLTLR, BGELR, BGTLR, BLELR,
//                 // Compare
//                 CMP, CMPI, CMPL, CMPLI, CMPW, CMPWI, CMPLW, CMPLWI
//                 >;
//   return parse_instr_tuple(instr_list {}, cts<c...> {});
   constexpr auto instr_list_trie =
      create_trie<
                  // RRIA
                  ANDI_DOT, ANDIS_DOT,
                  // RRID
                  ADDI, ADDIC, ADDIC_DOT, ADDIS,
                  // RRRA
                  AND, ANDC, EQV, OR,
                  // RRRD
                  ADD, ADDC, ADDE, DIVW, DIVWU,
                  // RR0A
                  CNTLZW, EXTSB, EXTSH,
                  // RR0D
                  ADDME, ADDZE,
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
                  CMP, CMPI, CMPL, CMPLI, CMPW, CMPWI, CMPLW, CMPLWI
                  >();

                  
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
