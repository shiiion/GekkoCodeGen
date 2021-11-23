#pragma once

#include "cts.hh"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <tuple>
#include <type_traits>

namespace gekko_code_gen {
enum class ParseGroupType {
   SEPARATOR,
   PARSER,
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

template <uint32_t off>
struct GPR_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      static_assert(sizeof...(c) > 1, "Malformed GPR. Must be of the form 'rX'");
      if constexpr (sizeof...(c) > 1) {
         static_assert(nth_char<0, c...>::val == 'r', "Malformed GPR. Must start with 'r'");
         if constexpr (nth_char<0, c...>::val == 'r') {
            constexpr uint32_t regnum = decimal_atoi((typename cts<c...>::trim<1>) {});
            static_assert(regnum < 32, "Invalid GPR number. Only valid numbers 0-31");
            if constexpr (regnum < 32) {
               return shift<regnum, off, 5>();
            } else {
               return 0;
            }
         } else {
            return 0;
         }
      } else {
         return 0;
      }
   }
};

template <uint32_t off>
struct FPR_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      static_assert(sizeof...(c) > 1, "Malformed FPR. Must be of the form 'fX'");
      if constexpr (sizeof...(c) > 1) {
         static_assert(nth_char<0, c...>::val == 'f', "Malformed GPR. Must start with 'f'");
         if constexpr (nth_char<0, c...>::val == 'f') {
            constexpr uint32_t regnum = decimal_atoi((typename cts<c...>::trim<1>) {});
            static_assert(regnum < 32, "Invalid FPR number. Only valid numbers 0-31");
            if constexpr (regnum < 32) {
               return shift<regnum, off, 5>();
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

template <uint32_t off, uint32_t width>
struct UIMM_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      constexpr uint32_t val = atoi_full(cts<c...> {});
      constexpr bool valid = (1 << width) > val;
      static_assert(valid, "Invalid range for UIMM");
      if constexpr (valid) {
         return shift<val, off, width>();
      } else {
         return std::nullopt;
      }
   }
};

template <uint32_t off, uint32_t width>
struct SIMM_PARSE {
   constexpr static ParseGroupType p_type = ParseGroupType::PARSER;

   template <char... c>
   constexpr static std::optional<uint32_t> parse(cts<c...>) {
      constexpr int32_t val = static_cast<int32_t>(atoi_full(cts<c...> {}));
      constexpr int32_t max = static_cast<int32_t>(1 << (width - 1)) - 1;
      constexpr int32_t min = -(max + 1);
      constexpr bool valid = (val >= min) && (val <= max);
      static_assert(valid, "Invalid range for SIMM");
      if (valid) {
         return shift<static_cast<uint32_t>(val), off, width>();
      } else {
         return std::nullopt;
      }
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

struct ADDI_FAMILY {
   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   GPR_PARSE<6>, CHARACTER_SEPARATOR<','>,
                                   GPR_PARSE<11>, CHARACTER_SEPARATOR<','>,
                                   SIMM_PARSE<16, 16>, WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct ADDI : ADDI_FAMILY {
   using match_groups = std::tuple<STRING_MATCH<shift<14, 0, 6>(),
                                                'a', 'd', 'd', 'i'>>;
};

struct ADDIC : ADDI_FAMILY {
   using match_groups = std::tuple<STRING_MATCH<shift<12, 0, 6>(),
                                                'a', 'd', 'd', 'i', 'c'>>;
};

struct ADDIC_DOT : ADDI_FAMILY {
   using match_groups = std::tuple<STRING_MATCH<shift<13, 0, 6>(),
                                                'a', 'd', 'd', 'i', 'c', '.'>>;
};

struct ADDIS : ADDI_FAMILY {
   using match_groups = std::tuple<STRING_MATCH<shift<15, 0, 6>(),
                                                'a', 'd', 'd', 'i', 's'>>;
};

struct ADD {
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<266, 22, 9>(),
                                                'a', 'd', 'd'>,
                                   CHARACTER_MATCH<shift<1, 21, 1>(), 'o'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;

   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   GPR_PARSE<6>, CHARACTER_SEPARATOR<','>,
                                   GPR_PARSE<11>, CHARACTER_SEPARATOR<','>,
                                   GPR_PARSE<16>, WHITESPACE_SEPARATOR,
                                   TERMINATION_PARSE>;
};

struct OR {
   using match_groups = std::tuple<STRING_MATCH<shift<31, 0, 6>() | shift<444, 21, 10>(),
                                                'o', 'r'>,
                                   CHARACTER_MATCH<shift<1, 31, 1>(), '.'>>;

   using parse_groups = std::tuple<WHITESPACE_SEPARATOR,
                                   GPR_PARSE<6>, CHARACTER_SEPARATOR<','>,
                                   GPR_PARSE<11>, CHARACTER_SEPARATOR<','>,
                                   GPR_PARSE<16>, WHITESPACE_SEPARATOR,
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
   } else if constexpr (PG0::p_type == ParseGroupType::PARSER) {
      static_assert(sizeof...(PG) > 0, "Instruction ends in bad parser type.");
      using next_sep = std::decay_t<decltype(std::get<0>(std::tuple<PG...> {}))>;
      constexpr auto clipped_str = next_sep::clipto(cts<c...> {});
      constexpr auto parsed_value = PG0::parse(clipped_str);
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

template <typename Inst0, typename... Inst, char... c>
constexpr uint32_t try_parse_inst(std::tuple<Inst0, Inst...>, cts<c...>) {
   constexpr auto header = WHITESPACE_SEPARATOR::clipto(cts<c...> {});
   constexpr bool match_result = run_match<Inst0>(header);
   static_assert(match_result || sizeof...(Inst) > 0, "Failed to parse instruction");
   if constexpr (match_result) {
      constexpr uint32_t base = get_match_data<Inst0>(header);
      return base | parse_inst_body<Inst0>(eat_match<Inst0>(cts<c...> {}));
   } else if (sizeof...(Inst) > 0) {
      return try_parse_inst(std::tuple<Inst...> {}, cts<c...>{});
   } else {
      return 0;
   }
}

template <char... c>
constexpr uint32_t parse(cts<c...>) {
   using instr_list = std::tuple<ADD, OR, ADDI, ADDIC, ADDIC_DOT, ADDIS>;
   
   return try_parse_inst(instr_list {}, cts<c...> {});
}
}
