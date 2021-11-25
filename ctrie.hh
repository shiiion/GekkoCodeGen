#pragma once

#include "cts.hh"

#include <cstddef>
#include <tuple>

namespace gekko_code_gen {

struct lookup_failure {};

template <char _key, typename _val, typename... Subnode>
struct trie_node {
   constexpr static bool leaf = sizeof...(Subnode) == 0;
   constexpr static char key = _key;
   using val = _val;
};

template <typename NewSubnode, char _key, typename _val, typename... Subnode>
constexpr static auto prepend(trie_node<_key, _val, Subnode...>) {
   return trie_node<_key, _val, NewSubnode, Subnode...> {};
}

template <typename ReplaceSubnode, char _key, typename _val, typename Sn0>
constexpr auto replace_subnode() {
   if constexpr (Sn0::key == ReplaceSubnode::key) {
      return trie_node<_key, _val, ReplaceSubnode> {};
   } else {
      return trie_node<_key, _val, Sn0> {};
   }
}

template <typename ReplaceSubnode, char _key, typename _val, typename Sn0, typename Sn1, typename... Subnode>
constexpr auto replace_subnode() {
   if constexpr (Sn0::key == ReplaceSubnode::key) {
      return trie_node<_key, _val, ReplaceSubnode, Sn1, Subnode...> {};
   } else {
      constexpr auto trie_repl = replace_subnode<ReplaceSubnode, _key, _val, Sn1, Subnode...>();
      return prepend<Sn0>(trie_repl);
   }
}

template <char c>
constexpr auto find_match() {
   return lookup_failure {};
}

template <char c, typename N0>
constexpr auto find_match() {
   if constexpr (c == N0::key) {
      return N0 {};
   } else {
      return lookup_failure {};
   }
}

template <char c, typename N0, typename N1, typename... NodeList>
constexpr auto find_match() {
   if constexpr (c == N0::key) {
      return N0 {};
   } else {
      return find_match<c, N1, NodeList...>();
   }
}

template <char _cur_key, typename _target_val, char... c>
constexpr auto build_trie(cts<c...>) {
   if constexpr (sizeof...(c) == 0) {
      return trie_node<_cur_key, _target_val> {};
   } else {
      constexpr auto new_subnode =
         build_trie<nth_char<0, c...>::val, _target_val>(typename cts<c...>::trim<1> {});
      return trie_node<_cur_key,
                       lookup_failure,
                       std::decay_t<decltype(build_trie<nth_char<0, c...>::val, _target_val>(
                                             typename cts<c...>::trim<1> {}))>> {};
   }
}

template <typename _insert_val, char _key, typename _val, typename... Subnode, char... c>
constexpr auto insert_trie(trie_node<_key, _val, Subnode...>, cts<c...>) {
   if constexpr (sizeof...(c) == 0) {
      // Update current node
      return trie_node<_key, _insert_val, Subnode...> {};
   } else {
      constexpr char c0 = nth_char<0, c...>::val;
      constexpr auto subnode_match = find_match<c0, Subnode...>();
      if constexpr (std::is_same_v<std::decay_t<decltype(subnode_match)>, lookup_failure>) {
         constexpr auto new_subnode =
            build_trie<c0, _insert_val>(typename cts<c...>::trim<1> {});
         return prepend<std::decay_t<decltype(new_subnode)>>(trie_node<_key, _val, Subnode...>());
      } else {
         constexpr auto updated_subnode =
            insert_trie<_insert_val>(subnode_match, typename cts<c...>::trim<1> {});
         return replace_subnode<std::decay_t<decltype(updated_subnode)>, _key, _val, Subnode...>();
      }
   }
}

template <typename T0>
constexpr auto create_trie() {
   return build_trie<0, T0>(typename T0::lookup_key {});
}

template <typename T0, typename T1, typename... Ts>
constexpr auto create_trie() {
   constexpr auto built_trie = create_trie<T1, Ts...>();
   return insert_trie<T0>(built_trie, typename T0::lookup_key {});
}

template <char _key, typename _val, typename... Subnode, char... c>
constexpr auto lookup_trie(cts<c...>, trie_node<_key, _val, Subnode...>) {
   if constexpr (sizeof...(c) == 0) {
      return _val {};
   } else {
      constexpr auto subnode_match = find_match<nth_char<0, c...>::val, Subnode...>();
      if constexpr (std::is_same_v<std::decay_t<decltype(subnode_match)>, lookup_failure>) {
         return lookup_failure {};
      } else {
         return lookup_trie(typename cts<c...>::trim<1> {}, subnode_match);
      }
   }
}

template <char _key, typename _val, typename... Subnode, char... c>
constexpr auto lookup_nearest_trie(cts<c...>, trie_node<_key, _val, Subnode...>) {
   if constexpr (sizeof...(c) == 0) {
      return _val {};
   } else {
      constexpr auto subnode_match = find_match<nth_char<0, c...>::val, Subnode...>();
      if constexpr (std::is_same_v<std::decay_t<decltype(subnode_match)>, lookup_failure>) {
         return _val {};
      } else {
         constexpr auto longest_submatch =
            lookup_nearest_trie(typename cts<c...>::trim<1> {}, subnode_match);
         if constexpr (std::is_same_v<std::decay_t<decltype(longest_submatch)>, lookup_failure>) {
            return _val {};
         } else {
            return longest_submatch;
         }
      }
   }
}
}
