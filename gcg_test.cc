#include "gekko_code_gen.hh"
#include <iostream>

int main() {
   constexpr auto l1 = GEN_STR("add r8, r2, r16");
   constexpr auto l2 = GEN_STR("addi r8, r2, 0x123");
   constexpr auto l3 = GEN_STR("addic. r0, r1, -1");
   constexpr auto l4 = GEN_STR("bl 0x12341");
   constexpr uint32_t i1 = gekko_code_gen::parse(l1);
   constexpr uint32_t i2 = gekko_code_gen::parse(l2);
   constexpr uint32_t i3 = gekko_code_gen::parse(l3);
   constexpr uint32_t i4 = gekko_code_gen::parse(l4);
   std::cout << l1.as_arr << " => " << std::hex << i1 << std::endl;
   std::cout << l2.as_arr << " => " << std::hex << i2 << std::endl;
   std::cout << l3.as_arr << " => " << std::hex << i3 << std::endl;
   std::cout << l4.as_arr << " => " << std::hex << i4 << std::endl;
}
