#include "gekko_code_gen.hh"
#include <iostream>

int main() {
   constexpr auto l1 = GEN_STR("add r8, r2, r16");
   constexpr auto l2 = GEN_STR("addi r8, r2, 0x123");
   constexpr auto l3 = GEN_STR("addic. r0, r1, -1");
   constexpr auto l4 = GEN_STR("bl 0x12341");
   constexpr auto l5 = GEN_STR("cmpw r3, r4");
   constexpr auto l6 = GEN_STR("cmpw cr0, r3, r4");
   constexpr auto l7 = GEN_STR("cmpw cr2, r3, r4");
   constexpr auto l8 = GEN_STR("blr");
   constexpr auto l9 = GEN_STR("beqlr");
   constexpr auto l10 = GEN_STR("bnelr");
   constexpr auto l11 = GEN_STR("bltlr");
   constexpr auto l12 = GEN_STR("bgelr");
   constexpr auto l13 = GEN_STR("bgtlr");
   constexpr auto l14 = GEN_STR("blelr");
   constexpr uint32_t i1 = gekko_code_gen::parse(l1);
   constexpr uint32_t i2 = gekko_code_gen::parse(l2);
   constexpr uint32_t i3 = gekko_code_gen::parse(l3);
   constexpr uint32_t i4 = gekko_code_gen::parse(l4);
   constexpr uint32_t i5 = gekko_code_gen::parse(l5);
   constexpr uint32_t i6 = gekko_code_gen::parse(l6);
   constexpr uint32_t i7 = gekko_code_gen::parse(l7);
   constexpr uint32_t i8 = gekko_code_gen::parse(l8);
   constexpr uint32_t i9 = gekko_code_gen::parse(l9);
   constexpr uint32_t i10 = gekko_code_gen::parse(l10);
   constexpr uint32_t i11 = gekko_code_gen::parse(l11);
   constexpr uint32_t i12 = gekko_code_gen::parse(l12);
   constexpr uint32_t i13 = gekko_code_gen::parse(l13);
   constexpr uint32_t i14 = gekko_code_gen::parse(l14);
   std::cout << l1.as_arr << " => " << std::hex << i1 << std::endl;
   std::cout << l2.as_arr << " => " << std::hex << i2 << std::endl;
   std::cout << l3.as_arr << " => " << std::hex << i3 << std::endl;
   std::cout << l4.as_arr << " => " << std::hex << i4 << std::endl;
   std::cout << l5.as_arr << " => " << std::hex << i5 << std::endl;
   std::cout << l6.as_arr << " => " << std::hex << i6 << std::endl;
   std::cout << l7.as_arr << " => " << std::hex << i7 << std::endl;
   std::cout << l8.as_arr << " => " << std::hex << i8 << std::endl;
   std::cout << l9.as_arr << " => " << std::hex << i9 << std::endl;
   std::cout << l10.as_arr << " => " << std::hex << i10 << std::endl;
   std::cout << l11.as_arr << " => " << std::hex << i11 << std::endl;
   std::cout << l12.as_arr << " => " << std::hex << i12 << std::endl;
   std::cout << l13.as_arr << " => " << std::hex << i13 << std::endl;
   std::cout << l14.as_arr << " => " << std::hex << i14 << std::endl;
}
