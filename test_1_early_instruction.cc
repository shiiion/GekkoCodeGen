#include "gekko_code_gen.hh"
#include <iostream>

int main() {
   gekko_code_gen::parse(GEN_STR("andi. r3, r4, 0x1000"));
}