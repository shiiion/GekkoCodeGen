#include "gekko_code_gen.hh"
#include <iostream>

int main() {
   gekko_code_gen::parse(GEN_STR("rlwinm r0, r2, 5, 5, 5"));
}
