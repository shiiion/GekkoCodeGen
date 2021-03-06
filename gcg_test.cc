#include "gekko_code_gen.hh"
#include <iostream>

int main() {
   static_assert(0x70831000 == GEKKO_ASM("andi. r3, r4, 0x1000"));
   static_assert(0x74831000 == GEKKO_ASM("andis. r3, r4, 0x1000"));
   static_assert(0x60831000 == GEKKO_ASM("ori r3, r4, 0x1000"));
   static_assert(0x64831000 == GEKKO_ASM("oris r3, r4, 0x1000"));
   static_assert(0x68831000 == GEKKO_ASM("xori r3, r4, 0x1000"));
   static_assert(0x6c831000 == GEKKO_ASM("xoris r3, r4, 0x1000"));
   static_assert(0x60000000 == GEKKO_ASM("nop"));
   static_assert(0x38641234 == GEKKO_ASM("addi r3, r4, 0x1234"));
   static_assert(0x30641234 == GEKKO_ASM("addic r3, r4, 0x1234"));
   static_assert(0x34641234 == GEKKO_ASM("addic. r3, r4, 0x1234"));
   static_assert(0x3c641234 == GEKKO_ASM("addis r3, r4, 0x1234"));
   static_assert(0x1c641234 == GEKKO_ASM("mulli r3, r4, 0x1234"));
   static_assert(0x20641234 == GEKKO_ASM("subfic r3, r4, 0x1234"));
   static_assert(0x7c832838 == GEKKO_ASM("and r3, r4, r5"));
   static_assert(0x7c832839 == GEKKO_ASM("and. r3, r4, r5"));
   static_assert(0x7c832878 == GEKKO_ASM("andc r3, r4, r5"));
   static_assert(0x7c832879 == GEKKO_ASM("andc. r3, r4, r5"));
   static_assert(0x7c832a38 == GEKKO_ASM("eqv r3, r4, r5"));
   static_assert(0x7c832a39 == GEKKO_ASM("eqv. r3, r4, r5"));
   static_assert(0x7c832b78 == GEKKO_ASM("or r3, r4, r5"));
   static_assert(0x7c832b79 == GEKKO_ASM("or. r3, r4, r5"));
   static_assert(0x7c832bb8 == GEKKO_ASM("nand r3, r4, r5"));
   static_assert(0x7c832bb9 == GEKKO_ASM("nand. r3, r4, r5"));
   static_assert(0x7c8328f8 == GEKKO_ASM("nor r3, r4, r5"));
   static_assert(0x7c8328f9 == GEKKO_ASM("nor. r3, r4, r5"));
   static_assert(0x7c832b38 == GEKKO_ASM("orc r3, r4, r5"));
   static_assert(0x7c832b39 == GEKKO_ASM("orc. r3, r4, r5"));
   static_assert(0x7c832830 == GEKKO_ASM("slw r3, r4, r5"));
   static_assert(0x7c832831 == GEKKO_ASM("slw. r3, r4, r5"));
   static_assert(0x7c832e30 == GEKKO_ASM("sraw r3, r4, r5"));
   static_assert(0x7c832e31 == GEKKO_ASM("sraw. r3, r4, r5"));
   static_assert(0x7c832c30 == GEKKO_ASM("srw r3, r4, r5"));
   static_assert(0x7c832c31 == GEKKO_ASM("srw. r3, r4, r5"));
   static_assert(0x7c6429ee == GEKKO_ASM("stbux r3, r4, r5"));
   static_assert(0x7c6429ae == GEKKO_ASM("stbx r3, r4, r5"));
   static_assert(0x7c642f2c == GEKKO_ASM("sthbrx r3, r4, r5"));
   static_assert(0x7c642b6e == GEKKO_ASM("sthux r3, r4, r5"));
   static_assert(0x7c642b2e == GEKKO_ASM("sthx r3, r4, r5"));
   static_assert(0x7c642d2c == GEKKO_ASM("stwbrx r3, r4, r5"));
   static_assert(0x7c64296e == GEKKO_ASM("stwux r3, r4, r5"));
   static_assert(0x7c64292e == GEKKO_ASM("stwx r3, r4, r5"));
   static_assert(0x7c832a78 == GEKKO_ASM("xor r3, r4, r5"));
   static_assert(0x7c832a79 == GEKKO_ASM("xor. r3, r4, r5"));
   static_assert(0x7c642a14 == GEKKO_ASM("add r3, r4, r5"));
   static_assert(0x7c642e14 == GEKKO_ASM("addo r3, r4, r5"));
   static_assert(0x7c642a15 == GEKKO_ASM("add. r3, r4, r5"));
   static_assert(0x7c642e15 == GEKKO_ASM("addo. r3, r4, r5"));
   static_assert(0x7c642814 == GEKKO_ASM("addc r3, r4, r5"));
   static_assert(0x7c642c14 == GEKKO_ASM("addco r3, r4, r5"));
   static_assert(0x7c642815 == GEKKO_ASM("addc. r3, r4, r5"));
   static_assert(0x7c642c15 == GEKKO_ASM("addco. r3, r4, r5"));
   static_assert(0x7c642914 == GEKKO_ASM("adde r3, r4, r5"));
   static_assert(0x7c642d14 == GEKKO_ASM("addeo r3, r4, r5"));
   static_assert(0x7c642915 == GEKKO_ASM("adde. r3, r4, r5"));
   static_assert(0x7c642d15 == GEKKO_ASM("addeo. r3, r4, r5"));
   static_assert(0x7c642bd6 == GEKKO_ASM("divw r3, r4, r5"));
   static_assert(0x7c642fd6 == GEKKO_ASM("divwo r3, r4, r5"));
   static_assert(0x7c642bd7 == GEKKO_ASM("divw. r3, r4, r5"));
   static_assert(0x7c642fd7 == GEKKO_ASM("divwo. r3, r4, r5"));
   static_assert(0x7c642b96 == GEKKO_ASM("divwu r3, r4, r5"));
   static_assert(0x7c642f96 == GEKKO_ASM("divwuo r3, r4, r5"));
   static_assert(0x7c642b97 == GEKKO_ASM("divwu. r3, r4, r5"));
   static_assert(0x7c642f97 == GEKKO_ASM("divwuo. r3, r4, r5"));
   static_assert(0x7c6428ee == GEKKO_ASM("lbzux r3, r4, r5"));
   static_assert(0x7c6428ae == GEKKO_ASM("lbzx r3, r4, r5"));
   static_assert(0x7c642aee == GEKKO_ASM("lhaux r3, r4, r5"));
   static_assert(0x7c642aae == GEKKO_ASM("lhax r3, r4, r5"));
   static_assert(0x7c642e2c == GEKKO_ASM("lhbrx r3, r4, r5"));
   static_assert(0x7c642a6e == GEKKO_ASM("lhzux r3, r4, r5"));
   static_assert(0x7c642a2e == GEKKO_ASM("lhzx r3, r4, r5"));
   static_assert(0x7c642c2c == GEKKO_ASM("lwbrx r3, r4, r5"));
   static_assert(0x7c64286e == GEKKO_ASM("lwzux r3, r4, r5"));
   static_assert(0x7c64282e == GEKKO_ASM("lwzx r3, r4, r5"));
   static_assert(0x7c642896 == GEKKO_ASM("mulhw r3, r4, r5"));
   static_assert(0x7c642897 == GEKKO_ASM("mulhw. r3, r4, r5"));
   static_assert(0x7c6429d6 == GEKKO_ASM("mullw r3, r4, r5"));
   static_assert(0x7c642dd6 == GEKKO_ASM("mullwo r3, r4, r5"));
   static_assert(0x7c6429d7 == GEKKO_ASM("mullw. r3, r4, r5"));
   static_assert(0x7c642dd7 == GEKKO_ASM("mullwo. r3, r4, r5"));
   static_assert(0x7c642850 == GEKKO_ASM("subf r3, r4, r5"));
   static_assert(0x7c642c50 == GEKKO_ASM("subfo r3, r4, r5"));
   static_assert(0x7c642851 == GEKKO_ASM("subf. r3, r4, r5"));
   static_assert(0x7c642c51 == GEKKO_ASM("subfo. r3, r4, r5"));
   static_assert(0x7c642810 == GEKKO_ASM("subfc r3, r4, r5"));
   static_assert(0x7c642c10 == GEKKO_ASM("subfco r3, r4, r5"));
   static_assert(0x7c642811 == GEKKO_ASM("subfc. r3, r4, r5"));
   static_assert(0x7c642c11 == GEKKO_ASM("subfco. r3, r4, r5"));
   static_assert(0x7c642910 == GEKKO_ASM("subfe r3, r4, r5"));
   static_assert(0x7c642d10 == GEKKO_ASM("subfeo r3, r4, r5"));
   static_assert(0x7c642911 == GEKKO_ASM("subfe. r3, r4, r5"));
   static_assert(0x7c642d11 == GEKKO_ASM("subfeo. r3, r4, r5"));
   static_assert(0x7c652050 == GEKKO_ASM("sub r3, r4, r5"));
   static_assert(0x7c652450 == GEKKO_ASM("subo r3, r4, r5"));
   static_assert(0x7c652051 == GEKKO_ASM("sub. r3, r4, r5"));
   static_assert(0x7c652451 == GEKKO_ASM("subo. r3, r4, r5"));
   static_assert(0x7c652010 == GEKKO_ASM("subc r3, r4, r5"));
   static_assert(0x7c652410 == GEKKO_ASM("subco r3, r4, r5"));
   static_assert(0x7c652011 == GEKKO_ASM("subc. r3, r4, r5"));
   static_assert(0x7c652411 == GEKKO_ASM("subco. r3, r4, r5"));
   static_assert(0x7c830034 == GEKKO_ASM("cntlzw r3, r4"));
   static_assert(0x7c830035 == GEKKO_ASM("cntlzw. r3, r4"));
   static_assert(0x7c830774 == GEKKO_ASM("extsb r3, r4"));
   static_assert(0x7c830775 == GEKKO_ASM("extsb. r3, r4"));
   static_assert(0x7c830734 == GEKKO_ASM("extsh r3, r4"));
   static_assert(0x7c830735 == GEKKO_ASM("extsh. r3, r4"));
   static_assert(0x7c834670 == GEKKO_ASM("srawi r3, r4, 8"));
   static_assert(0x7c834671 == GEKKO_ASM("srawi. r3, r4, 8"));
   static_assert(0x7c6401d4 == GEKKO_ASM("addme r3, r4"));
   static_assert(0x7c6401d5 == GEKKO_ASM("addme. r3, r4"));
   static_assert(0x7c640194 == GEKKO_ASM("addze r3, r4"));
   static_assert(0x7c640195 == GEKKO_ASM("addze. r3, r4"));
   static_assert(0x7c6400d0 == GEKKO_ASM("neg r3, r4"));
   static_assert(0x7c6400d1 == GEKKO_ASM("neg. r3, r4"));
   static_assert(0x7c6401d0 == GEKKO_ASM("subfme r3, r4"));
   static_assert(0x7c6401d1 == GEKKO_ASM("subfme. r3, r4"));
   static_assert(0x7c640190 == GEKKO_ASM("subfze r3, r4"));
   static_assert(0x7c640191 == GEKKO_ASM("subfze. r3, r4"));
   static_assert(0x4c411202 == GEKKO_ASM("crand eq, gt, eq"));
   static_assert(0x4c411102 == GEKKO_ASM("crandc eq, gt, eq"));
   static_assert(0x4c411242 == GEKKO_ASM("creqv eq, gt, eq"));
   static_assert(0x4c4111c2 == GEKKO_ASM("crnand eq, gt, eq"));
   static_assert(0x4c411042 == GEKKO_ASM("crnor eq, gt, eq"));
   static_assert(0x4c411382 == GEKKO_ASM("cror eq, gt, eq"));
   static_assert(0x4c411342 == GEKKO_ASM("crorc eq, gt, eq"));
   static_assert(0x4c411182 == GEKKO_ASM("crxor eq, gt, eq"));
   static_assert(0x4fe0fa02 == GEKKO_ASM("crand 31, 0, 31"));
   static_assert(0x4fe0f902 == GEKKO_ASM("crandc 31, 0, 31"));
   static_assert(0x4fe0fa42 == GEKKO_ASM("creqv 31, 0, 31"));
   static_assert(0x4fe0f9c2 == GEKKO_ASM("crnand 31, 0, 31"));
   static_assert(0x4fe0f842 == GEKKO_ASM("crnor 31, 0, 31"));
   static_assert(0x4fe0fb82 == GEKKO_ASM("cror 31, 0, 31"));
   static_assert(0x4fe0fb42 == GEKKO_ASM("crorc 31, 0, 31"));
   static_assert(0x4fe0f982 == GEKKO_ASM("crxor 31, 0, 31"));
   static_assert(0xfc602210 == GEKKO_ASM("fabs f3, f4"));
   static_assert(0xfc602211 == GEKKO_ASM("fabs. f3, f4"));
   static_assert(0xfc60201c == GEKKO_ASM("fctiw f3, f4"));
   static_assert(0xfc60201d == GEKKO_ASM("fctiw. f3, f4"));
   static_assert(0xfc60201e == GEKKO_ASM("fctiwz f3, f4"));
   static_assert(0xfc60201f == GEKKO_ASM("fctiwz. f3, f4"));
   static_assert(0xfc602090 == GEKKO_ASM("fmr f3, f4"));
   static_assert(0xfc602091 == GEKKO_ASM("fmr. f3, f4"));
   static_assert(0xfc602110 == GEKKO_ASM("fnabs f3, f4"));
   static_assert(0xfc602111 == GEKKO_ASM("fnabs. f3, f4"));
   static_assert(0xfc602050 == GEKKO_ASM("fneg f3, f4"));
   static_assert(0xfc602051 == GEKKO_ASM("fneg. f3, f4"));
   static_assert(0xec602030 == GEKKO_ASM("fres f3, f4"));
   static_assert(0xec602031 == GEKKO_ASM("fres. f3, f4"));
   static_assert(0xfc602018 == GEKKO_ASM("frsp f3, f4"));
   static_assert(0xfc602019 == GEKKO_ASM("frsp. f3, f4"));
   static_assert(0xfc602034 == GEKKO_ASM("frsqrte f3, f4"));
   static_assert(0xfc602035 == GEKKO_ASM("frsqrte. f3, f4"));
   static_assert(0xfc64282a == GEKKO_ASM("fadd f3, f4, f5"));
   static_assert(0xec64282a == GEKKO_ASM("fadds f3, f4, f5"));
   static_assert(0xfc642824 == GEKKO_ASM("fdiv f3, f4, f5"));
   static_assert(0xec642824 == GEKKO_ASM("fdivs f3, f4, f5"));
   static_assert(0xfc642828 == GEKKO_ASM("fsub f3, f4, f5"));
   static_assert(0xec642828 == GEKKO_ASM("fsubs f3, f4, f5"));
   static_assert(0xfc640172 == GEKKO_ASM("fmul f3, f4, f5"));
   static_assert(0xec640172 == GEKKO_ASM("fmuls f3, f4, f5"));
   static_assert(0xfc64317a == GEKKO_ASM("fmadd f3, f4, f5, f6"));
   static_assert(0xfc64317b == GEKKO_ASM("fmadd. f3, f4, f5, f6"));
   static_assert(0xec64317a == GEKKO_ASM("fmadds f3, f4, f5, f6"));
   static_assert(0xec64317b == GEKKO_ASM("fmadds. f3, f4, f5, f6"));
   static_assert(0xfc643178 == GEKKO_ASM("fmsub f3, f4, f5, f6"));
   static_assert(0xfc643179 == GEKKO_ASM("fmsub. f3, f4, f5, f6"));
   static_assert(0xec643178 == GEKKO_ASM("fmsubs f3, f4, f5, f6"));
   static_assert(0xec643179 == GEKKO_ASM("fmsubs. f3, f4, f5, f6"));
   static_assert(0xfc64317e == GEKKO_ASM("fnmadd f3, f4, f5, f6"));
   static_assert(0xfc64317f == GEKKO_ASM("fnmadd. f3, f4, f5, f6"));
   static_assert(0xec64317e == GEKKO_ASM("fnmadds f3, f4, f5, f6"));
   static_assert(0xec64317f == GEKKO_ASM("fnmadds. f3, f4, f5, f6"));
   static_assert(0xfc64317c == GEKKO_ASM("fnmsub f3, f4, f5, f6"));
   static_assert(0xfc64317d == GEKKO_ASM("fnmsub. f3, f4, f5, f6"));
   static_assert(0xec64317c == GEKKO_ASM("fnmsubs f3, f4, f5, f6"));
   static_assert(0xec64317d == GEKKO_ASM("fnmsubs. f3, f4, f5, f6"));
   static_assert(0xfc64316e == GEKKO_ASM("fsel f3, f4, f5, f6"));
   static_assert(0xfc64316f == GEKKO_ASM("fsel. f3, f4, f5, f6"));
   static_assert(0xfd842840 == GEKKO_ASM("fcmpo cr3, f4, f5"));
   static_assert(0xfd842800 == GEKKO_ASM("fcmpu cr3, f4, f5"));
   static_assert(0x48001234 == GEKKO_ASM("b 0x1234"));
   static_assert(0x48001235 == GEKKO_ASM("bl 0x1234"));
   static_assert(0x48001236 == GEKKO_ASM("ba 0x1234"));
   static_assert(0x48001237 == GEKKO_ASM("bla 0x1234"));
   static_assert(0x48004004 == GEKKO_ASM("b 0x80004000~0x80008004"));
   static_assert(0x4bffc004 == GEKKO_ASM("b 0x80008000~0x80004004"));
   static_assert(0x41821234 == GEKKO_ASM("bc 12, eq, 0x1234"));
   static_assert(0x41821235 == GEKKO_ASM("bcl 12, eq, 0x1234"));
   static_assert(0x41821236 == GEKKO_ASM("bca 12, eq, 0x1234"));
   static_assert(0x41821237 == GEKKO_ASM("bcla 12, eq, 0x1234"));
   static_assert(0x41811234 == GEKKO_ASM("bc 12, gt, 0x1234"));
   static_assert(0x41811235 == GEKKO_ASM("bcl 12, gt, 0x1234"));
   static_assert(0x41811236 == GEKKO_ASM("bca 12, gt, 0x1234"));
   static_assert(0x41811237 == GEKKO_ASM("bcla 12, gt, 0x1234"));
   static_assert(0x41801234 == GEKKO_ASM("bc 12, lt, 0x1234"));
   static_assert(0x41801235 == GEKKO_ASM("bcl 12, lt, 0x1234"));
   static_assert(0x41801236 == GEKKO_ASM("bca 12, lt, 0x1234"));
   static_assert(0x41801237 == GEKKO_ASM("bcla 12, lt, 0x1234"));
   static_assert(0x41831234 == GEKKO_ASM("bc 12, so, 0x1234"));
   static_assert(0x41831235 == GEKKO_ASM("bcl 12, so, 0x1234"));
   static_assert(0x41831236 == GEKKO_ASM("bca 12, so, 0x1234"));
   static_assert(0x41831237 == GEKKO_ASM("bcla 12, so, 0x1234"));
   static_assert(0x419f1234 == GEKKO_ASM("bc 12, 31, 0x1234"));
   static_assert(0x419f1235 == GEKKO_ASM("bcl 12, 31, 0x1234"));
   static_assert(0x419f1236 == GEKKO_ASM("bca 12, 31, 0x1234"));
   static_assert(0x419f1237 == GEKKO_ASM("bcla 12, 31, 0x1234"));
   static_assert(0x41821234 == GEKKO_ASM("beq 0x1234"));
   static_assert(0x41821235 == GEKKO_ASM("beql 0x1234"));
   static_assert(0x41821236 == GEKKO_ASM("beqa 0x1234"));
   static_assert(0x41821237 == GEKKO_ASM("beqla 0x1234"));
   static_assert(0x40821234 == GEKKO_ASM("bne 0x1234"));
   static_assert(0x40821235 == GEKKO_ASM("bnel 0x1234"));
   static_assert(0x40821236 == GEKKO_ASM("bnea 0x1234"));
   static_assert(0x40821237 == GEKKO_ASM("bnela 0x1234"));
   static_assert(0x41801234 == GEKKO_ASM("blt 0x1234"));
   static_assert(0x41801235 == GEKKO_ASM("bltl 0x1234"));
   static_assert(0x41801236 == GEKKO_ASM("blta 0x1234"));
   static_assert(0x41801237 == GEKKO_ASM("bltla 0x1234"));
   static_assert(0x40801234 == GEKKO_ASM("bge 0x1234"));
   static_assert(0x40801235 == GEKKO_ASM("bgel 0x1234"));
   static_assert(0x40801236 == GEKKO_ASM("bgea 0x1234"));
   static_assert(0x40801237 == GEKKO_ASM("bgela 0x1234"));
   static_assert(0x41811234 == GEKKO_ASM("bgt 0x1234"));
   static_assert(0x41811235 == GEKKO_ASM("bgtl 0x1234"));
   static_assert(0x41811236 == GEKKO_ASM("bgta 0x1234"));
   static_assert(0x41811237 == GEKKO_ASM("bgtla 0x1234"));
   static_assert(0x40811234 == GEKKO_ASM("ble 0x1234"));
   static_assert(0x40811235 == GEKKO_ASM("blel 0x1234"));
   static_assert(0x40811236 == GEKKO_ASM("blea 0x1234"));
   static_assert(0x40811237 == GEKKO_ASM("blela 0x1234"));
   static_assert(0x4d820420 == GEKKO_ASM("bcctr 12, eq"));
   static_assert(0x4d820421 == GEKKO_ASM("bcctrl 12, eq"));
   static_assert(0x4d810420 == GEKKO_ASM("bcctr 12, gt"));
   static_assert(0x4d810421 == GEKKO_ASM("bcctrl 12, gt"));
   static_assert(0x4d800420 == GEKKO_ASM("bcctr 12, lt"));
   static_assert(0x4d800421 == GEKKO_ASM("bcctrl 12, lt"));
   static_assert(0x4d830420 == GEKKO_ASM("bcctr 12, so"));
   static_assert(0x4d830421 == GEKKO_ASM("bcctrl 12, so"));
   static_assert(0x4d9f0420 == GEKKO_ASM("bcctr 12, 31"));
   static_assert(0x4d9f0421 == GEKKO_ASM("bcctrl 12, 31"));
   static_assert(0x4e800420 == GEKKO_ASM("bctr"));
   static_assert(0x4e800421 == GEKKO_ASM("bctrl"));
   static_assert(0x4d820420 == GEKKO_ASM("beqctr"));
   static_assert(0x4d820421 == GEKKO_ASM("beqctrl"));
   static_assert(0x4c820420 == GEKKO_ASM("bnectr"));
   static_assert(0x4c820421 == GEKKO_ASM("bnectrl"));
   static_assert(0x4d800420 == GEKKO_ASM("bltctr"));
   static_assert(0x4d800421 == GEKKO_ASM("bltctrl"));
   static_assert(0x4c800420 == GEKKO_ASM("bgectr"));
   static_assert(0x4c800421 == GEKKO_ASM("bgectrl"));
   static_assert(0x4d810420 == GEKKO_ASM("bgtctr"));
   static_assert(0x4d810421 == GEKKO_ASM("bgtctrl"));
   static_assert(0x4c810420 == GEKKO_ASM("blectr"));
   static_assert(0x4c810421 == GEKKO_ASM("blectrl"));
   static_assert(0x4d820020 == GEKKO_ASM("bclr 12, eq"));
   static_assert(0x4d820021 == GEKKO_ASM("bclrl 12, eq"));
   static_assert(0x4d810020 == GEKKO_ASM("bclr 12, gt"));
   static_assert(0x4d810021 == GEKKO_ASM("bclrl 12, gt"));
   static_assert(0x4d800020 == GEKKO_ASM("bclr 12, lt"));
   static_assert(0x4d800021 == GEKKO_ASM("bclrl 12, lt"));
   static_assert(0x4d830020 == GEKKO_ASM("bclr 12, so"));
   static_assert(0x4d830021 == GEKKO_ASM("bclrl 12, so"));
   static_assert(0x4d9f0020 == GEKKO_ASM("bclr 12, 31"));
   static_assert(0x4d9f0021 == GEKKO_ASM("bclrl 12, 31"));
   static_assert(0x4e800020 == GEKKO_ASM("blr"));
   static_assert(0x4e800021 == GEKKO_ASM("blrl"));
   static_assert(0x4d820020 == GEKKO_ASM("beqlr"));
   static_assert(0x4d820021 == GEKKO_ASM("beqlrl"));
   static_assert(0x4c820020 == GEKKO_ASM("bnelr"));
   static_assert(0x4c820021 == GEKKO_ASM("bnelrl"));
   static_assert(0x4d800020 == GEKKO_ASM("bltlr"));
   static_assert(0x4d800021 == GEKKO_ASM("bltlrl"));
   static_assert(0x4c800020 == GEKKO_ASM("bgelr"));
   static_assert(0x4c800021 == GEKKO_ASM("bgelrl"));
   static_assert(0x4d810020 == GEKKO_ASM("bgtlr"));
   static_assert(0x4d810021 == GEKKO_ASM("bgtlrl"));
   static_assert(0x4c810020 == GEKKO_ASM("blelr"));
   static_assert(0x4c810021 == GEKKO_ASM("blelrl"));
   static_assert(0x7c022000 == GEKKO_ASM("cmp cr0, 0, r2, r4"));
   static_assert(0x2c021234 == GEKKO_ASM("cmpi cr0, 0, r2, 0x1234"));
   static_assert(0x7c022040 == GEKKO_ASM("cmpl cr0, 0, r2, r4"));
   static_assert(0x28021234 == GEKKO_ASM("cmpli cr0, 0, r2, 0x1234"));
   static_assert(0x7c022000 == GEKKO_ASM("cmp cr0, r2, r4"));
   static_assert(0x2c021234 == GEKKO_ASM("cmpi cr0, r2, 0x1234"));
   static_assert(0x7c022040 == GEKKO_ASM("cmpl cr0, r2, r4"));
   static_assert(0x28021234 == GEKKO_ASM("cmpli cr0, r2, 0x1234"));
   static_assert(0x7c022000 == GEKKO_ASM("cmpw r2, r4"));
   static_assert(0x7f822000 == GEKKO_ASM("cmpw cr7, r2, r4"));
   static_assert(0x2c021234 == GEKKO_ASM("cmpwi r2, 0x1234"));
   static_assert(0x2f821234 == GEKKO_ASM("cmpwi cr7, r2, 0x1234"));
   static_assert(0x7c022040 == GEKKO_ASM("cmplw r2, r4"));
   static_assert(0x7f822040 == GEKKO_ASM("cmplw cr7, r2, r4"));
   static_assert(0x28021234 == GEKKO_ASM("cmplwi r2, 0x1234"));
   static_assert(0x2b821234 == GEKKO_ASM("cmplwi cr7, r2, 0x1234"));
   static_assert(0x887f1234 == GEKKO_ASM("lbz r3, 0x1234(r31)"));
   static_assert(0x8c7f1234 == GEKKO_ASM("lbzu r3, 0x1234(r31)"));
   static_assert(0xa87f1234 == GEKKO_ASM("lha r3, 0x1234(r31)"));
   static_assert(0xac7f1234 == GEKKO_ASM("lhau r3, 0x1234(r31)"));
   static_assert(0xa07f1234 == GEKKO_ASM("lhz r3, 0x1234(r31)"));
   static_assert(0xa47f1234 == GEKKO_ASM("lhzu r3, 0x1234(r31)"));
   static_assert(0xbbe11234 == GEKKO_ASM("lmw r31, 0x1234(r1)"));
   static_assert(0x807f1234 == GEKKO_ASM("lwz r3, 0x1234(r31)"));
   static_assert(0x847f1234 == GEKKO_ASM("lwzu r3, 0x1234(r31)"));
   static_assert(0x987f1234 == GEKKO_ASM("stb r3, 0x1234(r31)"));
   static_assert(0x9c7f1234 == GEKKO_ASM("stbu r3, 0x1234(r31)"));
   static_assert(0xb07f1234 == GEKKO_ASM("sth r3, 0x1234(r31)"));
   static_assert(0xb47f1234 == GEKKO_ASM("sthu r3, 0x1234(r31)"));
   static_assert(0xbc7f1234 == GEKKO_ASM("stmw r3, 0x1234(r31)"));
   static_assert(0x907f1234 == GEKKO_ASM("stw r3, 0x1234(r31)"));
   static_assert(0x947f1234 == GEKKO_ASM("stwu r3, 0x1234(r31)"));
   static_assert(0xc87f1234 == GEKKO_ASM("lfd f3, 0x1234(r31)"));
   static_assert(0xcc7f1234 == GEKKO_ASM("lfdu f3, 0x1234(r31)"));
   static_assert(0x7c642cee == GEKKO_ASM("lfdux f3, r4, r5"));
   static_assert(0x7c642cae == GEKKO_ASM("lfdx f3, r4, r5"));
   static_assert(0xc07f1234 == GEKKO_ASM("lfs f3, 0x1234(r31)"));
   static_assert(0xc47f1234 == GEKKO_ASM("lfsu f3, 0x1234(r31)"));
   static_assert(0x7c642c6e == GEKKO_ASM("lfsux f3, r4, r5"));
   static_assert(0x7c642c2e == GEKKO_ASM("lfsx f3, r4, r5"));
   static_assert(0xd87f1234 == GEKKO_ASM("stfd f3, 0x1234(r31)"));
   static_assert(0xdc7f1234 == GEKKO_ASM("stfdu f3, 0x1234(r31)"));
   static_assert(0x7c642dee == GEKKO_ASM("stfdux f3, r4, r5"));
   static_assert(0x7c642dae == GEKKO_ASM("stfdx f3, r4, r5"));
   static_assert(0xd07f1234 == GEKKO_ASM("stfs f3, 0x1234(r31)"));
   static_assert(0xd47f1234 == GEKKO_ASM("stfsu f3, 0x1234(r31)"));
   static_assert(0x7c642d6e == GEKKO_ASM("stfsux f3, r4, r5"));
   static_assert(0x7c642d2e == GEKKO_ASM("stfsx f3, r4, r5"));
   static_assert(0x4c1c0000 == GEKKO_ASM("mcrf cr0, cr7"));
   static_assert(0xfc1c0080 == GEKKO_ASM("mcrfs cr0, cr7"));
   static_assert(0x7f800400 == GEKKO_ASM("mcrxr cr7"));
   static_assert(0x7c000026 == GEKKO_ASM("mfcr r0"));
   static_assert(0xffe0048e == GEKKO_ASM("mffs f31"));
   static_assert(0x7fe000a6 == GEKKO_ASM("mfmsr r31"));
   static_assert(0x7c0102a6 == GEKKO_ASM("mfspr r0, xer"));
   static_assert(0x7c0802a6 == GEKKO_ASM("mfspr r0, lr"));
   static_assert(0x7c0902a6 == GEKKO_ASM("mfspr r0, ctr"));
   static_assert(0x7c1042a6 == GEKKO_ASM("mfspr r0, 272"));
   static_assert(0x7c0802a6 == GEKKO_ASM("mflr r0"));
   static_assert(0x7c0902a6 == GEKKO_ASM("mfctr r0"));
   static_assert(0x7c0102a6 == GEKKO_ASM("mfxer r0"));
   static_assert(0x7fe25120 == GEKKO_ASM("mtcrf 0x25, r31"));
   static_assert(0x7feff120 == GEKKO_ASM("mtcr r31"));
   static_assert(0x7c000124 == GEKKO_ASM("mtmsr r0"));
   static_assert(0x7c0103a6 == GEKKO_ASM("mtspr xer, r0"));
   static_assert(0x7c0803a6 == GEKKO_ASM("mtspr lr, r0"));
   static_assert(0x7c0903a6 == GEKKO_ASM("mtspr ctr, r0"));
   static_assert(0x7c1043a6 == GEKKO_ASM("mtspr 272, r0"));
   static_assert(0x7c0803a6 == GEKKO_ASM("mtlr r0"));
   static_assert(0x7c0903a6 == GEKKO_ASM("mtctr r0"));
   static_assert(0x7c0103a6 == GEKKO_ASM("mtxer r0"));
   static_assert(0x501ffffe == GEKKO_ASM("rlwimi r31, r0, 31, 31, 31"));
   static_assert(0x501ff83f == GEKKO_ASM("rlwimi. r31, r0, 31, 0, 31"));
   static_assert(0x541ff83e == GEKKO_ASM("rlwinm r31, r0, 31, 0, 31"));
   static_assert(0x541ff83f == GEKKO_ASM("rlwinm. r31, r0, 31, 0, 31"));
   static_assert(0x5c1f783e == GEKKO_ASM("rlwnm r31, r0, r15, 0, 31"));
   static_assert(0x5c1f783f == GEKKO_ASM("rlwnm. r31, r0, r15, 0, 31"));
   static_assert(0x7fe0fb78 == GEKKO_ASM("mr r0, r31"));
   static_assert(0x38607fff == GEKKO_ASM("li r3, 0x7fff"));
   static_assert(0x38608001 == GEKKO_ASM("li r3, -0x7fff"));
   static_assert(0x3c608001 == GEKKO_ASM("lis r3, 0x8001"));
   static_assert(0x3c608000 == GEKKO_ASM("lis r3, -0x8000"));
}
