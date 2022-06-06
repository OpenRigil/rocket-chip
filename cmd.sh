riscv64-unknown-elf-gcc -o mmm dependencies/chipyard/tests/mmm.c -std=gnu99 -O2 -fno-common -fno-builtin-printf -Wall -static -nostdlib -mcmodel=medany -T./htif.ld
./out/VerilatorTest/build/emulator +max-cycles=20000 +verbose -v./mmm.vcd ./mmm 2>&1 | grep -v 00000000000008 | grep -v 00000000000003 | ./out/spike/compile.dest/spike-dasm | tee baremetal.log
