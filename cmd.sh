rm mmm mmm.asm
$RISCV/bin/riscv64-unknown-elf-gcc -o mmm mmm.c entry.S -std=gnu99 -O0 -fno-common -fno-builtin-printf -Wall -static -nostdlib -mcmodel=medany -T./htif.ld
riscv64-elf-objdump -d mmm > mmm.asm

# +verbose   +max-cycles=60000 
./out/VerilatorTest/build/emulator +verbose -v./mmm.vcd ./mmm 2>&1 | grep -v 00000000000008 | grep -v 00000000000003 | ./out/spike/compile.dest/spike-dasm | tee baremetal.log
