    .section .text
    .global _entry
_entry:
    la sp, __heap_end
    j main

