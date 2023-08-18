#!/usr/bin/env python3
import sys

if len(sys.argv) == 2:
    exp = sys.argv[1]
else:
    exp = "33*1-1-2*2*3*3*3*1-2*3*3*3*" # phase 5 solution.


def f(c):
    ret = monkeys[0] if c == '2' else \
          monkeys[1] if c in '1' else \
          monkeys[2] if c == '3' else \
          b'-' if c == '-' else \
          b'+' if c == '*' else \
          b'*' if c == '+' else \
          b' '
    return ret

monkeys = [b"\xf0\x9f\x99\x88",
           b"\xf0\x9f\x99\x89",
           b"\xf0\x9f\x99\x8a"]


out = b" ".join([f(c) for c in exp]) + b"\x0a"
sys.stdout.buffer.write(out)
