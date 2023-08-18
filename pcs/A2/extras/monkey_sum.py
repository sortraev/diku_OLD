#!/usr/bin/env python3
import sys
if len(sys.argv) == 2:
    n = int(sys.argv[1])

    c = b"\xf0\x9f\x99\x89"
    monkeys = (b"%b %b -" % (c, c)   # == -1 - -1 = 0.
               + b" %b -" % c * n    # subtract -1 n times.
               + b"\n")

    sys.stdout.buffer.write(monkeys)
