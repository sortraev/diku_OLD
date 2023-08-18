#!/usr/bin/env python3

import sys, numpy as np
if __name__ == "__main__" and len(sys.argv) >= 3:
    cur  = int(sys.argv[1])
    offs = int(sys.argv[2])

    x   = ((np.arange(256) - cur) * 5) & 255
    res = np.argwhere(x == offs).flat[0]

    print(f"to jump {offs} bytes from ins #{cur}, use RUNNER {res}.\n"
          f"(note: the instruction number is zero-indexed, and the\n"
          f" {offs} bytes include the 5 bytes for the RUNNER at ins #{cur})")
