# PCS final -- `gotless`

### Anders Holst (`sortraev`, `wlc376`)

#### Status of hand-in

I manage to construct a ROP-chain to print the contents of `flag.txt`, as well
as the bonus task of exiting gracefully with exit code `66`.

## Introduction

There are multiple steps to a successful ROP-chain attack on `gotless`. In this
section, I go through each of them step by step, describing mitigations and
problems I ran into, and how I solved them.

To view my exploit, see `gotless/doit.py`. To view my ROP gadgets and chains,
see `gotless/rop_stuff.py`.


### Problem #0: find a window for injecting ROP-chain

I hope to find a window for a stack buffer overflow so I can easily inject my
ROP-chain. Thankfully, the developer of `gotless` made this easy for me -- the
`hash` function reads unconditionally until it finds a NULL byte, meaning my
ROP-chain is not restricted in length. Additionally, because of the way the
ROP-chain is encoded in `doit.py`, there can only be a NULL byte in the encoded
payload if is inserted *after* hashing, meaning my ROP-chain is not
(immediately) restricted in the set of bytes it may contain. There are, however,
other restrictions, as we shall see later.

256 bytes of stack space is allocated for `hash`, and 4 values are pushed in the
prologue, meaning I need to write `256 + 4 * 8 == 288` leading bytes of garbage
before I start to overflow the stack. We now have a window for injecting and
initiating the ROP-chain -- so far, so good.

The placement of `hash`'s frame on the stack relative to other function frames
is going to be important later -- for now, let's just pretend that we are free
to clobber the stack above `hash`'s frame without worry.

### Problem #1: find "traces of `libc`"

I want to find an entry point into `libc` such that I can eventually start
calling `libc` functions. The binary uses no dynamically linked functions, but
the hint in `README.md` states that there may still be traces of `libc` in the
binary.

If such traces truly exist, we should be able to find them using `readelf -r
gotless`, which displays relocations in the binary (if any):

```
Relocation section '.rela.dyn' at offset 0x468 contains 2 entries:
Offset Info      Type              Sym.Value  Sym. Name + Addend
403ff0 100000006 R_X86_64_GLOB_DAT 00000000   __libc_start_main@GLIBC_2.2.5 + 0
403ff8 200000006 R_X86_64_GLOB_DAT 00000000   __gmon_start__ + 0
```

Jackpot! We are going to use `__libc_start_main` at `0x403ff0` as our entry into
`libc`, and thus we have a window for a GOT overwrite attack.


### Problem #2: find out how to modify the GOT

In order to modify the GOT, I need a way to add offsets to memory. This one is
easy, since there is an `add [rdi], rsi` gadget at address `0x40110a`. As usual,
we also have gadgets to set `rdi` and `rsi` in `__libc_csu_init`.

Now I should be able to modify the GOT in only a couple of quadwords of
ROP-chain, right..?

View the relevant gadgets and chains in `gotless/rop_stuff.py`.

### Problem #3: the binary is RELRO

I write a small chain to modify the GOT, but I segfault on writing to address
`0x403ff0`. This gives me a bad feeling that the binary might be RELRO
(relocation read-only). I use gdb-gef to verify my hunch (some output omitted
here):

```gdb
$ gdb ./gotless
gef> b _start
gef> run $(./doit.py)
gef> vmmap
Start     End       Perm
0x400000  0x401000  r--
0x401000  0x402000  r-x
0x402000  0x403000  r--
0x403000  0x404000  r--  <-- GOT is here!
0x404000  0x405000  rw-  <-- (later, this page will be used for the new stack)
```

Sure enough, the GOT, and hence the GOT entry for `__libc_start_main`, is in a
read-only page! We could also have checked `/proc/<pid>/maps`.

This means that I am going to have to *relocate* the GOT entry to a read/write
enabled location before I can modify it. The page starting at `0x404000` is
`rw`, so I am going to copy it to this page.


### Problem #4: build read/write primitives using gadgets

All gadgets and chains discussed in this section can be viewed in
`gotless/rop_stuff.py`.

#### Write primitive chain

Using the `stosq [rdi], rax` gadget at `0x4011d2`, we can very simply write
arbitrary quadwords, assuming we know a way to set `rax`.

Setting `rax` is a little tricky. At `0x40114c` we have a `mov rax, [rsp + rdi*8
- 0x40]` gadget, which can read from memory assuming we know `rsp`. 

If what we want to write is a statically known value, we can simply place it in
the ROP-chain and set `rdi` accordingly, and then the chain becomes:

```python
def set_rax_chain(rax_new):
    return (set_rdi_gadget(rax_new) # insert rax_new on stack and skip over it
          + set_rdi_gadget(4) # set rdi=4, since rax_new will be at rsp+4*8-0x40
          + p64(0x40114c))    # addr of `mov rax, qword [rsp+rdi*8-0x40]`
```

Combining this with the `stosq` gadget at `0x4011d2` and the `set_rdi` gadget,
we have arbitrary writes of immediate values! The function
`rop_stuff.qword_write_chain()` implements this, while
`rop_stuff.arbitrary_write_chain()` can be used to create chains for writing
arbitrarily long byte sequences one quadword at a time. 

8 quadwords of ROP-chain space is required for each single quadword we want to
write using this method.

#### Read primitive chain

Creating a read primitive is going to be a little harder. The binary contains
only two gadgets which read from memory: `and eax, dword ptr [rsp]`, which we
cannot use for much since it loads `dword`s, and the same
`mov rax, [rsp + rdi*8 - 0x40]` that we used earlier to set `rax`. 

The problem is then that the instruction depends on `rsp`, and to read from
addresses that are not simply `rsp + k` for some statically known offset `k`, we
need to know `rsp`.

Assume for now that we know `rsp` (we shall alleviate this assumption in the next
section); then reading from arbitrary addresses is as simple as setting `rdi`
appropriately by solving the equation:

```python
rdi = (read_addr + 0x40 - current_rsp) // 8
```

where `read_addr` is the address we want to read from, and `current_rsp` is the
value of `rsp` *at the time of executing the load instruction*.

The read chain is implemented in `rop_stuff.qword_read_chain()`

#### Mem copy primitive chain

The two chains can be combined in a single chain for copying quadwords from one
mem location to another. See `rop_stuff.qword_copy_chain()`.


### Problem #5: migrating the stack (and continuing the ROP-chain)

In the previous section, we assumed to know `rsp`. Unfortunately, there is no
way to obtain `rsp` in the gadgets.

Well, if we can't find the stack, we'll just smoke it out of hiding!

The solution is to set the `rsp` to a known address using the `pop rsp` gadget
at `0x4011e0`. To do so and continue ROP-chaining seamlessly, we must first
write the rest of the chain to that known address *before* modifying `rsp`.
Fortunately, my `arbitrary_write_chain()` does not depend on `rsp`, so this is a
breeze.

Please see `gotless/doit.py:24-34` where I build the ROP-chain that is going to
be copied to the new stack.

<!-- TODO: is this relevant? -->
<!-- The downside is that we must use 8 quadwords of stack space for each -->
<!-- single quadword we write to the new stack location, meaning the size of the -->
<!-- ROP-chain explodes, so I should probably make sure my ROP-chain performs as much -->
<!-- work as possible before the migration. -->

#### Problem #5.5: location of new stack

We need to find a location for the new stack. When I went looking for the
permissions on the GOT table, I noticed only the page starting at `0x404000` was
`rw`-enabled, so this is my only choice.

By the time I finish developing the new stack, I see that it is 248 bytes long,
so I start the new stack at address `0x405000 - 248` to allow for as much room
as possible for the function frames of any `libc` functions we might want to
call later. Please see `gotless/doit.py:12-17`.


### Problem #6: calling `libc` functions

The new ROP-chain should copy the GOT entry to writable mem, add the appropriate
offset between `__libc_start_main()` and `system()`, then call `system("cat
flag.txt")`. Now that the stack is migrated, and we know `rsp`, this is not a
problem. After doing so, it should repeat the process with a call to `exit(66)`.

Here I omitted some of the more trivial details, such as the location of `"cat
flag.txt"` and how to actually call functions from pointers -- please see
`gotless/doit.py` and `gotless/rop_stuff.py`.


I write a utility class for making `libc` function calls, as shown later --
please see the `rop_stuff.LibcCaller` class.


### Problem #7: final boss fight: a clobbered environment

At this point, I would expect to have a working ROP-chain to call `system("cat
flag.txt")` and exit gracefully with `exit(66)`. However, I instead get a
burning crash. `strace -v -f -e execve` reveals that the inner `execve` call in
the child process spawned by `system()` crashes with error code `-1 (EFAULT)`,
and it would appear that `execve()` is called with excessively many and very
cryptic environment variables. 

The man page for `execve (2)` states that `EFAULT` indicates that one or more
pointers from `argv` or `envp` points to illegal memory. Using `gdb` I inspect
the arguments passed to `execve` in the child process, and sure enough, the
`envp` argument is absolutely clobbered. This is due to the global `environ`
variable (from which `execve` receives its `envp` pointer; see `man 2 execve`)
being overwritten by the ROP-chain.

I identify two possible solutions to the problem:

* Solution 1) insert a call to `clearenv` in the ROP-chain. This clears the global
    `environ` variable which `execve` typically receives its environment
    variable pointers from; or
* Solution 2) find out precisely at which point in the ROP-chain the `environ` variable
    is overwritten and modify the chain to instead overwrite it with something
    harmless (or less harmful).

Solution 1) is by far the safest and most portable solution, since it assumes
nothing of the location of `environ` (even though we can probably assume it to
be statically located), and nothing about how far our into the env pointers our
ROP-chain has overflowed. However, the extra `libc` function call is going to
cost a lot of extra ROP-chain space due to it needing to be copied to the new
stack. 

In fact, I successfully implemented both solutions, and the *encoded* payloads
were 4656 and 3824 bytes long, respectively.

I decide to go with solution 1) for the sake of reasonability, but below snippet
shows how to implement solution 2):

```python
split   = 544 # found via manual brute-force search using gdb. lol
payload = payload[:split] + set_rdi_gadget(0) * 2 + payload[split:]
```

## The full ROP-chain

I finally have a working ROP-chain.

To summarize, the below snippet from `gotless/doit.py` shows the full ROP-chain
construction:

```python
new_stack_base = 0x405000 - 248 # change this offset accordingly.

# build "actual" ROP-chain, which does all of the work.
new_stack_contents = (

    # copy relocated addr of __libc_start_main to a writable location.
    qword_copy_chain(addr_GOT_original, addr_GOT_copy,
                     current_rsp = new_stack_base) +

    # call clearenv() to avoid execve crashing due to the ROP-chain having
    # overflowed the global `environ` variable; then carry on with the program.
    libc_caller.call_libc_chain("clearenv") +
    libc_caller.call_libc_chain("system", rdi = addr_cat_flag_str) +
    libc_caller.call_libc_chain("exit",   rdi = 66)
)

# build initial rop-chain, which simply migrates the "actual" rop-chain.
payload = (
    rand_bytes(288) + # 288 bytes of garbage to overflow the `hash` buffer.

    # write the new stack to writable memory.
    arbitrary_write_chain(new_stack_base, new_stack_contents) +

    # set rbx = address that will eventually hold our copy of the GOT entry.
    set_rbx_gadget(addr_GOT_copy) +

    # update stack pointer (we start at -8 to account for `pop r13`).
    set_rsp_gadget(new_stack_base - 8)
)

encoded_payload = ...
```
