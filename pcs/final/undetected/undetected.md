# PCS final -- `undetected`

### Anders Holst (`sortraev`, `wlc376`)

#### Status of hand-in

I successfully identify a window for a heap buffer overflow attack and exploit
it to make the binary print the contents of `flag.txt`.

### Mitigations in the binary

The binary is compiled with stack protection, so a stack attack is going to be
tough, but spoiler alert: we are going to aim for a heap smashing attack, so
this is not a hinderance.

The binary is also compiled as no-PIE with NX set, but neither of these facts
are going to affect the process either, since -- again, spoiler alert -- our
heap attack is not going to be relying on actual addresses, but rather a heap
smashing window, and thus we also are not going to need to execute arbitrary
code on eg. the stack.

### Overview of the program via reverse engineering

I reverse the binary using the disassembly. If interested, please see the
attached `undetected_disas_commented.asm` for commented disassembly and
`undetected_reversed.c` for reverse engineered C code.

I will not go into detail with the disassembly here, as I will be discussing the
important findings in remaining sections of this report, but the general
structure of the program is this:

* in a loop of `42` iterations:
  - (1) `main()` allocates `32` bytes for user input, and `super_secret()`
      allocates and random-initializes `32` bytes -- call these allocations
      `buf` and `secret`;
  - (2) program asks user for a riddle, and `input()` reads up to `420` bytes of
      `\n`- or EOF-terminated user input into `buf` using `fgets()`;
  - (3) `main()` reads up to `33` bytes (including newline) of `\n`- or
      EOF-terminated user input into a local stack buffer and checks it against
      `secret` using `strncmp(secret, user_guess, 32)`;
  - (4a) if the strings *do not* match, program exits;
  - (4b) if the strings *do* match, `main()` reads an arbitrarily long
      `\n`-terminated user input into `buf`, complains about the user's humour,
      frees `secret` and `buf` (in that order) and loops back around.

* if loop runs to completion, `win()` is called, in turn calling `system("cat
  flag.txt")`.


### Heap smashing window in `input()`

There is a window for heap smashing in step (2), where the `input()` function
takes the heap-allocated user input buffer as argument. Here, up to `420` bytes
of user input is read from `stdin` into a local stack buffer using `fgets`, and
this is then subsequently `memcpy`'d into the user input buffer. Since the user
input buffer is 40 bytes long (including the `prev_size` field for the `secret`
allocation), we will start smashing into the `secret` allocation if we write any
more than 40 bytes.

However, this also smashes header bytes for the `secret` chunk, meaning the
`free(secret)` in step (4b) is likely to behave badly -- how badly depends on
what the header is overwritten with. There are multiple "worst case" scenarios,
but some of them are:

*  we might clear the `P` ("prev chunk in use") bit and set `prev_size` to
   something erroneous, which would coalesce potentially in-use allocations
*  we might overwrite the `size` field of `secret` with something erroneous,
   which means future allocations could potentially alias/overwrite succeeding
   in-use allocations.

In the best case scenario, we overwrite the header with what was there
previously, and the `free()`s occur as if no heap smashing had occured.


### Designing the exploit

Before designing the exploit, we will probably need an overview of the heap
layout in order to know how.

#### Heap layout and `secret`'s header

The `buf` and `secret` allocations are placed in immediate succession (as
indicated by the `offset` column), and since they are both allocated a fixed 32
bytes, the heap layout looks as such:

```
offset | address                    | thing
-------|----------------------------|----------------
0      | buf    - 16 .. buf    - 9  | buf prev_size
8      | buf    - 8  .. buf    - 1  | buf size+flags
16     | buf    + 0  .. buf    + 31 | buf user data
48     | secret - 16 .. secret - 9  | secret prev_size
64     | secret - 8  .. secret - 1  | secret size+flags
80     | secret + 0  .. secret + 31 | secret user data
```

For both allocations, I assume that both `A` and `M` flags are always `0`, since
we are not dealing with `mmap`'d memory.

Thus when both `buf` and `secret` are allocated, `secret`'s `size+flags`-field
is `49` because the allocation is 48 bytes and its `P` flag is set to indicate
the previous chunk (`buf`) is in use. Since `P` is set for `secret`, its
`prev_size` is invalid and the 8 bytes are technically usable by the `buf`
allocation. In addition, since the `secret` chunk is freed before `buf` in the
code, we do not need to worry about setting `secret`'s `prev_size` field
(although we know it would be simply `48`).

The `size+flags`-field for `buf` is either `48` or `49`, depending on whether
the previous chunk is in use, and its `prev_size` will be set accordingly.
However, we do not need to know about the header for `buf`, since our heap
smashing attack is never going to touch it.

#### Overwriting the secret __undetected__

In step (2), we use the stack smashing window to overwrite `secret` with some
known string -- for simplicity, say, all `A`'s -- then we know what to input
when the program asks what the secret is in step (3). This clobbers the `secret`
header, but for now we can only input a string, so we can't preserve the header
since the input would need to contain null bytes.

However, in step (4b) (which we reach because we guessed the secret), we can use
the unrestricted `scanf("%[^\n]%*c", buf)` to once again smash into `secret` and
restore its header to `49`. Again, note that we do not need to touch `prev_size`
for `secret`, since we set its `P` bit to 1 to indicate `buf` is still in use.

Thus we have successfully overwritten `secret` with something predictable and
done so undetected! We should be able to repeat the process 42 times without
problem.


### The final exploit

Below snippet shows the Python implementation of the exploit:

```python
def exploit(p):
    my_riddle           = b"i" * 80 + b"\n"
    secret_guess        = B"i" * 32
    answer_to_my_riddle = B"i" * 40 + p64(49) + b"\n"

    # write the above 42 times. no need to interact with process.
    exploit = (my_riddle + secret_guess + answer_to_my_riddle) * 42
    write(p, exploit)

    # discard program's output until it starts printing the flag ...
    read_until(p, b"Well, I didn't expect that.\n")

    # ... and here it comes!
    flag_out = read_until(p, b"\0").decode("ascii").strip()
    print(flag_out)
```

To run, simply use `./doit.py` from within the `undetected` dir.
