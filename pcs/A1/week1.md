# PCS Assignment 1

### `sortraev` (wlc376)


## Task 1 -- Understand a strange program

Below is the contents of my `strange_debug.gdb`:

```gdb
set logging file strange_gdb_debug.output
set logging overwrite on
set logging enabled on

b outer
commands
silent
printf ">> outer\n>> rdi = \"%s\"\n", $rdi
c
end

b update
commands
silent
printf ">> update\n"
info register rax rbx
c
end

run

set logging enabled off
```

From the `gdb` output, the output of `strange`, and a quick glance at
`strange.asm`, I'd say the program builds the string `ABCDEFGHIJKLMNOP` in
stack memory, using `eax` as an accumulator register to build the string 4 bytes
at a time in order to save a factor 4 `mov`s to stack mem, before finally
printing to stdout using `puts`.

## Task 2 -- Ternary numbers

Below is my x86 asm for conversion of ternary numbers in the `xyz`
representation:

```x86asm
ternary_convert:
    dec rdi

.skip_xs:                   ; skip prepended x's.
    inc rdi
    movzx r11, byte [rdi]
    cmp r11, 'x'
    je .skip_xs

    xor rax, rax            ; set acc = 0.
    mov r10, 3

.body:
    cmp r11, 0              ; on null terminator: return acc.
    je .end

    sub r11, 'x'            ; sub 120 and assert byte is now in range 0..2.
    cmp r11, 2
    ja .on_invalid          ; note: unsigned comparison!

    mul r10                 ; acc = acc * 3
    add rax, r11            ; acc = acc + (next ternary digit)

    inc rdi                 ; load next byte.
    movzx r11, byte [rdi]
    jmp .body

.on_invalid:
    xor rax, rax
.end:
    ret
```

The code is quite straight-forward and I hope the comments are helpful. It
parses ternary strings in a single left-to-right pass by multiplying the current
accumulator by 3 and adding the next valid ternary number char.

Subtraction with 120 and unsigned comparison with the number 2 is used to test
membership of the range `120..122`.

The function uses 4 caller-saves registers (where one is used only for `mul`,
and thus can be omitted if using `imul`). This is a conscious choice since the
function makes no inner calls. The function also has no need for a stack frame.

#### Process and challenges

My `x86` asm implementation is based on the below C implementation:

```c
uint64_t ternary_convert_c(char *rdi) {

  uint64_t r11, rax, r10;
  while (1) {
    r11 = *rdi;
    if (r11 == 'x')
      break;
  }

  rax = rax ^ rax;
  r10 = 3;
  while (1) {
    if (r11 == 0)
      break;

    r11 -= 'x';

    if (r11 > 2)
      return 0;

    rax *= r10;
    rax += r11;

    rdi++;
    r11 = *rdi;
  }
  return rax;
}
```

My assembly code passed all tests successfully after the first successful
compilation, so there were no big challenges.

#### Testing

A number of tests are written in the included `ternary_tests.c`. I test both
valid and invalid ternary string, ternary strings representing numbers larger
than 32 bit, as well as ternary numbers larger than 64 bit to test correct
overflow.

The tests are run consecutively to (hopefully) assert that callee-saves
registers and the stack are respected.

All tests pass successfully. To reproduce tests, run `make ternary_tests &&
./ternary_tests`.

## Task 3 -- Get a line

Below snippet shows my asm for `slurp`:

```x86asm
slurp:
    push r12                 ;; push callee-saved registers.
    push r13
    push r14
    mov r12, rdi
    mov r14, rsi
    xor r13, r13
.body:

    cmp r13, 126             ;; loop until >=126 bytes.
    jge .on_eol

    mov rdi, r12             ;; read a byte.
    call fgetc

    cmp eax, 0               ;; on EOL.
    jl .on_eol

    mov [r14 + r13], byte al ;; store the valid byte in buffer.
    inc r13

    cmp eax, 10              ;; repeat if byte was not a newline.
    jne .body

    mov [r14 + r13], byte 0  ;; if newline before 126 bytes: terminate.
    jmp .end

.on_eol:
    mov [r14 + r13],     byte 10
    mov [r14 + r13 + 1], byte 0
.end:
    mov rax, r13
    pop r14
    pop r13
    pop r12

    ret
```

The program uses 3 callee-saves registers (excluding `rdi` and `rsi`). These
are handled in the prologue/epilogue. The function requires no stack frame,
hence it is given none.

The `.body` loop reads bytes using `fgetc` until 126 are read or until
end-of-line (ie. `fgetc` returns `< 0`). If a byte is read, the function then
assumes the byte is valid ASCII (as per the assignment text), and writes it to
the buffer and increments the counter. If the read byte was a newline, we
terminate loop, insert a null byte, and return; else we loop back around.

If however we reached end-of-line, then the counter is *not* incremented; newline
and null terminator is inserted; and we return.

#### Implementation challenges

I ran into a lot of trouble because I forgot that `rdi` and `rsi` was
caller-saved, not callee-saved. This took some time to realize, and I only found
out when I used `gdb` to check the contents of all registers before and after
the call to `fgetc`. Fun times!

Aside from this, there were no big troubles with implementing `slurp`.

The implementation was based on the following C code using goto-style
programming to mimic the desired x86 code:

```c
uint64_t slurp_c(FILE *f, char *buf) {
  int i = 0;
  int c;

  while (1) {
    if (i >= MAX_LINE || (c = fgetc(f)) == EOF)
      goto on_eol;

    buf[i++] = (char) c;

    if (c != '\n')
      continue;

    buf[i] = '\0';
    goto end;
  }

on_eol:
  buf[i]     = '\n';
  buf[i + 1] = '\0';
end:
  return i;
}
```

#### Testing

I write a small number of positive tests in the attached `slurp_tests.c`. Since
we assume the input is all ASCII, I cannot think of any meaningful negative
tests.

The test cases include empty lines, strings with and without trailing newline,
lines of exactly 126 characters, and lines of more than 126 characters.

The tests are run consecutively to (hopefully) assert that callee-saves
registers and the stack are respected.

All tests pass successfully. To reproduce tests, run `make slurp_tests &&
./slurp_tests`.
