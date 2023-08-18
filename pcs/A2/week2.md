# PCS Assignment 2

### `sortraev` (wlc376)

_**This assignment was made in collaboration with Louis Normann**_ (`cqb868`).


## Status

I finished up to and including phase 5.5. I have a good understanding of all the
completed phases -- please see the attached `bomb_disasm_commented.asm`.

## Final contents of `solution.txt`

```
Why'd you leave the keys upon the table? Here you go, create another fable
nan inf
8589934592 17179869184
$#%-/'(,)./$/ *(,/# ,/ $" /")'!&+.
🙊 🙊 + 🙉 - 🙉 - 🙈 + 🙈 + 🙊 + 🙊 + 🙊 + 🙉 - 🙈 + 🙊 + 🙊 + 🙊 +
10 33165873224
1337 1337 1337 1337 1337 1337
```

## Disassembly

The attached `bomb_disasm_commented.asm` contains commented disassembly of
`bomb`, but everything not relevant to phases 1 through 5.5 has been stripped
away. For each phase, the corresponding C code is shown along with a small
description of the phase.

The attached `c_implementations` directory contains compilable implementations
of each of the phases 1 through 5.5.

## High-level explanations

### Phase 1

The first phase iterates the input line and checks each byte against a string in
memory. If any pair of bytes do not match, the bomb explodes. The phase loops
over the length of the password string only, meaning a valid input string can
contain extra bytes on the end and still pass the check.

For more information, please see the attached `bomb_disasm_commented.asm`.

### Phase 2

This phase reads two floats from the input line, and makes two seemingly
identical computations on those floats. The bomb explodes if the two
results are equal, which at first seems wrong!

However, the two expressions are only equal if we assume commutativity of
the maxss instruction, and the documentation clearly states that if either
operand to maxss is nan, then the second operand is returned. This eg.
means that `max(nan, 2) = 2`, but `max(2, nan) = nan`.

However, the value `0x803f0000` is intentionally chosen such that there
exists a non-nan value `x` such that `isnan(0x803f0000 ^ x)` is true. 
From a deep-dive into the bits, we find that float values `x` which satisfy this
condition are `+/- x = 3 + y` for `0 <= y < 1` (up to float rounding errors).

This means that valid strings are for example `nan 2` and `-2 nan`, but `nan 3`
and `-3 nan` are not. However, interestingly, `nan 3.999999999999` passes the
phase due to float errors.

### Phase 3

Phase 3 reads two (long long) ints `x` and `y` from the input line using sscanf
and checks that:

- number of ints read is 2;
- `x` is greater than `0x1dcd64fff == 7999999999`;
- `y` is greater than `0x12a05f1ff == 4999999999`;
- for each number, we must have `n & (n - 1) == 0`, meaning both `x` and `y`
   must be powers of 2;
- `x` and `y` must be different.

Hence we simply pick the first power of two greater than `7999999999`, set `x`
to this value, and choose `y = 2 * x`. easy :)


### Phase 4

For this phase, we can immediately see in the disassembly that the expected
password is `"IT'S OVER! I HAVE THE HIGH GROUND"`. However, as we look in gdb, we
see that our input is garbled into something else, hence we must find the input
which garbles into `"IT'S OVER ..."` to complete the phase (actually, there are
multiple valid inputs).

The string is garbled using the key string `"HUGTI'NOVRADES! "`; each character in
the input line is `AND`'ed with `0xf` (ie. `00...001111`), and the resulting number is
used to index the key string (which happens to be 16 chars long).


I write a small piece of Python code to generate all possible solutions, and to
give me all printable solutions:

```python
pw   = "IT'S OVER! I HAVE THE HIGH GROUND!"
keys = "HUGTI'NOVRADES! "
solutions = ["".join(chr(keys.find(c) + i * 16) for c in pw)
             for i in range(16)]
printable_solutions = [s for s in solutions if s.isprintable()]
```

### Phase 5

The meat of phase 5 is the function `funcHP48`.

`funcHP48` is essentially an evaluator of RPN expressions, where the possible
values are three unicode monkeys representing the numbers 2, -1, and 3, and the
possible operators are addition, subtraction, and multiplication. However, there
is a small "gotcha" in that the symbols for addition and multiplication are
switched!

Monkeys (and values as results of computations) are pushed and popped from the
stack using the push and pop functions declared above. A maximum of 0x13 values
can be on the stack at a time; if more are popped, or if pop is called on an
empty stack, the bomb explodes. If an expression is valid but contains less than
5 tokens, the bomb explodes.

To complete phase 5, we must pass a string containing an RPN expression equaling
`0xface == 64206`; else the bomb explodes.

To construct such an expression, I first find out how to construct an infix
expression using only `2`, `-1`, `3`, `+`, `*`, and `-`; then, using a converter
I found online, I convert the infix expression to RPN; finally, I swap all
addition and multiplication operators, and convert the numbers into their
respective monkey emojis.

### Phase 5.5

This phase has a sneaky solution. Looking at the disassembly, we see that:

- 1) the phase reads 2 long ints, `a` and `b`, from the input line, and explodes
       on less than 2 vals read.
- 2) the bomb explodes if the first value, `a`, is `<= 9`.
- 3) the phase calls `func_chebyshev(a - 1, a - 2)` and checks the result
       against the given `b`. If they do not match, the bomb explodes.
- 4) the bomb explodes if the result of `func_chebyshev(a - 1, a - 2)` is *not*
    equal to the given `b`

We see that the call to `func_chebyshev(a - 1, a - 2)` is independent of the
given `b`. This means that to solve the phase, all we need to do is choose
values for `a` and `b`, read out the result of `func_chebyshev(a - 1, a - 2)`
using `gdb`, and re-run with the same `a` and the result substituted for `b`. No
need to interpret `func_chebyshev`, haha :)

However, from implementing the phase in C, I notice that the number of calls
grows with `a` -- in fact, for a given `a`, the number of recursive calls made
by `func_chebyshev(a - 1, a - 2)` is `fib(a)`. Interesting! Hence I argue that
the optimal value for `a` is the smallest possible, ie. 10. The corresponding
`b` is then `33165873224`.
