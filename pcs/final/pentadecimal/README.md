Category: Shellcode

> The hexadecimal number system is so named because uses base 16. Analogously
> we may define the pentadecimal number system which uses base 15. Pentadecimal
> digits comprise `0` through `9` and `A` through `E`.
>
> What if we used pentadecimal digits for hexadecimal numbers? Then there
> would be no way to write `0x0F`, `0x1F`, `0x2F`, etc. as well as `0xF0`,
> `0xF1`, `0xF2`, etc. If we impose this limitation on x86 instructions surely
> it would be impossible to write malicious code! To be completely sure we will
> also forbid nulls (as usual) and limit the code to 50 bytes.
>
> However, one of my TAs claims that it's still possible to execute arbitrary
> code, and they are not being very nice about it.
>
> Can you make some shellcode that pops a shell?


What you are given
------------------

- `runpentadecimal`: An elf64 binary.

- `runpentadecimal.c`: The source code for `runpentadecimal`. Otherwise, you
  would just reverse the binary, right?

- `shellcode.asm`: An assembly skeleton for starting your shellcode.

What to hand in
---------------

Hand in the following:

- `pentadecimal.md` / `pentadecimal.txt` / `pentadecimal.pdf`: a short report.

- `shellcode.asm`: The shellcode you inject into `pentadecimal`, this
  shellcode should obtain a shell when it is run by `runpentadecimal`.

- Other files that you think are relevant (for instance, if you made a script
  to help you write your exploit or such like).

This task contains two equally important parts: Understanding the limitations
that your shellcode has to adhere to, and shellcode development. We will be
giving approximately equal weight to each part when grading this task.

We recommend that you try to solve the parts in order, as doing
otherwise is probably not going to be productive. We also suggest that
you hand in whatever you manage to achieve. Partial solutions are
likely to give some amount of points. If you cannot obey all the restrictions
simultaneously the first priority is to avoid `F` digits.


### Understanding the filter

Explain how shellcode is limited by the absence of `F` digits.
Which relevant instructions are ruled out?
What about parameters (immediates, offsets, etc.)?


### Adhering to the filter

Explain your overall strategy for working around the filter.

Perhaps start by giving the shellcode that you would have like to run, and
explain which transformations you had to do to work around the constraints.
