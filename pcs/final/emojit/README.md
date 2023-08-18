Category: Bug finding + Exploit development

> I've been working on a emoji language as a replacement for eBPF on
> Windows, using JIT compilation for maximum speed. Even though I'm
> using JIT compilation, the language is safe because the emoji
> language can only be used for numeric computations.
>
> For extra safety I'm using a compilation technique that prevents
> infinite loops. Thus, the compiler should be ready to accept
> arbitrary programs.
>
> However, one of my TAs claims that I have an arbitrary code
> execution vulnerability, and is not being very nice about it.
>
> Can you help me determine if they are right?


What you are given
------------------

- `emojit`: An elf64 binary. The binary comes with build-in usage
  instructions and hints.

- `emojit.c`: the source code for `emojit`. Otherwise, you would just
  reverse the binary, right?


What to hand in
---------------

Hand in the following:

- `emojit.md` / `emojit.txt` / `emojit.pdf`: a short report.

- `exploit.txt`: A program in the emoji language, that uses a
  vulnerability to obtain a shell (`/bin/sh`). *If* you can find any
  exploitable bugs.

- Other files that you think are relevant (for instance, if you made a
  script to help you write your exploit or such like).

This task contains three equally important parts: Reversing with source
code, Bug finding/categorising, Exploit development. We will be giving
approximately equal weight to each part when grading this task.

We recommend that you try to solve the parts in order, as doing
otherwise is probably not going to be productive. We also suggest that
you hand in whatever you manage to achieve. Partial solutions are
likely to give some amount of points.


### Reversing with source code

Describe the essential functionality of the program.

That is, at least, what the different emojinstructions means and how
they work. Keep it succinct, and don't use more than one or two
sentences for each emojinstruction. Maybe use a small machine model
for your description.

Also, feel free to describe other parts of the program that you deem
*relevant* and *essential*.


### Bug finding/categorising

For this part, we expect you to find bugs in the program and describe them in
the report. The goal is to find bugs of sufficient quality/quantity that you
can get arbitrary code execution.

For each bug you do find, we expect you to:

- Describe how to trigger the bug.

- Describe what an attacker can achieve using the bug.

- Describe how/if the bug can be fixed.

Bugs that can not be exploited as a security vulnerability are not
interesting.


### Exploit development.

A detailed explanation on how you went about exploiting the
vulnerabilities that you have found. How you can get arbitrary code
execution, what constraints did you have to work around.

For the exploit development part, we expect you to hand in an exploit
that executes `/bin/sh`. The exploit should be given as file,
`exploit.txt`, that should contain a program in the emoji
language. The exploit should be triggered by executing the command:

    emojit -run exploit.txt

If you make more than one exploit then make a `exploit2.txt` (and so
on) file for each exploit (put your favourite in `exploit.txt`).

We expect you to document your choices made and any problems
encountered in the report. If you have developed any helper scripts
(or such like) to help you develop your exploits feel free to include
these.
