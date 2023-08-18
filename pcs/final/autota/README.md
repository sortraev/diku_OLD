Category: Reversing + Stack attack

> Who needs OnlineTA, bah!
> Meet AutoTA!
> Get them to reveal the content of flag.txt

This task is about exploiting a buffer overflow in a no-PIE executable
with the NX flag set.

The technical objective of this task is to create an input to
`autota` that will make it print the content of the file `flag.txt`
(`flag.txt` will be in the same directory as `autota`).


What you are given
------------------

- `autota`: An elf64 binary. The vulnerable program.

- `doit.py`: A starting point for your exploit.

- `flag.txt`: A file with an example flag.


What to hand in
---------------

Hand in the following:

- `autota.md` / `autota.txt` / `autota.pdf`: a short report.

  You are expected to document:

   * Describe which relevant security mitigations are in place, and
     which are not in place.

   * You should explain where the buffer overflow is, and what
     restrictions it imposes on your exploit. And you should give an
     illustration of the stack layout in the relevant function you
     identified (perhaps in a separate file). Explain how you
     identified what and how much of the stack is relevant.

   * Likewise you should explain the overall plan for your exploit and
     what you try to achieve.

- Exploit `doit.py`: A python3 script that generates some input for
  `autota`, so that `./doit.py | ./autota` will print the
  content of the file `flag.txt`.

  Note that your python script should, obviously, not rely on being
  run in the same directory as the flag when creating the input to
  `autota`.

- Other files that you think are relevant (for instance, if you have
  made a stack drawing, if your `doit.py` need the `autota` binary for
  analysis, or such like).

Remember that is OK to hand in whatever you manage to achieve. Partial
solutions are likely to give some points.
