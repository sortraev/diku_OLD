Category: Stack + ROP

> Look ma, no syscalls and no libc functions either!  We present the newest
> hashing algorithm, `gotless`. It comes as an executable which takes a single
> argument on the command-line and outputs the hash as the program return
> value. The absence of syscalls or any use of dynamically linked functions
> should make it perfectly safe to run in an adversarial environment, although
> some say that traces of libc can never be truly erased...

This task is about exploiting a buffer overflow using ROP in a no-PIE executable
with the NX flag set.

The technical objective of this task is to create an input to
`gotless` that will make it print the contents of the file `flag.txt`
(`flag.txt` will be in the same directory as `gotless`).
Bonus points for making `gotless` exit gracefully afterwards with exit code
`66`.


What you are given
------------------

- `gotless`: An elf64 binary. The vulnerable program.

- `libc.so.6`: The specific version of libc that should be used with
  `LD_PRELOAD` when running `gotless`.

- `doit.py`: A starting point for your exploit.

- `flag.txt`: A file with an example flag.


What to hand in
---------------

Hand in the following:

- `gotless.md` / `gotless.txt` / `gotless.pdf`: a short report.

  You should explain where the buffer overflow is, and what
  restrictions it imposes on your ROP-chain. Likewise you should
  explain the overall plan for your ROP-chain and what you try to
  achieve.

- Exploit `doit.py`: A python3 script that generates some input for
  `gotless`, so that `./gotless $(./doit.py)` will print the
  content of the file `flag.txt`.

  Note that your python script should, obviously, not rely on being
  run in the same directory as the flag when creating the input to
  `gotless`.

- Other files that you think are relevant (for instance, if you have
  made a stack drawing, your `doit.py` needs the `gotless` binary for
  analysis, or such like).

Remember that is OK to hand in whatever you manage to achieve. Partial
solutions are likely to give some points.


Hints
-----

- The input to `gotless` is expected to use a simple "base-16" encoding scheme
  to allow passing arbitrary data. The provided Python script already takes
  care of suitably encoding the payload.

- You shouldn't need to use gadgets outside what's found in the `gotless`
  binary. But you might need to be a bit creative.

- A ROP payload is like x86 code: "instructions" have variable length and
  jumping into the middle of one can have interesting consequences.
