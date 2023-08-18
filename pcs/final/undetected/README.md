Category: Reversing + Exploit development

> Sometimes you need to put back together what you have smashed to go undetected

This task is about exploiting a no-PIE executable with the NX flag
set.

The objective of this task is to create an interactive exploit that
makes the program call the function `win`, which will make the program
read the file `flag.txt` and print the content.


What you are given
------------------

- `undetected`: An elf64 binary. The vulnerable program.

- `libc.so.6`: The specific version of libc that should be used with
  `LD_PRELOAD` when running `undetected`. (Should not be needed inside
  the VM, but you get it for completeness anyway.)

- `doit.py`: A starting point for your exploit.

- `flag.txt`: A file with an example flag.


What to hand in
---------------

Hand in the following:

- `undetected.md` / `undetected.txt` / `undetected.pdf`: a short report.

  You are expected to document:

   * Describe which relevant security mitigations are in place, and
     what that means for which kinds of exploits are feasible.

   * Describe a vulnerability in the program, how it is triggered, and
     how (you think) it can be exploited.

   * An illustration of the layout of relevant memory that can be
     exploited. Explain how you identified what memory and how much of
     that memory is relevant.

   * Description of how you bypass the relevant security mitigations.

- `doit.py`: A working and well-commented python3 script implementing
  an exploit for `undetected`, resulting in `undetected` printing the
  content of the file `flag.txt`.

  Note that your python script should, obviously, not rely on being
  run in the same directory as the flag.

- Other files that you think are relevant (for instance, if you have
  made a stack drawing, your `doit.py` needs the `undetected` binary for
  analysis, or such like).

Remember that is OK to hand in whatever you manage to achieve. Partial
solutions are likely to give some points.
