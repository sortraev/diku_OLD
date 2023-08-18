#!/usr/bin/env python3
import subprocess
import sys

def get_monkey_sum_exp(n):
    monkey = b"\xf0\x9f\x99\x89"
    return (b"%b %b -" % (monkey, monkey) # -1 - -1 = 0.
            + b" %b -" % monkey * n       # subtract -1 n times.
            + b"\n")


solution_template = b"""\
Why'd you leave the keys upon the table?
nan inf
8589934592 17179869184
$#%%-/'(,)./$/ *(,/# ,/ $" /")'!&+.
%b"""

gdb_commands = rb"""\
set environment LD_PRELOAD=./bomb_mods.so
b *(phase_5+33)
commands
silent
printf "\n>> rax after `call funcHP48` == %d\n", $rax
quit
end
run
"""

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <test sum>")
        sys.exit()

    n = int(sys.argv[1])

    p = subprocess.Popen(["gdb", "-quiet", "./bomb"],
                         stdin  = subprocess.PIPE,
                         stderr = subprocess.PIPE
                        )

    solution  = solution_template % get_monkey_sum_exp(n)
    gdb_input = gdb_commands + solution

    p.communicate(input = gdb_input)







