#!/usr/bin/env python3
import subprocess
import sys
import numpy as np
import os


monkeys = bytes("🙊 🙊 + 🙉 - 🙉 - 🙈 + 🙈 + 🙊 + 🙊 + 🙊 + 🙉 - 🙈 + 🙊 + 🙊 + 🙊 +",
                encoding = "utf8")


gdb_commands = rb"""\
set environment LD_PRELOAD=./bomb_mods.so

b *(phase_6+171)
commands
silent
printf "\n>> result of foo6:\n"
printf "   [rbp + 0*4 - 0x2c] == %ld\n", *(long*)($rbp + 0 * 4 - 0x2c)
printf "   [rbp + 1*4 - 0x2c] == %ld\n", *(long*)($rbp + 1 * 4 - 0x2c)
printf "   [rbp + 2*4 - 0x2c] == %ld\n", *(long*)($rbp + 2 * 4 - 0x2c)
printf "   [rbp + 3*4 - 0x2c] == %ld\n", *(long*)($rbp + 3 * 4 - 0x2c)
printf "   [rbp + 4*4 - 0x2c] == %ld\n", *(long*)($rbp + 4 * 4 - 0x2c)
printf "   [rbp + 5*4 - 0x2c] == %ld\n", *(long*)($rbp + 5 * 4 - 0x2c)
end

run
"""

import subprocess
import numpy as np
import os
my_env = os.environ.copy()
my_env["LD_PRELOAD"] = "/home/sortraev/data/pcs/A2/code/bomb_mods.so"

solution_template = (b"""\
Why'd you leave the keys upon the table?
nan inf
8589934592 17179869184
DCEMOGHLINODO@JHLOC@LO@DB@OBIGAFKN
%b
10 33165873224
""" % monkeys) + b"%d %d %d %d %d %d\n"

def search(i):
    print(f"hello from thread {i} :)")
    while True:
        phase_6_input = tuple(np.random.randint(-100, 100, 6))

        p = subprocess.Popen(["./bomb"],
                             env = my_env,
                             stdin  = subprocess.PIPE,
                             stderr = subprocess.PIPE,
                             stdout = subprocess.PIPE,
                            )

        solution = solution_template % phase_6_input

        bomb_output = p.communicate(input = solution)[0]
        if b"Phase 6 has been defused" in bomb_output:
            print(phase_6_input)
            break


from threading import Thread
if __name__ == "__main__":
    for i in range(8):
        thread = Thread(target = search, args = (i, ))
        thread.start()
    for i in range(8):
        thread.join()
