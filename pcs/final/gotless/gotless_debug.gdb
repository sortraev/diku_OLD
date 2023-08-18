set environment LD_PRELOAD=./libc.so.6

set follow-fork-mode child


# the start of our rop-chain
# b *(hash + 145)



# b main
# use the `breakpoint_gadget` from rop_stuff.py to break here.
# b *(0x40120d)

run $(./doit.py)
b main
run $(./louis_doit.py)
b execve
c
