set environment LD_PRELOAD=./bomb_overrides.so

# disas main
# b *(phase_6+192)
# commands
# printf "foo == %d\n", *(int*)($rbp-0x34)
# c
# end

b phase_7
run < solution.txt
