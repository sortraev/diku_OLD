# This line is a comment

set logging file strange_gdb_debug.output
set logging overwrite on
set logging enabled on

b outer
commands
silent
printf ">> outer\n>> rdi = \"%s\"\n", $rdi
c
end

b update
commands
silent
printf ">> update\n"
info register rax rbx
c
end

run

set logging enabled off
