# source ~/.gef-a85368fc771dcbb4db2b41818781e182845015b9.py
# b main

b exploit_me
commands
printf "exploit_me ret addr: %llx\n", *(long*)($rbp + 8)
end

b *(exploit_me+162)
commands
printf "exploit_me ret addr after scanf: %llx\n", *(long*)($rbp + 8)
end

run "foo"





