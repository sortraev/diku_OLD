b main
commands
printf "rbp main before get_answer: %llx\n", $rbp
end

b *(get_answer+8)
commands
printf "ret_addr before get_answer: %llx\n", *(long*) ($rbp + 8)
end

b *(get_answer+57)
commands
printf "ret_addr after get_answer: %llx\n", *(long*) ($rbp + 8)
end


b *(main+209)
commands
printf "rbp main after get_answer:  %llx\n", $rbp
end

run "$(cat shellcode)" < celeb_exploit.b
