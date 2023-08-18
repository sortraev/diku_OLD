b *(main + 24)
commands
printf ">> malloc(student) == %p\n", $rax
c
end

b *(slurp + 23)
commands
printf ">> malloc(line)    == %p\n", $rax
c
end


b *(leaveMessage + 33)
commands
printf ">> malloc(message) == %p\n", $rax
c
end


b *(doAction + 24)
commands
printf ">> doAction(%p)\n", *(long*)$rdx
c
end

