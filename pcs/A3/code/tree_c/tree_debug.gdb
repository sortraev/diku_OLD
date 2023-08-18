b hugtree.asm:90
command
printf ">> node == %p\n>> key  == %x\n", $rdx, *(unsigned int*) ($rdx + 8)
end

run
