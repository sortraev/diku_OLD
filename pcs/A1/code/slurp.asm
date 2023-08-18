[BITS 64]

global slurp, slurp2
extern fgetc

section .text

slurp:
    push r12                 ;; push callee-saved registers.
    push r13
    push r14
    mov r12, rdi
    mov r14, rsi

    xor r13, r13

.body:
    cmp r13, 126             ;; loop until >=126 bytes.
    jge .on_eol

    mov rdi, r12             ;; read a byte.
    call fgetc

    cmp eax, 0               ;; on EOL.
    jl .on_eol

    mov [r14 + r13], byte al ;; store the valid byte in buffer.
    inc r13

    cmp eax, 10              ;; repeat if byte was not a newline.
    jne .body

    mov [r14 + r13], byte 0  ;; if newline before 126 bytes: terminate.
    jmp .end

.on_eol:
    mov [r14 + r13],     byte 10
    mov [r14 + r13 + 1], byte 0

.end:
    mov rax, r13
    pop r14
    pop r13
    pop r12
    ret

slurp2:
    mov r10, rsi
    mov rdx, 1          ;; read 1 byte.
    xor r9, r9
.body:
    cmp r9, 126         ;; loop until >= 126 bytes.
    jge .on_eol
    xor rax, rax        ;; setup read syscall.
    lea rsi, [r10 + r9] ;; read directly into output buffer.
    syscall
    cmp rax, 0
    jle .on_eol
    inc r9
    cmp byte [r10 + r9 - 1], 10 ;; repeat if byte was not a newline.
    jne .body
    mov [r10 + r9], byte 0      ;; if newline before 126 bytes: terminate.
    jmp .end
.on_eol:
    mov [r10 + r9],     byte 10
    mov [r10 + r9 + 1], byte 0
.end:
    mov rax, r9
    ret

