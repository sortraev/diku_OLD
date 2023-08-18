[BITS 64]

global ternary_convert

section .text



ternary_convert:
    xor rax, rax
    jmp .loop_foot

.loop:
    sub cl, 'x'            ; sub 120 and assert byte is now in range 0..2.
    cmp cl, 2
    ja .on_invalid          ; note: unsigned comparison!

    imul rax, 3             ; acc = acc * 3
    add rax, rcx            ; acc = acc + (next ternary digit)

    inc rdi                 ; load next byte.

.loop_foot:
    movzx ecx, byte [rdi]
    jecxz .end
    jmp .loop

.on_invalid:
    xor rax, rax
.end:
    ret    


ternary_convert_old:
    dec rdi

.skip_xs:           ; skip prepended x's.

    inc rdi
    movzx r11, byte [rdi]
    cmp r11, 'x'
    je .skip_xs

    xor rax, rax            ; set acc = 0.
    mov r10, 3

.body:
    cmp r11, 0              ; on null terminator: return acc.
    je .end


    sub r11, 'x'            ; sub 120 and assert byte is now in range 0..2.
    cmp r11, 2
    ja .on_invalid          ; note: unsigned comparison!

    mul r10                 ; acc = acc * 3
    add rax, r11            ; acc = acc + (next ternary digit)

    inc rdi                 ; load next byte.
    movzx r11, byte [rdi]
    jmp .body

.on_invalid:
    xor rax, rax
.end:
    ret

