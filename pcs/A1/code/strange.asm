bits 64
extern puts
global main
section .text
main:
        sub rsp, 24
        mov BYTE [rsp + 16], 0
        mov rdi, rsp
        mov esi, 0
outer:
        mov eax, 0
        mov ecx, 3
inner:
        lea rbx, [4*rsi]
        add bl, 0x41
        add bl, cl
update:
        shl eax, 8
        mov al, bl
        dec ecx
        test ecx, ecx
        jns inner
        mov DWORD [rdi + 4*rsi], eax
        add esi, 1
        cmp esi, 4
        jl outer
        call puts
        add rsp, 24
        mov rax, 0
        ret
