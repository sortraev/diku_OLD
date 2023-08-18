bits 64
global abc, _start

_start:
abc:
  push byte 57    ; fork().
  pop rax
  cdq             ; rdx = 0. both child and parent benefit from this.
  syscall

  test eax, eax   ; assumes PID's are <= 32 bits.
  jne short .parent

.child:
  push word 0xa40 ; we will be building the write bytes at 
  push rsp        ; rsp starting with "@\n".
  pop rsi

  mov dl, 2       ; write 2 bytes at a time.
.loop:
  inc byte [rsi]       ; in the very first loop, we increment to 'A' and turn it
  xor byte [rsi], 32   ; into a lower case 'a' by flipping bit 5; and so on for the
                       ; next iterations.

  mov al, 1       ; syscall write (rax assumed to be 0 at this point).
  push rax        ; rdi = 1 (stdout FD).
  pop rdi
  syscall

  cmp byte [rsi], 'Z' ; loop until we have printed an upper case 'Z'.
  jne short .loop


.parent:
  mov edi, eax    ; child PID is in eax after forking.
  xor esi, esi    ; all other args to wait4 should be 0.
  xor r10, r10

  push byte 61    ; 61 = syscall wait4.
.parent_syscall:
  pop rax
  syscall

.do_exit:
  push byte 111
  pop rdi
  push byte 60    ; 60 = syscall exit.
  jmp short .parent_syscall


