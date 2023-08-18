; bits 64
;
; global find_1337cafe
;
; ;;;;;;;;;;;;;;;;;
; ;;; VERSION 0 ;;;
; ;;;;;;;;;;;;;;;;;
; %if 0
; find_1337cafe:
; _find_1337cafe:
;   xor eax, eax
;
;   test rdi, rdi  ; if (root == NULL) return NULL;
;   je .end
;   mov rdx, rdi
;
;   cmp dword [rdx + 8], 0x1337cafe ; check current node
;   je .found_it
;
;   mov rdi, [rdx]     ; check left subtree
;
;   push rdx
;   call _find_1337cafe
;   pop rdx
;
;   test rax, rax
;   jne .end
;
;   mov rdi, [rdx + 16]
;   push rdx
;   call _find_1337cafe
;   pop rdx
;
;   ret
;
; .found_it:
;   lea rax, [rdx + 24]
; .end:
;   ret
;
; %else
; bits 64
;
; find_1337cafe:
;   mov rcx, rsp
;   push rdi
;
; .next_node:
;   pop rdi
;   test rdi, rdi         ; if (node == NULL)
;   je .next_node
;
;   cmp dword [rdi + 8], 0x1337cafe
;   je .end
;
;   push qword [rdi]      ; push child nodes
;   push qword [rdi + 16]
;   jmp .next_node
;
; .end:
;   lea rax, [rdi + 24]
;   ; lea rsp, [rcx + 8]
;   ; jmp [rcx]
;
; %endif
;
;
; %if 0
; bits 64
;
; find_1337cafe:
;   push byte 1   ; always at least 1 node in the tree.
;   pop rcx
;   push rdi
;
; .process_node:
;   pop rdx
;   test rdx, rdx         ; if (node == NULL)
;   je .next
;
;   lea rbx, [rdx + 24]
;   cmp dword [rdx + 8], 0x1337cafe
;   cmove rax, rbx
;
;   add rcx, byte 2       ; push right and left child
;   push qword [rdx]
;   push qword [rdx + 16]
;
;
; .next:
;   loop .process_node
;   ret
; %endif
;

global find_1337cafe
bits 64

find_1337cafe:
  mov rbx, rsp
  push rdi

.next_node:
  pop rdi
  test rdi, rdi         ; if (node == NULL)
  je .next_node

  push qword [rdi]      ; push child nodes
  push qword [rdi + 16]
  cmp dword [rdi + 8], 0x1337cafe
  jne .next_node

.end:
  lea rax, [rdi + 24]
  mov rsp, rbx
  ret
