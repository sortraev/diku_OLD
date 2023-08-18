bits 64
global hugtree, _start


; %define cheats

%ifdef cheats

_start:
hugtree:
.loop:
  sub rdi, byte -128                     
  cmp dword [rdi - 120], 0x1337cafe 
  jne short .loop                   
  lea rax,  [rdi - 104]              
  ret

%else
_start:
hugtree:
  push rsp
  pop rbx
  push rdi

.next_node:
  pop rdi
  test rdi, rdi         ; if node == NULL; skip node.
  je .next_node

  push qword [rdi + 16] ; push right node first for left-to-right traversal
  push qword [rdi]

  cmp dword [rdi + 8], 0x1337cafe ; if this node holds secret tag, return early.
  jne .next_node

.end:
  lea rax, [rdi + 24]
  push rbx
  pop rsp
  ret

%endif
