# PCS Assignment 3

### `sortraev` (wlc376)

_**This assignment was made in collaboration with Louis Normann**_ (`cqb868`).


## Status of code handin

I finish all three code tasks with OnlineTA reporting "acceptable" for my
`careful` implementation, and `very good` for the other two. All three
implementations successfully pass my own tests as well as OnlineTA's.


## Task 1: `abc`

Below snippet shows my implementation.

```x86asm
 1: bits 64
 2: global abc, _start
 3: _start:
 4: abc:
 5:   push byte 57    ; fork a child.
 6:   pop rax
 7:   syscall
 8: 
 9:   test eax, eax   ; assumes PID's are smaller than 32 bits.
10:   je short .child
11: 
12: .parent:
13:   mov edi, eax    ; child PID is in eax after forking.
14:   xor esi, esi    ; all other args to wait4 should be 0.
15:   xor edx, edx
16:   xor r10d, r10d
17: 
18:   push byte 61    ; 61 = syscall wait4.
19: .parent_syscall:
20:   pop rax
21:   syscall
22: 
23: .do_exit:
24:   push byte 111
25:   pop rdi
26:   push byte 60    ; 60 = syscall exit.
27:   jmp short .parent_syscall
28: 
29: .child:
30:   mov cx, 0x0a40  ; initialize cx to the string "@\n".
31:                   ; cl will then be used to construct strings
32:                   ; for writing.
33: .loop:
34:   inc cl          ; in the very first loop, we increment to 'A' and turn it
35:   xor cl, 32      ; into a lower case 'a' by flipping bit 5; and so on for the
36:                   ; next iterations.
37: 
38:   push byte 2     ; write 2 bytes at a time.
39:   pop rdx
40: 
41:   push rcx        ; push the write string and get its address.
42:   push rsp
43:   pop rsi
44: 
45:   inc al          ; syscall write (rax assumed to be 0 at this point).
46:   push rax        ; rdi = FD for stdout.
47:   pop rdi
48:   syscall
49: 
50:   pop rcx
51:   cmp cl, 'Z'     ; loop until we have printed an upper case 'Z'>.
52:   jne short .loop
```

#### `abc`: Implementation walkthrough

The code first forks a child. The parent immediately calls `wait4 (2)` with the
child PID and all other args set to 0; after the wait is up, the parent exits
with exit code `111` (lines 13-21).

Meanwhile, the child uses a loop to print the alphabet, printing one character
and a newline in each iteration. I do so by first constructing the string
`"@\n"` in the `cx` register (line 30). The first byte of `cx` will then hold
the next letter to be printed in each iteration, while the second byte remains a
newline throughout.

In each iteration the first byte is incremented and XOR'ed to flip between
lower/upper case (lines 34-35), before `rcx` is `push`ed to the stack (line 41)
and its first two bytes are printed (lines 42-48). The loop continues until the
last character printed was `'Z'` (lines 51-52).


#### `abc`: Binary size

The compiled binary is 57 bytes. Aside from the usual tricks, such as using
`push byte/pop` to move byte immediates into registers in 3 bytes, some of the
methods I used to achieve the size are:

* 1) I use byte instructions whenever possible. For example, on line 44 I use
  `mov al, 1` to move 1 into `rax`, but this is only valid because I know `rax`
  is zero at this point (since it still holds the child's `fork()` result).
* 2) the `.parent_syscall` is used to perform both the `wait4` and `exit`
  syscalls of the parent.
* 3) since we do not care how the child process terminates, I simply let it
    segfault when the `cmp cl, 'Z'` on line 50 does not evaluate to zero.

    Alternatively, we *could* let the child jump to the `.do_exit` label and
    make a proper exit with code `111` for an extra 2 bytes. This would improve
    the runtime a little since waiting for the segfault takes about a second.

#### `abc`: Implementation challenges

The implementation itself was pretty straight forward. At first I forgot the
whole `fork()`-ing part of the task, but this was also pretty simple to build
around the existing printing code.

The biggest challenge was, of course, reducing the binary size. Changing my
program to make the print in a child process rather than simply in the parent
changed the byte count from 40 to 68.

Eliminating NULL-bytes in the code was not a problem at any point. I wish I
could write something interesting about it, but I simply paid attention to my
use of immediate values and instructions and then it happened naturally.

#### `abc`: Testing

I want to test that the child prints the right bytes and that the parent
correctly waits until the child is finished printing and segfaulting before
exiting with status 111. I also want to assert that the shellcode runs
successfully when injected into a random address.

First, I compile `abc` to a stand-alone executable using `nasm -f elf64 abc.asm
-o abc && ld abc.o -o abc` to facilitate testing.

To test that the correct bytes are printed, and no extra "hidden" (ie.
non-printable) bytes are written, I run `abc | hexdump -C` and inspect the
output. The hexdump shows only alphabetical characters and newlines are printed,
and so the test is successful.

To test that the parent successfully waits for the child, I use `strace ./abc`
and inspect the output. This shows a call to `wait4(<pid>, NULL, 0 NULL)` with
the correct `<pid>`; that the child exits with a `SIGSEGV`, as expected; and
that the parent exits with status `111`. Thus the test is successful.

---

## Task 2: `careful`

Below snippet shows my implementation of `careful`.

```x86asm
 1: bits 64
 2: global careful, _start
 3: _start:
 4: careful:
 5:   jmp short argv_envp_strs
 6: 
 7: build_argv_envp:
 8:   pop rdi              ; start address of argv and exe name.
 9: 
10:   lea rdx, [rdi + 19]  ; start address of username.
11:   lea rbx, [rdi + 30]  ; start address of password.
12: 
13:   push byte 48
14:   pop rcx
15: .foo:
16:   inc byte [rdi + rcx] ; increment each byte in the garbled string.
17:   loop .foo            ; such that the 0xff's become 0x00's.
18:                        ; rcx == 0 after this loop.
19: 
20:   push rcx     ; argv[2] = NULL;
21:   push rdx     ; argv[1] = "Numberwang";
22:   push rdi     ; argv[0] = "./handle_with_care";
23: 
24:   push rsp     ; rsi = pointer to argv.
25:   pop rsi
26: 
27:   push rcx     ; envp[1] = NULL;
28:   push rbx     ; envp[0] = "PCS_PASSWD=hunter2" (or "VAR_PCS=pcs r0cks");
29: 
30:   push rsp     ; rdx = pointer to envp.
31:   pop rdx
32: 
33:   push byte 59 ; syscall execve
34:   pop rax
35:   syscall
36: 
37: argv_envp_strs:
38:   call build_argv_envp
39:   db "..g`mckd^vhsg^b`qd", 0xff, "Mtladqv`mf", 0xff, "OBR^O@RRVC<gtmsdq1", 0xff
```

#### `careful`: Implementation walkthrough

To solve the task, we have to somehow build `argv` and `envp` in accessible
memory and then call `execve(argv[0], argv, envp)`, where:

```c
argv = { "./handle_with_care", "Numberwang", NULL }
envp = { "PCS_PASSWD=hunter2", NULL }
```

and where each string is `'\0'`-terminated. To achieve this, I hardcode the
three strings, including `'\0'`-terminators, but with each byte subtracted by 1
(line 37), which, during runtime, can then be reconstructed by adding 1 to each
byte.

First, I fetch the addresses of the three strings (lines 8-11), increment each
byte as described above (lines 13-17). Then I start pushing the `argv` and
`envp` arrays along with their NULL-terminators in reverse order (using the 0
that is in `rcx` after the loop), saving pointers in the argument registers
(lines 20-31), before making the `execve` call (line 33-35).


#### `careful`: Binary size and NULL bytes

The compiled binary is 87 bytes (or 86 with cheats, as described below). 49 of
these are for the three strings (and their NULL-terminators), meaning 39 bytes
are spent on trampolining, reconstructing the strings, and building `argv` and
`envp` in stack mem. 

Obviously we can't hardcode the strings with the string NULL terminators in
place, so I experimented with different ways of inserting them. My first idea
was to hardcode the strings in the binary with placeholder bytes for the NULL
terminators, and then to manually insert NULL-terminators using `mov [foo], al`,
where `rax` was set to zero beforehand. This gave a binary of 88 bytes, so I
only saved 1 byte by changing to the final method.

This also mostly explains how I eliminated NULL bytes in the binary.

#### `careful`: CHEATSS

In a disassembly of `handle_with_care`, I see that if the environment check for
`PCS_PASSWD=hunter2` fails, then the program will also accept `VAR_PCS=pcs
r0cks`. This can be done to "solve" the task in 1 byte less. However, the
assignment text specifically states that the program must set the `PCS_PASSWD`
env var, so I consider this a cheat. To build `careful` using the cheat, put a
`%define CHEATS` in `careful.asm`.


#### `careful`: Testing

I don't really test the program, except for checking the `execve` call using
`strace`, and also verifying that `./handle_with_care` gives the expected
output.

The test succeeds.

---
## Task 3: Treehugger

Below snippet shows my code for `hugtree`.

```x86asm
 1: bits 64
 2: hugtree:
 3:   mov rbx, rsp          ; store return address for early returning.
 4:   push rdi
 5: 
 6: .next_node:
 7:   pop rdi
 8:   test rdi, rdi         ; if node == NULL; skip node.
 9:   je .next_node
10: 
11:   push qword [rdi + 16] ; push right node first for left-to-right traversal
12:   push qword [rdi]
13: 
14:   cmp dword [rdi + 8], 0x1337cafe ; if this node is leet, return early.
15:   jne .next_node
16: 
17: .end:
18:   lea rax, [rdi + 24]
19:   mov rsp, rbx
20:   ret
```

#### `hugtree`: Implementation walkthrough

My code is pretty straight-forward. The implementation uses a pre-order
traversal to search the tree. The pre-order traversal is ensured by pushing the
*right* child before the left (lines 11-12), since they are popped in reverse
order.

The traversal returns early when the node with tag `0x1337cafe` is found (lines
14-15). To facilitate this early return, the stack pointer is stored/restored in
the prologue and epilogue.

In line 18, a `lea` is used rather than a `mov`, since the text field is a char
array rather than a char pointer, meaning the `MAX_STRING_LENGTH` bytes are
inherent to the struct.

Aside from this, there is not much special about the implementation.

#### `hugtree`: Binary size and NULL bytes

I implemented the traversal using the stack rather than explicit recursion since
I expected it to be both simpler and smaller in size (I later experimented with
a recursive function with two `call`s, and it was a whopping 52 bytes).

I also save a couple of bytes by checking the tag *after* pushing the two child
nodes, since this avoids a single jump.


#### `hugtree`: CHEATZ :E

By inspection of the disassembly of `./treehugger`, I see that all nodes of the
randomly generated tree are stored in a single, contiguous block of memory
returned by `calloc`, and that the pointer passed to `hugtree` is a pointer to
the first byte of this block.

I also see that each node takes 128 bytes of memory (makes sense, since this is
the size of the four fields after 8-byte alignment).

This means that instead of iterating the tree structure via child pointers, we
can simply iterate the tree in chunks of 128 bytes. Note that in the code, I
move the pointer by subtracting -128 rather than adding 128, since -128
fits in a byte while 128 does not.

I argue that this implementation is guaranteed to always succeed when the tree
is allocated in a contiguous block and the pointer passed to `hugtree` points to
the start of this block. However, for arbitrarily constructed, but valid trees,
the cheating implementation fails (it even failed my own tests!), and thus I
call it a cheat.

In any case, the cheating implementation uses 18 bytes. To test, insert a
`%define CHEATS` into `hugtree.asm` before compiling.

```x86asm
bits 64
hugtree:
.loop:
  cmp dword [rdi + 8], 0x1337cafe ; check tag and move to next node
  sub rdi, byte -128              ; (note: lea does not set ZF).
  jne short .loop
  lea rax, [rdi - 104]
  ret
```

#### `hugtree`: Testing

I write a small C program to randomly generate binary trees up to a maximum
depth of 12 (for a maximum of 4096 nodes), with the secret tag/password randomly
inserted into exactly 1 node in the tree. 

The function is then run on the root node, and the resulting string is tested
using `strcmp`. The test is repeated 100 times with new a random binary tree
each time.

The test is successful for the non-cheating version, but since each node is
allocated separately in my test code, the cheating version (as presented above)
obviously fails miserably.
