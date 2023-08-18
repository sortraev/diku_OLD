Disassembly of section .text:
...
00000000000011a9 <exploit_me>:
    11a9:	55                   	push   rbp
    11aa:	48 89 e5             	mov    rbp,rsp
    11ad:	48 83 ec 20          	sub    rsp,0x20
    11b1:	48 c7 45 e8 00 00 00 	mov    QWORD PTR [rbp-0x18],0x0
    11b8:	00
    11b9:	48 c7 45 e0 00 00 00 	mov    QWORD PTR [rbp-0x20],0x0
    11c0:	00
                                    ; setvbuf(stdout, NULL, mode = _IOFBF, size = 0);
    11c1:	48 8b 05 58 2e 00 00 	mov    rax,QWORD PTR [rip+0x2e58]        # 4020 <stdout@GLIBC_2.2.5>
    11c8:	b9 00 00 00 00       	mov    ecx,0x0
    11cd:	ba 02 00 00 00       	mov    edx,0x2
    11d2:	be 00 00 00 00       	mov    esi,0x0
    11d7:	48 89 c7             	mov    rdi,rax
    11da:	e8 81 fe ff ff       	call   1060 <setvbuf@plt>

                                    ; puts("Hello! What your name?")
    11df:	48 8d 3d 22 0e 00 00 	lea    rdi,[rip+0xe22]        # 2008 <_IO_stdin_used+0x8>
    11e6:	e8 45 fe ff ff       	call   1030 <puts@plt>

                                    ; char *name = NULL;
                                    ; char *n;
                                    ; ssize_t n_read = getline(&name, n, stdin);
    11eb:	48 8b 15 3e 2e 00 00 	mov    rdx,QWORD PTR [rip+0x2e3e]        # 4030 <stdin@GLIBC_2.2.5>
    11f2:	48 8d 4d e0          	lea    rcx,[rbp-0x20]
    11f6:	48 8d 45 e8          	lea    rax,[rbp-0x18]
    11fa:	48 89 ce             	mov    rsi,rcx
    11fd:	48 89 c7             	mov    rdi,rax
    1200:	e8 8b fe ff ff       	call   1090 <getline@plt>

                                    ; printf("Nice to meet you ");
    1205:	48 8d 3d 13 0e 00 00 	lea    rdi,[rip+0xe13]        # 201f <_IO_stdin_used+0x1f>
    120c:	b8 00 00 00 00       	mov    eax,0x0
    1211:	e8 2a fe ff ff       	call   1040 <printf@plt>

                                    ; here we have a format string vulnerability, since printf is
                                    ; called with a non-constant first argument. we can exploit
                                    ; this to print values on the stack, including return address.
                                    ; printf(name);
    1216:	48 8b 45 e8          	mov    rax,QWORD PTR [rbp-0x18]
    121a:	48 89 c7             	mov    rdi,rax
    121d:	b8 00 00 00 00       	mov    eax,0x0
    1222:	e8 19 fe ff ff       	call   1040 <printf@plt>

                                    ; puts("Do you know where waldo is?\n");
    1227:	48 8d 3d 03 0e 00 00 	lea    rdi,[rip+0xe03]        # 2031 <_IO_stdin_used+0x31>
    122e:	e8 fd fd ff ff       	call   1030 <puts@plt>

                                    ; read *everything* until the first newline
                                    ; character. this is the vulnerability!
                                    ; scanf("%[^\n]", [rbp-0x10]);
    1233:	48 8d 45 f0          	lea    rax,[rbp-0x10]
    1237:	48 89 c6             	mov    rsi,rax
    123a:	48 8d 3d 0c 0e 00 00 	lea    rdi,[rip+0xe0c]        # 204d <_IO_stdin_used+0x4d>
    1241:	b8 00 00 00 00       	mov    eax,0x0
    1246:	e8 35 fe ff ff       	call   1080 <__isoc99_scanf@plt>
    124b:	90                   	nop
    124c:	c9                   	leave
    124d:	c3                   	ret

000000000000124e <this_is_where_waldo_is>:
    124e:	55                   	push   rbp
    124f:	48 89 e5             	mov    rbp,rsp

                                    ; puts("Congratulations! You found waldo!");
    1252:	48 8d 3d ff 0d 00 00 	lea    rdi,[rip+0xdff]        # 2058 <_IO_stdin_used+0x58>
    1259:	e8 d2 fd ff ff       	call   1030 <puts@plt>

    125e:	48 8d 05 db 2d 00 00 	lea    rax,[rip+0x2ddb]        # 4040 <hellcode>

                                    ; // make the entire 4k block of memory in which
                                    ; // the shellcode is stored read, write, and
                                    ; // exec enabled.
                                    ; void *block = (void*)((uint64_t)shellcode & ~7);
                                    ; mprotect(page, 4096, PROT_READ | PROT_WRITE | PROT_EXEC);
    1265:	48 25 00 f0 ff ff    	and    rax,0xfffffffffffff000
    126b:	ba 07 00 00 00       	mov    edx,0x7    ; == PROT_READ | PROT_WRITE | PROT_EXEC.
    1270:	be 00 10 00 00       	mov    esi,0x1000 ; == 4096.
    1275:	48 89 c7             	mov    rdi,rax    ; == block containing shellcode.
    1278:	e8 f3 fd ff ff       	call   1070 <mprotect@plt>
    127d:	48 8d 05 bc 2d 00 00 	lea    rax,[rip+0x2dbc]        # 4040 <shellcode>
    1284:	ff d0                	call   rax
    1286:	bf 2a 00 00 00       	mov    edi,0x2a
    128b:	e8 10 fe ff ff       	call   10a0 <exit@plt>


0000000000001290 <main>:
    ; ...
    ; ...
    ; ...
    12ca:	e8 da fe ff ff       	call   11a9 <exploit_me>
                                    ; 12cf is the original return address of exploit_me.
    12cf:	b8 01 00 00 00       	mov    eax,0x1
    12d4:	c9                   	leave
    12d5:	c3                   	ret
    12d6:	66 2e 0f 1f 84 00 00 	cs nop WORD PTR [rax+rax*1+0x0]
    12dd:	00 00 00
