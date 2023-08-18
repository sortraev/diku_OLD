0000000000401336 <win>:
  401336:  f3 0f 1e fa            endbr64
  40133a:  50                     push   rax
  40133b:  58                     pop    rax
  40133c:  48 81 ec 98 00 00 00   sub    rsp,0x98
  401343:  64 48 8b 04 25 28 00   mov    rax,QWORD PTR fs:0x28
  40134a:  00 00 
  40134c:  48 89 84 24 88 00 00   mov    QWORD PTR [rsp+0x88],rax
  401353:  00 
  401354:  31 c0                  xor    eax,eax
  401356:  48 8d 35 a7 0c 00 00   lea    rsi,[rip+0xca7]        # 402004 <_IO_stdin_used+0x4>
  40135d:  48 8d 3d b0 0c 00 00   lea    rdi,[rip+0xcb0]        # 402014 <_IO_stdin_used+0x14>
  401364:  e8 87 fe ff ff         call   4011f0 <fopen@plt>
  401369:  48 85 c0               test   rax,rax
  40136c:  74 33                  je     4013a1 <win+0x6b>
  40136e:  48 89 c2               mov    rdx,rax
  401371:  48 89 e7               mov    rdi,rsp
  401374:  be 80 00 00 00         mov    esi,0x80
  401379:  e8 12 fe ff ff         call   401190 <fgets@plt>
  40137e:  48 85 c0               test   rax,rax
  401381:  74 3e                  je     4013c1 <win+0x8b>
  401383:  48 89 e7               mov    rdi,rsp
  401386:  e8 d5 fd ff ff         call   401160 <puts@plt>
  40138b:  48 8b 3d 2e 2d 00 00   mov    rdi,QWORD PTR [rip+0x2d2e]        # 4040c0 <stdout@GLIBC_2.2.5>
  401392:  e8 29 fe ff ff         call   4011c0 <fflush@plt>
  401397:  bf 00 00 00 00         mov    edi,0x0
  40139c:  e8 7f fe ff ff         call   401220 <exit@plt>
  4013a1:  48 8d 35 20 0d 00 00   lea    rsi,[rip+0xd20]        # 4020c8 <_IO_stdin_used+0xc8>
  4013a8:  bf 01 00 00 00         mov    edi,0x1
  4013ad:  b8 00 00 00 00         mov    eax,0x0
  4013b2:  e8 19 fe ff ff         call   4011d0 <__printf_chk@plt>
  4013b7:  bf 00 00 00 00         mov    edi,0x0
  4013bc:  e8 5f fe ff ff         call   401220 <exit@plt>
  4013c1:  48 8d 3d 3e 0c 00 00   lea    rdi,[rip+0xc3e]        # 402006 <_IO_stdin_used+0x6>
  4013c8:  e8 33 fe ff ff         call   401200 <perror@plt>
  4013cd:  bf 01 00 00 00         mov    edi,0x1
  4013d2:  e8 49 fe ff ff         call   401220 <exit@plt>

00000000004013d7 <input>:
  ; reads up to 420 bytes (including '\0') of user input into a local stack
  ; buffer, and then copies this to the user buffer pointed to by rdi.
  ;
  ; void input(char *buf_out) {
  ;   char buf[420];
  ;   if (fgets(buf, 420, stdin) == 0) {
  ;     puts("no input. :crying_cat_face"\n");
  ;     exit(1);
  ;   }
  ;   
  ;   size_t n_read = strlen(buf);
  ;   memcpy(buf_out, buf, n_read);
  ; }
  4013d7:  f3 0f 1e fa            endbr64
  4013db:  55                     push   rbp
  4013dc:  53                     push   rbx
  4013dd:  48 81 ec b8 01 00 00   sub    rsp,0x1b8
  4013e4:  48 89 fb               mov    rbx,rdi
  ; setup stack canary
  4013e7:  64 48 8b 04 25 28 00   mov    rax,QWORD PTR fs:0x28
  4013f0:  48 89 84 24 a8 01 00   mov    QWORD PTR [rsp+0x1a8],rax
  4013f8:  31 c0                  xor    eax,eax

  ; first arg = pointer to stack buffer
  4013fa:  48 89 e7               mov    rdi,rsp
  ; third arg = stdin
  4013fd:  48 8b 15 cc 2c 00 00   mov    rdx,QWORD PTR [rip+0x2ccc]        # 4040d0 <stdin@GLIBC_2.2.5>
  ; second arg = 420 bytes
  401404:  be a4 01 00 00         mov    esi,0x1a4

  ; read up to 419 bytes + NULL terminator using fgets
  ; if (fgts(buf, 420, stdin) == 0) {
  ;   puts("no input. :crying_cat_face"\n");
  ;   exit(1);
  ; }
  401409:  e8 82 fd ff ff         call   401190 <fgets@plt>
  40140e:  48 85 c0               test   rax,rax
  401411:  74 36                  je     401449 <input+0x72>

  ; size_t n_read = strlen(buf);
  401413:  48 89 e5               mov    rbp,rsp
  401416:  48 89 ef               mov    rdi,rbp
  401419:  e8 52 fd ff ff         call   401170 <strlen@plt>
  40141e:  48 63 d0               movsxd rdx,eax
  401421:  48 89 ee               mov    rsi,rbp
  401424:  48 89 df               mov    rdi,rbx
  401427:  e8 74 fd ff ff         call   4011a0 <memcpy@plt>
  40142c:  48 8b 84 24 a8 01 00   mov    rax,QWORD PTR [rsp+0x1a8]
  401433:  00 
  401434:  64 48 33 04 25 28 00   xor    rax,QWORD PTR fs:0x28
  40143b:  00 00 
  40143d:  75 20                  jne    40145f <input+0x88>
  40143f:  48 81 c4 b8 01 00 00   add    rsp,0x1b8
  401446:  5b                     pop    rbx
  401447:  5d                     pop    rbp
  401448:  c3                     ret

  ; puts("no input. :crying_cat_face:");
  401449:  48 8d 3d cd 0b 00 00   lea    rdi,[rip+0xbcd]        # 40201d <_IO_stdin_used+0x1d>
  401450:  e8 0b fd ff ff         call   401160 <puts@plt>
  401455:  bf 01 00 00 00         mov    edi,0x1
  40145a:  e8 c1 fd ff ff         call   401220 <exit@plt>
  40145f:  e8 1c fd ff ff         call   401180 <__stack_chk_fail@plt>

0000000000401464 <super_secret>:
  401464:  f3 0f 1e fa            endbr64
  401468:  53                     push   rbx
  ; char *super_secret() {
  ;   char *buf = malloc(32);
  ;   for (int i = 0; i < 32; i++)
  ;     buf[i] = 0;
  ;
  ;   if (getrandom(buf, 32, 0) < 32)
  ;     __fprintf_chk(stderr, 1, "running out of randomness\n");
  ;
  ;   char *p = buf;
  ;   char *buf_end = buf + 32;
  ;   do {
  ;     int c = *p;
  ;     if (c == '\0' || c == '\n')
  ;       *p = 1;
  ;   } while (++p != buf_end);
  ;
  ;   return buf;
  ; }
  ;
  ;   char *buf = malloc(32)
  401469:  bf 20 00 00 00         mov    edi,0x20
  40146e:  e8 3d fd ff ff         call   4011b0 <malloc@plt>
  401473:  48 89 c3               mov    rbx,rax

  ;   for (int i = 0; i < 32; i++)
  ;     buf[i] = 0;
  401476:  48 c7 00 00 00 00 00   mov    QWORD PTR [rax],0x0
  40147d:  48 c7 40 08 00000000   mov    QWORD PTR [rax+0x8],0x0
  401485:  48 c7 40 10 00000000   mov    QWORD PTR [rax+0x10],0x0
  40148d:  48 c7 40 18 00000000   mov    QWORD PTR [rax+0x18],0x0

  ; setup args to getrandom(buf, 32, 0);
  401495:  ba 00 00 00 00         mov    edx,0x0
  40149a:  be 20 00 00 00         mov    esi,0x20
  40149f:  48 89 c7               mov    rdi,rax

  ; here, control flow is a little different from the above C code.
  ;   if (getrandom(buf, 32, 0) < 32)
  ;     __fprintf_chk(stderr, 1, "running out of randomness\n");
  4014a2:  e8 99 fd ff ff         call   401240 <getrandom@plt>
  4014a7:  48 83 f8 1f            cmp    rax,0x1f
  4014ab:  7e 09                  jle    4014b6 <super_secret+0x52>

  ;   char *p = buf;
  ;   char *buf_end = buf + 32;
  4014ad:  48 89 d8               mov    rax,rbx
  4014b0:  48 8d 73 20            lea    rsi,[rbx+0x20]
  4014b4:  eb 2b                  jmp    4014e1 <super_secret+0x7d>

  4014b6:  48 8d 15 7b 0b 00 00   lea    rdx,[rip+0xb7b]        # 402038 <_IO_stdin_used+0x38>
  4014bd:  be 01 00 00 00         mov    esi,0x1
  4014c2:  48 8b 3d 17 2c 00 00   mov    rdi,QWORD PTR [rip+0x2c17]        # 4040e0 <stderr@GLIBC_2.2.5>
  4014c9:  b8 00 00 00 00         mov    eax,0x0
  4014ce:  e8 5d fd ff ff         call   401230 <__fprintf_chk@plt>
  4014d3:  eb d8                  jmp    4014ad <super_secret+0x49>

  ; the next block of code in the disassembly is the do while-loop seen in the
  ; above C code. however, the structure is of course a little different here.

  ;   *p = 1;
  4014d5:  c6 01 01               mov    BYTE PTR [rcx],0x1

  ;   if ++p == buf_end, exit loop.
  4014d8:  48 83 c0 01            add    rax,0x1
  4014dc:  48 39 f0               cmp    rax,rsi
  4014df:  74 11                  je     4014f2 <super_secret+0x8e>

  ;   do {
  ;     int c = *p;
  ;     if (c == '\0' || c == '\n')
  ;       // jmp to *p = 1 and/or loop condition
  4014e1:  48 89 c1               mov    rcx,rax
  4014e4:  0f b6 10               movzx  edx,BYTE PTR [rax]
  4014e7:  84 d2                  test   dl,dl
  4014e9:  74 ea                  je     4014d5 <super_secret+0x71>
  4014eb:  80 fa 0a               cmp    dl,0xa
  4014ee:  75 e8                  jne    4014d8 <super_secret+0x74>
  4014f0:  eb e3                  jmp    4014d5 <super_secret+0x71>

  ;   return buf;
  4014f2:  48 89 d8               mov    rax,rbx
  4014f5:  5b                     pop    rbx
  4014f6:  c3                     ret

0000000000401555 <main>:
  ; int main() {
  ;   for (int i = 42; i > 0; i--) {
  ;     puts("try to leak my secret. come on, I dare you");
  ;  
  ;     char *user_buf = malloc(32);
  ;     char *secrets  = super_secret();
  ;  
  ;     __printf_chk(1, "btw do you know a good riddle? ...");
  ;  
  ;     input(user_buf);
  ;  
  ;     __printf_chk(1, "I see. so your riddle is: ");
  ;     puts(user_buf);
  ;     __printf_chk(1, "anyway. what's my secret?");
  ;  
  ;     char user_guess[33];
  ;     if (fgets(user_guess, 33, stdin) == 0) {
  ;       puts("choosing not to play, I see. You still lose.");
  ;       exit(1);
  ;     }
  ;  
  ;     if (strncmp(secrets, user_guess, 32) != 0) {
  ;       puts("you failed. as expected. :smirk:");
  ;       exit(1);
  ;     }
  ;  
  ;     puts("wow. I didn't expect that");
  ;     puts("ok, so what is the answer to your riddle?");
  ;  
  ;     if (scanf("%[^\n]%*c", user_buf) != 1) {
  ;       puts("no input. :crying_cat_face:.");
  ;       exit(1);
  ;     }
  ;  
  ;     puts("hmm. I'm not sure I appreciate that kind of humour.");
  ;     free(secrets);
  ;     free(user_buf);
  ;     puts("Let's try again");
  ;   }
  ;   puts(":flustered: well, I didn't expect that");
  ;   win();
  ; }
  401555:  f3 0f 1e fa            endbr64
  401559:  41 55                  push   r13
  40155b:  41 54                  push   r12
  40155d:  55                     push   rbp
  40155e:  53                     push   rbx
  40155f:  48 83 ec 48            sub    rsp,0x48
  ; stack canary
  401563:  64 48 8b 04 25 28 00   mov    rax,QWORD PTR fs:0x28
  40156a:  00 00 
  40156c:  48 89 44 24 38         mov    QWORD PTR [rsp+0x38],rax
  401571:  31 c0                  xor    eax,eax

  ; "try to leak my secret. come on, I dare you"
  401573:  48 8d 3d 9e 0b 00 00   lea    rdi,[rip+0xb9e]        # 402118 <_IO_stdin_used+0x118>
  40157a:  e8 e1 fb ff ff         call   401160 <puts@plt>
  ; r12d = '*' (42)
  40157f:  41 bc 2a 00 00 00      mov    r12d,0x2a
  401585:  49 89 e5               mov    r13,rsp

  ; char *user_buf = malloc(32);
  ; char *secrets  = super_secret();
  401588:  bf 20 00 00 00         mov    edi,0x20
  40158d:  e8 1e fc ff ff         call   4011b0 <malloc@plt>
  401592:  48 89 c3               mov    rbx,rax
  401595:  b8 00 00 00 00         mov    eax,0x0
  40159a:  e8 c5 fe ff ff         call   401464 <super_secret>
  40159f:  48 89 c5               mov    rbp,rax

  ; __printf_chk(1, "btw do you know a good riddle? ...");
  4015a2:  48 8d 35 c7 0b 00 00   lea    rsi,[rip+0xbc7]        # 402170 <_IO_stdin_used+0x170>
  4015a9:  bf 01 00 00 00         mov    edi,0x1
  4015ae:  b8 00 00 00 00         mov    eax,0x0
  4015b3:  e8 18 fc ff ff         call   4011d0 <__printf_chk@plt>

  ; input(user_buf);
  4015b8:  48 89 df               mov    rdi,rbx
  4015bb:  e8 17 fe ff ff         call   4013d7 <input>

  ; __printf_chk(1, "I see. So your riddle is:");
  4015c0:  48 8d 35 8d 0a 00 00   lea    rsi,[rip+0xa8d]        # 402054 <_IO_stdin_used+0x54>
  4015c7:  bf 01 00 00 00         mov    edi,0x1
  4015cc:  b8 00 00 00 00         mov    eax,0x0
  4015d1:  e8 fa fb ff ff         call   4011d0 <__printf_chk@plt>
 0x4052a0 0x4052d0
  ; puts(user_buf);
  4015d6:  48 89 df               mov    rdi,rbx
  4015d9:  e8 82 fb ff ff         call   401160 <puts@plt>

  ; __printf_chk(1, "anyway. what's my secret?");
  4015de:  48 8d 35 8a 0a 00 00   lea    rsi,[rip+0xa8a]        # 40206f <_IO_stdin_used+0x6f>
  4015e5:  bf 01 00 00 00         mov    edi,0x1
  4015ea:  b8 00 00 00 00         mov    eax,0x0
  4015ef:  e8 dc fb ff ff         call   4011d0 <__printf_chk@plt>


  ; char user_guess[33];
  ; if (fgets(user_guess, 33, stdin) == 0) {
  ;   puts("choosing not to play, I see. You still lose.");
  ;   exit(1);
  ; }
  4015f4:  48 8b 15 d5 2a 00 00   mov    rdx,QWORD PTR [rip+0x2ad5]        # 4040d0 <stdin@GLIBC_2.2.5>
  4015fb:  be 21 00 00 00         mov    esi,0x21
  401600:  4c 89 ef               mov    rdi,r13
  401603:  e8 88 fb ff ff         call   401190 <fgets@plt>
  401608:  48 85 c0               test   rax,rax
  40160b:  0f 84 91 00 00 00      je     4016a2 <main+0x14d>

  ; if (strncmp(secret, user_guess, 32) == 0) {
  ; }
  401611:  ba 20 00 00 00         mov    edx,0x20
  401616:  4c 89 ee               mov    rsi,r13
  401619:  48 89 ef               mov    rdi,rbp
  40161c:  e8 2f fb ff ff         call   401150 <strncmp@plt>
  401621:  85 c0                  test   eax,eax
  401623:  0f 85 a5 00 00 00      jne    4016ce <main+0x179>

  ; puts("wow. I didnt expect that...");
  ; puts("ok, so what is the answer to your riddle?");
  401629:  48 8d 3d 5a 0a 00 00   lea    rdi,[rip+0xa5a]        # 40208a <_IO_stdin_used+0x8a>
  401630:  e8 2b fb ff ff         call   401160 <puts@plt>
  401635:  48 8d 3d b4 0b 00 00   lea    rdi,[rip+0xbb4]        # 4021f0 <_IO_stdin_used+0x1f0>
  40163c:  e8 1f fb ff ff         call   401160 <puts@plt>

  ; if (scanf("%[^\n]%*c", user_buf) == 1) {
  ;   puts("hmm. I'm not sure I appreciate that kind of humour.");
  ;   free(secrets);
  ;   free(user_buf);
  ;   puts("Let's try again");
  ;   i--;
  ; }
  401641:  48 89 de               mov    rsi,rbx
  401644:  48 8d 3d 5c 0a 00 00   lea    rdi,[rip+0xa5c]        # 4020a7 <_IO_stdin_used+0xa7>
  40164b:  b8 00 00 00 00         mov    eax,0x0
  401650:  e8 bb fb ff ff         call   401210 <__isoc99_scanf@plt>
  401655:  83 f8 01               cmp    eax,0x1
  401658:  75 5e                  jne    4016b8 <main+0x163>

  ; puts("hmm. I'm not sure I appreciate that kind of humour.");
  40165a:  48 8d 3d bf 0b 00 00   lea    rdi,[rip+0xbbf]        # 402220 <_IO_stdin_used+0x220>
  401661:  e8 fa fa ff ff         call   401160 <puts@plt>

  ; free(secrets);
  ; free(user_buf);
  401666:  48 89 ef               mov    rdi,rbp
  401669:  e8 d2 fa ff ff         call   401140 <free@plt>
  40166e:  48 89 df               mov    rdi,rbx
  401671:  e8 ca fa ff ff         call   401140 <free@plt>

  401676:  48 8d 3d 33 0a 00 00   lea    rdi,[rip+0xa33]        # 4020b0 <_IO_stdin_used+0xb0>
  40167d:  e8 de fa ff ff         call   401160 <puts@plt>
  401682:  41 83 ec 01            sub    r12d,0x1
  401686:  0f 85 fc fe ff ff      jne    401588 <main+0x33>

  ; puts(":flustered: well, I didn't expect that");
  ; win();
  40168c:  48 8d 3d b5 0a 00 00   lea    rdi,[rip+0xab5]        # 402148 <_IO_stdin_used+0x148>
  401693:  e8 c8 fa ff ff         call   401160 <puts@plt>
  401698:  b8 00 00 00 00         mov    eax,0x0
  40169d:  e8 94 fc ff ff         call   401336 <win>

  ;   puts("choosing not to play, I see. You still lose.");
  ;   exit(1);
  4016a2:  48 8d 3d 17 0b 00 00   lea    rdi,[rip+0xb17]        # 4021c0 <_IO_stdin_used+0x1c0>
  4016a9:  e8 b2 fa ff ff         call   401160 <puts@plt>
  4016ae:  bf 01 00 00 00         mov    edi,0x1
  4016b3:  e8 68 fb ff ff         call   401220 <exit@plt>

  ;   puts("no input. :crying_cat_face:.");
  ;   exit(1);
  4016b8:  48 8d 3d 5e 09 00 00   lea    rdi,[rip+0x95e]        # 40201d <_IO_stdin_used+0x1d>
  4016bf:  e8 9c fa ff ff         call   401160 <puts@plt>
  4016c4:  bf 01 00 00 00         mov    edi,0x1
  4016c9:  e8 52 fb ff ff         call   401220 <exit@plt>

  ;   puts("you failed. as expected. :smirk:");
  ;   exit(1);
  4016ce:  48 8d 3d 83 0b 00 00   lea    rdi,[rip+0xb83]        # 402258 <_IO_stdin_used+0x258>
  4016d5:  e8 86 fa ff ff         call   401160 <puts@plt>
  4016da:  bf 01 00 00 00         mov    edi,0x1
  4016df:  e8 3c fb ff ff         call   401220 <exit@plt>
