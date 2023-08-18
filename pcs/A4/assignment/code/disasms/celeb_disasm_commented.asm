00000000004012a2 <get_answer>:
  ; int get_answer(char *expected) {
  ;   int canary = 0xaa55;
  ;   char answer_buf[104];
  ;   int res = getaline(answer_buf);
  ;   if (canary != 0xaa55) {
  ;     puts("Ouch, don't overdo it!\n");
  ;     exit(1);
  ;   }
  ;   if (strcmp(expected, answer_buf) == 0)
  ;     return res;
  ;   else
  ;     return -res;
  ; }
  4012a2:	f3 0f 1e fa          	endbr64
  4012a6:	55                   	push   rbp
  4012a7:	48 89 e5             	mov    rbp,rsp
  4012aa:	48 83 c4 80          	add    rsp,0xffffffffffffff80
  4012ae:	48 89 7d 88          	mov    QWORD PTR [rbp-0x78],rdi
  4012b2:	c7 45 f8 55 aa 00 00 	mov    DWORD PTR [rbp-0x8],0xaa55
  4012b9:	48 8d 45 90          	lea    rax,[rbp-0x70]
  4012bd:	ba 68 00 00 00       	mov    edx,0x68
  4012c2:	be 00 00 00 00       	mov    esi,0x0
  4012c7:	48 89 c7             	mov    rdi,rax
  4012ca:	e8 11 fe ff ff       	call   4010e0 <memset@plt>
  4012cf:	48 8d 45 90          	lea    rax,[rbp-0x70]
  4012d3:	48 89 c7             	mov    rdi,rax

  4012d6:	e8 5b ff ff ff       	call   401236 <getaline>
  4012db:	89 45 fc             	mov    DWORD PTR [rbp-0x4],eax

                                    ; check that the canary was not overwritten.
  4012de:	8b 45 f8             	mov    eax,DWORD PTR [rbp-0x8]
  4012e1:	3d 55 aa 00 00       	cmp    eax,0xaa55
  4012e6:	74 16                	je     4012fe <get_answer+0x5c>

                                    ; if it was, print "don't overdo it" and
                                    ; exit(1).
  4012e8:	48 8d 3d 38 0f 00 00 	lea    rdi,[rip+0xf38]        # 402227 <_IO_stdin_used+0x227>
  4012ef:	e8 cc fd ff ff       	call   4010c0 <puts@plt>
  4012f4:	bf 01 00 00 00       	mov    edi,0x1
  4012f9:	e8 32 fe ff ff       	call   401130 <exit@plt>

                                    ; compare the given answer with the expected song line.
                                    ; strcmp(expected, answer_buf)
  4012fe:	48 8b 55 88          	mov    rdx,QWORD PTR [rbp-0x78]
  401302:	48 8d 45 90          	lea    rax,[rbp-0x70]
  401306:	48 89 d6             	mov    rsi,rdx
  401309:	48 89 c7             	mov    rdi,rax
  40130c:	e8 ff fd ff ff       	call   401110 <strcmp@plt>
  401311:	85 c0                	test   eax,eax
  401313:	75 05                	jne    40131a <get_answer+0x78>
  401315:	8b 45 fc             	mov    eax,DWORD PTR [rbp-0x4]
  401318:	eb 05                	jmp    40131f <get_answer+0x7d>
  40131a:	8b 45 fc             	mov    eax,DWORD PTR [rbp-0x4]
  40131d:	f7 d8                	neg    eax
  40131f:	c9                   	leave
  401320:	c3                   	ret

0000000000401351 <main>:
  401351:	f3 0f 1e fa          	endbr64
  401355:	55                   	push   rbp
  401356:	48 89 e5             	mov    rbp,rsp
  401359:	48 83 ec 20          	sub    rsp,0x20
  40135d:	89 7d ec             	mov    DWORD PTR [rbp-0x14],edi
  401360:	48 89 75 e0          	mov    QWORD PTR [rbp-0x20],rsi
  401364:	48 8d 05 d5 0e 00 00 	lea    rax,[rip+0xed5]        # 402240 <_IO_stdin_used+0x240>
  40136b:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
  40136f:	48 8b 45 e0          	mov    rax,QWORD PTR [rbp-0x20]
  401373:	48 8b 40 08          	mov    rax,QWORD PTR [rax+0x8]
  401377:	48 89 05 fa 2c 00 00 	mov    QWORD PTR [rip+0x2cfa],rax        # 404078 <shellcode>
  40137e:	bf 00 00 00 00       	mov    edi,0x0
  401383:	e8 98 fd ff ff       	call   401120 <time@plt>
  401388:	89 c7                	mov    edi,eax
  40138a:	e8 71 fd ff ff       	call   401100 <srand@plt>

                                    ; get rand in range 0..6 (inclusive).
                                    ; i = random_bounded(); //
  40138f:	bf 07 00 00 00       	mov    edi,0x7
  401394:	e8 88 ff ff ff       	call   401321 <random_bounded>
  401399:	89 45 f4             	mov    DWORD PTR [addr_random_number],eax
  401399 - 401351

                                    ; puts("Let's play a little quiz...");
  40139c:	48 8d 3d c7 0e 00 00 	lea    rdi,[rip+0xec7]        # 40226a <_IO_stdin_used+0x26a>
  4013a3:	e8 18 fd ff ff       	call   4010c0 <puts@plt>

                                    ; puts("I'll give you a line from the best song EVAR...");
  4013a8:	48 8d 3d d9 0e 00 00 	lea    rdi,[rip+0xed9]        # 402288 <_IO_stdin_used+0x288>
  4013af:	e8 0c fd ff ff       	call   4010c0 <puts@plt>

                                    ; puts("You'll have to repeat with next line. Ready?");
  4013b4:	48 8d 3d fd 0e 00 00 	lea    rdi,[rip+0xefd]        # 4022b8 <_IO_stdin_used+0x2b8>
  4013bb:	e8 00 fd ff ff       	call   4010c0 <puts@plt>
  4013c0:	8b 45 f4             	mov    eax,DWORD PTR [addr_random_number]
  4013c3:	48 98                	cdqe
  4013c5:	48 c1 e0 04          	shl    rax,0x4
  4013c9:	48 89 c2             	mov    rdx,rax

                                    ; char *line = lines[i];
  4013cc:	48 8d 05 cd 29 00 00 	lea    rax,[rip+0x29cd]        # 403da0 <db>
  4013d3:	48 8b 04 02          	mov    rax,QWORD PTR [rdx+rax*1]
  4013d7:	48 89 c6             	mov    rsi,rax
  4013da:	48 8d 3d 05 0f 00 00 	lea    rdi,[rip+0xf05]        # 4022e6 <_IO_stdin_used+0x2e6>
  4013e1:	b8 00 00 00 00       	mov    eax,0x0
  4013e6:	e8 e5 fc ff ff       	call   4010d0 <printf@plt>
  4013eb:	48 8d 3d 01 0f 00 00 	lea    rdi,[rip+0xf01]        # 4022f3 <_IO_stdin_used+0x2f3>
  4013f2:	e8 c9 fc ff ff       	call   4010c0 <puts@plt>
  4013f7:	8b 45 f4             	mov    eax,DWORD PTR [addr_random_number]
  4013fa:	48 98                	cdqe
  4013fc:	48 c1 e0 04          	shl    rax,0x4
  401400:	48 89 c2             	mov    rdx,rax
  401403:	48 8d 05 9e 29 00 00 	lea    rax,[rip+0x299e]        # 403da8 <db+0x8>
  40140a:	48 8b 04 02          	mov    rax,QWORD PTR [rdx+rax*1]
  40140e:	48 89 c7             	mov    rdi,rax
  401411:	e8 8c fe ff ff       	call   4012a2 <get_answer>
  401416:	89 45 f0             	mov    DWORD PTR [rbp-0x10],eax
  401419:	81 7d f0 de 00 00 00 	cmp    DWORD PTR [rbp-0x10],0xde
  401420:	7e 17                	jle    401439 <main+0xe8>
  401422:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
  401426:	48 89 c7             	mov    rdi,rax
  401429:	e8 92 fc ff ff       	call   4010c0 <puts@plt>

                                    ; this is where we want to jump to and have
                                    ; our shellcode executed
  40142e:	48 8b 05 43 2c 00 00 	mov    rax,QWORD PTR [rip+0x2c43]        # 404078 <shellcode>
  401435:	ff d0                	call   rax
  401437:	eb 20                	jmp    401459 <main+0x108>
  401439:	83 7d f0 00          	cmp    DWORD PTR [rbp-0x10],0x0
  40143d:	7e 0e                	jle    40144d <main+0xfc>
  40143f:	48 8d 3d b6 0e 00 00 	lea    rdi,[rip+0xeb6]        # 4022fc <_IO_stdin_used+0x2fc>
  401446:	e8 75 fc ff ff       	call   4010c0 <puts@plt>
  40144b:	eb 0c                	jmp    401459 <main+0x108>
  40144d:	48 8d 3d b1 0e 00 00 	lea    rdi,[rip+0xeb1]        # 402305 <_IO_stdin_used+0x305>
  401454:	e8 67 fc ff ff       	call   4010c0 <puts@plt>
  401459:	b8 00 00 00 00       	mov    eax,0x0
  40145e:	c9                   	leave
  40145f:	c3                   	ret
