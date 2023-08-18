
gotless:     file format elf64-x86-64


Disassembly of section .interp:

0000000000400318 <.interp>:
  400318:	2f                   	(bad)
  400319:	6c                   	ins    BYTE PTR es:[rdi],dx
  40031a:	69 62 36 34 2f 6c 64 	imul   esp,DWORD PTR [rdx+0x36],0x646c2f34
  400321:	2d 6c 69 6e 75       	sub    eax,0x756e696c
  400326:	78 2d                	js     400355 <_init-0xcab>
  400328:	78 38                	js     400362 <_init-0xc9e>
  40032a:	36 2d 36 34 2e 73    	ss sub eax,0x732e3436
  400330:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  400331:	2e 32 00             	cs xor al,BYTE PTR [rax]

Disassembly of section .note.gnu.property:

0000000000400338 <.note.gnu.property>:
  400338:	04 00                	add    al,0x0
  40033a:	00 00                	add    BYTE PTR [rax],al
  40033c:	10 00                	adc    BYTE PTR [rax],al
  40033e:	00 00                	add    BYTE PTR [rax],al
  400340:	05 00 00 00 47       	add    eax,0x47000000
  400345:	4e 55                	rex.WRX push rbp
  400347:	00 02                	add    BYTE PTR [rdx],al
  400349:	00 00                	add    BYTE PTR [rax],al
  40034b:	c0 04 00 00          	rol    BYTE PTR [rax+rax*1],0x0
  40034f:	00 03                	add    BYTE PTR [rbx],al
  400351:	00 00                	add    BYTE PTR [rax],al
  400353:	00 00                	add    BYTE PTR [rax],al
  400355:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .note.gnu.build-id:

0000000000400358 <.note.gnu.build-id>:
  400358:	04 00                	add    al,0x0
  40035a:	00 00                	add    BYTE PTR [rax],al
  40035c:	14 00                	adc    al,0x0
  40035e:	00 00                	add    BYTE PTR [rax],al
  400360:	03 00                	add    eax,DWORD PTR [rax]
  400362:	00 00                	add    BYTE PTR [rax],al
  400364:	47                   	rex.RXB
  400365:	4e 55                	rex.WRX push rbp
  400367:	00 d4                	add    ah,dl
  400369:	4b f4                	rex.WXB hlt
  40036b:	b9 bf 7f d4 eb       	mov    ecx,0xebd47fbf
  400370:	12 16                	adc    dl,BYTE PTR [rsi]
  400372:	2c 52                	sub    al,0x52
  400374:	cf                   	iret
  400375:	f9                   	stc
  400376:	e9 fb 97 67 c9       	jmp    ffffffffc9a79b76 <__TMC_END__+0xffffffffc9675ace>
  40037b:	c7                   	.byte 0xc7

Disassembly of section .note.ABI-tag:

000000000040037c <.note.ABI-tag>:
  40037c:	04 00                	add    al,0x0
  40037e:	00 00                	add    BYTE PTR [rax],al
  400380:	10 00                	adc    BYTE PTR [rax],al
  400382:	00 00                	add    BYTE PTR [rax],al
  400384:	01 00                	add    DWORD PTR [rax],eax
  400386:	00 00                	add    BYTE PTR [rax],al
  400388:	47                   	rex.RXB
  400389:	4e 55                	rex.WRX push rbp
  40038b:	00 00                	add    BYTE PTR [rax],al
  40038d:	00 00                	add    BYTE PTR [rax],al
  40038f:	00 03                	add    BYTE PTR [rbx],al
  400391:	00 00                	add    BYTE PTR [rax],al
  400393:	00 02                	add    BYTE PTR [rdx],al
  400395:	00 00                	add    BYTE PTR [rax],al
  400397:	00 00                	add    BYTE PTR [rax],al
  400399:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .gnu.hash:

00000000004003a0 <.gnu.hash>:
  4003a0:	01 00                	add    DWORD PTR [rax],eax
  4003a2:	00 00                	add    BYTE PTR [rax],al
  4003a4:	01 00                	add    DWORD PTR [rax],eax
  4003a6:	00 00                	add    BYTE PTR [rax],al
  4003a8:	01 00                	add    DWORD PTR [rax],eax
	...

Disassembly of section .dynsym:

00000000004003c0 <.dynsym>:
	...
  4003d8:	0b 00                	or     eax,DWORD PTR [rax]
  4003da:	00 00                	add    BYTE PTR [rax],al
  4003dc:	12 00                	adc    al,BYTE PTR [rax]
	...
  4003ee:	00 00                	add    BYTE PTR [rax],al
  4003f0:	29 00                	sub    DWORD PTR [rax],eax
  4003f2:	00 00                	add    BYTE PTR [rax],al
  4003f4:	20 00                	and    BYTE PTR [rax],al
	...

Disassembly of section .dynstr:

0000000000400408 <.dynstr>:
  400408:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
  40040c:	63 2e                	movsxd ebp,DWORD PTR [rsi]
  40040e:	73 6f                	jae    40047f <_init-0xb81>
  400410:	2e 36 00 5f 5f       	cs ss add BYTE PTR [rdi+0x5f],bl
  400415:	6c                   	ins    BYTE PTR es:[rdi],dx
  400416:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
  40041d:	72 74                	jb     400493 <_init-0xb6d>
  40041f:	5f                   	pop    rdi
  400420:	6d                   	ins    DWORD PTR es:[rdi],dx
  400421:	61                   	(bad)
  400422:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
  400429:	43 5f                	rex.XB pop r15
  40042b:	32 2e                	xor    ch,BYTE PTR [rsi]
  40042d:	32 2e                	xor    ch,BYTE PTR [rsi]
  40042f:	35 00 5f 5f 67       	xor    eax,0x675f5f00
  400434:	6d                   	ins    DWORD PTR es:[rdi],dx
  400435:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  400436:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  400437:	5f                   	pop    rdi
  400438:	73 74                	jae    4004ae <_init-0xb52>
  40043a:	61                   	(bad)
  40043b:	72 74                	jb     4004b1 <_init-0xb4f>
  40043d:	5f                   	pop    rdi
  40043e:	5f                   	pop    rdi
	...

Disassembly of section .gnu.version:

0000000000400440 <.gnu.version>:
  400440:	00 00                	add    BYTE PTR [rax],al
  400442:	02 00                	add    al,BYTE PTR [rax]
	...

Disassembly of section .gnu.version_r:

0000000000400448 <.gnu.version_r>:
  400448:	01 00                	add    DWORD PTR [rax],eax
  40044a:	01 00                	add    DWORD PTR [rax],eax
  40044c:	01 00                	add    DWORD PTR [rax],eax
  40044e:	00 00                	add    BYTE PTR [rax],al
  400450:	10 00                	adc    BYTE PTR [rax],al
  400452:	00 00                	add    BYTE PTR [rax],al
  400454:	00 00                	add    BYTE PTR [rax],al
  400456:	00 00                	add    BYTE PTR [rax],al
  400458:	75 1a                	jne    400474 <_init-0xb8c>
  40045a:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
  400460:	1d 00 00 00 00       	sbb    eax,0x0
  400465:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000400468 <.rela.dyn>:
  400468:	f0 3f                	lock (bad)
  40046a:	40 00 00             	rex add BYTE PTR [rax],al
  40046d:	00 00                	add    BYTE PTR [rax],al
  40046f:	00 06                	add    BYTE PTR [rsi],al
  400471:	00 00                	add    BYTE PTR [rax],al
  400473:	00 01                	add    BYTE PTR [rcx],al
	...
  40047d:	00 00                	add    BYTE PTR [rax],al
  40047f:	00 f8                	add    al,bh
  400481:	3f                   	(bad)
  400482:	40 00 00             	rex add BYTE PTR [rax],al
  400485:	00 00                	add    BYTE PTR [rax],al
  400487:	00 06                	add    BYTE PTR [rsi],al
  400489:	00 00                	add    BYTE PTR [rax],al
  40048b:	00 02                	add    BYTE PTR [rdx],al
	...

Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    rsp,0x8
  401008:	48 8b 05 e9 2f 00 00 	mov    rax,QWORD PTR [rip+0x2fe9]        # 403ff8 <__gmon_start__>
  40100f:	48 85 c0             	test   rax,rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   rax
  401016:	48 83 c4 08          	add    rsp,0x8
  40101a:	c3                   	ret

Disassembly of section .text:

0000000000401020 <_start>:
  401020:	f3 0f 1e fa          	endbr64
  401024:	31 ed                	xor    ebp,ebp
  401026:	49 89 d1             	mov    r9,rdx
  401029:	5e                   	pop    rsi
  40102a:	48 89 e2             	mov    rdx,rsp
  40102d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
  401031:	50                   	push   rax
  401032:	54                   	push   rsp
  401033:	49 c7 c0 80 12 40 00 	mov    r8,0x401280
  40103a:	48 c7 c1 10 12 40 00 	mov    rcx,0x401210
  401041:	48 c7 c7 00 12 40 00 	mov    rdi,0x401200
  401048:	ff 15 a2 2f 00 00    	call   QWORD PTR [rip+0x2fa2]        # 403ff0 <__libc_start_main@GLIBC_2.2.5>
  40104e:	f4                   	hlt
  40104f:	90                   	nop

0000000000401050 <_dl_relocate_static_pie>:
  401050:	f3 0f 1e fa          	endbr64
  401054:	c3                   	ret
  401055:	66 2e 0f 1f 84 00 00 	cs nop WORD PTR [rax+rax*1+0x0]
  40105c:	00 00 00 
  40105f:	90                   	nop

0000000000401060 <deregister_tm_clones>:
  401060:	b8 a8 40 40 00       	mov    eax,0x4040a8
  401065:	48 3d a8 40 40 00    	cmp    rax,0x4040a8
  40106b:	74 13                	je     401080 <deregister_tm_clones+0x20>
  40106d:	b8 00 00 00 00       	mov    eax,0x0
  401072:	48 85 c0             	test   rax,rax
  401075:	74 09                	je     401080 <deregister_tm_clones+0x20>
  401077:	bf a8 40 40 00       	mov    edi,0x4040a8
  40107c:	ff e0                	jmp    rax
  40107e:	66 90                	xchg   ax,ax
  401080:	c3                   	ret
  401081:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  401088:	00 00 00 00 
  40108c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401090 <register_tm_clones>:
  401090:	be a8 40 40 00       	mov    esi,0x4040a8
  401095:	48 81 ee a8 40 40 00 	sub    rsi,0x4040a8
  40109c:	48 89 f0             	mov    rax,rsi
  40109f:	48 c1 ee 3f          	shr    rsi,0x3f
  4010a3:	48 c1 f8 03          	sar    rax,0x3
  4010a7:	48 01 c6             	add    rsi,rax
  4010aa:	48 d1 fe             	sar    rsi,1
  4010ad:	74 11                	je     4010c0 <register_tm_clones+0x30>
  4010af:	b8 00 00 00 00       	mov    eax,0x0
  4010b4:	48 85 c0             	test   rax,rax
  4010b7:	74 07                	je     4010c0 <register_tm_clones+0x30>
  4010b9:	bf a8 40 40 00       	mov    edi,0x4040a8
  4010be:	ff e0                	jmp    rax
  4010c0:	c3                   	ret
  4010c1:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  4010c8:	00 00 00 00 
  4010cc:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

00000000004010d0 <__do_global_dtors_aux>:
  4010d0:	f3 0f 1e fa          	endbr64
  4010d4:	80 3d cc 2f 00 00 00 	cmp    BYTE PTR [rip+0x2fcc],0x0        # 4040a7 <completed.8061>
  4010db:	75 13                	jne    4010f0 <__do_global_dtors_aux+0x20>
  4010dd:	55                   	push   rbp
  4010de:	48 89 e5             	mov    rbp,rsp
  4010e1:	e8 7a ff ff ff       	call   401060 <deregister_tm_clones>
  4010e6:	c6 05 ba 2f 00 00 01 	mov    BYTE PTR [rip+0x2fba],0x1        # 4040a7 <completed.8061>
  4010ed:	5d                   	pop    rbp
  4010ee:	c3                   	ret
  4010ef:	90                   	nop
  4010f0:	c3                   	ret
  4010f1:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  4010f8:	00 00 00 00 
  4010fc:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401100 <frame_dummy>:
  401100:	f3 0f 1e fa          	endbr64
  401104:	eb 8a                	jmp    401090 <register_tm_clones>

0000000000401106 <advance>:
  401106:	f3 0f 1e fa          	endbr64
  40110a:	48 01 37             	add    QWORD PTR [rdi],rsi
  40110d:	c3                   	ret

000000000040110e <magic>:
  40110e:	f3 0f 1e fa          	endbr64
  401112:	b8 48 83 ec 40       	mov    eax,0x40ec8348
  401117:	c3                   	ret

0000000000401118 <fib>:
  401118:	f3 0f 1e fa          	endbr64
  40111c:	48 c7 44 24 c0 00 00 	mov    QWORD PTR [rsp-0x40],0x0
  401123:	00 00 
  401125:	48 c7 44 24 c8 01 00 	mov    QWORD PTR [rsp-0x38],0x1
  40112c:	00 00 
  40112e:	48 8d 44 24 c0       	lea    rax,[rsp-0x40]
  401133:	48 8d 4c 24 f0       	lea    rcx,[rsp-0x10]
  401138:	48 8b 50 08          	mov    rdx,QWORD PTR [rax+0x8]
  40113c:	48 03 10             	add    rdx,QWORD PTR [rax]
  40113f:	48 89 50 10          	mov    QWORD PTR [rax+0x10],rdx
  401143:	48 83 c0 08          	add    rax,0x8
  401147:	48 39 c8             	cmp    rax,rcx
  40114a:	75 ec                	jne    401138 <fib+0x20>
  40114c:	48 8b 44 fc c0       	mov    rax,QWORD PTR [rsp+rdi*8-0x40]
  401151:	c3                   	ret

0000000000401152 <hash>:
  ; foo hash(char *s) {
  ;   char buf[256];
  ;   int res;
  ;   int c = (int) *s;
  ;   if (c == '\0') {
  ;     res = magic() & *buf;
  ;   }
  ;   else {
  ;
  ;   }
  ; }
  401152:	f3 0f 1e fa          	endbr64
  401156:	41 55                	push   r13
  401158:	41 54                	push   r12
  40115a:	55                   	push   rbp
  40115b:	53                   	push   rbx

  ; char buf[256];
  ; int c = (int) s[0];
  ; if (c == '\0')
  ;   ...
  40115c:	48 81 ec 00 01 00 00 	sub    rsp,0x100
  401163:	0f b6 07             	movzx  eax,BYTE PTR [rdi]
  401166:	84 c0                	test   al,al
  401168:	74 7a                	je     4011e4 <hash+0x92>


  ; int i = 0;
  ; while (s[2 * i] != '\0') {
  ;   int tmp = (int) s[2*i + 1];
  ;   buf[i] = (tmp << 4) | (c & 0xf);
  ;   i++;
  ; } while (*(s + 2 * i) != '\0');
  40116a:	bb 00 00 00 00       	mov    ebx,0x0
  40116f:	0f be 54 5f 01       	movsx  edx,BYTE PTR [rdi+rbx*2+0x1]
  401174:	c1 e2 04             	shl    edx,0x4
  401177:	83 e0 0f             	and    eax,0xf
  40117a:	09 d0                	or     eax,edx
  40117c:	88 04 1c             	mov    BYTE PTR [rsp+rbx*1],al
  40117f:	48 83 c3 01          	add    rbx,0x1
  401183:	0f b6 04 5f          	movzx  eax,BYTE PTR [rdi+rbx*2]
  401187:	84 c0                	test   al,al
  401189:	75 e4                	jne    40116f <hash+0x1d>

  40118b:	e8 7e ff ff ff       	call   40110e <magic>
  401190:	23 04 24             	and    eax,DWORD PTR [rsp]
  401193:	41 89 c5             	mov    r13d,eax
  401196:	48 85 db             	test   rbx,rbx
  401199:	74 33                	je     4011ce <hash+0x7c>
  40119b:	49 89 e4             	mov    r12,rsp
  40119e:	4c 01 e3             	add    rbx,r12
  4011a1:	41 0f b6 2c 24       	movzx  ebp,BYTE PTR [r12]
  4011a6:	48 89 ef             	mov    rdi,rbp
  4011a9:	83 e7 07             	and    edi,0x7
  4011ac:	e8 67 ff ff ff       	call   401118 <fib>
  4011b1:	48 89 c2             	mov    rdx,rax
  4011b4:	83 e5 3f             	and    ebp,0x3f
  4011b7:	48 0f be 85 40 40 40 	movsx  rax,BYTE PTR [rbp+0x404040]
  4011be:	00 
  4011bf:	48 09 d0             	or     rax,rdx
  4011c2:	41 31 c5             	xor    r13d,eax
  4011c5:	49 83 c4 01          	add    r12,0x1
  4011c9:	4c 39 e3             	cmp    rbx,r12
  4011cc:	75 d3                	jne    4011a1 <hash+0x4f>

  4011ce:	44 89 e8             	mov    eax,r13d
  4011d1:	0d 48 ab c3 cc       	or     eax,0xccc3ab48
  4011d6:	48 81 c4 00 01 00 00 	add    rsp,0x100
  4011dd:	5b                   	pop    rbx
  4011de:	5d                   	pop    rbp
  4011df:	41 5c                	pop    r12
  4011e1:	41 5d                	pop    r13
  4011e3:	c3                   	ret

  4011e4:	b8 00 00 00 00       	mov    eax,0x0
  4011e9:	e8 20 ff ff ff       	call   40110e <magic>
  4011ee:	23 04 24             	and    eax,DWORD PTR [rsp]
  4011f1:	41 89 c5             	mov    r13d,eax
  4011f4:	eb d8                	jmp    4011ce <hash+0x7c>

00000000004011f6 <hash_wrapper>:
  4011f6:	f3 0f 1e fa          	endbr64
  4011fa:	e8 53 ff ff ff       	call   401152 <hash>
  4011ff:	c3                   	ret

0000000000401200 <main>:
  401200:	f3 0f 1e fa          	endbr64
  401204:	48 8b 7e 08          	mov    rdi,QWORD PTR [rsi+0x8]
  401208:	e8 e9 ff ff ff       	call   4011f6 <hash_wrapper>
  40120d:	c3                   	ret
  40120e:	66 90                	xchg   ax,ax

0000000000401210 <__libc_csu_init>:
  401210:	f3 0f 1e fa          	endbr64
  401214:	41 57                	push   r15
  401216:	4c 8d 3d 33 2c 00 00 	lea    r15,[rip+0x2c33]        # 403e50 <__frame_dummy_init_array_entry>
  40121d:	41 56                	push   r14
  40121f:	49 89 d6             	mov    r14,rdx
  401222:	41 55                	push   r13
  401224:	49 89 f5             	mov    r13,rsi
  401227:	41 54                	push   r12
  401229:	41 89 fc             	mov    r12d,edi
  40122c:	55                   	push   rbp
  40122d:	48 8d 2d 24 2c 00 00 	lea    rbp,[rip+0x2c24]        # 403e58 <__do_global_dtors_aux_fini_array_entry>
  401234:	53                   	push   rbx
  401235:	4c 29 fd             	sub    rbp,r15
  401238:	48 83 ec 08          	sub    rsp,0x8
  40123c:	e8 bf fd ff ff       	call   401000 <_init>
  401241:	48 c1 fd 03          	sar    rbp,0x3
  401245:	74 1f                	je     401266 <__libc_csu_init+0x56>
  401247:	31 db                	xor    ebx,ebx
  401249:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
  401250:	4c 89 f2             	mov    rdx,r14
  401253:	4c 89 ee             	mov    rsi,r13
  401256:	44 89 e7             	mov    edi,r12d
  401259:	41 ff 14 df          	call   QWORD PTR [r15+rbx*8]
  40125d:	48 83 c3 01          	add    rbx,0x1
  401261:	48 39 dd             	cmp    rbp,rbx
  401264:	75 ea                	jne    401250 <__libc_csu_init+0x40>
  401266:	48 83 c4 08          	add    rsp,0x8
  40126a:	5b                   	pop    rbx
  40126b:	5d                   	pop    rbp
  40126c:	41 5c                	pop    r12
  40126e:	41 5d                	pop    r13
  401270:	41 5e                	pop    r14
  401272:	41 5f                	pop    r15
  401274:	c3                   	ret
  401275:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  40127c:	00 00 00 00 

0000000000401280 <__libc_csu_fini>:
  401280:	f3 0f 1e fa          	endbr64
  401284:	c3                   	ret

Disassembly of section .fini:

0000000000401288 <_fini>:
  401288:	f3 0f 1e fa          	endbr64
  40128c:	48 83 ec 08          	sub    rsp,0x8
  401290:	48 83 c4 08          	add    rsp,0x8
  401294:	c3                   	ret

Disassembly of section .rodata:

0000000000402000 <_IO_stdin_used>:
  402000:	01 00                	add    DWORD PTR [rax],eax
  402002:	02 00                	add    al,BYTE PTR [rax]

Disassembly of section .eh_frame_hdr:

0000000000402004 <__GNU_EH_FRAME_HDR>:
  402004:	01 1b                	add    DWORD PTR [rbx],ebx
  402006:	03 3b                	add    edi,DWORD PTR [rbx]
  402008:	58                   	pop    rax
  402009:	00 00                	add    BYTE PTR [rax],al
  40200b:	00 0a                	add    BYTE PTR [rdx],cl
  40200d:	00 00                	add    BYTE PTR [rax],al
  40200f:	00 1c f0             	add    BYTE PTR [rax+rsi*8],bl
  402012:	ff                   	(bad)
  402013:	ff 74 00 00          	push   QWORD PTR [rax+rax*1+0x0]
  402017:	00 4c f0 ff          	add    BYTE PTR [rax+rsi*8-0x1],cl
  40201b:	ff 88 00 00 00 02    	dec    DWORD PTR [rax+0x2000000]
  402021:	f1                   	int1
  402022:	ff                   	(bad)
  402023:	ff 9c 00 00 00 0a f1 	call   FWORD PTR [rax+rax*1-0xef60000]
  40202a:	ff                   	(bad)
  40202b:	ff b0 00 00 00 14    	push   QWORD PTR [rax+0x14000000]
  402031:	f1                   	int1
  402032:	ff                   	(bad)
  402033:	ff c4                	inc    esp
  402035:	00 00                	add    BYTE PTR [rax],al
  402037:	00 4e f1             	add    BYTE PTR [rsi-0xf],cl
  40203a:	ff                   	(bad)
  40203b:	ff                   	(bad)
  40203c:	d8 00                	fadd   DWORD PTR [rax]
  40203e:	00 00                	add    BYTE PTR [rax],al
  402040:	f2 f1                	repnz int1
  402042:	ff                   	(bad)
  402043:	ff 14 01             	call   QWORD PTR [rcx+rax*1]
  402046:	00 00                	add    BYTE PTR [rax],al
  402048:	fc                   	cld
  402049:	f1                   	int1
  40204a:	ff                   	(bad)
  40204b:	ff 28                	jmp    FWORD PTR [rax]
  40204d:	01 00                	add    DWORD PTR [rax],eax
  40204f:	00 0c f2             	add    BYTE PTR [rdx+rsi*8],cl
  402052:	ff                   	(bad)
  402053:	ff                   	(bad)
  402054:	3c 01                	cmp    al,0x1
  402056:	00 00                	add    BYTE PTR [rax],al
  402058:	7c f2                	jl     40204c <__GNU_EH_FRAME_HDR+0x48>
  40205a:	ff                   	(bad)
  40205b:	ff                   	.byte 0xff
  40205c:	84 01                	test   BYTE PTR [rcx],al
	...

Disassembly of section .eh_frame:

0000000000402060 <__FRAME_END__-0x13c>:
  402060:	14 00                	adc    al,0x0
  402062:	00 00                	add    BYTE PTR [rax],al
  402064:	00 00                	add    BYTE PTR [rax],al
  402066:	00 00                	add    BYTE PTR [rax],al
  402068:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
  40206b:	00 01                	add    BYTE PTR [rcx],al
  40206d:	78 10                	js     40207f <__GNU_EH_FRAME_HDR+0x7b>
  40206f:	01 1b                	add    DWORD PTR [rbx],ebx
  402071:	0c 07                	or     al,0x7
  402073:	08 90 01 00 00 10    	or     BYTE PTR [rax+0x10000001],dl
  402079:	00 00                	add    BYTE PTR [rax],al
  40207b:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  40207e:	00 00                	add    BYTE PTR [rax],al
  402080:	a0 ef ff ff 2f 00 00 	movabs al,ds:0x2fffffef
  402087:	00 00 
  402089:	44 07                	rex.R (bad)
  40208b:	10 10                	adc    BYTE PTR [rax],dl
  40208d:	00 00                	add    BYTE PTR [rax],al
  40208f:	00 30                	add    BYTE PTR [rax],dh
  402091:	00 00                	add    BYTE PTR [rax],al
  402093:	00 bc ef ff ff 05 00 	add    BYTE PTR [rdi+rbp*8+0x5ffff],bh
  40209a:	00 00                	add    BYTE PTR [rax],al
  40209c:	00 00                	add    BYTE PTR [rax],al
  40209e:	00 00                	add    BYTE PTR [rax],al
  4020a0:	10 00                	adc    BYTE PTR [rax],al
  4020a2:	00 00                	add    BYTE PTR [rax],al
  4020a4:	44 00 00             	add    BYTE PTR [rax],r8b
  4020a7:	00 5e f0             	add    BYTE PTR [rsi-0x10],bl
  4020aa:	ff                   	(bad)
  4020ab:	ff 08                	dec    DWORD PTR [rax]
  4020ad:	00 00                	add    BYTE PTR [rax],al
  4020af:	00 00                	add    BYTE PTR [rax],al
  4020b1:	00 00                	add    BYTE PTR [rax],al
  4020b3:	00 10                	add    BYTE PTR [rax],dl
  4020b5:	00 00                	add    BYTE PTR [rax],al
  4020b7:	00 58 00             	add    BYTE PTR [rax+0x0],bl
  4020ba:	00 00                	add    BYTE PTR [rax],al
  4020bc:	52                   	push   rdx
  4020bd:	f0 ff                	lock (bad)
  4020bf:	ff 0a                	dec    DWORD PTR [rdx]
  4020c1:	00 00                	add    BYTE PTR [rax],al
  4020c3:	00 00                	add    BYTE PTR [rax],al
  4020c5:	00 00                	add    BYTE PTR [rax],al
  4020c7:	00 10                	add    BYTE PTR [rax],dl
  4020c9:	00 00                	add    BYTE PTR [rax],al
  4020cb:	00 6c 00 00          	add    BYTE PTR [rax+rax*1+0x0],ch
  4020cf:	00 48 f0             	add    BYTE PTR [rax-0x10],cl
  4020d2:	ff                   	(bad)
  4020d3:	ff                   	(bad)
  4020d4:	3a 00                	cmp    al,BYTE PTR [rax]
  4020d6:	00 00                	add    BYTE PTR [rax],al
  4020d8:	00 00                	add    BYTE PTR [rax],al
  4020da:	00 00                	add    BYTE PTR [rax],al
  4020dc:	38 00                	cmp    BYTE PTR [rax],al
  4020de:	00 00                	add    BYTE PTR [rax],al
  4020e0:	80 00 00             	add    BYTE PTR [rax],0x0
  4020e3:	00 6e f0             	add    BYTE PTR [rsi-0x10],ch
  4020e6:	ff                   	(bad)
  4020e7:	ff a4 00 00 00 00 46 	jmp    QWORD PTR [rax+rax*1+0x46000000]
  4020ee:	0e                   	(bad)
  4020ef:	10 8d 02 42 0e 18    	adc    BYTE PTR [rbp+0x180e4202],cl
  4020f5:	8c 03                	mov    WORD PTR [rbx],es
  4020f7:	41 0e                	rex.B (bad)
  4020f9:	20 86 04 41 0e 28    	and    BYTE PTR [rsi+0x280e4104],al
  4020ff:	83 05 47 0e a8 02 02 	add    DWORD PTR [rip+0x2a80e47],0x2        # 2e82f4d <__TMC_END__+0x2a7eea5>
  402106:	7a 0a                	jp     402112 <__GNU_EH_FRAME_HDR+0x10e>
  402108:	0e                   	(bad)
  402109:	28 41 0e             	sub    BYTE PTR [rcx+0xe],al
  40210c:	20 41 0e             	and    BYTE PTR [rcx+0xe],al
  40210f:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
  402112:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
  402115:	08 41 0b             	or     BYTE PTR [rcx+0xb],al
  402118:	10 00                	adc    BYTE PTR [rax],al
  40211a:	00 00                	add    BYTE PTR [rax],al
  40211c:	bc 00 00 00 d6       	mov    esp,0xd6000000
  402121:	f0 ff                	lock (bad)
  402123:	ff 0a                	dec    DWORD PTR [rdx]
  402125:	00 00                	add    BYTE PTR [rax],al
  402127:	00 00                	add    BYTE PTR [rax],al
  402129:	00 00                	add    BYTE PTR [rax],al
  40212b:	00 10                	add    BYTE PTR [rax],dl
  40212d:	00 00                	add    BYTE PTR [rax],al
  40212f:	00 d0                	add    al,dl
  402131:	00 00                	add    BYTE PTR [rax],al
  402133:	00 cc                	add    ah,cl
  402135:	f0 ff                	lock (bad)
  402137:	ff 0e                	dec    DWORD PTR [rsi]
  402139:	00 00                	add    BYTE PTR [rax],al
  40213b:	00 00                	add    BYTE PTR [rax],al
  40213d:	00 00                	add    BYTE PTR [rax],al
  40213f:	00 44 00 00          	add    BYTE PTR [rax+rax*1+0x0],al
  402143:	00 e4                	add    ah,ah
  402145:	00 00                	add    BYTE PTR [rax],al
  402147:	00 c8                	add    al,cl
  402149:	f0 ff                	lock (bad)
  40214b:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
  40214e:	00 00                	add    BYTE PTR [rax],al
  402150:	00 46 0e             	add    BYTE PTR [rsi+0xe],al
  402153:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
  402159:	8e 03                	mov    es,WORD PTR [rbx]
  40215b:	45 0e                	rex.RB (bad)
  40215d:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
  402163:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff86702fad <__TMC_END__+0xffffffff862fef05>
  402169:	06                   	(bad)
  40216a:	48 0e                	rex.W (bad)
  40216c:	38 83 07 47 0e 40    	cmp    BYTE PTR [rbx+0x400e4707],al
  402172:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  402173:	0e                   	(bad)
  402174:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
  402177:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
  40217a:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
  40217d:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
  402180:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
  402183:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
  402186:	08 00                	or     BYTE PTR [rax],al
  402188:	10 00                	adc    BYTE PTR [rax],al
  40218a:	00 00                	add    BYTE PTR [rax],al
  40218c:	2c 01                	sub    al,0x1
  40218e:	00 00                	add    BYTE PTR [rax],al
  402190:	f0 f0 ff             	lock lock (bad)
  402193:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 402199 <__GNU_EH_FRAME_HDR+0x195>
  402199:	00 00                	add    BYTE PTR [rax],al
	...

000000000040219c <__FRAME_END__>:
  40219c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000403e50 <__frame_dummy_init_array_entry>:
  403e50:	00 11                	add    BYTE PTR [rcx],dl
  403e52:	40 00 00             	rex add BYTE PTR [rax],al
  403e55:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000403e58 <__do_global_dtors_aux_fini_array_entry>:
  403e58:	d0 10                	rcl    BYTE PTR [rax],1
  403e5a:	40 00 00             	rex add BYTE PTR [rax],al
  403e5d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000403e60 <_DYNAMIC>:
  403e60:	01 00                	add    DWORD PTR [rax],eax
  403e62:	00 00                	add    BYTE PTR [rax],al
  403e64:	00 00                	add    BYTE PTR [rax],al
  403e66:	00 00                	add    BYTE PTR [rax],al
  403e68:	01 00                	add    DWORD PTR [rax],eax
  403e6a:	00 00                	add    BYTE PTR [rax],al
  403e6c:	00 00                	add    BYTE PTR [rax],al
  403e6e:	00 00                	add    BYTE PTR [rax],al
  403e70:	0c 00                	or     al,0x0
  403e72:	00 00                	add    BYTE PTR [rax],al
  403e74:	00 00                	add    BYTE PTR [rax],al
  403e76:	00 00                	add    BYTE PTR [rax],al
  403e78:	00 10                	add    BYTE PTR [rax],dl
  403e7a:	40 00 00             	rex add BYTE PTR [rax],al
  403e7d:	00 00                	add    BYTE PTR [rax],al
  403e7f:	00 0d 00 00 00 00    	add    BYTE PTR [rip+0x0],cl        # 403e85 <_DYNAMIC+0x25>
  403e85:	00 00                	add    BYTE PTR [rax],al
  403e87:	00 88 12 40 00 00    	add    BYTE PTR [rax+0x4012],cl
  403e8d:	00 00                	add    BYTE PTR [rax],al
  403e8f:	00 19                	add    BYTE PTR [rcx],bl
  403e91:	00 00                	add    BYTE PTR [rax],al
  403e93:	00 00                	add    BYTE PTR [rax],al
  403e95:	00 00                	add    BYTE PTR [rax],al
  403e97:	00 50 3e             	add    BYTE PTR [rax+0x3e],dl
  403e9a:	40 00 00             	rex add BYTE PTR [rax],al
  403e9d:	00 00                	add    BYTE PTR [rax],al
  403e9f:	00 1b                	add    BYTE PTR [rbx],bl
  403ea1:	00 00                	add    BYTE PTR [rax],al
  403ea3:	00 00                	add    BYTE PTR [rax],al
  403ea5:	00 00                	add    BYTE PTR [rax],al
  403ea7:	00 08                	add    BYTE PTR [rax],cl
  403ea9:	00 00                	add    BYTE PTR [rax],al
  403eab:	00 00                	add    BYTE PTR [rax],al
  403ead:	00 00                	add    BYTE PTR [rax],al
  403eaf:	00 1a                	add    BYTE PTR [rdx],bl
  403eb1:	00 00                	add    BYTE PTR [rax],al
  403eb3:	00 00                	add    BYTE PTR [rax],al
  403eb5:	00 00                	add    BYTE PTR [rax],al
  403eb7:	00 58 3e             	add    BYTE PTR [rax+0x3e],bl
  403eba:	40 00 00             	rex add BYTE PTR [rax],al
  403ebd:	00 00                	add    BYTE PTR [rax],al
  403ebf:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  403ec2:	00 00                	add    BYTE PTR [rax],al
  403ec4:	00 00                	add    BYTE PTR [rax],al
  403ec6:	00 00                	add    BYTE PTR [rax],al
  403ec8:	08 00                	or     BYTE PTR [rax],al
  403eca:	00 00                	add    BYTE PTR [rax],al
  403ecc:	00 00                	add    BYTE PTR [rax],al
  403ece:	00 00                	add    BYTE PTR [rax],al
  403ed0:	f5                   	cmc
  403ed1:	fe                   	(bad)
  403ed2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  403ed5:	00 00                	add    BYTE PTR [rax],al
  403ed7:	00 a0 03 40 00 00    	add    BYTE PTR [rax+0x4003],ah
  403edd:	00 00                	add    BYTE PTR [rax],al
  403edf:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 403ee5 <_DYNAMIC+0x85>
  403ee5:	00 00                	add    BYTE PTR [rax],al
  403ee7:	00 08                	add    BYTE PTR [rax],cl
  403ee9:	04 40                	add    al,0x40
  403eeb:	00 00                	add    BYTE PTR [rax],al
  403eed:	00 00                	add    BYTE PTR [rax],al
  403eef:	00 06                	add    BYTE PTR [rsi],al
  403ef1:	00 00                	add    BYTE PTR [rax],al
  403ef3:	00 00                	add    BYTE PTR [rax],al
  403ef5:	00 00                	add    BYTE PTR [rax],al
  403ef7:	00 c0                	add    al,al
  403ef9:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
  403efc:	00 00                	add    BYTE PTR [rax],al
  403efe:	00 00                	add    BYTE PTR [rax],al
  403f00:	0a 00                	or     al,BYTE PTR [rax]
  403f02:	00 00                	add    BYTE PTR [rax],al
  403f04:	00 00                	add    BYTE PTR [rax],al
  403f06:	00 00                	add    BYTE PTR [rax],al
  403f08:	38 00                	cmp    BYTE PTR [rax],al
  403f0a:	00 00                	add    BYTE PTR [rax],al
  403f0c:	00 00                	add    BYTE PTR [rax],al
  403f0e:	00 00                	add    BYTE PTR [rax],al
  403f10:	0b 00                	or     eax,DWORD PTR [rax]
  403f12:	00 00                	add    BYTE PTR [rax],al
  403f14:	00 00                	add    BYTE PTR [rax],al
  403f16:	00 00                	add    BYTE PTR [rax],al
  403f18:	18 00                	sbb    BYTE PTR [rax],al
  403f1a:	00 00                	add    BYTE PTR [rax],al
  403f1c:	00 00                	add    BYTE PTR [rax],al
  403f1e:	00 00                	add    BYTE PTR [rax],al
  403f20:	15 00 00 00 00       	adc    eax,0x0
	...
  403f2d:	00 00                	add    BYTE PTR [rax],al
  403f2f:	00 07                	add    BYTE PTR [rdi],al
  403f31:	00 00                	add    BYTE PTR [rax],al
  403f33:	00 00                	add    BYTE PTR [rax],al
  403f35:	00 00                	add    BYTE PTR [rax],al
  403f37:	00 68 04             	add    BYTE PTR [rax+0x4],ch
  403f3a:	40 00 00             	rex add BYTE PTR [rax],al
  403f3d:	00 00                	add    BYTE PTR [rax],al
  403f3f:	00 08                	add    BYTE PTR [rax],cl
  403f41:	00 00                	add    BYTE PTR [rax],al
  403f43:	00 00                	add    BYTE PTR [rax],al
  403f45:	00 00                	add    BYTE PTR [rax],al
  403f47:	00 30                	add    BYTE PTR [rax],dh
  403f49:	00 00                	add    BYTE PTR [rax],al
  403f4b:	00 00                	add    BYTE PTR [rax],al
  403f4d:	00 00                	add    BYTE PTR [rax],al
  403f4f:	00 09                	add    BYTE PTR [rcx],cl
  403f51:	00 00                	add    BYTE PTR [rax],al
  403f53:	00 00                	add    BYTE PTR [rax],al
  403f55:	00 00                	add    BYTE PTR [rax],al
  403f57:	00 18                	add    BYTE PTR [rax],bl
  403f59:	00 00                	add    BYTE PTR [rax],al
  403f5b:	00 00                	add    BYTE PTR [rax],al
  403f5d:	00 00                	add    BYTE PTR [rax],al
  403f5f:	00 fe                	add    dh,bh
  403f61:	ff                   	(bad)
  403f62:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  403f65:	00 00                	add    BYTE PTR [rax],al
  403f67:	00 48 04             	add    BYTE PTR [rax+0x4],cl
  403f6a:	40 00 00             	rex add BYTE PTR [rax],al
  403f6d:	00 00                	add    BYTE PTR [rax],al
  403f6f:	00 ff                	add    bh,bh
  403f71:	ff                   	(bad)
  403f72:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  403f75:	00 00                	add    BYTE PTR [rax],al
  403f77:	00 01                	add    BYTE PTR [rcx],al
  403f79:	00 00                	add    BYTE PTR [rax],al
  403f7b:	00 00                	add    BYTE PTR [rax],al
  403f7d:	00 00                	add    BYTE PTR [rax],al
  403f7f:	00 f0                	add    al,dh
  403f81:	ff                   	(bad)
  403f82:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  403f85:	00 00                	add    BYTE PTR [rax],al
  403f87:	00 40 04             	add    BYTE PTR [rax+0x4],al
  403f8a:	40 00 00             	rex add BYTE PTR [rax],al
	...

Disassembly of section .got:

0000000000403ff0 <.got>:
	...

Disassembly of section .got.plt:

0000000000404000 <_GLOBAL_OFFSET_TABLE_>:
  404000:	60                   	(bad)
  404001:	3e 40 00 00          	ds rex add BYTE PTR [rax],al
	...

Disassembly of section .data:

0000000000404020 <__data_start>:
	...

0000000000404028 <__dso_handle>:
	...

0000000000404040 <salt>:
  404040:	49 6e                	rex.WB outs dx,BYTE PTR ds:[rsi]
  404042:	20 74 68 65          	and    BYTE PTR [rax+rbp*2+0x65],dh
  404046:	20 62 65             	and    BYTE PTR [rdx+0x65],ah
  404049:	67 69 6e 6e 69 6e 67 	imul   ebp,DWORD PTR [esi+0x6e],0x20676e69
  404050:	20 
  404051:	77 61                	ja     4040b4 <__TMC_END__+0xc>
  404053:	73 20                	jae    404075 <salt+0x35>
  404055:	74 68                	je     4040bf <__TMC_END__+0x17>
  404057:	65 20 52 4f          	and    BYTE PTR gs:[rdx+0x4f],dl
  40405b:	50                   	push   rax
  40405c:	2c 20                	sub    al,0x20
  40405e:	61                   	(bad)
  40405f:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  404060:	64 20 74 68 65       	and    BYTE PTR fs:[rax+rbp*2+0x65],dh
  404065:	20 52 4f             	and    BYTE PTR [rdx+0x4f],dl
  404068:	50                   	push   rax
  404069:	20 77 61             	and    BYTE PTR [rdi+0x61],dh
  40406c:	73 20                	jae    40408e <salt+0x4e>
  40406e:	77 69                	ja     4040d9 <__TMC_END__+0x31>
  404070:	74 68                	je     4040da <__TMC_END__+0x32>
  404072:	20 47 4f             	and    BYTE PTR [rdi+0x4f],al
  404075:	54                   	push   rsp
  404076:	2c 20                	sub    al,0x20
  404078:	61                   	(bad)
  404079:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  40407a:	64 20 74 68 65       	and    BYTE PTR fs:[rax+rbp*2+0x65],dh
  40407f:	20 52 4f             	and    BYTE PTR [rdx+0x4f],dl
  404082:	50                   	push   rax
  404083:	20 77 61             	and    BYTE PTR [rdi+0x61],dh
  404086:	73 20                	jae    4040a8 <__TMC_END__>
  404088:	47                   	rex.RXB
  404089:	4f 54                	rex.WRXB push r12
  40408b:	2e 20 47 6f          	cs and BYTE PTR [rdi+0x6f],al
  40408f:	20 66 6f             	and    BYTE PTR [rsi+0x6f],ah
  404092:	72 74                	jb     404108 <__TMC_END__+0x60>
  404094:	68 20 61 6e 64       	push   0x646e6120
  404099:	20 63 61             	and    BYTE PTR [rbx+0x61],ah
  40409c:	74 20                	je     4040be <__TMC_END__+0x16>
  40409e:	66 6c                	data16 ins BYTE PTR es:[rdi],dx
  4040a0:	61                   	(bad)
  4040a1:	67 2e 74 78          	addr32 cs je 40411d <__TMC_END__+0x75>
  4040a5:	74 00                	je     4040a7 <_edata>

Disassembly of section .bss:

00000000004040a7 <completed.8061>:
	...

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	47                   	rex.RXB
   1:	43                   	rex.XB
   2:	43 3a 20             	rex.XB cmp spl,BYTE PTR [r8]
   5:	28 55 62             	sub    BYTE PTR [rbp+0x62],dl
   8:	75 6e                	jne    78 <_init-0x400f88>
   a:	74 75                	je     81 <_init-0x400f7f>
   c:	20 39                	and    BYTE PTR [rcx],bh
   e:	2e 34 2e             	cs xor al,0x2e
  11:	30 2d 31 75 62 75    	xor    BYTE PTR [rip+0x75627531],ch        # 75627548 <__TMC_END__+0x752234a0>
  17:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  18:	74 75                	je     8f <_init-0x400f71>
  1a:	31 7e 32             	xor    DWORD PTR [rsi+0x32],edi
  1d:	30 2e                	xor    BYTE PTR [rsi],ch
  1f:	30 34 2e             	xor    BYTE PTR [rsi+rbp*1],dh
  22:	31 29                	xor    DWORD PTR [rcx],ebp
  24:	20 39                	and    BYTE PTR [rcx],bh
  26:	2e 34 2e             	cs xor al,0x2e
  29:	30 00                	xor    BYTE PTR [rax],al
