
ex5:     file format elf64-x86-64


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
  400367:	00 e6                	add    dh,ah
  400369:	bb 2c e3 74 65       	mov    ebx,0x6574e32c
  40036e:	79 52                	jns    4003c2 <_init-0xc3e>
  400370:	2f                   	(bad)
  400371:	1c af                	sbb    al,0xaf
  400373:	47 b6 1f             	rex.RXB mov r14b,0x1f
  400376:	dc d0                	(bad)
  400378:	f3 e6 6d             	repz out 0x6d,al
  40037b:	12                   	.byte 0x12

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
  4003f0:	12 00                	adc    al,BYTE PTR [rax]
  4003f2:	00 00                	add    BYTE PTR [rax],al
  4003f4:	12 00                	adc    al,BYTE PTR [rax]
	...
  400406:	00 00                	add    BYTE PTR [rax],al
  400408:	17                   	(bad)
  400409:	00 00                	add    BYTE PTR [rax],al
  40040b:	00 12                	add    BYTE PTR [rdx],dl
	...
  40041d:	00 00                	add    BYTE PTR [rax],al
  40041f:	00 1e                	add    BYTE PTR [rsi],bl
  400421:	00 00                	add    BYTE PTR [rax],al
  400423:	00 12                	add    BYTE PTR [rdx],dl
	...
  400435:	00 00                	add    BYTE PTR [rax],al
  400437:	00 3c 00             	add    BYTE PTR [rax+rax*1],bh
  40043a:	00 00                	add    BYTE PTR [rax],al
  40043c:	20 00                	and    BYTE PTR [rax],al
	...

Disassembly of section .dynstr:

0000000000400450 <.dynstr>:
  400450:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
  400454:	63 2e                	movsxd ebp,DWORD PTR [rsi]
  400456:	73 6f                	jae    4004c7 <_init-0xb39>
  400458:	2e 36 00 73 74       	cs ss add BYTE PTR [rbx+0x74],dh
  40045d:	72 63                	jb     4004c2 <_init-0xb3e>
  40045f:	70 79                	jo     4004da <_init-0xb26>
  400461:	00 70 75             	add    BYTE PTR [rax+0x75],dh
  400464:	74 73                	je     4004d9 <_init-0xb27>
  400466:	00 70 72             	add    BYTE PTR [rax+0x72],dh
  400469:	69 6e 74 66 00 5f 5f 	imul   ebp,DWORD PTR [rsi+0x74],0x5f5f0066
  400470:	6c                   	ins    BYTE PTR es:[rdi],dx
  400471:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
  400478:	72 74                	jb     4004ee <_init-0xb12>
  40047a:	5f                   	pop    rdi
  40047b:	6d                   	ins    DWORD PTR es:[rdi],dx
  40047c:	61                   	(bad)
  40047d:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
  400484:	43 5f                	rex.XB pop r15
  400486:	32 2e                	xor    ch,BYTE PTR [rsi]
  400488:	32 2e                	xor    ch,BYTE PTR [rsi]
  40048a:	35 00 5f 5f 67       	xor    eax,0x675f5f00
  40048f:	6d                   	ins    DWORD PTR es:[rdi],dx
  400490:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  400491:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  400492:	5f                   	pop    rdi
  400493:	73 74                	jae    400509 <_init-0xaf7>
  400495:	61                   	(bad)
  400496:	72 74                	jb     40050c <_init-0xaf4>
  400498:	5f                   	pop    rdi
  400499:	5f                   	pop    rdi
	...

Disassembly of section .gnu.version:

000000000040049c <.gnu.version>:
  40049c:	00 00                	add    BYTE PTR [rax],al
  40049e:	02 00                	add    al,BYTE PTR [rax]
  4004a0:	02 00                	add    al,BYTE PTR [rax]
  4004a2:	02 00                	add    al,BYTE PTR [rax]
  4004a4:	02 00                	add    al,BYTE PTR [rax]
	...

Disassembly of section .gnu.version_r:

00000000004004a8 <.gnu.version_r>:
  4004a8:	01 00                	add    DWORD PTR [rax],eax
  4004aa:	01 00                	add    DWORD PTR [rax],eax
  4004ac:	01 00                	add    DWORD PTR [rax],eax
  4004ae:	00 00                	add    BYTE PTR [rax],al
  4004b0:	10 00                	adc    BYTE PTR [rax],al
  4004b2:	00 00                	add    BYTE PTR [rax],al
  4004b4:	00 00                	add    BYTE PTR [rax],al
  4004b6:	00 00                	add    BYTE PTR [rax],al
  4004b8:	75 1a                	jne    4004d4 <_init-0xb2c>
  4004ba:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
  4004c0:	30 00                	xor    BYTE PTR [rax],al
  4004c2:	00 00                	add    BYTE PTR [rax],al
  4004c4:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

00000000004004c8 <.rela.dyn>:
  4004c8:	f0 3f                	lock (bad)
  4004ca:	40 00 00             	rex add BYTE PTR [rax],al
  4004cd:	00 00                	add    BYTE PTR [rax],al
  4004cf:	00 06                	add    BYTE PTR [rsi],al
  4004d1:	00 00                	add    BYTE PTR [rax],al
  4004d3:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...
  4004de:	00 00                	add    BYTE PTR [rax],al
  4004e0:	f8                   	clc
  4004e1:	3f                   	(bad)
  4004e2:	40 00 00             	rex add BYTE PTR [rax],al
  4004e5:	00 00                	add    BYTE PTR [rax],al
  4004e7:	00 06                	add    BYTE PTR [rsi],al
  4004e9:	00 00                	add    BYTE PTR [rax],al
  4004eb:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 4004f1 <_init-0xb0f>
  4004f1:	00 00                	add    BYTE PTR [rax],al
  4004f3:	00 00                	add    BYTE PTR [rax],al
  4004f5:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.plt:

00000000004004f8 <.rela.plt>:
  4004f8:	18 40 40             	sbb    BYTE PTR [rax+0x40],al
  4004fb:	00 00                	add    BYTE PTR [rax],al
  4004fd:	00 00                	add    BYTE PTR [rax],al
  4004ff:	00 07                	add    BYTE PTR [rdi],al
  400501:	00 00                	add    BYTE PTR [rax],al
  400503:	00 01                	add    BYTE PTR [rcx],al
	...
  40050d:	00 00                	add    BYTE PTR [rax],al
  40050f:	00 20                	add    BYTE PTR [rax],ah
  400511:	40                   	rex
  400512:	40 00 00             	rex add BYTE PTR [rax],al
  400515:	00 00                	add    BYTE PTR [rax],al
  400517:	00 07                	add    BYTE PTR [rdi],al
  400519:	00 00                	add    BYTE PTR [rax],al
  40051b:	00 02                	add    BYTE PTR [rdx],al
	...
  400525:	00 00                	add    BYTE PTR [rax],al
  400527:	00 28                	add    BYTE PTR [rax],ch
  400529:	40                   	rex
  40052a:	40 00 00             	rex add BYTE PTR [rax],al
  40052d:	00 00                	add    BYTE PTR [rax],al
  40052f:	00 07                	add    BYTE PTR [rdi],al
  400531:	00 00                	add    BYTE PTR [rax],al
  400533:	00 03                	add    BYTE PTR [rbx],al
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

Disassembly of section .plt:

0000000000401020 <.plt>:
  401020:	ff 35 e2 2f 00 00    	push   QWORD PTR [rip+0x2fe2]        # 404008 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	f2 ff 25 e3 2f 00 00 	bnd jmp QWORD PTR [rip+0x2fe3]        # 404010 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102d:	0f 1f 00             	nop    DWORD PTR [rax]
  401030:	f3 0f 1e fa          	endbr64
  401034:	68 00 00 00 00       	push   0x0
  401039:	f2 e9 e1 ff ff ff    	bnd jmp 401020 <.plt>
  40103f:	90                   	nop
  401040:	f3 0f 1e fa          	endbr64
  401044:	68 01 00 00 00       	push   0x1
  401049:	f2 e9 d1 ff ff ff    	bnd jmp 401020 <.plt>
  40104f:	90                   	nop
  401050:	f3 0f 1e fa          	endbr64
  401054:	68 02 00 00 00       	push   0x2
  401059:	f2 e9 c1 ff ff ff    	bnd jmp 401020 <.plt>
  40105f:	90                   	nop

Disassembly of section .plt.sec:

0000000000401060 <strcpy@plt>:
  401060:	f3 0f 1e fa          	endbr64
  401064:	f2 ff 25 ad 2f 00 00 	bnd jmp QWORD PTR [rip+0x2fad]        # 404018 <strcpy@GLIBC_2.2.5>
  40106b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000401070 <puts@plt>:
  401070:	f3 0f 1e fa          	endbr64
  401074:	f2 ff 25 a5 2f 00 00 	bnd jmp QWORD PTR [rip+0x2fa5]        # 404020 <puts@GLIBC_2.2.5>
  40107b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000401080 <printf@plt>:
  401080:	f3 0f 1e fa          	endbr64
  401084:	f2 ff 25 9d 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f9d]        # 404028 <printf@GLIBC_2.2.5>
  40108b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .text:

0000000000401090 <_start>:
  401090:	f3 0f 1e fa          	endbr64
  401094:	31 ed                	xor    ebp,ebp
  401096:	49 89 d1             	mov    r9,rdx
  401099:	5e                   	pop    rsi
  40109a:	48 89 e2             	mov    rdx,rsp
  40109d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
  4010a1:	50                   	push   rax
  4010a2:	54                   	push   rsp
  4010a3:	49 c7 c0 d0 12 40 00 	mov    r8,0x4012d0
  4010aa:	48 c7 c1 60 12 40 00 	mov    rcx,0x401260
  4010b1:	48 c7 c7 07 12 40 00 	mov    rdi,0x401207
  4010b8:	ff 15 32 2f 00 00    	call   QWORD PTR [rip+0x2f32]        # 403ff0 <__libc_start_main@GLIBC_2.2.5>
  4010be:	f4                   	hlt
  4010bf:	90                   	nop

00000000004010c0 <_dl_relocate_static_pie>:
  4010c0:	f3 0f 1e fa          	endbr64
  4010c4:	c3                   	ret
  4010c5:	66 2e 0f 1f 84 00 00 	cs nop WORD PTR [rax+rax*1+0x0]
  4010cc:	00 00 00 
  4010cf:	90                   	nop

00000000004010d0 <deregister_tm_clones>:
  4010d0:	b8 40 40 40 00       	mov    eax,0x404040
  4010d5:	48 3d 40 40 40 00    	cmp    rax,0x404040
  4010db:	74 13                	je     4010f0 <deregister_tm_clones+0x20>
  4010dd:	b8 00 00 00 00       	mov    eax,0x0
  4010e2:	48 85 c0             	test   rax,rax
  4010e5:	74 09                	je     4010f0 <deregister_tm_clones+0x20>
  4010e7:	bf 40 40 40 00       	mov    edi,0x404040
  4010ec:	ff e0                	jmp    rax
  4010ee:	66 90                	xchg   ax,ax
  4010f0:	c3                   	ret
  4010f1:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  4010f8:	00 00 00 00 
  4010fc:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401100 <register_tm_clones>:
  401100:	be 40 40 40 00       	mov    esi,0x404040
  401105:	48 81 ee 40 40 40 00 	sub    rsi,0x404040
  40110c:	48 89 f0             	mov    rax,rsi
  40110f:	48 c1 ee 3f          	shr    rsi,0x3f
  401113:	48 c1 f8 03          	sar    rax,0x3
  401117:	48 01 c6             	add    rsi,rax
  40111a:	48 d1 fe             	sar    rsi,1
  40111d:	74 11                	je     401130 <register_tm_clones+0x30>
  40111f:	b8 00 00 00 00       	mov    eax,0x0
  401124:	48 85 c0             	test   rax,rax
  401127:	74 07                	je     401130 <register_tm_clones+0x30>
  401129:	bf 40 40 40 00       	mov    edi,0x404040
  40112e:	ff e0                	jmp    rax
  401130:	c3                   	ret
  401131:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  401138:	00 00 00 00 
  40113c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401140 <__do_global_dtors_aux>:
  401140:	f3 0f 1e fa          	endbr64
  401144:	80 3d f5 2e 00 00 00 	cmp    BYTE PTR [rip+0x2ef5],0x0        # 404040 <__TMC_END__>
  40114b:	75 13                	jne    401160 <__do_global_dtors_aux+0x20>
  40114d:	55                   	push   rbp
  40114e:	48 89 e5             	mov    rbp,rsp
  401151:	e8 7a ff ff ff       	call   4010d0 <deregister_tm_clones>
  401156:	c6 05 e3 2e 00 00 01 	mov    BYTE PTR [rip+0x2ee3],0x1        # 404040 <__TMC_END__>
  40115d:	5d                   	pop    rbp
  40115e:	c3                   	ret
  40115f:	90                   	nop
  401160:	c3                   	ret
  401161:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  401168:	00 00 00 00 
  40116c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401170 <frame_dummy>:
  401170:	f3 0f 1e fa          	endbr64
  401174:	eb 8a                	jmp    401100 <register_tm_clones>

0000000000401176 <exploit_me>:
  401176:	f3 0f 1e fa          	endbr64
  40117a:	55                   	push   rbp
  40117b:	48 89 e5             	mov    rbp,rsp
  40117e:	48 81 ec 00 10 00 00 	sub    rsp,0x1000
  401185:	48 83 0c 24 00       	or     QWORD PTR [rsp],0x0
  40118a:	48 83 ec 30          	sub    rsp,0x30
  40118e:	48 89 bd d8 ef ff ff 	mov    QWORD PTR [rbp-0x1028],rdi
  401195:	48 89 b5 d0 ef ff ff 	mov    QWORD PTR [rbp-0x1030],rsi
  40119c:	48 8b 85 d8 ef ff ff 	mov    rax,QWORD PTR [rbp-0x1028]
  4011a3:	48 89 c6             	mov    rsi,rax
  4011a6:	48 8d 3d 57 0e 00 00 	lea    rdi,[rip+0xe57]        # 402004 <_IO_stdin_used+0x4>
  4011ad:	b8 00 00 00 00       	mov    eax,0x0
  4011b2:	e8 c9 fe ff ff       	call   401080 <printf@plt>
  4011b7:	48 8b 85 d0 ef ff ff 	mov    rax,QWORD PTR [rbp-0x1030]
  4011be:	48 89 c6             	mov    rsi,rax
  4011c1:	48 8d 3d 4d 0e 00 00 	lea    rdi,[rip+0xe4d]        # 402015 <_IO_stdin_used+0x15>
  4011c8:	b8 00 00 00 00       	mov    eax,0x0
  4011cd:	e8 ae fe ff ff       	call   401080 <printf@plt>
  4011d2:	48 8b 95 d8 ef ff ff 	mov    rdx,QWORD PTR [rbp-0x1028]
  4011d9:	48 8d 85 00 f0 ff ff 	lea    rax,[rbp-0x1000]
  4011e0:	48 89 d6             	mov    rsi,rdx
  4011e3:	48 89 c7             	mov    rdi,rax
  4011e6:	e8 75 fe ff ff       	call   401060 <strcpy@plt>
  4011eb:	48 8b 95 d0 ef ff ff 	mov    rdx,QWORD PTR [rbp-0x1030]
  4011f2:	48 8d 85 e0 ef ff ff 	lea    rax,[rbp-0x1020]
  4011f9:	48 89 d6             	mov    rsi,rdx
  4011fc:	48 89 c7             	mov    rdi,rax
  4011ff:	e8 5c fe ff ff       	call   401060 <strcpy@plt>
  401204:	90                   	nop
  401205:	c9                   	leave
  401206:	c3                   	ret

0000000000401207 <main>:
  401207:	f3 0f 1e fa          	endbr64
  40120b:	55                   	push   rbp
  40120c:	48 89 e5             	mov    rbp,rsp
  40120f:	48 83 ec 10          	sub    rsp,0x10
  401213:	89 7d fc             	mov    DWORD PTR [rbp-0x4],edi
  401216:	48 89 75 f0          	mov    QWORD PTR [rbp-0x10],rsi
  40121a:	83 7d fc 03          	cmp    DWORD PTR [rbp-0x4],0x3
  40121e:	74 13                	je     401233 <main+0x2c>
  401220:	48 8d 3d ff 0d 00 00 	lea    rdi,[rip+0xdff]        # 402026 <_IO_stdin_used+0x26>
  401227:	e8 44 fe ff ff       	call   401070 <puts@plt>
  40122c:	b8 01 00 00 00       	mov    eax,0x1
  401231:	eb 26                	jmp    401259 <main+0x52>
  401233:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
  401237:	48 83 c0 10          	add    rax,0x10
  40123b:	48 8b 10             	mov    rdx,QWORD PTR [rax]
  40123e:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
  401242:	48 83 c0 08          	add    rax,0x8
  401246:	48 8b 00             	mov    rax,QWORD PTR [rax]
  401249:	48 89 d6             	mov    rsi,rdx
  40124c:	48 89 c7             	mov    rdi,rax
  40124f:	e8 22 ff ff ff       	call   401176 <exploit_me>
  401254:	b8 00 00 00 00       	mov    eax,0x0
  401259:	c9                   	leave
  40125a:	c3                   	ret
  40125b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000401260 <__libc_csu_init>:
  401260:	f3 0f 1e fa          	endbr64
  401264:	41 57                	push   r15
  401266:	4c 8d 3d a3 2b 00 00 	lea    r15,[rip+0x2ba3]        # 403e10 <__frame_dummy_init_array_entry>
  40126d:	41 56                	push   r14
  40126f:	49 89 d6             	mov    r14,rdx
  401272:	41 55                	push   r13
  401274:	49 89 f5             	mov    r13,rsi
  401277:	41 54                	push   r12
  401279:	41 89 fc             	mov    r12d,edi
  40127c:	55                   	push   rbp
  40127d:	48 8d 2d 94 2b 00 00 	lea    rbp,[rip+0x2b94]        # 403e18 <__do_global_dtors_aux_fini_array_entry>
  401284:	53                   	push   rbx
  401285:	4c 29 fd             	sub    rbp,r15
  401288:	48 83 ec 08          	sub    rsp,0x8
  40128c:	e8 6f fd ff ff       	call   401000 <_init>
  401291:	48 c1 fd 03          	sar    rbp,0x3
  401295:	74 1f                	je     4012b6 <__libc_csu_init+0x56>
  401297:	31 db                	xor    ebx,ebx
  401299:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
  4012a0:	4c 89 f2             	mov    rdx,r14
  4012a3:	4c 89 ee             	mov    rsi,r13
  4012a6:	44 89 e7             	mov    edi,r12d
  4012a9:	41 ff 14 df          	call   QWORD PTR [r15+rbx*8]
  4012ad:	48 83 c3 01          	add    rbx,0x1
  4012b1:	48 39 dd             	cmp    rbp,rbx
  4012b4:	75 ea                	jne    4012a0 <__libc_csu_init+0x40>
  4012b6:	48 83 c4 08          	add    rsp,0x8
  4012ba:	5b                   	pop    rbx
  4012bb:	5d                   	pop    rbp
  4012bc:	41 5c                	pop    r12
  4012be:	41 5d                	pop    r13
  4012c0:	41 5e                	pop    r14
  4012c2:	41 5f                	pop    r15
  4012c4:	c3                   	ret
  4012c5:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
  4012cc:	00 00 00 00 

00000000004012d0 <__libc_csu_fini>:
  4012d0:	f3 0f 1e fa          	endbr64
  4012d4:	c3                   	ret

Disassembly of section .fini:

00000000004012d8 <_fini>:
  4012d8:	f3 0f 1e fa          	endbr64
  4012dc:	48 83 ec 08          	sub    rsp,0x8
  4012e0:	48 83 c4 08          	add    rsp,0x8
  4012e4:	c3                   	ret

Disassembly of section .rodata:

0000000000402000 <_IO_stdin_used>:
  402000:	01 00                	add    DWORD PTR [rax],eax
  402002:	02 00                	add    al,BYTE PTR [rax]
  402004:	73 68                	jae    40206e <__GNU_EH_FRAME_HDR+0x2e>
  402006:	65 6c                	gs ins BYTE PTR es:[rdi],dx
  402008:	6c                   	ins    BYTE PTR es:[rdi],dx
  402009:	63 6f 64             	movsxd ebp,DWORD PTR [rdi+0x64]
  40200c:	65 20 3d 3d 20 25 73 	and    BYTE PTR gs:[rip+0x7325203d],bh        # 73654050 <_end+0x73250008>
  402013:	0a 00                	or     al,BYTE PTR [rax]
  402015:	65 78 70             	gs js  402088 <__GNU_EH_FRAME_HDR+0x48>
  402018:	6c                   	ins    BYTE PTR es:[rdi],dx
  402019:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  40201a:	69 74 20 20 20 3d 3d 	imul   esi,DWORD PTR [rax+riz*1+0x20],0x203d3d20
  402021:	20 
  402022:	25 73 0a 00 3e       	and    eax,0x3e000a73
  402027:	3e 20 45 78          	ds and BYTE PTR [rbp+0x78],al
  40202b:	70 65                	jo     402092 <__GNU_EH_FRAME_HDR+0x52>
  40202d:	63 74 65 64          	movsxd esi,DWORD PTR [rbp+riz*2+0x64]
  402031:	20 32                	and    BYTE PTR [rdx],dh
  402033:	20 73 74             	and    BYTE PTR [rbx+0x74],dh
  402036:	72 69                	jb     4020a1 <__GNU_EH_FRAME_HDR+0x61>
  402038:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  402039:	67 73 21             	addr32 jae 40205d <__GNU_EH_FRAME_HDR+0x1d>
	...

Disassembly of section .eh_frame_hdr:

0000000000402040 <__GNU_EH_FRAME_HDR>:
  402040:	01 1b                	add    DWORD PTR [rbx],ebx
  402042:	03 3b                	add    edi,DWORD PTR [rbx]
  402044:	4c 00 00             	rex.WR add BYTE PTR [rax],r8b
  402047:	00 08                	add    BYTE PTR [rax],cl
  402049:	00 00                	add    BYTE PTR [rax],al
  40204b:	00 e0                	add    al,ah
  40204d:	ef                   	out    dx,eax
  40204e:	ff                   	(bad)
  40204f:	ff 90 00 00 00 20    	call   QWORD PTR [rax+0x20000000]
  402055:	f0 ff                	lock (bad)
  402057:	ff                   	(bad)
  402058:	b8 00 00 00 50       	mov    eax,0x50000000
  40205d:	f0 ff                	lock (bad)
  40205f:	ff 68 00             	jmp    FWORD PTR [rax+0x0]
  402062:	00 00                	add    BYTE PTR [rax],al
  402064:	80 f0 ff             	xor    al,0xff
  402067:	ff                   	(bad)
  402068:	7c 00                	jl     40206a <__GNU_EH_FRAME_HDR+0x2a>
  40206a:	00 00                	add    BYTE PTR [rax],al
  40206c:	36 f1                	ss int1
  40206e:	ff                   	(bad)
  40206f:	ff d0                	call   rax
  402071:	00 00                	add    BYTE PTR [rax],al
  402073:	00 c7                	add    bh,al
  402075:	f1                   	int1
  402076:	ff                   	(bad)
  402077:	ff f0                	push   rax
  402079:	00 00                	add    BYTE PTR [rax],al
  40207b:	00 20                	add    BYTE PTR [rax],ah
  40207d:	f2 ff                	repnz (bad)
  40207f:	ff 10                	call   QWORD PTR [rax]
  402081:	01 00                	add    DWORD PTR [rax],eax
  402083:	00 90 f2 ff ff 58    	add    BYTE PTR [rax+0x58fffff2],dl
  402089:	01 00                	add    DWORD PTR [rax],eax
	...

Disassembly of section .eh_frame:

0000000000402090 <__FRAME_END__-0x11c>:
  402090:	14 00                	adc    al,0x0
  402092:	00 00                	add    BYTE PTR [rax],al
  402094:	00 00                	add    BYTE PTR [rax],al
  402096:	00 00                	add    BYTE PTR [rax],al
  402098:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
  40209b:	00 01                	add    BYTE PTR [rcx],al
  40209d:	78 10                	js     4020af <__GNU_EH_FRAME_HDR+0x6f>
  40209f:	01 1b                	add    DWORD PTR [rbx],ebx
  4020a1:	0c 07                	or     al,0x7
  4020a3:	08 90 01 00 00 10    	or     BYTE PTR [rax+0x10000001],dl
  4020a9:	00 00                	add    BYTE PTR [rax],al
  4020ab:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  4020ae:	00 00                	add    BYTE PTR [rax],al
  4020b0:	e0 ef                	loopne 4020a1 <__GNU_EH_FRAME_HDR+0x61>
  4020b2:	ff                   	(bad)
  4020b3:	ff 2f                	jmp    FWORD PTR [rdi]
  4020b5:	00 00                	add    BYTE PTR [rax],al
  4020b7:	00 00                	add    BYTE PTR [rax],al
  4020b9:	44 07                	rex.R (bad)
  4020bb:	10 10                	adc    BYTE PTR [rax],dl
  4020bd:	00 00                	add    BYTE PTR [rax],al
  4020bf:	00 30                	add    BYTE PTR [rax],dh
  4020c1:	00 00                	add    BYTE PTR [rax],al
  4020c3:	00 fc                	add    ah,bh
  4020c5:	ef                   	out    dx,eax
  4020c6:	ff                   	(bad)
  4020c7:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 4020cd <__GNU_EH_FRAME_HDR+0x8d>
  4020cd:	00 00                	add    BYTE PTR [rax],al
  4020cf:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
  4020d2:	00 00                	add    BYTE PTR [rax],al
  4020d4:	44 00 00             	add    BYTE PTR [rax],r8b
  4020d7:	00 48 ef             	add    BYTE PTR [rax-0x11],cl
  4020da:	ff                   	(bad)
  4020db:	ff 40 00             	inc    DWORD PTR [rax+0x0]
  4020de:	00 00                	add    BYTE PTR [rax],al
  4020e0:	00 0e                	add    BYTE PTR [rsi],cl
  4020e2:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
  4020e5:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
  4020e8:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
  4020eb:	80 00 3f             	add    BYTE PTR [rax],0x3f
  4020ee:	1a 3a                	sbb    bh,BYTE PTR [rdx]
  4020f0:	2a 33                	sub    dh,BYTE PTR [rbx]
  4020f2:	24 22                	and    al,0x22
  4020f4:	00 00                	add    BYTE PTR [rax],al
  4020f6:	00 00                	add    BYTE PTR [rax],al
  4020f8:	14 00                	adc    al,0x0
  4020fa:	00 00                	add    BYTE PTR [rax],al
  4020fc:	6c                   	ins    BYTE PTR es:[rdi],dx
  4020fd:	00 00                	add    BYTE PTR [rax],al
  4020ff:	00 60 ef             	add    BYTE PTR [rax-0x11],ah
  402102:	ff                   	(bad)
  402103:	ff 30                	push   QWORD PTR [rax]
	...
  40210d:	00 00                	add    BYTE PTR [rax],al
  40210f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  402112:	00 00                	add    BYTE PTR [rax],al
  402114:	84 00                	test   BYTE PTR [rax],al
  402116:	00 00                	add    BYTE PTR [rax],al
  402118:	5e                   	pop    rsi
  402119:	f0 ff                	lock (bad)
  40211b:	ff 91 00 00 00 00    	call   QWORD PTR [rcx+0x0]
  402121:	45 0e                	rex.RB (bad)
  402123:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  402129:	02 88 0c 07 08 00    	add    cl,BYTE PTR [rax+0x8070c]
  40212f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  402132:	00 00                	add    BYTE PTR [rax],al
  402134:	a4                   	movs   BYTE PTR es:[rdi],BYTE PTR ds:[rsi]
  402135:	00 00                	add    BYTE PTR [rax],al
  402137:	00 cf                	add    bh,cl
  402139:	f0 ff                	lock (bad)
  40213b:	ff 54 00 00          	call   QWORD PTR [rax+rax*1+0x0]
  40213f:	00 00                	add    BYTE PTR [rax],al
  402141:	45 0e                	rex.RB (bad)
  402143:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  402149:	02 4b 0c             	add    cl,BYTE PTR [rbx+0xc]
  40214c:	07                   	(bad)
  40214d:	08 00                	or     BYTE PTR [rax],al
  40214f:	00 44 00 00          	add    BYTE PTR [rax+rax*1+0x0],al
  402153:	00 c4                	add    ah,al
  402155:	00 00                	add    BYTE PTR [rax],al
  402157:	00 08                	add    BYTE PTR [rax],cl
  402159:	f1                   	int1
  40215a:	ff                   	(bad)
  40215b:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
  40215e:	00 00                	add    BYTE PTR [rax],al
  402160:	00 46 0e             	add    BYTE PTR [rsi+0xe],al
  402163:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
  402169:	8e 03                	mov    es,WORD PTR [rbx]
  40216b:	45 0e                	rex.RB (bad)
  40216d:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
  402173:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff86702fbd <_end+0xffffffff862fef75>
  402179:	06                   	(bad)
  40217a:	48 0e                	rex.W (bad)
  40217c:	38 83 07 47 0e 40    	cmp    BYTE PTR [rbx+0x400e4707],al
  402182:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  402183:	0e                   	(bad)
  402184:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
  402187:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
  40218a:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
  40218d:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
  402190:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
  402193:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
  402196:	08 00                	or     BYTE PTR [rax],al
  402198:	10 00                	adc    BYTE PTR [rax],al
  40219a:	00 00                	add    BYTE PTR [rax],al
  40219c:	0c 01                	or     al,0x1
  40219e:	00 00                	add    BYTE PTR [rax],al
  4021a0:	30 f1                	xor    cl,dh
  4021a2:	ff                   	(bad)
  4021a3:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 4021a9 <__GNU_EH_FRAME_HDR+0x169>
  4021a9:	00 00                	add    BYTE PTR [rax],al
	...

00000000004021ac <__FRAME_END__>:
  4021ac:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000403e10 <__frame_dummy_init_array_entry>:
  403e10:	70 11                	jo     403e23 <_DYNAMIC+0x3>
  403e12:	40 00 00             	rex add BYTE PTR [rax],al
  403e15:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000403e18 <__do_global_dtors_aux_fini_array_entry>:
  403e18:	40 11 40 00          	rex adc DWORD PTR [rax+0x0],eax
  403e1c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000403e20 <_DYNAMIC>:
  403e20:	01 00                	add    DWORD PTR [rax],eax
  403e22:	00 00                	add    BYTE PTR [rax],al
  403e24:	00 00                	add    BYTE PTR [rax],al
  403e26:	00 00                	add    BYTE PTR [rax],al
  403e28:	01 00                	add    DWORD PTR [rax],eax
  403e2a:	00 00                	add    BYTE PTR [rax],al
  403e2c:	00 00                	add    BYTE PTR [rax],al
  403e2e:	00 00                	add    BYTE PTR [rax],al
  403e30:	0c 00                	or     al,0x0
  403e32:	00 00                	add    BYTE PTR [rax],al
  403e34:	00 00                	add    BYTE PTR [rax],al
  403e36:	00 00                	add    BYTE PTR [rax],al
  403e38:	00 10                	add    BYTE PTR [rax],dl
  403e3a:	40 00 00             	rex add BYTE PTR [rax],al
  403e3d:	00 00                	add    BYTE PTR [rax],al
  403e3f:	00 0d 00 00 00 00    	add    BYTE PTR [rip+0x0],cl        # 403e45 <_DYNAMIC+0x25>
  403e45:	00 00                	add    BYTE PTR [rax],al
  403e47:	00 d8                	add    al,bl
  403e49:	12 40 00             	adc    al,BYTE PTR [rax+0x0]
  403e4c:	00 00                	add    BYTE PTR [rax],al
  403e4e:	00 00                	add    BYTE PTR [rax],al
  403e50:	19 00                	sbb    DWORD PTR [rax],eax
  403e52:	00 00                	add    BYTE PTR [rax],al
  403e54:	00 00                	add    BYTE PTR [rax],al
  403e56:	00 00                	add    BYTE PTR [rax],al
  403e58:	10 3e                	adc    BYTE PTR [rsi],bh
  403e5a:	40 00 00             	rex add BYTE PTR [rax],al
  403e5d:	00 00                	add    BYTE PTR [rax],al
  403e5f:	00 1b                	add    BYTE PTR [rbx],bl
  403e61:	00 00                	add    BYTE PTR [rax],al
  403e63:	00 00                	add    BYTE PTR [rax],al
  403e65:	00 00                	add    BYTE PTR [rax],al
  403e67:	00 08                	add    BYTE PTR [rax],cl
  403e69:	00 00                	add    BYTE PTR [rax],al
  403e6b:	00 00                	add    BYTE PTR [rax],al
  403e6d:	00 00                	add    BYTE PTR [rax],al
  403e6f:	00 1a                	add    BYTE PTR [rdx],bl
  403e71:	00 00                	add    BYTE PTR [rax],al
  403e73:	00 00                	add    BYTE PTR [rax],al
  403e75:	00 00                	add    BYTE PTR [rax],al
  403e77:	00 18                	add    BYTE PTR [rax],bl
  403e79:	3e 40 00 00          	ds rex add BYTE PTR [rax],al
  403e7d:	00 00                	add    BYTE PTR [rax],al
  403e7f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  403e82:	00 00                	add    BYTE PTR [rax],al
  403e84:	00 00                	add    BYTE PTR [rax],al
  403e86:	00 00                	add    BYTE PTR [rax],al
  403e88:	08 00                	or     BYTE PTR [rax],al
  403e8a:	00 00                	add    BYTE PTR [rax],al
  403e8c:	00 00                	add    BYTE PTR [rax],al
  403e8e:	00 00                	add    BYTE PTR [rax],al
  403e90:	f5                   	cmc
  403e91:	fe                   	(bad)
  403e92:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  403e95:	00 00                	add    BYTE PTR [rax],al
  403e97:	00 a0 03 40 00 00    	add    BYTE PTR [rax+0x4003],ah
  403e9d:	00 00                	add    BYTE PTR [rax],al
  403e9f:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 403ea5 <_DYNAMIC+0x85>
  403ea5:	00 00                	add    BYTE PTR [rax],al
  403ea7:	00 50 04             	add    BYTE PTR [rax+0x4],dl
  403eaa:	40 00 00             	rex add BYTE PTR [rax],al
  403ead:	00 00                	add    BYTE PTR [rax],al
  403eaf:	00 06                	add    BYTE PTR [rsi],al
  403eb1:	00 00                	add    BYTE PTR [rax],al
  403eb3:	00 00                	add    BYTE PTR [rax],al
  403eb5:	00 00                	add    BYTE PTR [rax],al
  403eb7:	00 c0                	add    al,al
  403eb9:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
  403ebc:	00 00                	add    BYTE PTR [rax],al
  403ebe:	00 00                	add    BYTE PTR [rax],al
  403ec0:	0a 00                	or     al,BYTE PTR [rax]
  403ec2:	00 00                	add    BYTE PTR [rax],al
  403ec4:	00 00                	add    BYTE PTR [rax],al
  403ec6:	00 00                	add    BYTE PTR [rax],al
  403ec8:	4b 00 00             	rex.WXB add BYTE PTR [r8],al
  403ecb:	00 00                	add    BYTE PTR [rax],al
  403ecd:	00 00                	add    BYTE PTR [rax],al
  403ecf:	00 0b                	add    BYTE PTR [rbx],cl
  403ed1:	00 00                	add    BYTE PTR [rax],al
  403ed3:	00 00                	add    BYTE PTR [rax],al
  403ed5:	00 00                	add    BYTE PTR [rax],al
  403ed7:	00 18                	add    BYTE PTR [rax],bl
  403ed9:	00 00                	add    BYTE PTR [rax],al
  403edb:	00 00                	add    BYTE PTR [rax],al
  403edd:	00 00                	add    BYTE PTR [rax],al
  403edf:	00 15 00 00 00 00    	add    BYTE PTR [rip+0x0],dl        # 403ee5 <_DYNAMIC+0xc5>
	...
  403eed:	00 00                	add    BYTE PTR [rax],al
  403eef:	00 03                	add    BYTE PTR [rbx],al
	...
  403ef9:	40                   	rex
  403efa:	40 00 00             	rex add BYTE PTR [rax],al
  403efd:	00 00                	add    BYTE PTR [rax],al
  403eff:	00 02                	add    BYTE PTR [rdx],al
  403f01:	00 00                	add    BYTE PTR [rax],al
  403f03:	00 00                	add    BYTE PTR [rax],al
  403f05:	00 00                	add    BYTE PTR [rax],al
  403f07:	00 48 00             	add    BYTE PTR [rax+0x0],cl
  403f0a:	00 00                	add    BYTE PTR [rax],al
  403f0c:	00 00                	add    BYTE PTR [rax],al
  403f0e:	00 00                	add    BYTE PTR [rax],al
  403f10:	14 00                	adc    al,0x0
  403f12:	00 00                	add    BYTE PTR [rax],al
  403f14:	00 00                	add    BYTE PTR [rax],al
  403f16:	00 00                	add    BYTE PTR [rax],al
  403f18:	07                   	(bad)
  403f19:	00 00                	add    BYTE PTR [rax],al
  403f1b:	00 00                	add    BYTE PTR [rax],al
  403f1d:	00 00                	add    BYTE PTR [rax],al
  403f1f:	00 17                	add    BYTE PTR [rdi],dl
  403f21:	00 00                	add    BYTE PTR [rax],al
  403f23:	00 00                	add    BYTE PTR [rax],al
  403f25:	00 00                	add    BYTE PTR [rax],al
  403f27:	00 f8                	add    al,bh
  403f29:	04 40                	add    al,0x40
  403f2b:	00 00                	add    BYTE PTR [rax],al
  403f2d:	00 00                	add    BYTE PTR [rax],al
  403f2f:	00 07                	add    BYTE PTR [rdi],al
  403f31:	00 00                	add    BYTE PTR [rax],al
  403f33:	00 00                	add    BYTE PTR [rax],al
  403f35:	00 00                	add    BYTE PTR [rax],al
  403f37:	00 c8                	add    al,cl
  403f39:	04 40                	add    al,0x40
  403f3b:	00 00                	add    BYTE PTR [rax],al
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
  403f67:	00 a8 04 40 00 00    	add    BYTE PTR [rax+0x4004],ch
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
  403f87:	00 9c 04 40 00 00 00 	add    BYTE PTR [rsp+rax*1+0x40],bl
	...

Disassembly of section .got:

0000000000403ff0 <.got>:
	...

Disassembly of section .got.plt:

0000000000404000 <_GLOBAL_OFFSET_TABLE_>:
  404000:	20 3e                	and    BYTE PTR [rsi],bh
  404002:	40 00 00             	rex add BYTE PTR [rax],al
	...
  404015:	00 00                	add    BYTE PTR [rax],al
  404017:	00 30                	add    BYTE PTR [rax],dh
  404019:	10 40 00             	adc    BYTE PTR [rax+0x0],al
  40401c:	00 00                	add    BYTE PTR [rax],al
  40401e:	00 00                	add    BYTE PTR [rax],al
  404020:	40 10 40 00          	rex adc BYTE PTR [rax+0x0],al
  404024:	00 00                	add    BYTE PTR [rax],al
  404026:	00 00                	add    BYTE PTR [rax],al
  404028:	50                   	push   rax
  404029:	10 40 00             	adc    BYTE PTR [rax+0x0],al
  40402c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .data:

0000000000404030 <__data_start>:
	...

0000000000404038 <__dso_handle>:
	...

Disassembly of section .bss:

0000000000404040 <completed.8061>:
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
  11:	30 2d 31 75 62 75    	xor    BYTE PTR [rip+0x75627531],ch        # 75627548 <_end+0x75223500>
  17:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  18:	74 75                	je     8f <_init-0x400f71>
  1a:	31 7e 32             	xor    DWORD PTR [rsi+0x32],edi
  1d:	30 2e                	xor    BYTE PTR [rsi],ch
  1f:	30 34 2e             	xor    BYTE PTR [rsi+rbp*1],dh
  22:	31 29                	xor    DWORD PTR [rcx],ebp
  24:	20 39                	and    BYTE PTR [rcx],bh
  26:	2e 34 2e             	cs xor al,0x2e
  29:	30 00                	xor    BYTE PTR [rax],al
