
hello:     file format elf64-x86-64


Disassembly of section .interp:

00000000004002a8 <.interp>:
  4002a8:	2f                   	(bad)  
  4002a9:	6c                   	ins    BYTE PTR es:[rdi],dx
  4002aa:	69 62 36 34 2f 6c 64 	imul   esp,DWORD PTR [rdx+0x36],0x646c2f34
  4002b1:	2d 6c 69 6e 75       	sub    eax,0x756e696c
  4002b6:	78 2d                	js     4002e5 <_init-0xd1b>
  4002b8:	78 38                	js     4002f2 <_init-0xd0e>
  4002ba:	36 2d 36 34 2e 73    	ss sub eax,0x732e3436
  4002c0:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  4002c1:	2e 32 00             	xor    al,BYTE PTR cs:[rax]

Disassembly of section .note.gnu.build-id:

00000000004002c4 <.note.gnu.build-id>:
  4002c4:	04 00                	add    al,0x0
  4002c6:	00 00                	add    BYTE PTR [rax],al
  4002c8:	14 00                	adc    al,0x0
  4002ca:	00 00                	add    BYTE PTR [rax],al
  4002cc:	03 00                	add    eax,DWORD PTR [rax]
  4002ce:	00 00                	add    BYTE PTR [rax],al
  4002d0:	47                   	rex.RXB
  4002d1:	4e 55                	rex.WRX push rbp
  4002d3:	00 d7                	add    bh,dl
  4002d5:	ba c1 75 df ec       	mov    edx,0xecdf75c1
  4002da:	8f                   	(bad)  
  4002db:	25 e0 8c 31 b5       	and    eax,0xb5318ce0
  4002e0:	98                   	cwde   
  4002e1:	66 a7                	cmps   WORD PTR ds:[rsi],WORD PTR es:[rdi]
  4002e3:	e4 03                	in     al,0x3
  4002e5:	1a 6f 8e             	sbb    ch,BYTE PTR [rdi-0x72]

Disassembly of section .note.ABI-tag:

00000000004002e8 <.note.ABI-tag>:
  4002e8:	04 00                	add    al,0x0
  4002ea:	00 00                	add    BYTE PTR [rax],al
  4002ec:	10 00                	adc    BYTE PTR [rax],al
  4002ee:	00 00                	add    BYTE PTR [rax],al
  4002f0:	01 00                	add    DWORD PTR [rax],eax
  4002f2:	00 00                	add    BYTE PTR [rax],al
  4002f4:	47                   	rex.RXB
  4002f5:	4e 55                	rex.WRX push rbp
  4002f7:	00 00                	add    BYTE PTR [rax],al
  4002f9:	00 00                	add    BYTE PTR [rax],al
  4002fb:	00 03                	add    BYTE PTR [rbx],al
  4002fd:	00 00                	add    BYTE PTR [rax],al
  4002ff:	00 02                	add    BYTE PTR [rdx],al
  400301:	00 00                	add    BYTE PTR [rax],al
  400303:	00 00                	add    BYTE PTR [rax],al
  400305:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .gnu.hash:

0000000000400308 <.gnu.hash>:
  400308:	02 00                	add    al,BYTE PTR [rax]
  40030a:	00 00                	add    BYTE PTR [rax],al
  40030c:	07                   	(bad)  
  40030d:	00 00                	add    BYTE PTR [rax],al
  40030f:	00 01                	add    BYTE PTR [rcx],al
  400311:	00 00                	add    BYTE PTR [rax],al
  400313:	00 06                	add    BYTE PTR [rsi],al
  400315:	00 00                	add    BYTE PTR [rax],al
  400317:	00 00                	add    BYTE PTR [rax],al
  400319:	01 00                	add    DWORD PTR [rax],eax
  40031b:	00 00                	add    BYTE PTR [rax],al
  40031d:	00 00                	add    BYTE PTR [rax],al
  40031f:	02 00                	add    al,BYTE PTR [rax]
  400321:	00 00                	add    BYTE PTR [rax],al
  400323:	00 07                	add    BYTE PTR [rdi],al
  400325:	00 00                	add    BYTE PTR [rax],al
  400327:	00 39                	add    BYTE PTR [rcx],bh
  400329:	f2                   	repnz
  40032a:	8b                   	.byte 0x8b
  40032b:	1c                   	.byte 0x1c

Disassembly of section .dynsym:

0000000000400330 <.dynsym>:
	...
  400348:	12 00                	adc    al,BYTE PTR [rax]
  40034a:	00 00                	add    BYTE PTR [rax],al
  40034c:	12 00                	adc    al,BYTE PTR [rax]
	...
  40035e:	00 00                	add    BYTE PTR [rax],al
  400360:	0b 00                	or     eax,DWORD PTR [rax]
  400362:	00 00                	add    BYTE PTR [rax],al
  400364:	12 00                	adc    al,BYTE PTR [rax]
	...
  400376:	00 00                	add    BYTE PTR [rax],al
  400378:	1a 00                	sbb    al,BYTE PTR [rax]
  40037a:	00 00                	add    BYTE PTR [rax],al
  40037c:	12 00                	adc    al,BYTE PTR [rax]
	...
  40038e:	00 00                	add    BYTE PTR [rax],al
  400390:	2e 00 00             	add    BYTE PTR cs:[rax],al
  400393:	00 12                	add    BYTE PTR [rdx],dl
	...
  4003a5:	00 00                	add    BYTE PTR [rax],al
  4003a7:	00 26                	add    BYTE PTR [rsi],ah
  4003a9:	00 00                	add    BYTE PTR [rax],al
  4003ab:	00 12                	add    BYTE PTR [rdx],dl
	...
  4003bd:	00 00                	add    BYTE PTR [rax],al
  4003bf:	00 4c 00 00          	add    BYTE PTR [rax+rax*1+0x0],cl
  4003c3:	00 20                	add    BYTE PTR [rax],ah
	...
  4003d5:	00 00                	add    BYTE PTR [rax],al
  4003d7:	00 1f                	add    BYTE PTR [rdi],bl
  4003d9:	00 00                	add    BYTE PTR [rax],al
  4003db:	00 11                	add    BYTE PTR [rcx],dl
  4003dd:	00 18                	add    BYTE PTR [rax],bl
  4003df:	00 60 40             	add    BYTE PTR [rax+0x40],ah
  4003e2:	40 00 00             	add    BYTE PTR [rax],al
  4003e5:	00 00                	add    BYTE PTR [rax],al
  4003e7:	00 08                	add    BYTE PTR [rax],cl
  4003e9:	00 00                	add    BYTE PTR [rax],al
  4003eb:	00 00                	add    BYTE PTR [rax],al
  4003ed:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynstr:

00000000004003f0 <.dynstr>:
  4003f0:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
  4003f4:	63 2e                	movsxd ebp,DWORD PTR [rsi]
  4003f6:	73 6f                	jae    400467 <_init-0xb99>
  4003f8:	2e 36 00 73 74       	cs add BYTE PTR ss:[rbx+0x74],dh
  4003fd:	72 63                	jb     400462 <_init-0xb9e>
  4003ff:	70 79                	jo     40047a <_init-0xb86>
  400401:	00 73 74             	add    BYTE PTR [rbx+0x74],dh
  400404:	72 6e                	jb     400474 <_init-0xb8c>
  400406:	63 6d 70             	movsxd ebp,DWORD PTR [rbp+0x70]
  400409:	00 70 75             	add    BYTE PTR [rax+0x75],dh
  40040c:	74 73                	je     400481 <_init-0xb7f>
  40040e:	00 73 74             	add    BYTE PTR [rbx+0x74],dh
  400411:	64 65 72 72          	fs gs jb 400487 <_init-0xb79>
  400415:	00 66 70             	add    BYTE PTR [rsi+0x70],ah
  400418:	72 69                	jb     400483 <_init-0xb7d>
  40041a:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  40041b:	74 66                	je     400483 <_init-0xb7d>
  40041d:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
  400420:	6c                   	ins    BYTE PTR es:[rdi],dx
  400421:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
  400428:	72 74                	jb     40049e <_init-0xb62>
  40042a:	5f                   	pop    rdi
  40042b:	6d                   	ins    DWORD PTR es:[rdi],dx
  40042c:	61                   	(bad)  
  40042d:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
  400434:	43 5f                	rex.XB pop r15
  400436:	32 2e                	xor    ch,BYTE PTR [rsi]
  400438:	32 2e                	xor    ch,BYTE PTR [rsi]
  40043a:	35 00 5f 5f 67       	xor    eax,0x675f5f00
  40043f:	6d                   	ins    DWORD PTR es:[rdi],dx
  400440:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  400441:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  400442:	5f                   	pop    rdi
  400443:	73 74                	jae    4004b9 <_init-0xb47>
  400445:	61                   	(bad)  
  400446:	72 74                	jb     4004bc <_init-0xb44>
  400448:	5f                   	pop    rdi
  400449:	5f                   	pop    rdi
	...

Disassembly of section .gnu.version:

000000000040044c <.gnu.version>:
  40044c:	00 00                	add    BYTE PTR [rax],al
  40044e:	02 00                	add    al,BYTE PTR [rax]
  400450:	02 00                	add    al,BYTE PTR [rax]
  400452:	02 00                	add    al,BYTE PTR [rax]
  400454:	02 00                	add    al,BYTE PTR [rax]
  400456:	02 00                	add    al,BYTE PTR [rax]
  400458:	00 00                	add    BYTE PTR [rax],al
  40045a:	02 00                	add    al,BYTE PTR [rax]

Disassembly of section .gnu.version_r:

0000000000400460 <.gnu.version_r>:
  400460:	01 00                	add    DWORD PTR [rax],eax
  400462:	01 00                	add    DWORD PTR [rax],eax
  400464:	01 00                	add    DWORD PTR [rax],eax
  400466:	00 00                	add    BYTE PTR [rax],al
  400468:	10 00                	adc    BYTE PTR [rax],al
  40046a:	00 00                	add    BYTE PTR [rax],al
  40046c:	00 00                	add    BYTE PTR [rax],al
  40046e:	00 00                	add    BYTE PTR [rax],al
  400470:	75 1a                	jne    40048c <_init-0xb74>
  400472:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
  400478:	40 00 00             	add    BYTE PTR [rax],al
  40047b:	00 00                	add    BYTE PTR [rax],al
  40047d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000400480 <.rela.dyn>:
  400480:	f0 3f                	lock (bad) 
  400482:	40 00 00             	add    BYTE PTR [rax],al
  400485:	00 00                	add    BYTE PTR [rax],al
  400487:	00 06                	add    BYTE PTR [rsi],al
  400489:	00 00                	add    BYTE PTR [rax],al
  40048b:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...
  400496:	00 00                	add    BYTE PTR [rax],al
  400498:	f8                   	clc    
  400499:	3f                   	(bad)  
  40049a:	40 00 00             	add    BYTE PTR [rax],al
  40049d:	00 00                	add    BYTE PTR [rax],al
  40049f:	00 06                	add    BYTE PTR [rsi],al
  4004a1:	00 00                	add    BYTE PTR [rax],al
  4004a3:	00 06                	add    BYTE PTR [rsi],al
	...
  4004ad:	00 00                	add    BYTE PTR [rax],al
  4004af:	00 60 40             	add    BYTE PTR [rax+0x40],ah
  4004b2:	40 00 00             	add    BYTE PTR [rax],al
  4004b5:	00 00                	add    BYTE PTR [rax],al
  4004b7:	00 05 00 00 00 07    	add    BYTE PTR [rip+0x7000000],al        # 74004bd <_end+0x6ffc44d>
	...

Disassembly of section .rela.plt:

00000000004004c8 <.rela.plt>:
  4004c8:	18 40 40             	sbb    BYTE PTR [rax+0x40],al
  4004cb:	00 00                	add    BYTE PTR [rax],al
  4004cd:	00 00                	add    BYTE PTR [rax],al
  4004cf:	00 07                	add    BYTE PTR [rdi],al
  4004d1:	00 00                	add    BYTE PTR [rax],al
  4004d3:	00 01                	add    BYTE PTR [rcx],al
	...
  4004dd:	00 00                	add    BYTE PTR [rax],al
  4004df:	00 20                	add    BYTE PTR [rax],ah
  4004e1:	40                   	rex
  4004e2:	40 00 00             	add    BYTE PTR [rax],al
  4004e5:	00 00                	add    BYTE PTR [rax],al
  4004e7:	00 07                	add    BYTE PTR [rdi],al
  4004e9:	00 00                	add    BYTE PTR [rax],al
  4004eb:	00 02                	add    BYTE PTR [rdx],al
	...
  4004f5:	00 00                	add    BYTE PTR [rax],al
  4004f7:	00 28                	add    BYTE PTR [rax],ch
  4004f9:	40                   	rex
  4004fa:	40 00 00             	add    BYTE PTR [rax],al
  4004fd:	00 00                	add    BYTE PTR [rax],al
  4004ff:	00 07                	add    BYTE PTR [rdi],al
  400501:	00 00                	add    BYTE PTR [rax],al
  400503:	00 03                	add    BYTE PTR [rbx],al
	...
  40050d:	00 00                	add    BYTE PTR [rax],al
  40050f:	00 30                	add    BYTE PTR [rax],dh
  400511:	40                   	rex
  400512:	40 00 00             	add    BYTE PTR [rax],al
  400515:	00 00                	add    BYTE PTR [rax],al
  400517:	00 07                	add    BYTE PTR [rdi],al
  400519:	00 00                	add    BYTE PTR [rax],al
  40051b:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 400521 <_init-0xadf>
  400521:	00 00                	add    BYTE PTR [rax],al
  400523:	00 00                	add    BYTE PTR [rax],al
  400525:	00 00                	add    BYTE PTR [rax],al
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
  401026:	ff 25 e4 2f 00 00    	jmp    QWORD PTR [rip+0x2fe4]        # 404010 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401030 <strncmp@plt>:
  401030:	ff 25 e2 2f 00 00    	jmp    QWORD PTR [rip+0x2fe2]        # 404018 <strncmp@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <.plt>

0000000000401040 <strcpy@plt>:
  401040:	ff 25 da 2f 00 00    	jmp    QWORD PTR [rip+0x2fda]        # 404020 <strcpy@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <.plt>

0000000000401050 <puts@plt>:
  401050:	ff 25 d2 2f 00 00    	jmp    QWORD PTR [rip+0x2fd2]        # 404028 <puts@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <.plt>

0000000000401060 <fprintf@plt>:
  401060:	ff 25 ca 2f 00 00    	jmp    QWORD PTR [rip+0x2fca]        # 404030 <fprintf@GLIBC_2.2.5>
  401066:	68 03 00 00 00       	push   0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <.plt>

Disassembly of section .text:

0000000000401070 <_start>:
  401070:	f3 0f 1e fa          	endbr64 
  401074:	31 ed                	xor    ebp,ebp
  401076:	49 89 d1             	mov    r9,rdx
  401079:	5e                   	pop    rsi
  40107a:	48 89 e2             	mov    rdx,rsp
  40107d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
  401081:	50                   	push   rax
  401082:	54                   	push   rsp
  401083:	49 c7 c0 80 12 40 00 	mov    r8,0x401280
  40108a:	48 c7 c1 10 12 40 00 	mov    rcx,0x401210
  401091:	48 c7 c7 b3 11 40 00 	mov    rdi,0x4011b3
  401098:	ff 15 52 2f 00 00    	call   QWORD PTR [rip+0x2f52]        # 403ff0 <__libc_start_main@GLIBC_2.2.5>
  40109e:	f4                   	hlt    
  40109f:	90                   	nop

00000000004010a0 <_dl_relocate_static_pie>:
  4010a0:	f3 0f 1e fa          	endbr64 
  4010a4:	c3                   	ret    
  4010a5:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
  4010ac:	00 00 00 
  4010af:	90                   	nop

00000000004010b0 <deregister_tm_clones>:
  4010b0:	b8 48 40 40 00       	mov    eax,0x404048
  4010b5:	48 3d 48 40 40 00    	cmp    rax,0x404048
  4010bb:	74 13                	je     4010d0 <deregister_tm_clones+0x20>
  4010bd:	b8 00 00 00 00       	mov    eax,0x0
  4010c2:	48 85 c0             	test   rax,rax
  4010c5:	74 09                	je     4010d0 <deregister_tm_clones+0x20>
  4010c7:	bf 48 40 40 00       	mov    edi,0x404048
  4010cc:	ff e0                	jmp    rax
  4010ce:	66 90                	xchg   ax,ax
  4010d0:	c3                   	ret    
  4010d1:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
  4010d8:	00 00 00 00 
  4010dc:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

00000000004010e0 <register_tm_clones>:
  4010e0:	be 48 40 40 00       	mov    esi,0x404048
  4010e5:	48 81 ee 48 40 40 00 	sub    rsi,0x404048
  4010ec:	48 89 f0             	mov    rax,rsi
  4010ef:	48 c1 ee 3f          	shr    rsi,0x3f
  4010f3:	48 c1 f8 03          	sar    rax,0x3
  4010f7:	48 01 c6             	add    rsi,rax
  4010fa:	48 d1 fe             	sar    rsi,1
  4010fd:	74 11                	je     401110 <register_tm_clones+0x30>
  4010ff:	b8 00 00 00 00       	mov    eax,0x0
  401104:	48 85 c0             	test   rax,rax
  401107:	74 07                	je     401110 <register_tm_clones+0x30>
  401109:	bf 48 40 40 00       	mov    edi,0x404048
  40110e:	ff e0                	jmp    rax
  401110:	c3                   	ret    
  401111:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
  401118:	00 00 00 00 
  40111c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401120 <__do_global_dtors_aux>:
  401120:	f3 0f 1e fa          	endbr64 
  401124:	80 3d 3d 2f 00 00 00 	cmp    BYTE PTR [rip+0x2f3d],0x0        # 404068 <completed.8061>
  40112b:	75 13                	jne    401140 <__do_global_dtors_aux+0x20>
  40112d:	55                   	push   rbp
  40112e:	48 89 e5             	mov    rbp,rsp
  401131:	e8 7a ff ff ff       	call   4010b0 <deregister_tm_clones>
  401136:	c6 05 2b 2f 00 00 01 	mov    BYTE PTR [rip+0x2f2b],0x1        # 404068 <completed.8061>
  40113d:	5d                   	pop    rbp
  40113e:	c3                   	ret    
  40113f:	90                   	nop
  401140:	c3                   	ret    
  401141:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
  401148:	00 00 00 00 
  40114c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401150 <frame_dummy>:
  401150:	f3 0f 1e fa          	endbr64 
  401154:	eb 8a                	jmp    4010e0 <register_tm_clones>

0000000000401156 <win>:
  401156:	55                   	push   rbp
  401157:	48 89 e5             	mov    rbp,rsp
  40115a:	bf 04 20 40 00       	mov    edi,0x402004
  40115f:	e8 ec fe ff ff       	call   401050 <puts@plt>
  401164:	90                   	nop
  401165:	5d                   	pop    rbp
  401166:	c3                   	ret    

0000000000401167 <func>:
  401167:	55                   	push   rbp
  401168:	48 89 e5             	mov    rbp,rsp
  40116b:	48 83 ec 50          	sub    rsp,0x50
  40116f:	48 89 7d b8          	mov    QWORD PTR [rbp-0x48],rdi
  401173:	48 8b 45 b8          	mov    rax,QWORD PTR [rbp-0x48]
  401177:	48 83 c0 02          	add    rax,0x2
  40117b:	ba 05 00 00 00       	mov    edx,0x5
  401180:	be 13 20 40 00       	mov    esi,0x402013
  401185:	48 89 c7             	mov    rdi,rax
  401188:	e8 a3 fe ff ff       	call   401030 <strncmp@plt>
  40118d:	85 c0                	test   eax,eax
  40118f:	74 0c                	je     40119d <func+0x36>
  401191:	bf 19 20 40 00       	mov    edi,0x402019
  401196:	e8 b5 fe ff ff       	call   401050 <puts@plt>
  40119b:	eb 13                	jmp    4011b0 <func+0x49>
  40119d:	48 8b 55 b8          	mov    rdx,QWORD PTR [rbp-0x48]
  4011a1:	48 8d 45 c0          	lea    rax,[rbp-0x40]
  4011a5:	48 89 d6             	mov    rsi,rdx
  4011a8:	48 89 c7             	mov    rdi,rax
  4011ab:	e8 90 fe ff ff       	call   401040 <strcpy@plt>
  4011b0:	90                   	nop
  4011b1:	c9                   	leave  
  4011b2:	c3                   	ret    

00000000004011b3 <main>:
  4011b3:	55                   	push   rbp
  4011b4:	48 89 e5             	mov    rbp,rsp
  4011b7:	48 83 ec 10          	sub    rsp,0x10
  4011bb:	89 7d fc             	mov    DWORD PTR [rbp-0x4],edi
  4011be:	48 89 75 f0          	mov    QWORD PTR [rbp-0x10],rsi
  4011c2:	83 7d fc 02          	cmp    DWORD PTR [rbp-0x4],0x2
  4011c6:	74 27                	je     4011ef <main+0x3c>
  4011c8:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
  4011cc:	48 8b 10             	mov    rdx,QWORD PTR [rax]
  4011cf:	48 8b 05 8a 2e 00 00 	mov    rax,QWORD PTR [rip+0x2e8a]        # 404060 <stderr@@GLIBC_2.2.5>
  4011d6:	be 36 20 40 00       	mov    esi,0x402036
  4011db:	48 89 c7             	mov    rdi,rax
  4011de:	b8 00 00 00 00       	mov    eax,0x0
  4011e3:	e8 78 fe ff ff       	call   401060 <fprintf@plt>
  4011e8:	b8 01 00 00 00       	mov    eax,0x1
  4011ed:	eb 18                	jmp    401207 <main+0x54>
  4011ef:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
  4011f3:	48 83 c0 08          	add    rax,0x8
  4011f7:	48 8b 00             	mov    rax,QWORD PTR [rax]
  4011fa:	48 89 c7             	mov    rdi,rax
  4011fd:	e8 65 ff ff ff       	call   401167 <func>
  401202:	b8 00 00 00 00       	mov    eax,0x0
  401207:	c9                   	leave  
  401208:	c3                   	ret    
  401209:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

0000000000401210 <__libc_csu_init>:
  401210:	f3 0f 1e fa          	endbr64 
  401214:	41 57                	push   r15
  401216:	4c 8d 3d f3 2b 00 00 	lea    r15,[rip+0x2bf3]        # 403e10 <__frame_dummy_init_array_entry>
  40121d:	41 56                	push   r14
  40121f:	49 89 d6             	mov    r14,rdx
  401222:	41 55                	push   r13
  401224:	49 89 f5             	mov    r13,rsi
  401227:	41 54                	push   r12
  401229:	41 89 fc             	mov    r12d,edi
  40122c:	55                   	push   rbp
  40122d:	48 8d 2d e4 2b 00 00 	lea    rbp,[rip+0x2be4]        # 403e18 <__do_global_dtors_aux_fini_array_entry>
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
  401275:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
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
  402004:	47 72 65             	rex.RXB jb 40206c <__GNU_EH_FRAME_HDR+0x24>
  402007:	61                   	(bad)  
  402008:	74 20                	je     40202a <_IO_stdin_used+0x2a>
  40200a:	73 75                	jae    402081 <__GNU_EH_FRAME_HDR+0x39>
  40200c:	63 63 65             	movsxd esp,DWORD PTR [rbx+0x65]
  40200f:	73 73                	jae    402084 <__GNU_EH_FRAME_HDR+0x3c>
  402011:	21 00                	and    DWORD PTR [rax],eax
  402013:	48                   	rex.W
  402014:	45                   	rex.RB
  402015:	4c                   	rex.WR
  402016:	4c                   	rex.WR
  402017:	4f 00 59 6f          	rex.WRXB add BYTE PTR [r9+0x6f],r11b
  40201b:	75 20                	jne    40203d <_IO_stdin_used+0x3d>
  40201d:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  40201e:	65 65 64 20 74 6f 20 	gs gs and BYTE PTR fs:[rdi+rbp*2+0x20],dh
  402025:	73 61                	jae    402088 <__GNU_EH_FRAME_HDR+0x40>
  402027:	79 20                	jns    402049 <__GNU_EH_FRAME_HDR+0x1>
  402029:	48                   	rex.W
  40202a:	45                   	rex.RB
  40202b:	4c                   	rex.WR
  40202c:	4c                   	rex.WR
  40202d:	4f 20 74 6f 20       	rex.WRXB and BYTE PTR [r15+r13*2+0x20],r14b
  402032:	6d                   	ins    DWORD PTR es:[rdi],dx
  402033:	65 21 00             	and    DWORD PTR gs:[rax],eax
  402036:	55                   	push   rbp
  402037:	73 61                	jae    40209a <__GNU_EH_FRAME_HDR+0x52>
  402039:	67 65 3a 20          	cmp    ah,BYTE PTR gs:[eax]
  40203d:	25 73 20 73 74       	and    eax,0x74732073
  402042:	72 69                	jb     4020ad <__GNU_EH_FRAME_HDR+0x65>
  402044:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  402045:	67 0a 00             	or     al,BYTE PTR [eax]

Disassembly of section .eh_frame_hdr:

0000000000402048 <__GNU_EH_FRAME_HDR>:
  402048:	01 1b                	add    DWORD PTR [rbx],ebx
  40204a:	03 3b                	add    edi,DWORD PTR [rbx]
  40204c:	4c 00 00             	rex.WR add BYTE PTR [rax],r8b
  40204f:	00 08                	add    BYTE PTR [rax],cl
  402051:	00 00                	add    BYTE PTR [rax],al
  402053:	00 d8                	add    al,bl
  402055:	ef                   	out    dx,eax
  402056:	ff                   	(bad)  
  402057:	ff 90 00 00 00 28    	call   QWORD PTR [rax+0x28000000]
  40205d:	f0 ff                	lock (bad) 
  40205f:	ff 68 00             	jmp    FWORD PTR [rax+0x0]
  402062:	00 00                	add    BYTE PTR [rax],al
  402064:	58                   	pop    rax
  402065:	f0 ff                	lock (bad) 
  402067:	ff                   	(bad)  
  402068:	7c 00                	jl     40206a <__GNU_EH_FRAME_HDR+0x22>
  40206a:	00 00                	add    BYTE PTR [rax],al
  40206c:	0e                   	(bad)  
  40206d:	f1                   	icebp  
  40206e:	ff                   	(bad)  
  40206f:	ff                   	(bad)  
  402070:	b8 00 00 00 1f       	mov    eax,0x1f000000
  402075:	f1                   	icebp  
  402076:	ff                   	(bad)  
  402077:	ff                   	(bad)  
  402078:	d8 00                	fadd   DWORD PTR [rax]
  40207a:	00 00                	add    BYTE PTR [rax],al
  40207c:	6b f1 ff             	imul   esi,ecx,0xffffffff
  40207f:	ff                   	(bad)  
  402080:	f8                   	clc    
  402081:	00 00                	add    BYTE PTR [rax],al
  402083:	00 c8                	add    al,cl
  402085:	f1                   	icebp  
  402086:	ff                   	(bad)  
  402087:	ff 18                	call   FWORD PTR [rax]
  402089:	01 00                	add    DWORD PTR [rax],eax
  40208b:	00 38                	add    BYTE PTR [rax],bh
  40208d:	f2 ff                	repnz (bad) 
  40208f:	ff 60 01             	jmp    QWORD PTR [rax+0x1]
	...

Disassembly of section .eh_frame:

0000000000402098 <__FRAME_END__-0x124>:
  402098:	14 00                	adc    al,0x0
  40209a:	00 00                	add    BYTE PTR [rax],al
  40209c:	00 00                	add    BYTE PTR [rax],al
  40209e:	00 00                	add    BYTE PTR [rax],al
  4020a0:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
  4020a3:	00 01                	add    BYTE PTR [rcx],al
  4020a5:	78 10                	js     4020b7 <__GNU_EH_FRAME_HDR+0x6f>
  4020a7:	01 1b                	add    DWORD PTR [rbx],ebx
  4020a9:	0c 07                	or     al,0x7
  4020ab:	08 90 01 00 00 10    	or     BYTE PTR [rax+0x10000001],dl
  4020b1:	00 00                	add    BYTE PTR [rax],al
  4020b3:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  4020b6:	00 00                	add    BYTE PTR [rax],al
  4020b8:	b8 ef ff ff 2f       	mov    eax,0x2fffffef
  4020bd:	00 00                	add    BYTE PTR [rax],al
  4020bf:	00 00                	add    BYTE PTR [rax],al
  4020c1:	44 07                	rex.R (bad) 
  4020c3:	10 10                	adc    BYTE PTR [rax],dl
  4020c5:	00 00                	add    BYTE PTR [rax],al
  4020c7:	00 30                	add    BYTE PTR [rax],dh
  4020c9:	00 00                	add    BYTE PTR [rax],al
  4020cb:	00 d4                	add    ah,dl
  4020cd:	ef                   	out    dx,eax
  4020ce:	ff                   	(bad)  
  4020cf:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 4020d5 <__GNU_EH_FRAME_HDR+0x8d>
  4020d5:	00 00                	add    BYTE PTR [rax],al
  4020d7:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
  4020da:	00 00                	add    BYTE PTR [rax],al
  4020dc:	44 00 00             	add    BYTE PTR [rax],r8b
  4020df:	00 40 ef             	add    BYTE PTR [rax-0x11],al
  4020e2:	ff                   	(bad)  
  4020e3:	ff 50 00             	call   QWORD PTR [rax+0x0]
  4020e6:	00 00                	add    BYTE PTR [rax],al
  4020e8:	00 0e                	add    BYTE PTR [rsi],cl
  4020ea:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
  4020ed:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
  4020f0:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
  4020f3:	80 00 3f             	add    BYTE PTR [rax],0x3f
  4020f6:	1a 3b                	sbb    bh,BYTE PTR [rbx]
  4020f8:	2a 33                	sub    dh,BYTE PTR [rbx]
  4020fa:	24 22                	and    al,0x22
  4020fc:	00 00                	add    BYTE PTR [rax],al
  4020fe:	00 00                	add    BYTE PTR [rax],al
  402100:	1c 00                	sbb    al,0x0
  402102:	00 00                	add    BYTE PTR [rax],al
  402104:	6c                   	ins    BYTE PTR es:[rdi],dx
  402105:	00 00                	add    BYTE PTR [rax],al
  402107:	00 4e f0             	add    BYTE PTR [rsi-0x10],cl
  40210a:	ff                   	(bad)  
  40210b:	ff 11                	call   QWORD PTR [rcx]
  40210d:	00 00                	add    BYTE PTR [rax],al
  40210f:	00 00                	add    BYTE PTR [rax],al
  402111:	41 0e                	rex.B (bad) 
  402113:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  402119:	4c 0c 07             	rex.WR or al,0x7
  40211c:	08 00                	or     BYTE PTR [rax],al
  40211e:	00 00                	add    BYTE PTR [rax],al
  402120:	1c 00                	sbb    al,0x0
  402122:	00 00                	add    BYTE PTR [rax],al
  402124:	8c 00                	mov    WORD PTR [rax],es
  402126:	00 00                	add    BYTE PTR [rax],al
  402128:	3f                   	(bad)  
  402129:	f0 ff                	lock (bad) 
  40212b:	ff 4c 00 00          	dec    DWORD PTR [rax+rax*1+0x0]
  40212f:	00 00                	add    BYTE PTR [rax],al
  402131:	41 0e                	rex.B (bad) 
  402133:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  402139:	02 47 0c             	add    al,BYTE PTR [rdi+0xc]
  40213c:	07                   	(bad)  
  40213d:	08 00                	or     BYTE PTR [rax],al
  40213f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  402142:	00 00                	add    BYTE PTR [rax],al
  402144:	ac                   	lods   al,BYTE PTR ds:[rsi]
  402145:	00 00                	add    BYTE PTR [rax],al
  402147:	00 6b f0             	add    BYTE PTR [rbx-0x10],ch
  40214a:	ff                   	(bad)  
  40214b:	ff 56 00             	call   QWORD PTR [rsi+0x0]
  40214e:	00 00                	add    BYTE PTR [rax],al
  402150:	00 41 0e             	add    BYTE PTR [rcx+0xe],al
  402153:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  402159:	02 51 0c             	add    dl,BYTE PTR [rcx+0xc]
  40215c:	07                   	(bad)  
  40215d:	08 00                	or     BYTE PTR [rax],al
  40215f:	00 44 00 00          	add    BYTE PTR [rax+rax*1+0x0],al
  402163:	00 cc                	add    ah,cl
  402165:	00 00                	add    BYTE PTR [rax],al
  402167:	00 a8 f0 ff ff 65    	add    BYTE PTR [rax+0x65fffff0],ch
  40216d:	00 00                	add    BYTE PTR [rax],al
  40216f:	00 00                	add    BYTE PTR [rax],al
  402171:	46 0e                	rex.RX (bad) 
  402173:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
  402179:	8e 03                	mov    es,WORD PTR [rbx]
  40217b:	45 0e                	rex.RB (bad) 
  40217d:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
  402183:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff86702fcd <_end+0xffffffff862fef5d>
  402189:	06                   	(bad)  
  40218a:	48 0e                	rex.W (bad) 
  40218c:	38 83 07 47 0e 40    	cmp    BYTE PTR [rbx+0x400e4707],al
  402192:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  402193:	0e                   	(bad)  
  402194:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
  402197:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
  40219a:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
  40219d:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
  4021a0:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
  4021a3:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
  4021a6:	08 00                	or     BYTE PTR [rax],al
  4021a8:	10 00                	adc    BYTE PTR [rax],al
  4021aa:	00 00                	add    BYTE PTR [rax],al
  4021ac:	14 01                	adc    al,0x1
  4021ae:	00 00                	add    BYTE PTR [rax],al
  4021b0:	d0 f0                	shl    al,1
  4021b2:	ff                   	(bad)  
  4021b3:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 4021b9 <__GNU_EH_FRAME_HDR+0x171>
  4021b9:	00 00                	add    BYTE PTR [rax],al
	...

00000000004021bc <__FRAME_END__>:
  4021bc:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000403e10 <__frame_dummy_init_array_entry>:
  403e10:	50                   	push   rax
  403e11:	11 40 00             	adc    DWORD PTR [rax+0x0],eax
  403e14:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000403e18 <__do_global_dtors_aux_fini_array_entry>:
  403e18:	20 11                	and    BYTE PTR [rcx],dl
  403e1a:	40 00 00             	add    BYTE PTR [rax],al
  403e1d:	00 00                	add    BYTE PTR [rax],al
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
  403e3a:	40 00 00             	add    BYTE PTR [rax],al
  403e3d:	00 00                	add    BYTE PTR [rax],al
  403e3f:	00 0d 00 00 00 00    	add    BYTE PTR [rip+0x0],cl        # 403e45 <_DYNAMIC+0x25>
  403e45:	00 00                	add    BYTE PTR [rax],al
  403e47:	00 88 12 40 00 00    	add    BYTE PTR [rax+0x4012],cl
  403e4d:	00 00                	add    BYTE PTR [rax],al
  403e4f:	00 19                	add    BYTE PTR [rcx],bl
  403e51:	00 00                	add    BYTE PTR [rax],al
  403e53:	00 00                	add    BYTE PTR [rax],al
  403e55:	00 00                	add    BYTE PTR [rax],al
  403e57:	00 10                	add    BYTE PTR [rax],dl
  403e59:	3e 40 00 00          	add    BYTE PTR ds:[rax],al
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
  403e79:	3e 40 00 00          	add    BYTE PTR ds:[rax],al
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
  403e97:	00 08                	add    BYTE PTR [rax],cl
  403e99:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
  403e9c:	00 00                	add    BYTE PTR [rax],al
  403e9e:	00 00                	add    BYTE PTR [rax],al
  403ea0:	05 00 00 00 00       	add    eax,0x0
  403ea5:	00 00                	add    BYTE PTR [rax],al
  403ea7:	00 f0                	add    al,dh
  403ea9:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
  403eac:	00 00                	add    BYTE PTR [rax],al
  403eae:	00 00                	add    BYTE PTR [rax],al
  403eb0:	06                   	(bad)  
  403eb1:	00 00                	add    BYTE PTR [rax],al
  403eb3:	00 00                	add    BYTE PTR [rax],al
  403eb5:	00 00                	add    BYTE PTR [rax],al
  403eb7:	00 30                	add    BYTE PTR [rax],dh
  403eb9:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
  403ebc:	00 00                	add    BYTE PTR [rax],al
  403ebe:	00 00                	add    BYTE PTR [rax],al
  403ec0:	0a 00                	or     al,BYTE PTR [rax]
  403ec2:	00 00                	add    BYTE PTR [rax],al
  403ec4:	00 00                	add    BYTE PTR [rax],al
  403ec6:	00 00                	add    BYTE PTR [rax],al
  403ec8:	5b                   	pop    rbx
  403ec9:	00 00                	add    BYTE PTR [rax],al
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
  403efa:	40 00 00             	add    BYTE PTR [rax],al
  403efd:	00 00                	add    BYTE PTR [rax],al
  403eff:	00 02                	add    BYTE PTR [rdx],al
  403f01:	00 00                	add    BYTE PTR [rax],al
  403f03:	00 00                	add    BYTE PTR [rax],al
  403f05:	00 00                	add    BYTE PTR [rax],al
  403f07:	00 60 00             	add    BYTE PTR [rax+0x0],ah
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
  403f27:	00 c8                	add    al,cl
  403f29:	04 40                	add    al,0x40
  403f2b:	00 00                	add    BYTE PTR [rax],al
  403f2d:	00 00                	add    BYTE PTR [rax],al
  403f2f:	00 07                	add    BYTE PTR [rdi],al
  403f31:	00 00                	add    BYTE PTR [rax],al
  403f33:	00 00                	add    BYTE PTR [rax],al
  403f35:	00 00                	add    BYTE PTR [rax],al
  403f37:	00 80 04 40 00 00    	add    BYTE PTR [rax+0x4004],al
  403f3d:	00 00                	add    BYTE PTR [rax],al
  403f3f:	00 08                	add    BYTE PTR [rax],cl
  403f41:	00 00                	add    BYTE PTR [rax],al
  403f43:	00 00                	add    BYTE PTR [rax],al
  403f45:	00 00                	add    BYTE PTR [rax],al
  403f47:	00 48 00             	add    BYTE PTR [rax+0x0],cl
  403f4a:	00 00                	add    BYTE PTR [rax],al
  403f4c:	00 00                	add    BYTE PTR [rax],al
  403f4e:	00 00                	add    BYTE PTR [rax],al
  403f50:	09 00                	or     DWORD PTR [rax],eax
  403f52:	00 00                	add    BYTE PTR [rax],al
  403f54:	00 00                	add    BYTE PTR [rax],al
  403f56:	00 00                	add    BYTE PTR [rax],al
  403f58:	18 00                	sbb    BYTE PTR [rax],al
  403f5a:	00 00                	add    BYTE PTR [rax],al
  403f5c:	00 00                	add    BYTE PTR [rax],al
  403f5e:	00 00                	add    BYTE PTR [rax],al
  403f60:	fe                   	(bad)  
  403f61:	ff                   	(bad)  
  403f62:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  403f65:	00 00                	add    BYTE PTR [rax],al
  403f67:	00 60 04             	add    BYTE PTR [rax+0x4],ah
  403f6a:	40 00 00             	add    BYTE PTR [rax],al
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
  403f87:	00 4c 04 40          	add    BYTE PTR [rsp+rax*1+0x40],cl
	...

Disassembly of section .got:

0000000000403ff0 <.got>:
	...

Disassembly of section .got.plt:

0000000000404000 <_GLOBAL_OFFSET_TABLE_>:
  404000:	20 3e                	and    BYTE PTR [rsi],bh
  404002:	40 00 00             	add    BYTE PTR [rax],al
	...
  404015:	00 00                	add    BYTE PTR [rax],al
  404017:	00 36                	add    BYTE PTR [rsi],dh
  404019:	10 40 00             	adc    BYTE PTR [rax+0x0],al
  40401c:	00 00                	add    BYTE PTR [rax],al
  40401e:	00 00                	add    BYTE PTR [rax],al
  404020:	46 10 40 00          	rex.RX adc BYTE PTR [rax+0x0],r8b
  404024:	00 00                	add    BYTE PTR [rax],al
  404026:	00 00                	add    BYTE PTR [rax],al
  404028:	56                   	push   rsi
  404029:	10 40 00             	adc    BYTE PTR [rax+0x0],al
  40402c:	00 00                	add    BYTE PTR [rax],al
  40402e:	00 00                	add    BYTE PTR [rax],al
  404030:	66 10 40 00          	data16 adc BYTE PTR [rax+0x0],al
  404034:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .data:

0000000000404038 <__data_start>:
	...

0000000000404040 <__dso_handle>:
	...

Disassembly of section .bss:

0000000000404060 <stderr@@GLIBC_2.2.5>:
	...

0000000000404068 <completed.8061>:
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
  11:	30 2d 31 75 62 75    	xor    BYTE PTR [rip+0x75627531],ch        # 75627548 <_end+0x752234d8>
  17:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  18:	74 75                	je     8f <_init-0x400f71>
  1a:	31 7e 32             	xor    DWORD PTR [rsi+0x32],edi
  1d:	30 2e                	xor    BYTE PTR [rsi],ch
  1f:	30 34 2e             	xor    BYTE PTR [rsi+rbp*1],dh
  22:	31 29                	xor    DWORD PTR [rcx],ebp
  24:	20 39                	and    BYTE PTR [rcx],bh
  26:	2e 34 2e             	cs xor al,0x2e
  29:	30 00                	xor    BYTE PTR [rax],al
