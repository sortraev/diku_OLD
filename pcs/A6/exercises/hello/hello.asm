
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
  4002d3:	00 8e 77 02 4b 41    	add    BYTE PTR [rsi+0x414b0277],cl
  4002d9:	c5 2f 16             	(bad)
  4002dc:	24 e9                	and    al,0xe9
  4002de:	b4 e0                	mov    ah,0xe0
  4002e0:	46 e2 44             	rex.RX loop 400327 <_init-0xcd9>
  4002e3:	98                   	cwde   
  4002e4:	17                   	(bad)  
  4002e5:	b3 af                	mov    bl,0xaf
  4002e7:	14                   	.byte 0x14

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
  400308:	01 00                	add    DWORD PTR [rax],eax
  40030a:	00 00                	add    BYTE PTR [rax],al
  40030c:	01 00                	add    DWORD PTR [rax],eax
  40030e:	00 00                	add    BYTE PTR [rax],al
  400310:	01 00                	add    DWORD PTR [rax],eax
	...

Disassembly of section .dynsym:

0000000000400328 <.dynsym>:
	...
  400340:	15 00 00 00 12       	adc    eax,0x12000000
	...
  400355:	00 00                	add    BYTE PTR [rax],al
  400357:	00 1a                	add    BYTE PTR [rdx],bl
  400359:	00 00                	add    BYTE PTR [rax],al
  40035b:	00 12                	add    BYTE PTR [rdx],dl
	...
  40036d:	00 00                	add    BYTE PTR [rax],al
  40036f:	00 38                	add    BYTE PTR [rax],bh
  400371:	00 00                	add    BYTE PTR [rax],al
  400373:	00 20                	add    BYTE PTR [rax],ah
	...
  400385:	00 00                	add    BYTE PTR [rax],al
  400387:	00 0b                	add    BYTE PTR [rbx],cl
  400389:	00 00                	add    BYTE PTR [rax],al
  40038b:	00 12                	add    BYTE PTR [rdx],dl
	...
  40039d:	00 00                	add    BYTE PTR [rax],al
  40039f:	00 10                	add    BYTE PTR [rax],dl
  4003a1:	00 00                	add    BYTE PTR [rax],al
  4003a3:	00 12                	add    BYTE PTR [rdx],dl
	...

Disassembly of section .dynstr:

00000000004003b8 <.dynstr>:
  4003b8:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
  4003bc:	63 2e                	movsxd ebp,DWORD PTR [rsi]
  4003be:	73 6f                	jae    40042f <_init-0xbd1>
  4003c0:	2e 36 00 67 65       	cs add BYTE PTR ss:[rdi+0x65],ah
  4003c5:	74 73                	je     40043a <_init-0xbc6>
  4003c7:	00 65 78             	add    BYTE PTR [rbp+0x78],ah
  4003ca:	69 74 00 70 75 74 73 	imul   esi,DWORD PTR [rax+rax*1+0x70],0x737475
  4003d1:	00 
  4003d2:	5f                   	pop    rdi
  4003d3:	5f                   	pop    rdi
  4003d4:	6c                   	ins    BYTE PTR es:[rdi],dx
  4003d5:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
  4003dc:	72 74                	jb     400452 <_init-0xbae>
  4003de:	5f                   	pop    rdi
  4003df:	6d                   	ins    DWORD PTR es:[rdi],dx
  4003e0:	61                   	(bad)  
  4003e1:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
  4003e8:	43 5f                	rex.XB pop r15
  4003ea:	32 2e                	xor    ch,BYTE PTR [rsi]
  4003ec:	32 2e                	xor    ch,BYTE PTR [rsi]
  4003ee:	35 00 5f 5f 67       	xor    eax,0x675f5f00
  4003f3:	6d                   	ins    DWORD PTR es:[rdi],dx
  4003f4:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  4003f5:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  4003f6:	5f                   	pop    rdi
  4003f7:	73 74                	jae    40046d <_init-0xb93>
  4003f9:	61                   	(bad)  
  4003fa:	72 74                	jb     400470 <_init-0xb90>
  4003fc:	5f                   	pop    rdi
  4003fd:	5f                   	pop    rdi
	...

Disassembly of section .gnu.version:

0000000000400400 <.gnu.version>:
  400400:	00 00                	add    BYTE PTR [rax],al
  400402:	02 00                	add    al,BYTE PTR [rax]
  400404:	02 00                	add    al,BYTE PTR [rax]
  400406:	00 00                	add    BYTE PTR [rax],al
  400408:	02 00                	add    al,BYTE PTR [rax]
  40040a:	02 00                	add    al,BYTE PTR [rax]

Disassembly of section .gnu.version_r:

0000000000400410 <.gnu.version_r>:
  400410:	01 00                	add    DWORD PTR [rax],eax
  400412:	01 00                	add    DWORD PTR [rax],eax
  400414:	01 00                	add    DWORD PTR [rax],eax
  400416:	00 00                	add    BYTE PTR [rax],al
  400418:	10 00                	adc    BYTE PTR [rax],al
  40041a:	00 00                	add    BYTE PTR [rax],al
  40041c:	00 00                	add    BYTE PTR [rax],al
  40041e:	00 00                	add    BYTE PTR [rax],al
  400420:	75 1a                	jne    40043c <_init-0xbc4>
  400422:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
  400428:	2c 00                	sub    al,0x0
  40042a:	00 00                	add    BYTE PTR [rax],al
  40042c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000400430 <.rela.dyn>:
  400430:	f0 3f                	lock (bad) 
  400432:	40 00 00             	add    BYTE PTR [rax],al
  400435:	00 00                	add    BYTE PTR [rax],al
  400437:	00 06                	add    BYTE PTR [rsi],al
  400439:	00 00                	add    BYTE PTR [rax],al
  40043b:	00 02                	add    BYTE PTR [rdx],al
	...
  400445:	00 00                	add    BYTE PTR [rax],al
  400447:	00 f8                	add    al,bh
  400449:	3f                   	(bad)  
  40044a:	40 00 00             	add    BYTE PTR [rax],al
  40044d:	00 00                	add    BYTE PTR [rax],al
  40044f:	00 06                	add    BYTE PTR [rsi],al
  400451:	00 00                	add    BYTE PTR [rax],al
  400453:	00 03                	add    BYTE PTR [rbx],al
	...

Disassembly of section .rela.plt:

0000000000400460 <.rela.plt>:
  400460:	18 40 40             	sbb    BYTE PTR [rax+0x40],al
  400463:	00 00                	add    BYTE PTR [rax],al
  400465:	00 00                	add    BYTE PTR [rax],al
  400467:	00 07                	add    BYTE PTR [rdi],al
  400469:	00 00                	add    BYTE PTR [rax],al
  40046b:	00 01                	add    BYTE PTR [rcx],al
	...
  400475:	00 00                	add    BYTE PTR [rax],al
  400477:	00 20                	add    BYTE PTR [rax],ah
  400479:	40                   	rex
  40047a:	40 00 00             	add    BYTE PTR [rax],al
  40047d:	00 00                	add    BYTE PTR [rax],al
  40047f:	00 07                	add    BYTE PTR [rdi],al
  400481:	00 00                	add    BYTE PTR [rax],al
  400483:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...
  40048e:	00 00                	add    BYTE PTR [rax],al
  400490:	28 40 40             	sub    BYTE PTR [rax+0x40],al
  400493:	00 00                	add    BYTE PTR [rax],al
  400495:	00 00                	add    BYTE PTR [rax],al
  400497:	00 07                	add    BYTE PTR [rdi],al
  400499:	00 00                	add    BYTE PTR [rax],al
  40049b:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 4004a1 <_init-0xb5f>
  4004a1:	00 00                	add    BYTE PTR [rax],al
  4004a3:	00 00                	add    BYTE PTR [rax],al
  4004a5:	00 00                	add    BYTE PTR [rax],al
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

0000000000401030 <puts@plt>:
  401030:	ff 25 e2 2f 00 00    	jmp    QWORD PTR [rip+0x2fe2]        # 404018 <puts@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <.plt>

0000000000401040 <gets@plt>:
  401040:	ff 25 da 2f 00 00    	jmp    QWORD PTR [rip+0x2fda]        # 404020 <gets@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <.plt>

0000000000401050 <exit@plt>:
  401050:	ff 25 d2 2f 00 00    	jmp    QWORD PTR [rip+0x2fd2]        # 404028 <exit@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <.plt>

Disassembly of section .text:

0000000000401060 <_start>:
  401060:	f3 0f 1e fa          	endbr64 
  401064:	31 ed                	xor    ebp,ebp
  401066:	49 89 d1             	mov    r9,rdx
  401069:	5e                   	pop    rsi
  40106a:	48 89 e2             	mov    rdx,rsp
  40106d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
  401071:	50                   	push   rax
  401072:	54                   	push   rsp
  401073:	49 c7 c0 10 12 40 00 	mov    r8,0x401210
  40107a:	48 c7 c1 a0 11 40 00 	mov    rcx,0x4011a0
  401081:	48 c7 c7 7f 11 40 00 	mov    rdi,0x40117f
  401088:	ff 15 62 2f 00 00    	call   QWORD PTR [rip+0x2f62]        # 403ff0 <__libc_start_main@GLIBC_2.2.5>
  40108e:	f4                   	hlt    
  40108f:	90                   	nop

0000000000401090 <_dl_relocate_static_pie>:
  401090:	f3 0f 1e fa          	endbr64 
  401094:	c3                   	ret    
  401095:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
  40109c:	00 00 00 
  40109f:	90                   	nop

00000000004010a0 <deregister_tm_clones>:
  4010a0:	b8 40 40 40 00       	mov    eax,0x404040
  4010a5:	48 3d 40 40 40 00    	cmp    rax,0x404040
  4010ab:	74 13                	je     4010c0 <deregister_tm_clones+0x20>
  4010ad:	b8 00 00 00 00       	mov    eax,0x0
  4010b2:	48 85 c0             	test   rax,rax
  4010b5:	74 09                	je     4010c0 <deregister_tm_clones+0x20>
  4010b7:	bf 40 40 40 00       	mov    edi,0x404040
  4010bc:	ff e0                	jmp    rax
  4010be:	66 90                	xchg   ax,ax
  4010c0:	c3                   	ret    
  4010c1:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
  4010c8:	00 00 00 00 
  4010cc:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

00000000004010d0 <register_tm_clones>:
  4010d0:	be 40 40 40 00       	mov    esi,0x404040
  4010d5:	48 81 ee 40 40 40 00 	sub    rsi,0x404040
  4010dc:	48 89 f0             	mov    rax,rsi
  4010df:	48 c1 ee 3f          	shr    rsi,0x3f
  4010e3:	48 c1 f8 03          	sar    rax,0x3
  4010e7:	48 01 c6             	add    rsi,rax
  4010ea:	48 d1 fe             	sar    rsi,1
  4010ed:	74 11                	je     401100 <register_tm_clones+0x30>
  4010ef:	b8 00 00 00 00       	mov    eax,0x0
  4010f4:	48 85 c0             	test   rax,rax
  4010f7:	74 07                	je     401100 <register_tm_clones+0x30>
  4010f9:	bf 40 40 40 00       	mov    edi,0x404040
  4010fe:	ff e0                	jmp    rax
  401100:	c3                   	ret    
  401101:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
  401108:	00 00 00 00 
  40110c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401110 <__do_global_dtors_aux>:
  401110:	f3 0f 1e fa          	endbr64 
  401114:	80 3d 25 2f 00 00 00 	cmp    BYTE PTR [rip+0x2f25],0x0        # 404040 <__TMC_END__>
  40111b:	75 13                	jne    401130 <__do_global_dtors_aux+0x20>
  40111d:	55                   	push   rbp
  40111e:	48 89 e5             	mov    rbp,rsp
  401121:	e8 7a ff ff ff       	call   4010a0 <deregister_tm_clones>
  401126:	c6 05 13 2f 00 00 01 	mov    BYTE PTR [rip+0x2f13],0x1        # 404040 <__TMC_END__>
  40112d:	5d                   	pop    rbp
  40112e:	c3                   	ret    
  40112f:	90                   	nop
  401130:	c3                   	ret    
  401131:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
  401138:	00 00 00 00 
  40113c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000401140 <frame_dummy>:
  401140:	f3 0f 1e fa          	endbr64 
  401144:	eb 8a                	jmp    4010d0 <register_tm_clones>

0000000000401146 <hello>:
  401146:	55                   	push   rbp
  401147:	48 89 e5             	mov    rbp,rsp
  40114a:	bf 08 20 40 00       	mov    edi,0x402008
  40114f:	e8 dc fe ff ff       	call   401030 <puts@plt>
  401154:	bf 00 00 00 00       	mov    edi,0x0
  401159:	e8 f2 fe ff ff       	call   401050 <exit@plt>

000000000040115e <pwn_me>:
  40115e:	55                   	push   rbp
  40115f:	48 89 e5             	mov    rbp,rsp
  401162:	48 83 ec 10          	sub    rsp,0x10
  401166:	bf 31 20 40 00       	mov    edi,0x402031
  40116b:	e8 c0 fe ff ff       	call   401030 <puts@plt>
  401170:	48 8d 45 f4          	lea    rax,[rbp-0xc]
  401174:	48 89 c7             	mov    rdi,rax
  401177:	e8 c4 fe ff ff       	call   401040 <gets@plt>
  40117c:	90                   	nop
  40117d:	c9                   	leave  
  40117e:	c3                   	ret    

000000000040117f <main>:
  40117f:	55                   	push   rbp
  401180:	48 89 e5             	mov    rbp,rsp
  401183:	b8 00 00 00 00       	mov    eax,0x0
  401188:	e8 d1 ff ff ff       	call   40115e <pwn_me>
  40118d:	bf 4b 20 40 00       	mov    edi,0x40204b
  401192:	e8 99 fe ff ff       	call   401030 <puts@plt>
  401197:	b8 00 00 00 00       	mov    eax,0x0
  40119c:	5d                   	pop    rbp
  40119d:	c3                   	ret    
  40119e:	66 90                	xchg   ax,ax

00000000004011a0 <__libc_csu_init>:
  4011a0:	f3 0f 1e fa          	endbr64 
  4011a4:	41 57                	push   r15
  4011a6:	4c 8d 3d 63 2c 00 00 	lea    r15,[rip+0x2c63]        # 403e10 <__frame_dummy_init_array_entry>
  4011ad:	41 56                	push   r14
  4011af:	49 89 d6             	mov    r14,rdx
  4011b2:	41 55                	push   r13
  4011b4:	49 89 f5             	mov    r13,rsi
  4011b7:	41 54                	push   r12
  4011b9:	41 89 fc             	mov    r12d,edi
  4011bc:	55                   	push   rbp
  4011bd:	48 8d 2d 54 2c 00 00 	lea    rbp,[rip+0x2c54]        # 403e18 <__do_global_dtors_aux_fini_array_entry>
  4011c4:	53                   	push   rbx
  4011c5:	4c 29 fd             	sub    rbp,r15
  4011c8:	48 83 ec 08          	sub    rsp,0x8
  4011cc:	e8 2f fe ff ff       	call   401000 <_init>
  4011d1:	48 c1 fd 03          	sar    rbp,0x3
  4011d5:	74 1f                	je     4011f6 <__libc_csu_init+0x56>
  4011d7:	31 db                	xor    ebx,ebx
  4011d9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
  4011e0:	4c 89 f2             	mov    rdx,r14
  4011e3:	4c 89 ee             	mov    rsi,r13
  4011e6:	44 89 e7             	mov    edi,r12d
  4011e9:	41 ff 14 df          	call   QWORD PTR [r15+rbx*8]
  4011ed:	48 83 c3 01          	add    rbx,0x1
  4011f1:	48 39 dd             	cmp    rbp,rbx
  4011f4:	75 ea                	jne    4011e0 <__libc_csu_init+0x40>
  4011f6:	48 83 c4 08          	add    rsp,0x8
  4011fa:	5b                   	pop    rbx
  4011fb:	5d                   	pop    rbp
  4011fc:	41 5c                	pop    r12
  4011fe:	41 5d                	pop    r13
  401200:	41 5e                	pop    r14
  401202:	41 5f                	pop    r15
  401204:	c3                   	ret    
  401205:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
  40120c:	00 00 00 00 

0000000000401210 <__libc_csu_fini>:
  401210:	f3 0f 1e fa          	endbr64 
  401214:	c3                   	ret    

Disassembly of section .fini:

0000000000401218 <_fini>:
  401218:	f3 0f 1e fa          	endbr64 
  40121c:	48 83 ec 08          	sub    rsp,0x8
  401220:	48 83 c4 08          	add    rsp,0x8
  401224:	c3                   	ret    

Disassembly of section .rodata:

0000000000402000 <_IO_stdin_used>:
  402000:	01 00                	add    DWORD PTR [rax],eax
  402002:	02 00                	add    al,BYTE PTR [rax]
  402004:	00 00                	add    BYTE PTR [rax],al
  402006:	00 00                	add    BYTE PTR [rax],al
  402008:	22 48 65             	and    cl,BYTE PTR [rax+0x65]
  40200b:	6c                   	ins    BYTE PTR es:[rdi],dx
  40200c:	6c                   	ins    BYTE PTR es:[rdi],dx
  40200d:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  40200e:	22 2e                	and    ch,BYTE PTR [rsi]
  402010:	2e 2e 20 49 66       	cs and BYTE PTR cs:[rcx+0x66],cl
  402015:	20 79 6f             	and    BYTE PTR [rcx+0x6f],bh
  402018:	75 20                	jne    40203a <_IO_stdin_used+0x3a>
  40201a:	73 65                	jae    402081 <__GNU_EH_FRAME_HDR+0x31>
  40201c:	65 20 74 68 69       	and    BYTE PTR gs:[rax+rbp*2+0x69],dh
  402021:	73 2c                	jae    40204f <_IO_stdin_used+0x4f>
  402023:	20 79 6f             	and    BYTE PTR [rcx+0x6f],bh
  402026:	75 27                	jne    40204f <_IO_stdin_used+0x4f>
  402028:	72 65                	jb     40208f <__GNU_EH_FRAME_HDR+0x3f>
  40202a:	20 64 6f 6e          	and    BYTE PTR [rdi+rbp*2+0x6e],ah
  40202e:	65 21 00             	and    DWORD PTR gs:[rax],eax
  402031:	4d 61                	rex.WRB (bad) 
  402033:	6b 65 20 6d          	imul   esp,DWORD PTR [rbp+0x20],0x6d
  402037:	65 20 73 61          	and    BYTE PTR gs:[rbx+0x61],dh
  40203b:	79 20                	jns    40205d <__GNU_EH_FRAME_HDR+0xd>
  40203d:	68 65 6c 6c 6f       	push   0x6f6c6c65
  402042:	20 74 6f 20          	and    BYTE PTR [rdi+rbp*2+0x20],dh
  402046:	79 6f                	jns    4020b7 <__GNU_EH_FRAME_HDR+0x67>
  402048:	75 2e                	jne    402078 <__GNU_EH_FRAME_HDR+0x28>
  40204a:	00 42 79             	add    BYTE PTR [rdx+0x79],al
  40204d:	65 21 00             	and    DWORD PTR gs:[rax],eax

Disassembly of section .eh_frame_hdr:

0000000000402050 <__GNU_EH_FRAME_HDR>:
  402050:	01 1b                	add    DWORD PTR [rbx],ebx
  402052:	03 3b                	add    edi,DWORD PTR [rbx]
  402054:	4c 00 00             	rex.WR add BYTE PTR [rax],r8b
  402057:	00 08                	add    BYTE PTR [rax],cl
  402059:	00 00                	add    BYTE PTR [rax],al
  40205b:	00 d0                	add    al,dl
  40205d:	ef                   	out    dx,eax
  40205e:	ff                   	(bad)  
  40205f:	ff 90 00 00 00 10    	call   QWORD PTR [rax+0x10000000]
  402065:	f0 ff                	lock (bad) 
  402067:	ff 68 00             	jmp    FWORD PTR [rax+0x0]
  40206a:	00 00                	add    BYTE PTR [rax],al
  40206c:	40                   	rex
  40206d:	f0 ff                	lock (bad) 
  40206f:	ff                   	(bad)  
  402070:	7c 00                	jl     402072 <__GNU_EH_FRAME_HDR+0x22>
  402072:	00 00                	add    BYTE PTR [rax],al
  402074:	f6 f0                	div    al
  402076:	ff                   	(bad)  
  402077:	ff                   	(bad)  
  402078:	b8 00 00 00 0e       	mov    eax,0xe000000
  40207d:	f1                   	icebp  
  40207e:	ff                   	(bad)  
  40207f:	ff d4                	call   rsp
  402081:	00 00                	add    BYTE PTR [rax],al
  402083:	00 2f                	add    BYTE PTR [rdi],ch
  402085:	f1                   	icebp  
  402086:	ff                   	(bad)  
  402087:	ff f4                	push   rsp
  402089:	00 00                	add    BYTE PTR [rax],al
  40208b:	00 50 f1             	add    BYTE PTR [rax-0xf],dl
  40208e:	ff                   	(bad)  
  40208f:	ff 18                	call   FWORD PTR [rax]
  402091:	01 00                	add    DWORD PTR [rax],eax
  402093:	00 c0                	add    al,al
  402095:	f1                   	icebp  
  402096:	ff                   	(bad)  
  402097:	ff 60 01             	jmp    QWORD PTR [rax+0x1]
	...

Disassembly of section .eh_frame:

00000000004020a0 <__FRAME_END__-0x124>:
  4020a0:	14 00                	adc    al,0x0
  4020a2:	00 00                	add    BYTE PTR [rax],al
  4020a4:	00 00                	add    BYTE PTR [rax],al
  4020a6:	00 00                	add    BYTE PTR [rax],al
  4020a8:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
  4020ab:	00 01                	add    BYTE PTR [rcx],al
  4020ad:	78 10                	js     4020bf <__GNU_EH_FRAME_HDR+0x6f>
  4020af:	01 1b                	add    DWORD PTR [rbx],ebx
  4020b1:	0c 07                	or     al,0x7
  4020b3:	08 90 01 00 00 10    	or     BYTE PTR [rax+0x10000001],dl
  4020b9:	00 00                	add    BYTE PTR [rax],al
  4020bb:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  4020be:	00 00                	add    BYTE PTR [rax],al
  4020c0:	a0 ef ff ff 2f 00 00 	movabs al,ds:0x2fffffef
  4020c7:	00 00 
  4020c9:	44 07                	rex.R (bad) 
  4020cb:	10 10                	adc    BYTE PTR [rax],dl
  4020cd:	00 00                	add    BYTE PTR [rax],al
  4020cf:	00 30                	add    BYTE PTR [rax],dh
  4020d1:	00 00                	add    BYTE PTR [rax],al
  4020d3:	00 bc ef ff ff 05 00 	add    BYTE PTR [rdi+rbp*8+0x5ffff],bh
  4020da:	00 00                	add    BYTE PTR [rax],al
  4020dc:	00 00                	add    BYTE PTR [rax],al
  4020de:	00 00                	add    BYTE PTR [rax],al
  4020e0:	24 00                	and    al,0x0
  4020e2:	00 00                	add    BYTE PTR [rax],al
  4020e4:	44 00 00             	add    BYTE PTR [rax],r8b
  4020e7:	00 38                	add    BYTE PTR [rax],bh
  4020e9:	ef                   	out    dx,eax
  4020ea:	ff                   	(bad)  
  4020eb:	ff 40 00             	inc    DWORD PTR [rax+0x0]
  4020ee:	00 00                	add    BYTE PTR [rax],al
  4020f0:	00 0e                	add    BYTE PTR [rsi],cl
  4020f2:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
  4020f5:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
  4020f8:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
  4020fb:	80 00 3f             	add    BYTE PTR [rax],0x3f
  4020fe:	1a 3b                	sbb    bh,BYTE PTR [rbx]
  402100:	2a 33                	sub    dh,BYTE PTR [rbx]
  402102:	24 22                	and    al,0x22
  402104:	00 00                	add    BYTE PTR [rax],al
  402106:	00 00                	add    BYTE PTR [rax],al
  402108:	18 00                	sbb    BYTE PTR [rax],al
  40210a:	00 00                	add    BYTE PTR [rax],al
  40210c:	6c                   	ins    BYTE PTR es:[rdi],dx
  40210d:	00 00                	add    BYTE PTR [rax],al
  40210f:	00 36                	add    BYTE PTR [rsi],dh
  402111:	f0 ff                	lock (bad) 
  402113:	ff 18                	call   FWORD PTR [rax]
  402115:	00 00                	add    BYTE PTR [rax],al
  402117:	00 00                	add    BYTE PTR [rax],al
  402119:	41 0e                	rex.B (bad) 
  40211b:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  402121:	00 00                	add    BYTE PTR [rax],al
  402123:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  402126:	00 00                	add    BYTE PTR [rax],al
  402128:	88 00                	mov    BYTE PTR [rax],al
  40212a:	00 00                	add    BYTE PTR [rax],al
  40212c:	32 f0                	xor    dh,al
  40212e:	ff                   	(bad)  
  40212f:	ff 21                	jmp    QWORD PTR [rcx]
  402131:	00 00                	add    BYTE PTR [rax],al
  402133:	00 00                	add    BYTE PTR [rax],al
  402135:	41 0e                	rex.B (bad) 
  402137:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  40213d:	5c                   	pop    rsp
  40213e:	0c 07                	or     al,0x7
  402140:	08 00                	or     BYTE PTR [rax],al
  402142:	00 00                	add    BYTE PTR [rax],al
  402144:	20 00                	and    BYTE PTR [rax],al
  402146:	00 00                	add    BYTE PTR [rax],al
  402148:	a8 00                	test   al,0x0
  40214a:	00 00                	add    BYTE PTR [rax],al
  40214c:	33 f0                	xor    esi,eax
  40214e:	ff                   	(bad)  
  40214f:	ff 1f                	call   FWORD PTR [rdi]
  402151:	00 00                	add    BYTE PTR [rax],al
  402153:	00 00                	add    BYTE PTR [rax],al
  402155:	41 0e                	rex.B (bad) 
  402157:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  40215d:	5a                   	pop    rdx
  40215e:	0c 07                	or     al,0x7
  402160:	08 00                	or     BYTE PTR [rax],al
  402162:	00 00                	add    BYTE PTR [rax],al
  402164:	00 00                	add    BYTE PTR [rax],al
  402166:	00 00                	add    BYTE PTR [rax],al
  402168:	44 00 00             	add    BYTE PTR [rax],r8b
  40216b:	00 cc                	add    ah,cl
  40216d:	00 00                	add    BYTE PTR [rax],al
  40216f:	00 30                	add    BYTE PTR [rax],dh
  402171:	f0 ff                	lock (bad) 
  402173:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
  402176:	00 00                	add    BYTE PTR [rax],al
  402178:	00 46 0e             	add    BYTE PTR [rsi+0xe],al
  40217b:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
  402181:	8e 03                	mov    es,WORD PTR [rbx]
  402183:	45 0e                	rex.RB (bad) 
  402185:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
  40218b:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff86702fd5 <_end+0xffffffff862fef8d>
  402191:	06                   	(bad)  
  402192:	48 0e                	rex.W (bad) 
  402194:	38 83 07 47 0e 40    	cmp    BYTE PTR [rbx+0x400e4707],al
  40219a:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  40219b:	0e                   	(bad)  
  40219c:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
  40219f:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
  4021a2:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
  4021a5:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
  4021a8:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
  4021ab:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
  4021ae:	08 00                	or     BYTE PTR [rax],al
  4021b0:	10 00                	adc    BYTE PTR [rax],al
  4021b2:	00 00                	add    BYTE PTR [rax],al
  4021b4:	14 01                	adc    al,0x1
  4021b6:	00 00                	add    BYTE PTR [rax],al
  4021b8:	58                   	pop    rax
  4021b9:	f0 ff                	lock (bad) 
  4021bb:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 4021c1 <__GNU_EH_FRAME_HDR+0x171>
  4021c1:	00 00                	add    BYTE PTR [rax],al
	...

00000000004021c4 <__FRAME_END__>:
  4021c4:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000403e10 <__frame_dummy_init_array_entry>:
  403e10:	40 11 40 00          	rex adc DWORD PTR [rax+0x0],eax
  403e14:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000403e18 <__do_global_dtors_aux_fini_array_entry>:
  403e18:	10 11                	adc    BYTE PTR [rcx],dl
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
  403e47:	00 18                	add    BYTE PTR [rax],bl
  403e49:	12 40 00             	adc    al,BYTE PTR [rax+0x0]
  403e4c:	00 00                	add    BYTE PTR [rax],al
  403e4e:	00 00                	add    BYTE PTR [rax],al
  403e50:	19 00                	sbb    DWORD PTR [rax],eax
  403e52:	00 00                	add    BYTE PTR [rax],al
  403e54:	00 00                	add    BYTE PTR [rax],al
  403e56:	00 00                	add    BYTE PTR [rax],al
  403e58:	10 3e                	adc    BYTE PTR [rsi],bh
  403e5a:	40 00 00             	add    BYTE PTR [rax],al
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
  403ea7:	00 b8 03 40 00 00    	add    BYTE PTR [rax+0x4003],bh
  403ead:	00 00                	add    BYTE PTR [rax],al
  403eaf:	00 06                	add    BYTE PTR [rsi],al
  403eb1:	00 00                	add    BYTE PTR [rax],al
  403eb3:	00 00                	add    BYTE PTR [rax],al
  403eb5:	00 00                	add    BYTE PTR [rax],al
  403eb7:	00 28                	add    BYTE PTR [rax],ch
  403eb9:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
  403ebc:	00 00                	add    BYTE PTR [rax],al
  403ebe:	00 00                	add    BYTE PTR [rax],al
  403ec0:	0a 00                	or     al,BYTE PTR [rax]
  403ec2:	00 00                	add    BYTE PTR [rax],al
  403ec4:	00 00                	add    BYTE PTR [rax],al
  403ec6:	00 00                	add    BYTE PTR [rax],al
  403ec8:	47 00 00             	rex.RXB add BYTE PTR [r8],r8b
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
  403f27:	00 60 04             	add    BYTE PTR [rax+0x4],ah
  403f2a:	40 00 00             	add    BYTE PTR [rax],al
  403f2d:	00 00                	add    BYTE PTR [rax],al
  403f2f:	00 07                	add    BYTE PTR [rdi],al
  403f31:	00 00                	add    BYTE PTR [rax],al
  403f33:	00 00                	add    BYTE PTR [rax],al
  403f35:	00 00                	add    BYTE PTR [rax],al
  403f37:	00 30                	add    BYTE PTR [rax],dh
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
  403f67:	00 10                	add    BYTE PTR [rax],dl
  403f69:	04 40                	add    al,0x40
  403f6b:	00 00                	add    BYTE PTR [rax],al
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
  403f87:	00 00                	add    BYTE PTR [rax],al
  403f89:	04 40                	add    al,0x40
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
