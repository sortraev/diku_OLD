
ex2a:     file format elf64-x86-64


Disassembly of section .interp:

0000000000000318 <.interp>:
 318:	2f                   	(bad)
 319:	6c                   	ins    BYTE PTR es:[rdi],dx
 31a:	69 62 36 34 2f 6c 64 	imul   esp,DWORD PTR [rdx+0x36],0x646c2f34
 321:	2d 6c 69 6e 75       	sub    eax,0x756e696c
 326:	78 2d                	js     355 <_init-0xcab>
 328:	78 38                	js     362 <_init-0xc9e>
 32a:	36 2d 36 34 2e 73    	ss sub eax,0x732e3436
 330:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 331:	2e 32 00             	cs xor al,BYTE PTR [rax]

Disassembly of section .note.gnu.property:

0000000000000338 <.note.gnu.property>:
 338:	04 00                	add    al,0x0
 33a:	00 00                	add    BYTE PTR [rax],al
 33c:	30 00                	xor    BYTE PTR [rax],al
 33e:	00 00                	add    BYTE PTR [rax],al
 340:	05 00 00 00 47       	add    eax,0x47000000
 345:	4e 55                	rex.WRX push rbp
 347:	00 02                	add    BYTE PTR [rdx],al
 349:	80 00 c0             	add    BYTE PTR [rax],0xc0
 34c:	04 00                	add    al,0x0
 34e:	00 00                	add    BYTE PTR [rax],al
 350:	01 00                	add    DWORD PTR [rax],eax
 352:	00 00                	add    BYTE PTR [rax],al
 354:	00 00                	add    BYTE PTR [rax],al
 356:	00 00                	add    BYTE PTR [rax],al
 358:	01 00                	add    DWORD PTR [rax],eax
 35a:	01 c0                	add    eax,eax
 35c:	04 00                	add    al,0x0
 35e:	00 00                	add    BYTE PTR [rax],al
 360:	01 00                	add    DWORD PTR [rax],eax
 362:	00 00                	add    BYTE PTR [rax],al
 364:	00 00                	add    BYTE PTR [rax],al
 366:	00 00                	add    BYTE PTR [rax],al
 368:	02 00                	add    al,BYTE PTR [rax]
 36a:	01 c0                	add    eax,eax
 36c:	04 00                	add    al,0x0
	...

Disassembly of section .note.gnu.build-id:

0000000000000378 <.note.gnu.build-id>:
 378:	04 00                	add    al,0x0
 37a:	00 00                	add    BYTE PTR [rax],al
 37c:	14 00                	adc    al,0x0
 37e:	00 00                	add    BYTE PTR [rax],al
 380:	03 00                	add    eax,DWORD PTR [rax]
 382:	00 00                	add    BYTE PTR [rax],al
 384:	47                   	rex.RXB
 385:	4e 55                	rex.WRX push rbp
 387:	00 ad 00 2e 1d 0a    	add    BYTE PTR [rbp+0xa1d2e00],ch
 38d:	14 65                	adc    al,0x65
 38f:	e6 4b                	out    0x4b,al
 391:	73 f4                	jae    387 <_init-0xc79>
 393:	02 35 6c a5 d1 48    	add    dh,BYTE PTR [rip+0x48d1a56c]        # 48d1a905 <_end+0x48d168dd>
 399:	51                   	push   rcx
 39a:	ac                   	lods   al,BYTE PTR ds:[rsi]
 39b:	e4                   	.byte 0xe4

Disassembly of section .note.ABI-tag:

000000000000039c <.note.ABI-tag>:
 39c:	04 00                	add    al,0x0
 39e:	00 00                	add    BYTE PTR [rax],al
 3a0:	10 00                	adc    BYTE PTR [rax],al
 3a2:	00 00                	add    BYTE PTR [rax],al
 3a4:	01 00                	add    DWORD PTR [rax],eax
 3a6:	00 00                	add    BYTE PTR [rax],al
 3a8:	47                   	rex.RXB
 3a9:	4e 55                	rex.WRX push rbp
 3ab:	00 00                	add    BYTE PTR [rax],al
 3ad:	00 00                	add    BYTE PTR [rax],al
 3af:	00 04 00             	add    BYTE PTR [rax+rax*1],al
 3b2:	00 00                	add    BYTE PTR [rax],al
 3b4:	04 00                	add    al,0x0
 3b6:	00 00                	add    BYTE PTR [rax],al
 3b8:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .gnu.hash:

00000000000003c0 <.gnu.hash>:
 3c0:	01 00                	add    DWORD PTR [rax],eax
 3c2:	00 00                	add    BYTE PTR [rax],al
 3c4:	01 00                	add    DWORD PTR [rax],eax
 3c6:	00 00                	add    BYTE PTR [rax],al
 3c8:	01 00                	add    DWORD PTR [rax],eax
	...

Disassembly of section .dynsym:

00000000000003e0 <.dynsym>:
	...
 3f8:	0d 00 00 00 12       	or     eax,0x12000000
	...
 40d:	00 00                	add    BYTE PTR [rax],al
 40f:	00 4f 00             	add    BYTE PTR [rdi+0x0],cl
 412:	00 00                	add    BYTE PTR [rax],al
 414:	20 00                	and    BYTE PTR [rax],al
	...
 426:	00 00                	add    BYTE PTR [rax],al
 428:	01 00                	add    DWORD PTR [rax],eax
 42a:	00 00                	add    BYTE PTR [rax],al
 42c:	12 00                	adc    al,BYTE PTR [rax]
	...
 43e:	00 00                	add    BYTE PTR [rax],al
 440:	08 00                	or     BYTE PTR [rax],al
 442:	00 00                	add    BYTE PTR [rax],al
 444:	12 00                	adc    al,BYTE PTR [rax]
	...
 456:	00 00                	add    BYTE PTR [rax],al
 458:	6b 00 00             	imul   eax,DWORD PTR [rax],0x0
 45b:	00 20                	add    BYTE PTR [rax],ah
	...
 46d:	00 00                	add    BYTE PTR [rax],al
 46f:	00 7a 00             	add    BYTE PTR [rdx+0x0],bh
 472:	00 00                	add    BYTE PTR [rax],al
 474:	20 00                	and    BYTE PTR [rax],al
	...
 486:	00 00                	add    BYTE PTR [rax],al
 488:	1f                   	(bad)
 489:	00 00                	add    BYTE PTR [rax],al
 48b:	00 22                	add    BYTE PTR [rdx],ah
	...

Disassembly of section .dynstr:

00000000000004a0 <.dynstr>:
 4a0:	00 73 74             	add    BYTE PTR [rbx+0x74],dh
 4a3:	72 63                	jb     508 <_init-0xaf8>
 4a5:	70 79                	jo     520 <_init-0xae0>
 4a7:	00 70 75             	add    BYTE PTR [rax+0x75],dh
 4aa:	74 73                	je     51f <_init-0xae1>
 4ac:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 4af:	6c                   	ins    BYTE PTR es:[rdi],dx
 4b0:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
 4b7:	72 74                	jb     52d <_init-0xad3>
 4b9:	5f                   	pop    rdi
 4ba:	6d                   	ins    DWORD PTR es:[rdi],dx
 4bb:	61                   	(bad)
 4bc:	69 6e 00 5f 5f 63 78 	imul   ebp,DWORD PTR [rsi+0x0],0x78635f5f
 4c3:	61                   	(bad)
 4c4:	5f                   	pop    rdi
 4c5:	66 69 6e 61 6c 69    	imul   bp,WORD PTR [rsi+0x61],0x696c
 4cb:	7a 65                	jp     532 <_init-0xace>
 4cd:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
 4d1:	63 2e                	movsxd ebp,DWORD PTR [rsi]
 4d3:	73 6f                	jae    544 <_init-0xabc>
 4d5:	2e 36 00 47 4c       	cs ss add BYTE PTR [rdi+0x4c],al
 4da:	49                   	rex.WB
 4db:	42                   	rex.X
 4dc:	43 5f                	rex.XB pop r15
 4de:	32 2e                	xor    ch,BYTE PTR [rsi]
 4e0:	32 2e                	xor    ch,BYTE PTR [rsi]
 4e2:	35 00 47 4c 49       	xor    eax,0x494c4700
 4e7:	42                   	rex.X
 4e8:	43 5f                	rex.XB pop r15
 4ea:	32 2e                	xor    ch,BYTE PTR [rsi]
 4ec:	33 34 00             	xor    esi,DWORD PTR [rax+rax*1]
 4ef:	5f                   	pop    rdi
 4f0:	49 54                	rex.WB push r12
 4f2:	4d 5f                	rex.WRB pop r15
 4f4:	64 65 72 65          	fs gs jb 55d <_init-0xaa3>
 4f8:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 4ff:	4d 
 500:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 502:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 503:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 504:	65 54                	gs push rsp
 506:	61                   	(bad)
 507:	62                   	(bad)
 508:	6c                   	ins    BYTE PTR es:[rdi],dx
 509:	65 00 5f 5f          	add    BYTE PTR gs:[rdi+0x5f],bl
 50d:	67 6d                	ins    DWORD PTR es:[edi],dx
 50f:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 510:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 511:	5f                   	pop    rdi
 512:	73 74                	jae    588 <_init-0xa78>
 514:	61                   	(bad)
 515:	72 74                	jb     58b <_init-0xa75>
 517:	5f                   	pop    rdi
 518:	5f                   	pop    rdi
 519:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 51c:	54                   	push   rsp
 51d:	4d 5f                	rex.WRB pop r15
 51f:	72 65                	jb     586 <_init-0xa7a>
 521:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 528:	4d 
 529:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 52b:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 52c:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 52d:	65 54                	gs push rsp
 52f:	61                   	(bad)
 530:	62                   	.byte 0x62
 531:	6c                   	ins    BYTE PTR es:[rdi],dx
 532:	65                   	gs
	...

Disassembly of section .gnu.version:

0000000000000534 <.gnu.version>:
 534:	00 00                	add    BYTE PTR [rax],al
 536:	02 00                	add    al,BYTE PTR [rax]
 538:	01 00                	add    DWORD PTR [rax],eax
 53a:	03 00                	add    eax,DWORD PTR [rax]
 53c:	03 00                	add    eax,DWORD PTR [rax]
 53e:	01 00                	add    DWORD PTR [rax],eax
 540:	01 00                	add    DWORD PTR [rax],eax
 542:	03 00                	add    eax,DWORD PTR [rax]

Disassembly of section .gnu.version_r:

0000000000000548 <.gnu.version_r>:
 548:	01 00                	add    DWORD PTR [rax],eax
 54a:	02 00                	add    al,BYTE PTR [rax]
 54c:	2e 00 00             	cs add BYTE PTR [rax],al
 54f:	00 10                	add    BYTE PTR [rax],dl
 551:	00 00                	add    BYTE PTR [rax],al
 553:	00 00                	add    BYTE PTR [rax],al
 555:	00 00                	add    BYTE PTR [rax],al
 557:	00 75 1a             	add    BYTE PTR [rbp+0x1a],dh
 55a:	69 09 00 00 03 00    	imul   ecx,DWORD PTR [rcx],0x30000
 560:	38 00                	cmp    BYTE PTR [rax],al
 562:	00 00                	add    BYTE PTR [rax],al
 564:	10 00                	adc    BYTE PTR [rax],al
 566:	00 00                	add    BYTE PTR [rax],al
 568:	b4 91                	mov    ah,0x91
 56a:	96                   	xchg   esi,eax
 56b:	06                   	(bad)
 56c:	00 00                	add    BYTE PTR [rax],al
 56e:	02 00                	add    al,BYTE PTR [rax]
 570:	44 00 00             	add    BYTE PTR [rax],r8b
 573:	00 00                	add    BYTE PTR [rax],al
 575:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000000578 <.rela.dyn>:
 578:	d0 3d 00 00 00 00    	sar    BYTE PTR [rip+0x0],1        # 57e <_init-0xa82>
 57e:	00 00                	add    BYTE PTR [rax],al
 580:	08 00                	or     BYTE PTR [rax],al
 582:	00 00                	add    BYTE PTR [rax],al
 584:	00 00                	add    BYTE PTR [rax],al
 586:	00 00                	add    BYTE PTR [rax],al
 588:	40 11 00             	rex adc DWORD PTR [rax],eax
 58b:	00 00                	add    BYTE PTR [rax],al
 58d:	00 00                	add    BYTE PTR [rax],al
 58f:	00 d8                	add    al,bl
 591:	3d 00 00 00 00       	cmp    eax,0x0
 596:	00 00                	add    BYTE PTR [rax],al
 598:	08 00                	or     BYTE PTR [rax],al
 59a:	00 00                	add    BYTE PTR [rax],al
 59c:	00 00                	add    BYTE PTR [rax],al
 59e:	00 00                	add    BYTE PTR [rax],al
 5a0:	f0 10 00             	lock adc BYTE PTR [rax],al
 5a3:	00 00                	add    BYTE PTR [rax],al
 5a5:	00 00                	add    BYTE PTR [rax],al
 5a7:	00 18                	add    BYTE PTR [rax],bl
 5a9:	40 00 00             	rex add BYTE PTR [rax],al
 5ac:	00 00                	add    BYTE PTR [rax],al
 5ae:	00 00                	add    BYTE PTR [rax],al
 5b0:	08 00                	or     BYTE PTR [rax],al
 5b2:	00 00                	add    BYTE PTR [rax],al
 5b4:	00 00                	add    BYTE PTR [rax],al
 5b6:	00 00                	add    BYTE PTR [rax],al
 5b8:	18 40 00             	sbb    BYTE PTR [rax+0x0],al
 5bb:	00 00                	add    BYTE PTR [rax],al
 5bd:	00 00                	add    BYTE PTR [rax],al
 5bf:	00 c0                	add    al,al
 5c1:	3f                   	(bad)
 5c2:	00 00                	add    BYTE PTR [rax],al
 5c4:	00 00                	add    BYTE PTR [rax],al
 5c6:	00 00                	add    BYTE PTR [rax],al
 5c8:	06                   	(bad)
 5c9:	00 00                	add    BYTE PTR [rax],al
 5cb:	00 01                	add    BYTE PTR [rcx],al
	...
 5d5:	00 00                	add    BYTE PTR [rax],al
 5d7:	00 c8                	add    al,cl
 5d9:	3f                   	(bad)
 5da:	00 00                	add    BYTE PTR [rax],al
 5dc:	00 00                	add    BYTE PTR [rax],al
 5de:	00 00                	add    BYTE PTR [rax],al
 5e0:	06                   	(bad)
 5e1:	00 00                	add    BYTE PTR [rax],al
 5e3:	00 02                	add    BYTE PTR [rdx],al
	...
 5ed:	00 00                	add    BYTE PTR [rax],al
 5ef:	00 d0                	add    al,dl
 5f1:	3f                   	(bad)
 5f2:	00 00                	add    BYTE PTR [rax],al
 5f4:	00 00                	add    BYTE PTR [rax],al
 5f6:	00 00                	add    BYTE PTR [rax],al
 5f8:	06                   	(bad)
 5f9:	00 00                	add    BYTE PTR [rax],al
 5fb:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 601 <_init-0x9ff>
 601:	00 00                	add    BYTE PTR [rax],al
 603:	00 00                	add    BYTE PTR [rax],al
 605:	00 00                	add    BYTE PTR [rax],al
 607:	00 d8                	add    al,bl
 609:	3f                   	(bad)
 60a:	00 00                	add    BYTE PTR [rax],al
 60c:	00 00                	add    BYTE PTR [rax],al
 60e:	00 00                	add    BYTE PTR [rax],al
 610:	06                   	(bad)
 611:	00 00                	add    BYTE PTR [rax],al
 613:	00 06                	add    BYTE PTR [rsi],al
	...
 61d:	00 00                	add    BYTE PTR [rax],al
 61f:	00 e0                	add    al,ah
 621:	3f                   	(bad)
 622:	00 00                	add    BYTE PTR [rax],al
 624:	00 00                	add    BYTE PTR [rax],al
 626:	00 00                	add    BYTE PTR [rax],al
 628:	06                   	(bad)
 629:	00 00                	add    BYTE PTR [rax],al
 62b:	00 07                	add    BYTE PTR [rdi],al
	...

Disassembly of section .rela.plt:

0000000000000638 <.rela.plt>:
 638:	00 40 00             	add    BYTE PTR [rax+0x0],al
 63b:	00 00                	add    BYTE PTR [rax],al
 63d:	00 00                	add    BYTE PTR [rax],al
 63f:	00 07                	add    BYTE PTR [rdi],al
 641:	00 00                	add    BYTE PTR [rax],al
 643:	00 03                	add    BYTE PTR [rbx],al
	...
 64d:	00 00                	add    BYTE PTR [rax],al
 64f:	00 08                	add    BYTE PTR [rax],cl
 651:	40 00 00             	rex add BYTE PTR [rax],al
 654:	00 00                	add    BYTE PTR [rax],al
 656:	00 00                	add    BYTE PTR [rax],al
 658:	07                   	(bad)
 659:	00 00                	add    BYTE PTR [rax],al
 65b:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...

Disassembly of section .init:

0000000000001000 <_init>:
    1000:	f3 0f 1e fa          	endbr64
    1004:	48 83 ec 08          	sub    rsp,0x8
    1008:	48 8b 05 c1 2f 00 00 	mov    rax,QWORD PTR [rip+0x2fc1]        # 3fd0 <__gmon_start__@Base>
    100f:	48 85 c0             	test   rax,rax
    1012:	74 02                	je     1016 <_init+0x16>
    1014:	ff d0                	call   rax
    1016:	48 83 c4 08          	add    rsp,0x8
    101a:	c3                   	ret

Disassembly of section .plt:

0000000000001020 <strcpy@plt-0x10>:
    1020:	ff 35 ca 2f 00 00    	push   QWORD PTR [rip+0x2fca]        # 3ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
    1026:	ff 25 cc 2f 00 00    	jmp    QWORD PTR [rip+0x2fcc]        # 3ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
    102c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

0000000000001030 <strcpy@plt>:
    1030:	ff 25 ca 2f 00 00    	jmp    QWORD PTR [rip+0x2fca]        # 4000 <strcpy@GLIBC_2.2.5>
    1036:	68 00 00 00 00       	push   0x0
    103b:	e9 e0 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001040 <puts@plt>:
    1040:	ff 25 c2 2f 00 00    	jmp    QWORD PTR [rip+0x2fc2]        # 4008 <puts@GLIBC_2.2.5>
    1046:	68 01 00 00 00       	push   0x1
    104b:	e9 d0 ff ff ff       	jmp    1020 <_init+0x20>

Disassembly of section .text:

0000000000001050 <_start>:
    1050:	f3 0f 1e fa          	endbr64
    1054:	31 ed                	xor    ebp,ebp
    1056:	49 89 d1             	mov    r9,rdx
    1059:	5e                   	pop    rsi
    105a:	48 89 e2             	mov    rdx,rsp
    105d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
    1061:	50                   	push   rax
    1062:	54                   	push   rsp
    1063:	45 31 c0             	xor    r8d,r8d
    1066:	31 c9                	xor    ecx,ecx
    1068:	48 8d 3d 1d 01 00 00 	lea    rdi,[rip+0x11d]        # 118c <main>
    106f:	ff 15 4b 2f 00 00    	call   QWORD PTR [rip+0x2f4b]        # 3fc0 <__libc_start_main@GLIBC_2.34>
    1075:	f4                   	hlt
    1076:	66 2e 0f 1f 84 00 00 	cs nop WORD PTR [rax+rax*1+0x0]
    107d:	00 00 00 
    1080:	48 8d 3d 99 2f 00 00 	lea    rdi,[rip+0x2f99]        # 4020 <__TMC_END__>
    1087:	48 8d 05 92 2f 00 00 	lea    rax,[rip+0x2f92]        # 4020 <__TMC_END__>
    108e:	48 39 f8             	cmp    rax,rdi
    1091:	74 15                	je     10a8 <_start+0x58>
    1093:	48 8b 05 2e 2f 00 00 	mov    rax,QWORD PTR [rip+0x2f2e]        # 3fc8 <_ITM_deregisterTMCloneTable@Base>
    109a:	48 85 c0             	test   rax,rax
    109d:	74 09                	je     10a8 <_start+0x58>
    109f:	ff e0                	jmp    rax
    10a1:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    10a8:	c3                   	ret
    10a9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    10b0:	48 8d 3d 69 2f 00 00 	lea    rdi,[rip+0x2f69]        # 4020 <__TMC_END__>
    10b7:	48 8d 35 62 2f 00 00 	lea    rsi,[rip+0x2f62]        # 4020 <__TMC_END__>
    10be:	48 29 fe             	sub    rsi,rdi
    10c1:	48 89 f0             	mov    rax,rsi
    10c4:	48 c1 ee 3f          	shr    rsi,0x3f
    10c8:	48 c1 f8 03          	sar    rax,0x3
    10cc:	48 01 c6             	add    rsi,rax
    10cf:	48 d1 fe             	sar    rsi,1
    10d2:	74 14                	je     10e8 <_start+0x98>
    10d4:	48 8b 05 fd 2e 00 00 	mov    rax,QWORD PTR [rip+0x2efd]        # 3fd8 <_ITM_registerTMCloneTable@Base>
    10db:	48 85 c0             	test   rax,rax
    10de:	74 08                	je     10e8 <_start+0x98>
    10e0:	ff e0                	jmp    rax
    10e2:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]
    10e8:	c3                   	ret
    10e9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    10f0:	f3 0f 1e fa          	endbr64
    10f4:	80 3d 25 2f 00 00 00 	cmp    BYTE PTR [rip+0x2f25],0x0        # 4020 <__TMC_END__>
    10fb:	75 33                	jne    1130 <_start+0xe0>
    10fd:	55                   	push   rbp
    10fe:	48 83 3d da 2e 00 00 	cmp    QWORD PTR [rip+0x2eda],0x0        # 3fe0 <__cxa_finalize@GLIBC_2.2.5>
    1105:	00 
    1106:	48 89 e5             	mov    rbp,rsp
    1109:	74 0d                	je     1118 <_start+0xc8>
    110b:	48 8b 3d 06 2f 00 00 	mov    rdi,QWORD PTR [rip+0x2f06]        # 4018 <__dso_handle>
    1112:	ff 15 c8 2e 00 00    	call   QWORD PTR [rip+0x2ec8]        # 3fe0 <__cxa_finalize@GLIBC_2.2.5>
    1118:	e8 63 ff ff ff       	call   1080 <_start+0x30>
    111d:	c6 05 fc 2e 00 00 01 	mov    BYTE PTR [rip+0x2efc],0x1        # 4020 <__TMC_END__>
    1124:	5d                   	pop    rbp
    1125:	c3                   	ret
    1126:	66 2e 0f 1f 84 00 00 	cs nop WORD PTR [rax+rax*1+0x0]
    112d:	00 00 00 
    1130:	c3                   	ret
    1131:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
    1138:	00 00 00 00 
    113c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]
    1140:	f3 0f 1e fa          	endbr64
    1144:	e9 67 ff ff ff       	jmp    10b0 <_start+0x60>

0000000000001149 <exploit_me>:
    1149:	55                   	push   rbp
    114a:	48 89 e5             	mov    rbp,rsp
    114d:	48 83 ec 40          	sub    rsp,0x40
    1151:	48 89 7d c8          	mov    QWORD PTR [rbp-0x38],rdi
    1155:	c7 45 d0 00 00 00 00 	mov    DWORD PTR [rbp-0x30],0x0
    115c:	48 8b 45 c8          	mov    rax,QWORD PTR [rbp-0x38]
    1160:	48 8d 55 d0          	lea    rdx,[rbp-0x30]
    1164:	48 83 c2 04          	add    rdx,0x4
    1168:	48 89 c6             	mov    rsi,rax
    116b:	48 89 d7             	mov    rdi,rdx
    116e:	e8 bd fe ff ff       	call   1030 <strcpy@plt>
    1173:	8b 45 d0             	mov    eax,DWORD PTR [rbp-0x30]
    1176:	85 c0                	test   eax,eax
    1178:	74 0f                	je     1189 <exploit_me+0x40>
    117a:	48 8d 05 83 0e 00 00 	lea    rax,[rip+0xe83]        # 2004 <_IO_stdin_used+0x4>
    1181:	48 89 c7             	mov    rdi,rax
    1184:	e8 b7 fe ff ff       	call   1040 <puts@plt>
    1189:	90                   	nop
    118a:	c9                   	leave
    118b:	c3                   	ret

000000000000118c <main>:
    118c:	55                   	push   rbp
    118d:	48 89 e5             	mov    rbp,rsp
    1190:	48 83 ec 10          	sub    rsp,0x10
    1194:	89 7d fc             	mov    DWORD PTR [rbp-0x4],edi
    1197:	48 89 75 f0          	mov    QWORD PTR [rbp-0x10],rsi
    119b:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
    119f:	48 83 c0 08          	add    rax,0x8
    11a3:	48 8b 00             	mov    rax,QWORD PTR [rax]
    11a6:	48 89 c7             	mov    rdi,rax
    11a9:	e8 9b ff ff ff       	call   1149 <exploit_me>
    11ae:	b8 00 00 00 00       	mov    eax,0x0
    11b3:	c9                   	leave
    11b4:	c3                   	ret

Disassembly of section .fini:

00000000000011b8 <_fini>:
    11b8:	f3 0f 1e fa          	endbr64
    11bc:	48 83 ec 08          	sub    rsp,0x8
    11c0:	48 83 c4 08          	add    rsp,0x8
    11c4:	c3                   	ret

Disassembly of section .rodata:

0000000000002000 <_IO_stdin_used>:
    2000:	01 00                	add    DWORD PTR [rax],eax
    2002:	02 00                	add    al,BYTE PTR [rax]
    2004:	47 6f                	rex.RXB outs dx,DWORD PTR ds:[rsi]
    2006:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2007:	64 20 64 61 79       	and    BYTE PTR fs:[rcx+riz*2+0x79],ah
    200c:	2c 20                	sub    al,0x20
    200e:	6d                   	ins    DWORD PTR es:[rdi],dx
    200f:	61                   	(bad)
    2010:	73 74                	jae    2086 <__GNU_EH_FRAME_HDR+0x6e>
    2012:	65 72 00             	gs jb  2015 <_IO_stdin_used+0x15>

Disassembly of section .eh_frame_hdr:

0000000000002018 <__GNU_EH_FRAME_HDR>:
    2018:	01 1b                	add    DWORD PTR [rbx],ebx
    201a:	03 3b                	add    edi,DWORD PTR [rbx]
    201c:	2c 00                	sub    al,0x0
    201e:	00 00                	add    BYTE PTR [rax],al
    2020:	04 00                	add    al,0x0
    2022:	00 00                	add    BYTE PTR [rax],al
    2024:	08 f0                	or     al,dh
    2026:	ff                   	(bad)
    2027:	ff 60 00             	jmp    QWORD PTR [rax+0x0]
    202a:	00 00                	add    BYTE PTR [rax],al
    202c:	38 f0                	cmp    al,dh
    202e:	ff                   	(bad)
    202f:	ff 48 00             	dec    DWORD PTR [rax+0x0]
    2032:	00 00                	add    BYTE PTR [rax],al
    2034:	31 f1                	xor    ecx,esi
    2036:	ff                   	(bad)
    2037:	ff 88 00 00 00 74    	dec    DWORD PTR [rax+0x74000000]
    203d:	f1                   	int1
    203e:	ff                   	(bad)
    203f:	ff                   	.byte 0xff
    2040:	a8 00                	test   al,0x0
	...

Disassembly of section .eh_frame:

0000000000002048 <.eh_frame>:
    2048:	14 00                	adc    al,0x0
    204a:	00 00                	add    BYTE PTR [rax],al
    204c:	00 00                	add    BYTE PTR [rax],al
    204e:	00 00                	add    BYTE PTR [rax],al
    2050:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
    2053:	00 01                	add    BYTE PTR [rcx],al
    2055:	78 10                	js     2067 <__GNU_EH_FRAME_HDR+0x4f>
    2057:	01 1b                	add    DWORD PTR [rbx],ebx
    2059:	0c 07                	or     al,0x7
    205b:	08 90 01 00 00 14    	or     BYTE PTR [rax+0x14000001],dl
    2061:	00 00                	add    BYTE PTR [rax],al
    2063:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    2066:	00 00                	add    BYTE PTR [rax],al
    2068:	e8 ef ff ff 26       	call   2700205c <_end+0x26ffe034>
    206d:	00 00                	add    BYTE PTR [rax],al
    206f:	00 00                	add    BYTE PTR [rax],al
    2071:	44 07                	rex.R (bad)
    2073:	10 00                	adc    BYTE PTR [rax],al
    2075:	00 00                	add    BYTE PTR [rax],al
    2077:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
    207a:	00 00                	add    BYTE PTR [rax],al
    207c:	34 00                	xor    al,0x0
    207e:	00 00                	add    BYTE PTR [rax],al
    2080:	a0 ef ff ff 30 00 00 	movabs al,ds:0x30ffffef
    2087:	00 00 
    2089:	0e                   	(bad)
    208a:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
    208d:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
    2090:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
    2093:	80 00 3f             	add    BYTE PTR [rax],0x3f
    2096:	1a 3b                	sbb    bh,BYTE PTR [rbx]
    2098:	2a 33                	sub    dh,BYTE PTR [rbx]
    209a:	24 22                	and    al,0x22
    209c:	00 00                	add    BYTE PTR [rax],al
    209e:	00 00                	add    BYTE PTR [rax],al
    20a0:	1c 00                	sbb    al,0x0
    20a2:	00 00                	add    BYTE PTR [rax],al
    20a4:	5c                   	pop    rsp
    20a5:	00 00                	add    BYTE PTR [rax],al
    20a7:	00 a1 f0 ff ff 43    	add    BYTE PTR [rcx+0x43fffff0],ah
    20ad:	00 00                	add    BYTE PTR [rax],al
    20af:	00 00                	add    BYTE PTR [rax],al
    20b1:	41 0e                	rex.B (bad)
    20b3:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    20b9:	7e 0c                	jle    20c7 <__GNU_EH_FRAME_HDR+0xaf>
    20bb:	07                   	(bad)
    20bc:	08 00                	or     BYTE PTR [rax],al
    20be:	00 00                	add    BYTE PTR [rax],al
    20c0:	1c 00                	sbb    al,0x0
    20c2:	00 00                	add    BYTE PTR [rax],al
    20c4:	7c 00                	jl     20c6 <__GNU_EH_FRAME_HDR+0xae>
    20c6:	00 00                	add    BYTE PTR [rax],al
    20c8:	c4                   	(bad)
    20c9:	f0 ff                	lock (bad)
    20cb:	ff 29                	jmp    FWORD PTR [rcx]
    20cd:	00 00                	add    BYTE PTR [rax],al
    20cf:	00 00                	add    BYTE PTR [rax],al
    20d1:	41 0e                	rex.B (bad)
    20d3:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    20d9:	64 0c 07             	fs or  al,0x7
    20dc:	08 00                	or     BYTE PTR [rax],al
    20de:	00 00                	add    BYTE PTR [rax],al
    20e0:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000003dd0 <.init_array>:
    3dd0:	40 11 00             	rex adc DWORD PTR [rax],eax
    3dd3:	00 00                	add    BYTE PTR [rax],al
    3dd5:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000003dd8 <.fini_array>:
    3dd8:	f0 10 00             	lock adc BYTE PTR [rax],al
    3ddb:	00 00                	add    BYTE PTR [rax],al
    3ddd:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000003de0 <_DYNAMIC>:
    3de0:	01 00                	add    DWORD PTR [rax],eax
    3de2:	00 00                	add    BYTE PTR [rax],al
    3de4:	00 00                	add    BYTE PTR [rax],al
    3de6:	00 00                	add    BYTE PTR [rax],al
    3de8:	2e 00 00             	cs add BYTE PTR [rax],al
    3deb:	00 00                	add    BYTE PTR [rax],al
    3ded:	00 00                	add    BYTE PTR [rax],al
    3def:	00 0c 00             	add    BYTE PTR [rax+rax*1],cl
    3df2:	00 00                	add    BYTE PTR [rax],al
    3df4:	00 00                	add    BYTE PTR [rax],al
    3df6:	00 00                	add    BYTE PTR [rax],al
    3df8:	00 10                	add    BYTE PTR [rax],dl
    3dfa:	00 00                	add    BYTE PTR [rax],al
    3dfc:	00 00                	add    BYTE PTR [rax],al
    3dfe:	00 00                	add    BYTE PTR [rax],al
    3e00:	0d 00 00 00 00       	or     eax,0x0
    3e05:	00 00                	add    BYTE PTR [rax],al
    3e07:	00 b8 11 00 00 00    	add    BYTE PTR [rax+0x11],bh
    3e0d:	00 00                	add    BYTE PTR [rax],al
    3e0f:	00 19                	add    BYTE PTR [rcx],bl
    3e11:	00 00                	add    BYTE PTR [rax],al
    3e13:	00 00                	add    BYTE PTR [rax],al
    3e15:	00 00                	add    BYTE PTR [rax],al
    3e17:	00 d0                	add    al,dl
    3e19:	3d 00 00 00 00       	cmp    eax,0x0
    3e1e:	00 00                	add    BYTE PTR [rax],al
    3e20:	1b 00                	sbb    eax,DWORD PTR [rax]
    3e22:	00 00                	add    BYTE PTR [rax],al
    3e24:	00 00                	add    BYTE PTR [rax],al
    3e26:	00 00                	add    BYTE PTR [rax],al
    3e28:	08 00                	or     BYTE PTR [rax],al
    3e2a:	00 00                	add    BYTE PTR [rax],al
    3e2c:	00 00                	add    BYTE PTR [rax],al
    3e2e:	00 00                	add    BYTE PTR [rax],al
    3e30:	1a 00                	sbb    al,BYTE PTR [rax]
    3e32:	00 00                	add    BYTE PTR [rax],al
    3e34:	00 00                	add    BYTE PTR [rax],al
    3e36:	00 00                	add    BYTE PTR [rax],al
    3e38:	d8 3d 00 00 00 00    	fdivr  DWORD PTR [rip+0x0]        # 3e3e <_DYNAMIC+0x5e>
    3e3e:	00 00                	add    BYTE PTR [rax],al
    3e40:	1c 00                	sbb    al,0x0
    3e42:	00 00                	add    BYTE PTR [rax],al
    3e44:	00 00                	add    BYTE PTR [rax],al
    3e46:	00 00                	add    BYTE PTR [rax],al
    3e48:	08 00                	or     BYTE PTR [rax],al
    3e4a:	00 00                	add    BYTE PTR [rax],al
    3e4c:	00 00                	add    BYTE PTR [rax],al
    3e4e:	00 00                	add    BYTE PTR [rax],al
    3e50:	f5                   	cmc
    3e51:	fe                   	(bad)
    3e52:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3e55:	00 00                	add    BYTE PTR [rax],al
    3e57:	00 c0                	add    al,al
    3e59:	03 00                	add    eax,DWORD PTR [rax]
    3e5b:	00 00                	add    BYTE PTR [rax],al
    3e5d:	00 00                	add    BYTE PTR [rax],al
    3e5f:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 3e65 <_DYNAMIC+0x85>
    3e65:	00 00                	add    BYTE PTR [rax],al
    3e67:	00 a0 04 00 00 00    	add    BYTE PTR [rax+0x4],ah
    3e6d:	00 00                	add    BYTE PTR [rax],al
    3e6f:	00 06                	add    BYTE PTR [rsi],al
    3e71:	00 00                	add    BYTE PTR [rax],al
    3e73:	00 00                	add    BYTE PTR [rax],al
    3e75:	00 00                	add    BYTE PTR [rax],al
    3e77:	00 e0                	add    al,ah
    3e79:	03 00                	add    eax,DWORD PTR [rax]
    3e7b:	00 00                	add    BYTE PTR [rax],al
    3e7d:	00 00                	add    BYTE PTR [rax],al
    3e7f:	00 0a                	add    BYTE PTR [rdx],cl
    3e81:	00 00                	add    BYTE PTR [rax],al
    3e83:	00 00                	add    BYTE PTR [rax],al
    3e85:	00 00                	add    BYTE PTR [rax],al
    3e87:	00 94 00 00 00 00 00 	add    BYTE PTR [rax+rax*1+0x0],dl
    3e8e:	00 00                	add    BYTE PTR [rax],al
    3e90:	0b 00                	or     eax,DWORD PTR [rax]
    3e92:	00 00                	add    BYTE PTR [rax],al
    3e94:	00 00                	add    BYTE PTR [rax],al
    3e96:	00 00                	add    BYTE PTR [rax],al
    3e98:	18 00                	sbb    BYTE PTR [rax],al
    3e9a:	00 00                	add    BYTE PTR [rax],al
    3e9c:	00 00                	add    BYTE PTR [rax],al
    3e9e:	00 00                	add    BYTE PTR [rax],al
    3ea0:	15 00 00 00 00       	adc    eax,0x0
	...
    3ead:	00 00                	add    BYTE PTR [rax],al
    3eaf:	00 03                	add    BYTE PTR [rbx],al
    3eb1:	00 00                	add    BYTE PTR [rax],al
    3eb3:	00 00                	add    BYTE PTR [rax],al
    3eb5:	00 00                	add    BYTE PTR [rax],al
    3eb7:	00 e8                	add    al,ch
    3eb9:	3f                   	(bad)
    3eba:	00 00                	add    BYTE PTR [rax],al
    3ebc:	00 00                	add    BYTE PTR [rax],al
    3ebe:	00 00                	add    BYTE PTR [rax],al
    3ec0:	02 00                	add    al,BYTE PTR [rax]
    3ec2:	00 00                	add    BYTE PTR [rax],al
    3ec4:	00 00                	add    BYTE PTR [rax],al
    3ec6:	00 00                	add    BYTE PTR [rax],al
    3ec8:	30 00                	xor    BYTE PTR [rax],al
    3eca:	00 00                	add    BYTE PTR [rax],al
    3ecc:	00 00                	add    BYTE PTR [rax],al
    3ece:	00 00                	add    BYTE PTR [rax],al
    3ed0:	14 00                	adc    al,0x0
    3ed2:	00 00                	add    BYTE PTR [rax],al
    3ed4:	00 00                	add    BYTE PTR [rax],al
    3ed6:	00 00                	add    BYTE PTR [rax],al
    3ed8:	07                   	(bad)
    3ed9:	00 00                	add    BYTE PTR [rax],al
    3edb:	00 00                	add    BYTE PTR [rax],al
    3edd:	00 00                	add    BYTE PTR [rax],al
    3edf:	00 17                	add    BYTE PTR [rdi],dl
    3ee1:	00 00                	add    BYTE PTR [rax],al
    3ee3:	00 00                	add    BYTE PTR [rax],al
    3ee5:	00 00                	add    BYTE PTR [rax],al
    3ee7:	00 38                	add    BYTE PTR [rax],bh
    3ee9:	06                   	(bad)
    3eea:	00 00                	add    BYTE PTR [rax],al
    3eec:	00 00                	add    BYTE PTR [rax],al
    3eee:	00 00                	add    BYTE PTR [rax],al
    3ef0:	07                   	(bad)
    3ef1:	00 00                	add    BYTE PTR [rax],al
    3ef3:	00 00                	add    BYTE PTR [rax],al
    3ef5:	00 00                	add    BYTE PTR [rax],al
    3ef7:	00 78 05             	add    BYTE PTR [rax+0x5],bh
    3efa:	00 00                	add    BYTE PTR [rax],al
    3efc:	00 00                	add    BYTE PTR [rax],al
    3efe:	00 00                	add    BYTE PTR [rax],al
    3f00:	08 00                	or     BYTE PTR [rax],al
    3f02:	00 00                	add    BYTE PTR [rax],al
    3f04:	00 00                	add    BYTE PTR [rax],al
    3f06:	00 00                	add    BYTE PTR [rax],al
    3f08:	c0 00 00             	rol    BYTE PTR [rax],0x0
    3f0b:	00 00                	add    BYTE PTR [rax],al
    3f0d:	00 00                	add    BYTE PTR [rax],al
    3f0f:	00 09                	add    BYTE PTR [rcx],cl
    3f11:	00 00                	add    BYTE PTR [rax],al
    3f13:	00 00                	add    BYTE PTR [rax],al
    3f15:	00 00                	add    BYTE PTR [rax],al
    3f17:	00 18                	add    BYTE PTR [rax],bl
    3f19:	00 00                	add    BYTE PTR [rax],al
    3f1b:	00 00                	add    BYTE PTR [rax],al
    3f1d:	00 00                	add    BYTE PTR [rax],al
    3f1f:	00 fb                	add    bl,bh
    3f21:	ff                   	(bad)
    3f22:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f25:	00 00                	add    BYTE PTR [rax],al
    3f27:	00 00                	add    BYTE PTR [rax],al
    3f29:	00 00                	add    BYTE PTR [rax],al
    3f2b:	08 00                	or     BYTE PTR [rax],al
    3f2d:	00 00                	add    BYTE PTR [rax],al
    3f2f:	00 fe                	add    dh,bh
    3f31:	ff                   	(bad)
    3f32:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f35:	00 00                	add    BYTE PTR [rax],al
    3f37:	00 48 05             	add    BYTE PTR [rax+0x5],cl
    3f3a:	00 00                	add    BYTE PTR [rax],al
    3f3c:	00 00                	add    BYTE PTR [rax],al
    3f3e:	00 00                	add    BYTE PTR [rax],al
    3f40:	ff                   	(bad)
    3f41:	ff                   	(bad)
    3f42:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f45:	00 00                	add    BYTE PTR [rax],al
    3f47:	00 01                	add    BYTE PTR [rcx],al
    3f49:	00 00                	add    BYTE PTR [rax],al
    3f4b:	00 00                	add    BYTE PTR [rax],al
    3f4d:	00 00                	add    BYTE PTR [rax],al
    3f4f:	00 f0                	add    al,dh
    3f51:	ff                   	(bad)
    3f52:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f55:	00 00                	add    BYTE PTR [rax],al
    3f57:	00 34 05 00 00 00 00 	add    BYTE PTR [rax*1+0x0],dh
    3f5e:	00 00                	add    BYTE PTR [rax],al
    3f60:	f9                   	stc
    3f61:	ff                   	(bad)
    3f62:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f65:	00 00                	add    BYTE PTR [rax],al
    3f67:	00 03                	add    BYTE PTR [rbx],al
	...

Disassembly of section .got:

0000000000003fc0 <.got>:
	...

Disassembly of section .got.plt:

0000000000003fe8 <_GLOBAL_OFFSET_TABLE_>:
    3fe8:	e0 3d                	loopne 4027 <__TMC_END__+0x7>
	...
    3ffe:	00 00                	add    BYTE PTR [rax],al
    4000:	36 10 00             	ss adc BYTE PTR [rax],al
    4003:	00 00                	add    BYTE PTR [rax],al
    4005:	00 00                	add    BYTE PTR [rax],al
    4007:	00 46 10             	add    BYTE PTR [rsi+0x10],al
    400a:	00 00                	add    BYTE PTR [rax],al
    400c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .data:

0000000000004010 <__data_start>:
	...

0000000000004018 <__dso_handle>:
    4018:	18 40 00             	sbb    BYTE PTR [rax+0x0],al
    401b:	00 00                	add    BYTE PTR [rax],al
    401d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .bss:

0000000000004020 <__bss_start>:
	...

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	47                   	rex.RXB
   1:	43                   	rex.XB
   2:	43 3a 20             	rex.XB cmp spl,BYTE PTR [r8]
   5:	28 47 4e             	sub    BYTE PTR [rdi+0x4e],al
   8:	55                   	push   rbp
   9:	29 20                	sub    DWORD PTR [rax],esp
   b:	31 32                	xor    DWORD PTR [rdx],esi
   d:	2e 32 2e             	cs xor ch,BYTE PTR [rsi]
  10:	31 20                	xor    DWORD PTR [rax],esp
  12:	32 30                	xor    dh,BYTE PTR [rax]
  14:	32 33                	xor    dh,BYTE PTR [rbx]
  16:	30 32                	xor    BYTE PTR [rdx],dh
  18:	30 31                	xor    BYTE PTR [rcx],dh
	...
