
ex1_stackguard:     file format elf64-x86-64


Disassembly of section .interp:

0000000000000318 <.interp>:
 318:	2f                   	(bad)  
 319:	6c                   	ins    BYTE PTR es:[rdi],dx
 31a:	69 62 36 34 2f 6c 64 	imul   esp,DWORD PTR [rdx+0x36],0x646c2f34
 321:	2d 6c 69 6e 75       	sub    eax,0x756e696c
 326:	78 2d                	js     355 <__cxa_finalize@plt-0xd0b>
 328:	78 38                	js     362 <__cxa_finalize@plt-0xcfe>
 32a:	36 2d 36 34 2e 73    	ss sub eax,0x732e3436
 330:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 331:	2e 32 00             	xor    al,BYTE PTR cs:[rax]

Disassembly of section .note.gnu.property:

0000000000000338 <.note.gnu.property>:
 338:	04 00                	add    al,0x0
 33a:	00 00                	add    BYTE PTR [rax],al
 33c:	10 00                	adc    BYTE PTR [rax],al
 33e:	00 00                	add    BYTE PTR [rax],al
 340:	05 00 00 00 47       	add    eax,0x47000000
 345:	4e 55                	rex.WRX push rbp
 347:	00 02                	add    BYTE PTR [rdx],al
 349:	00 00                	add    BYTE PTR [rax],al
 34b:	c0 04 00 00          	rol    BYTE PTR [rax+rax*1],0x0
 34f:	00 03                	add    BYTE PTR [rbx],al
 351:	00 00                	add    BYTE PTR [rax],al
 353:	00 00                	add    BYTE PTR [rax],al
 355:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .note.gnu.build-id:

0000000000000358 <.note.gnu.build-id>:
 358:	04 00                	add    al,0x0
 35a:	00 00                	add    BYTE PTR [rax],al
 35c:	14 00                	adc    al,0x0
 35e:	00 00                	add    BYTE PTR [rax],al
 360:	03 00                	add    eax,DWORD PTR [rax]
 362:	00 00                	add    BYTE PTR [rax],al
 364:	47                   	rex.RXB
 365:	4e 55                	rex.WRX push rbp
 367:	00 00                	add    BYTE PTR [rax],al
 369:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 36a:	d0 fb                	sar    bl,1
 36c:	1b 79 38             	sbb    edi,DWORD PTR [rcx+0x38]
 36f:	f0 3f                	lock (bad) 
 371:	4d 01 cb             	add    r11,r9
 374:	66 42 28 51 06       	data16 rex.X sub BYTE PTR [rcx+0x6],dl
 379:	91                   	xchg   ecx,eax
 37a:	77 be                	ja     33a <__cxa_finalize@plt-0xd26>

Disassembly of section .note.ABI-tag:

000000000000037c <.note.ABI-tag>:
 37c:	04 00                	add    al,0x0
 37e:	00 00                	add    BYTE PTR [rax],al
 380:	10 00                	adc    BYTE PTR [rax],al
 382:	00 00                	add    BYTE PTR [rax],al
 384:	01 00                	add    DWORD PTR [rax],eax
 386:	00 00                	add    BYTE PTR [rax],al
 388:	47                   	rex.RXB
 389:	4e 55                	rex.WRX push rbp
 38b:	00 00                	add    BYTE PTR [rax],al
 38d:	00 00                	add    BYTE PTR [rax],al
 38f:	00 03                	add    BYTE PTR [rbx],al
 391:	00 00                	add    BYTE PTR [rax],al
 393:	00 02                	add    BYTE PTR [rdx],al
 395:	00 00                	add    BYTE PTR [rax],al
 397:	00 00                	add    BYTE PTR [rax],al
 399:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .gnu.hash:

00000000000003a0 <.gnu.hash>:
 3a0:	02 00                	add    al,BYTE PTR [rax]
 3a2:	00 00                	add    BYTE PTR [rax],al
 3a4:	08 00                	or     BYTE PTR [rax],al
 3a6:	00 00                	add    BYTE PTR [rax],al
 3a8:	01 00                	add    DWORD PTR [rax],eax
 3aa:	00 00                	add    BYTE PTR [rax],al
 3ac:	06                   	(bad)  
 3ad:	00 00                	add    BYTE PTR [rax],al
 3af:	00 00                	add    BYTE PTR [rax],al
 3b1:	00 81 00 00 00 00    	add    BYTE PTR [rcx+0x0],al
 3b7:	00 08                	add    BYTE PTR [rax],cl
 3b9:	00 00                	add    BYTE PTR [rax],al
 3bb:	00 00                	add    BYTE PTR [rax],al
 3bd:	00 00                	add    BYTE PTR [rax],al
 3bf:	00 d1                	add    cl,dl
 3c1:	65 ce                	gs (bad) 
 3c3:	6d                   	ins    DWORD PTR es:[rdi],dx

Disassembly of section .dynsym:

00000000000003c8 <.dynsym>:
	...
 3e0:	5f                   	pop    rdi
 3e1:	00 00                	add    BYTE PTR [rax],al
 3e3:	00 20                	add    BYTE PTR [rax],ah
	...
 3f5:	00 00                	add    BYTE PTR [rax],al
 3f7:	00 0b                	add    BYTE PTR [rbx],cl
 3f9:	00 00                	add    BYTE PTR [rax],al
 3fb:	00 12                	add    BYTE PTR [rdx],dl
	...
 40d:	00 00                	add    BYTE PTR [rax],al
 40f:	00 12                	add    BYTE PTR [rdx],dl
 411:	00 00                	add    BYTE PTR [rax],al
 413:	00 12                	add    BYTE PTR [rdx],dl
	...
 425:	00 00                	add    BYTE PTR [rax],al
 427:	00 17                	add    BYTE PTR [rdi],dl
 429:	00 00                	add    BYTE PTR [rax],al
 42b:	00 12                	add    BYTE PTR [rdx],dl
	...
 43d:	00 00                	add    BYTE PTR [rax],al
 43f:	00 37                	add    BYTE PTR [rdi],dh
 441:	00 00                	add    BYTE PTR [rax],al
 443:	00 12                	add    BYTE PTR [rdx],dl
	...
 455:	00 00                	add    BYTE PTR [rax],al
 457:	00 7b 00             	add    BYTE PTR [rbx+0x0],bh
 45a:	00 00                	add    BYTE PTR [rax],al
 45c:	20 00                	and    BYTE PTR [rax],al
	...
 46e:	00 00                	add    BYTE PTR [rax],al
 470:	8a 00                	mov    al,BYTE PTR [rax]
 472:	00 00                	add    BYTE PTR [rax],al
 474:	20 00                	and    BYTE PTR [rax],al
	...
 486:	00 00                	add    BYTE PTR [rax],al
 488:	28 00                	sub    BYTE PTR [rax],al
 48a:	00 00                	add    BYTE PTR [rax],al
 48c:	22 00                	and    al,BYTE PTR [rax]
	...

Disassembly of section .dynstr:

00000000000004a0 <.dynstr>:
 4a0:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
 4a4:	63 2e                	movsxd ebp,DWORD PTR [rsi]
 4a6:	73 6f                	jae    517 <__cxa_finalize@plt-0xb49>
 4a8:	2e 36 00 73 74       	cs add BYTE PTR ss:[rbx+0x74],dh
 4ad:	72 63                	jb     512 <__cxa_finalize@plt-0xb4e>
 4af:	70 79                	jo     52a <__cxa_finalize@plt-0xb36>
 4b1:	00 70 75             	add    BYTE PTR [rax+0x75],dh
 4b4:	74 73                	je     529 <__cxa_finalize@plt-0xb37>
 4b6:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 4b9:	73 74                	jae    52f <__cxa_finalize@plt-0xb31>
 4bb:	61                   	(bad)  
 4bc:	63 6b 5f             	movsxd ebp,DWORD PTR [rbx+0x5f]
 4bf:	63 68 6b             	movsxd ebp,DWORD PTR [rax+0x6b]
 4c2:	5f                   	pop    rdi
 4c3:	66 61                	data16 (bad) 
 4c5:	69 6c 00 5f 5f 63 78 	imul   ebp,DWORD PTR [rax+rax*1+0x5f],0x6178635f
 4cc:	61 
 4cd:	5f                   	pop    rdi
 4ce:	66 69 6e 61 6c 69    	imul   bp,WORD PTR [rsi+0x61],0x696c
 4d4:	7a 65                	jp     53b <__cxa_finalize@plt-0xb25>
 4d6:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 4d9:	6c                   	ins    BYTE PTR es:[rdi],dx
 4da:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
 4e1:	72 74                	jb     557 <__cxa_finalize@plt-0xb09>
 4e3:	5f                   	pop    rdi
 4e4:	6d                   	ins    DWORD PTR es:[rdi],dx
 4e5:	61                   	(bad)  
 4e6:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
 4ed:	43 5f                	rex.XB pop r15
 4ef:	32 2e                	xor    ch,BYTE PTR [rsi]
 4f1:	34 00                	xor    al,0x0
 4f3:	47                   	rex.RXB
 4f4:	4c                   	rex.WR
 4f5:	49                   	rex.WB
 4f6:	42                   	rex.X
 4f7:	43 5f                	rex.XB pop r15
 4f9:	32 2e                	xor    ch,BYTE PTR [rsi]
 4fb:	32 2e                	xor    ch,BYTE PTR [rsi]
 4fd:	35 00 5f 49 54       	xor    eax,0x54495f00
 502:	4d 5f                	rex.WRB pop r15
 504:	64 65 72 65          	fs gs jb 56d <__cxa_finalize@plt-0xaf3>
 508:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 50f:	4d 
 510:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 512:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 513:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 514:	65 54                	gs push rsp
 516:	61                   	(bad)  
 517:	62                   	(bad)  
 518:	6c                   	ins    BYTE PTR es:[rdi],dx
 519:	65 00 5f 5f          	add    BYTE PTR gs:[rdi+0x5f],bl
 51d:	67 6d                	ins    DWORD PTR es:[edi],dx
 51f:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 520:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 521:	5f                   	pop    rdi
 522:	73 74                	jae    598 <__cxa_finalize@plt-0xac8>
 524:	61                   	(bad)  
 525:	72 74                	jb     59b <__cxa_finalize@plt-0xac5>
 527:	5f                   	pop    rdi
 528:	5f                   	pop    rdi
 529:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 52c:	54                   	push   rsp
 52d:	4d 5f                	rex.WRB pop r15
 52f:	72 65                	jb     596 <__cxa_finalize@plt-0xaca>
 531:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 538:	4d 
 539:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 53b:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 53c:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 53d:	65 54                	gs push rsp
 53f:	61                   	(bad)  
 540:	62                   	.byte 0x62
 541:	6c                   	ins    BYTE PTR es:[rdi],dx
 542:	65                   	gs
	...

Disassembly of section .gnu.version:

0000000000000544 <.gnu.version>:
 544:	00 00                	add    BYTE PTR [rax],al
 546:	00 00                	add    BYTE PTR [rax],al
 548:	02 00                	add    al,BYTE PTR [rax]
 54a:	02 00                	add    al,BYTE PTR [rax]
 54c:	03 00                	add    eax,DWORD PTR [rax]
 54e:	02 00                	add    al,BYTE PTR [rax]
 550:	00 00                	add    BYTE PTR [rax],al
 552:	00 00                	add    BYTE PTR [rax],al
 554:	02 00                	add    al,BYTE PTR [rax]

Disassembly of section .gnu.version_r:

0000000000000558 <.gnu.version_r>:
 558:	01 00                	add    DWORD PTR [rax],eax
 55a:	02 00                	add    al,BYTE PTR [rax]
 55c:	01 00                	add    DWORD PTR [rax],eax
 55e:	00 00                	add    BYTE PTR [rax],al
 560:	10 00                	adc    BYTE PTR [rax],al
 562:	00 00                	add    BYTE PTR [rax],al
 564:	00 00                	add    BYTE PTR [rax],al
 566:	00 00                	add    BYTE PTR [rax],al
 568:	14 69                	adc    al,0x69
 56a:	69 0d 00 00 03 00 49 	imul   ecx,DWORD PTR [rip+0x30000],0x49        # 30574 <__stack_chk_fail@plt+0x2f4e4>
 571:	00 00 00 
 574:	10 00                	adc    BYTE PTR [rax],al
 576:	00 00                	add    BYTE PTR [rax],al
 578:	75 1a                	jne    594 <__cxa_finalize@plt-0xacc>
 57a:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
 580:	53                   	push   rbx
 581:	00 00                	add    BYTE PTR [rax],al
 583:	00 00                	add    BYTE PTR [rax],al
 585:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000000588 <.rela.dyn>:
 588:	a8 3d                	test   al,0x3d
 58a:	00 00                	add    BYTE PTR [rax],al
 58c:	00 00                	add    BYTE PTR [rax],al
 58e:	00 00                	add    BYTE PTR [rax],al
 590:	08 00                	or     BYTE PTR [rax],al
 592:	00 00                	add    BYTE PTR [rax],al
 594:	00 00                	add    BYTE PTR [rax],al
 596:	00 00                	add    BYTE PTR [rax],al
 598:	80 11 00             	adc    BYTE PTR [rcx],0x0
 59b:	00 00                	add    BYTE PTR [rax],al
 59d:	00 00                	add    BYTE PTR [rax],al
 59f:	00 b0 3d 00 00 00    	add    BYTE PTR [rax+0x3d],dh
 5a5:	00 00                	add    BYTE PTR [rax],al
 5a7:	00 08                	add    BYTE PTR [rax],cl
 5a9:	00 00                	add    BYTE PTR [rax],al
 5ab:	00 00                	add    BYTE PTR [rax],al
 5ad:	00 00                	add    BYTE PTR [rax],al
 5af:	00 40 11             	add    BYTE PTR [rax+0x11],al
 5b2:	00 00                	add    BYTE PTR [rax],al
 5b4:	00 00                	add    BYTE PTR [rax],al
 5b6:	00 00                	add    BYTE PTR [rax],al
 5b8:	08 40 00             	or     BYTE PTR [rax+0x0],al
 5bb:	00 00                	add    BYTE PTR [rax],al
 5bd:	00 00                	add    BYTE PTR [rax],al
 5bf:	00 08                	add    BYTE PTR [rax],cl
 5c1:	00 00                	add    BYTE PTR [rax],al
 5c3:	00 00                	add    BYTE PTR [rax],al
 5c5:	00 00                	add    BYTE PTR [rax],al
 5c7:	00 08                	add    BYTE PTR [rax],cl
 5c9:	40 00 00             	add    BYTE PTR [rax],al
 5cc:	00 00                	add    BYTE PTR [rax],al
 5ce:	00 00                	add    BYTE PTR [rax],al
 5d0:	d8 3f                	fdivr  DWORD PTR [rdi]
 5d2:	00 00                	add    BYTE PTR [rax],al
 5d4:	00 00                	add    BYTE PTR [rax],al
 5d6:	00 00                	add    BYTE PTR [rax],al
 5d8:	06                   	(bad)  
 5d9:	00 00                	add    BYTE PTR [rax],al
 5db:	00 01                	add    BYTE PTR [rcx],al
	...
 5e5:	00 00                	add    BYTE PTR [rax],al
 5e7:	00 e0                	add    al,ah
 5e9:	3f                   	(bad)  
 5ea:	00 00                	add    BYTE PTR [rax],al
 5ec:	00 00                	add    BYTE PTR [rax],al
 5ee:	00 00                	add    BYTE PTR [rax],al
 5f0:	06                   	(bad)  
 5f1:	00 00                	add    BYTE PTR [rax],al
 5f3:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 5f9 <__cxa_finalize@plt-0xa67>
 5f9:	00 00                	add    BYTE PTR [rax],al
 5fb:	00 00                	add    BYTE PTR [rax],al
 5fd:	00 00                	add    BYTE PTR [rax],al
 5ff:	00 e8                	add    al,ch
 601:	3f                   	(bad)  
 602:	00 00                	add    BYTE PTR [rax],al
 604:	00 00                	add    BYTE PTR [rax],al
 606:	00 00                	add    BYTE PTR [rax],al
 608:	06                   	(bad)  
 609:	00 00                	add    BYTE PTR [rax],al
 60b:	00 06                	add    BYTE PTR [rsi],al
	...
 615:	00 00                	add    BYTE PTR [rax],al
 617:	00 f0                	add    al,dh
 619:	3f                   	(bad)  
 61a:	00 00                	add    BYTE PTR [rax],al
 61c:	00 00                	add    BYTE PTR [rax],al
 61e:	00 00                	add    BYTE PTR [rax],al
 620:	06                   	(bad)  
 621:	00 00                	add    BYTE PTR [rax],al
 623:	00 07                	add    BYTE PTR [rdi],al
	...
 62d:	00 00                	add    BYTE PTR [rax],al
 62f:	00 f8                	add    al,bh
 631:	3f                   	(bad)  
 632:	00 00                	add    BYTE PTR [rax],al
 634:	00 00                	add    BYTE PTR [rax],al
 636:	00 00                	add    BYTE PTR [rax],al
 638:	06                   	(bad)  
 639:	00 00                	add    BYTE PTR [rax],al
 63b:	00 08                	add    BYTE PTR [rax],cl
	...

Disassembly of section .rela.plt:

0000000000000648 <.rela.plt>:
 648:	c0 3f 00             	sar    BYTE PTR [rdi],0x0
 64b:	00 00                	add    BYTE PTR [rax],al
 64d:	00 00                	add    BYTE PTR [rax],al
 64f:	00 07                	add    BYTE PTR [rdi],al
 651:	00 00                	add    BYTE PTR [rax],al
 653:	00 02                	add    BYTE PTR [rdx],al
	...
 65d:	00 00                	add    BYTE PTR [rax],al
 65f:	00 c8                	add    al,cl
 661:	3f                   	(bad)  
 662:	00 00                	add    BYTE PTR [rax],al
 664:	00 00                	add    BYTE PTR [rax],al
 666:	00 00                	add    BYTE PTR [rax],al
 668:	07                   	(bad)  
 669:	00 00                	add    BYTE PTR [rax],al
 66b:	00 03                	add    BYTE PTR [rbx],al
	...
 675:	00 00                	add    BYTE PTR [rax],al
 677:	00 d0                	add    al,dl
 679:	3f                   	(bad)  
 67a:	00 00                	add    BYTE PTR [rax],al
 67c:	00 00                	add    BYTE PTR [rax],al
 67e:	00 00                	add    BYTE PTR [rax],al
 680:	07                   	(bad)  
 681:	00 00                	add    BYTE PTR [rax],al
 683:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...

Disassembly of section .init:

0000000000001000 <.init>:
    1000:	f3 0f 1e fa          	endbr64 
    1004:	48 83 ec 08          	sub    rsp,0x8
    1008:	48 8b 05 d9 2f 00 00 	mov    rax,QWORD PTR [rip+0x2fd9]        # 3fe8 <__stack_chk_fail@plt+0x2f58>
    100f:	48 85 c0             	test   rax,rax
    1012:	74 02                	je     1016 <__cxa_finalize@plt-0x4a>
    1014:	ff d0                	call   rax
    1016:	48 83 c4 08          	add    rsp,0x8
    101a:	c3                   	ret    

Disassembly of section .plt:

0000000000001020 <.plt>:
    1020:	ff 35 8a 2f 00 00    	push   QWORD PTR [rip+0x2f8a]        # 3fb0 <__stack_chk_fail@plt+0x2f20>
    1026:	f2 ff 25 8b 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f8b]        # 3fb8 <__stack_chk_fail@plt+0x2f28>
    102d:	0f 1f 00             	nop    DWORD PTR [rax]
    1030:	f3 0f 1e fa          	endbr64 
    1034:	68 00 00 00 00       	push   0x0
    1039:	f2 e9 e1 ff ff ff    	bnd jmp 1020 <__cxa_finalize@plt-0x40>
    103f:	90                   	nop
    1040:	f3 0f 1e fa          	endbr64 
    1044:	68 01 00 00 00       	push   0x1
    1049:	f2 e9 d1 ff ff ff    	bnd jmp 1020 <__cxa_finalize@plt-0x40>
    104f:	90                   	nop
    1050:	f3 0f 1e fa          	endbr64 
    1054:	68 02 00 00 00       	push   0x2
    1059:	f2 e9 c1 ff ff ff    	bnd jmp 1020 <__cxa_finalize@plt-0x40>
    105f:	90                   	nop

Disassembly of section .plt.got:

0000000000001060 <__cxa_finalize@plt>:
    1060:	f3 0f 1e fa          	endbr64 
    1064:	f2 ff 25 8d 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f8d]        # 3ff8 <__stack_chk_fail@plt+0x2f68>
    106b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .plt.sec:

0000000000001070 <strcpy@plt>:
    1070:	f3 0f 1e fa          	endbr64 
    1074:	f2 ff 25 45 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f45]        # 3fc0 <__stack_chk_fail@plt+0x2f30>
    107b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001080 <puts@plt>:
    1080:	f3 0f 1e fa          	endbr64 
    1084:	f2 ff 25 3d 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f3d]        # 3fc8 <__stack_chk_fail@plt+0x2f38>
    108b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001090 <__stack_chk_fail@plt>:
    1090:	f3 0f 1e fa          	endbr64 
    1094:	f2 ff 25 35 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f35]        # 3fd0 <__stack_chk_fail@plt+0x2f40>
    109b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .text:

00000000000010a0 <.text>:
    10a0:	f3 0f 1e fa          	endbr64 
    10a4:	31 ed                	xor    ebp,ebp
    10a6:	49 89 d1             	mov    r9,rdx
    10a9:	5e                   	pop    rsi
    10aa:	48 89 e2             	mov    rdx,rsp
    10ad:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
    10b1:	50                   	push   rax
    10b2:	54                   	push   rsp
    10b3:	4c 8d 05 d6 01 00 00 	lea    r8,[rip+0x1d6]        # 1290 <__stack_chk_fail@plt+0x200>
    10ba:	48 8d 0d 5f 01 00 00 	lea    rcx,[rip+0x15f]        # 1220 <__stack_chk_fail@plt+0x190>
    10c1:	48 8d 3d 23 01 00 00 	lea    rdi,[rip+0x123]        # 11eb <__stack_chk_fail@plt+0x15b>
    10c8:	ff 15 12 2f 00 00    	call   QWORD PTR [rip+0x2f12]        # 3fe0 <__stack_chk_fail@plt+0x2f50>
    10ce:	f4                   	hlt    
    10cf:	90                   	nop
    10d0:	48 8d 3d 39 2f 00 00 	lea    rdi,[rip+0x2f39]        # 4010 <__stack_chk_fail@plt+0x2f80>
    10d7:	48 8d 05 32 2f 00 00 	lea    rax,[rip+0x2f32]        # 4010 <__stack_chk_fail@plt+0x2f80>
    10de:	48 39 f8             	cmp    rax,rdi
    10e1:	74 15                	je     10f8 <__stack_chk_fail@plt+0x68>
    10e3:	48 8b 05 ee 2e 00 00 	mov    rax,QWORD PTR [rip+0x2eee]        # 3fd8 <__stack_chk_fail@plt+0x2f48>
    10ea:	48 85 c0             	test   rax,rax
    10ed:	74 09                	je     10f8 <__stack_chk_fail@plt+0x68>
    10ef:	ff e0                	jmp    rax
    10f1:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    10f8:	c3                   	ret    
    10f9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1100:	48 8d 3d 09 2f 00 00 	lea    rdi,[rip+0x2f09]        # 4010 <__stack_chk_fail@plt+0x2f80>
    1107:	48 8d 35 02 2f 00 00 	lea    rsi,[rip+0x2f02]        # 4010 <__stack_chk_fail@plt+0x2f80>
    110e:	48 29 fe             	sub    rsi,rdi
    1111:	48 89 f0             	mov    rax,rsi
    1114:	48 c1 ee 3f          	shr    rsi,0x3f
    1118:	48 c1 f8 03          	sar    rax,0x3
    111c:	48 01 c6             	add    rsi,rax
    111f:	48 d1 fe             	sar    rsi,1
    1122:	74 14                	je     1138 <__stack_chk_fail@plt+0xa8>
    1124:	48 8b 05 c5 2e 00 00 	mov    rax,QWORD PTR [rip+0x2ec5]        # 3ff0 <__stack_chk_fail@plt+0x2f60>
    112b:	48 85 c0             	test   rax,rax
    112e:	74 08                	je     1138 <__stack_chk_fail@plt+0xa8>
    1130:	ff e0                	jmp    rax
    1132:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]
    1138:	c3                   	ret    
    1139:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1140:	f3 0f 1e fa          	endbr64 
    1144:	80 3d c5 2e 00 00 00 	cmp    BYTE PTR [rip+0x2ec5],0x0        # 4010 <__stack_chk_fail@plt+0x2f80>
    114b:	75 2b                	jne    1178 <__stack_chk_fail@plt+0xe8>
    114d:	55                   	push   rbp
    114e:	48 83 3d a2 2e 00 00 	cmp    QWORD PTR [rip+0x2ea2],0x0        # 3ff8 <__stack_chk_fail@plt+0x2f68>
    1155:	00 
    1156:	48 89 e5             	mov    rbp,rsp
    1159:	74 0c                	je     1167 <__stack_chk_fail@plt+0xd7>
    115b:	48 8b 3d a6 2e 00 00 	mov    rdi,QWORD PTR [rip+0x2ea6]        # 4008 <__stack_chk_fail@plt+0x2f78>
    1162:	e8 f9 fe ff ff       	call   1060 <__cxa_finalize@plt>
    1167:	e8 64 ff ff ff       	call   10d0 <__stack_chk_fail@plt+0x40>
    116c:	c6 05 9d 2e 00 00 01 	mov    BYTE PTR [rip+0x2e9d],0x1        # 4010 <__stack_chk_fail@plt+0x2f80>
    1173:	5d                   	pop    rbp
    1174:	c3                   	ret    
    1175:	0f 1f 00             	nop    DWORD PTR [rax]
    1178:	c3                   	ret    
    1179:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1180:	f3 0f 1e fa          	endbr64 
    1184:	e9 77 ff ff ff       	jmp    1100 <__stack_chk_fail@plt+0x70>
    1189:	f3 0f 1e fa          	endbr64 
    118d:	55                   	push   rbp
    118e:	48 89 e5             	mov    rbp,rsp
    1191:	48 83 ec 50          	sub    rsp,0x50
    1195:	48 89 7d b8          	mov    QWORD PTR [rbp-0x48],rdi
    1199:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    11a0:	00 00 
    11a2:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    11a6:	31 c0                	xor    eax,eax
    11a8:	c7 45 cc 00 00 00 00 	mov    DWORD PTR [rbp-0x34],0x0
    11af:	48 8b 55 b8          	mov    rdx,QWORD PTR [rbp-0x48]
    11b3:	48 8d 45 d0          	lea    rax,[rbp-0x30]
    11b7:	48 89 d6             	mov    rsi,rdx
    11ba:	48 89 c7             	mov    rdi,rax
    11bd:	e8 ae fe ff ff       	call   1070 <strcpy@plt>
    11c2:	83 7d cc 00          	cmp    DWORD PTR [rbp-0x34],0x0
    11c6:	74 0c                	je     11d4 <__stack_chk_fail@plt+0x144>
    11c8:	48 8d 3d 35 0e 00 00 	lea    rdi,[rip+0xe35]        # 2004 <__stack_chk_fail@plt+0xf74>
    11cf:	e8 ac fe ff ff       	call   1080 <puts@plt>
    11d4:	90                   	nop
    11d5:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    11d9:	64 48 33 04 25 28 00 	xor    rax,QWORD PTR fs:0x28
    11e0:	00 00 
    11e2:	74 05                	je     11e9 <__stack_chk_fail@plt+0x159>
    11e4:	e8 a7 fe ff ff       	call   1090 <__stack_chk_fail@plt>
    11e9:	c9                   	leave  
    11ea:	c3                   	ret    
    11eb:	f3 0f 1e fa          	endbr64 
    11ef:	55                   	push   rbp
    11f0:	48 89 e5             	mov    rbp,rsp
    11f3:	48 83 ec 10          	sub    rsp,0x10
    11f7:	89 7d fc             	mov    DWORD PTR [rbp-0x4],edi
    11fa:	48 89 75 f0          	mov    QWORD PTR [rbp-0x10],rsi
    11fe:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
    1202:	48 83 c0 08          	add    rax,0x8
    1206:	48 8b 00             	mov    rax,QWORD PTR [rax]
    1209:	48 89 c7             	mov    rdi,rax
    120c:	e8 78 ff ff ff       	call   1189 <__stack_chk_fail@plt+0xf9>
    1211:	b8 00 00 00 00       	mov    eax,0x0
    1216:	c9                   	leave  
    1217:	c3                   	ret    
    1218:	0f 1f 84 00 00 00 00 	nop    DWORD PTR [rax+rax*1+0x0]
    121f:	00 
    1220:	f3 0f 1e fa          	endbr64 
    1224:	41 57                	push   r15
    1226:	4c 8d 3d 7b 2b 00 00 	lea    r15,[rip+0x2b7b]        # 3da8 <__stack_chk_fail@plt+0x2d18>
    122d:	41 56                	push   r14
    122f:	49 89 d6             	mov    r14,rdx
    1232:	41 55                	push   r13
    1234:	49 89 f5             	mov    r13,rsi
    1237:	41 54                	push   r12
    1239:	41 89 fc             	mov    r12d,edi
    123c:	55                   	push   rbp
    123d:	48 8d 2d 6c 2b 00 00 	lea    rbp,[rip+0x2b6c]        # 3db0 <__stack_chk_fail@plt+0x2d20>
    1244:	53                   	push   rbx
    1245:	4c 29 fd             	sub    rbp,r15
    1248:	48 83 ec 08          	sub    rsp,0x8
    124c:	e8 af fd ff ff       	call   1000 <__cxa_finalize@plt-0x60>
    1251:	48 c1 fd 03          	sar    rbp,0x3
    1255:	74 1f                	je     1276 <__stack_chk_fail@plt+0x1e6>
    1257:	31 db                	xor    ebx,ebx
    1259:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1260:	4c 89 f2             	mov    rdx,r14
    1263:	4c 89 ee             	mov    rsi,r13
    1266:	44 89 e7             	mov    edi,r12d
    1269:	41 ff 14 df          	call   QWORD PTR [r15+rbx*8]
    126d:	48 83 c3 01          	add    rbx,0x1
    1271:	48 39 dd             	cmp    rbp,rbx
    1274:	75 ea                	jne    1260 <__stack_chk_fail@plt+0x1d0>
    1276:	48 83 c4 08          	add    rsp,0x8
    127a:	5b                   	pop    rbx
    127b:	5d                   	pop    rbp
    127c:	41 5c                	pop    r12
    127e:	41 5d                	pop    r13
    1280:	41 5e                	pop    r14
    1282:	41 5f                	pop    r15
    1284:	c3                   	ret    
    1285:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
    128c:	00 00 00 00 
    1290:	f3 0f 1e fa          	endbr64 
    1294:	c3                   	ret    

Disassembly of section .fini:

0000000000001298 <.fini>:
    1298:	f3 0f 1e fa          	endbr64 
    129c:	48 83 ec 08          	sub    rsp,0x8
    12a0:	48 83 c4 08          	add    rsp,0x8
    12a4:	c3                   	ret    

Disassembly of section .rodata:

0000000000002000 <.rodata>:
    2000:	01 00                	add    DWORD PTR [rax],eax
    2002:	02 00                	add    al,BYTE PTR [rax]
    2004:	47 6f                	rex.RXB outs dx,DWORD PTR ds:[rsi]
    2006:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2007:	64 20 64 61 79       	and    BYTE PTR fs:[rcx+riz*2+0x79],ah
    200c:	2c 20                	sub    al,0x20
    200e:	6d                   	ins    DWORD PTR es:[rdi],dx
    200f:	61                   	(bad)  
    2010:	73 74                	jae    2086 <__stack_chk_fail@plt+0xff6>
    2012:	65 72 00             	gs jb  2015 <__stack_chk_fail@plt+0xf85>

Disassembly of section .eh_frame_hdr:

0000000000002018 <.eh_frame_hdr>:
    2018:	01 1b                	add    DWORD PTR [rbx],ebx
    201a:	03 3b                	add    edi,DWORD PTR [rbx]
    201c:	4c 00 00             	rex.WR add BYTE PTR [rax],r8b
    201f:	00 08                	add    BYTE PTR [rax],cl
    2021:	00 00                	add    BYTE PTR [rax],al
    2023:	00 08                	add    BYTE PTR [rax],cl
    2025:	f0 ff                	lock (bad) 
    2027:	ff 80 00 00 00 48    	inc    DWORD PTR [rax+0x48000000]
    202d:	f0 ff                	lock (bad) 
    202f:	ff a8 00 00 00 58    	jmp    FWORD PTR [rax+0x58000000]
    2035:	f0 ff                	lock (bad) 
    2037:	ff c0                	inc    eax
    2039:	00 00                	add    BYTE PTR [rax],al
    203b:	00 88 f0 ff ff 68    	add    BYTE PTR [rax+0x68fffff0],cl
    2041:	00 00                	add    BYTE PTR [rax],al
    2043:	00 71 f1             	add    BYTE PTR [rcx-0xf],dh
    2046:	ff                   	(bad)  
    2047:	ff                   	(bad)  
    2048:	d8 00                	fadd   DWORD PTR [rax]
    204a:	00 00                	add    BYTE PTR [rax],al
    204c:	d3 f1                	shl    ecx,cl
    204e:	ff                   	(bad)  
    204f:	ff                   	(bad)  
    2050:	f8                   	clc    
    2051:	00 00                	add    BYTE PTR [rax],al
    2053:	00 08                	add    BYTE PTR [rax],cl
    2055:	f2 ff                	repnz (bad) 
    2057:	ff 18                	call   FWORD PTR [rax]
    2059:	01 00                	add    DWORD PTR [rax],eax
    205b:	00 78 f2             	add    BYTE PTR [rax-0xe],bh
    205e:	ff                   	(bad)  
    205f:	ff 60 01             	jmp    QWORD PTR [rax+0x1]
	...

Disassembly of section .eh_frame:

0000000000002068 <.eh_frame>:
    2068:	14 00                	adc    al,0x0
    206a:	00 00                	add    BYTE PTR [rax],al
    206c:	00 00                	add    BYTE PTR [rax],al
    206e:	00 00                	add    BYTE PTR [rax],al
    2070:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
    2073:	00 01                	add    BYTE PTR [rcx],al
    2075:	78 10                	js     2087 <__stack_chk_fail@plt+0xff7>
    2077:	01 1b                	add    DWORD PTR [rbx],ebx
    2079:	0c 07                	or     al,0x7
    207b:	08 90 01 00 00 14    	or     BYTE PTR [rax+0x14000001],dl
    2081:	00 00                	add    BYTE PTR [rax],al
    2083:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    2086:	00 00                	add    BYTE PTR [rax],al
    2088:	18 f0                	sbb    al,dh
    208a:	ff                   	(bad)  
    208b:	ff 2f                	jmp    FWORD PTR [rdi]
    208d:	00 00                	add    BYTE PTR [rax],al
    208f:	00 00                	add    BYTE PTR [rax],al
    2091:	44 07                	rex.R (bad) 
    2093:	10 00                	adc    BYTE PTR [rax],al
    2095:	00 00                	add    BYTE PTR [rax],al
    2097:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
    209a:	00 00                	add    BYTE PTR [rax],al
    209c:	34 00                	xor    al,0x0
    209e:	00 00                	add    BYTE PTR [rax],al
    20a0:	80 ef ff             	sub    bh,0xff
    20a3:	ff 40 00             	inc    DWORD PTR [rax+0x0]
    20a6:	00 00                	add    BYTE PTR [rax],al
    20a8:	00 0e                	add    BYTE PTR [rsi],cl
    20aa:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
    20ad:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
    20b0:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
    20b3:	80 00 3f             	add    BYTE PTR [rax],0x3f
    20b6:	1a 3a                	sbb    bh,BYTE PTR [rdx]
    20b8:	2a 33                	sub    dh,BYTE PTR [rbx]
    20ba:	24 22                	and    al,0x22
    20bc:	00 00                	add    BYTE PTR [rax],al
    20be:	00 00                	add    BYTE PTR [rax],al
    20c0:	14 00                	adc    al,0x0
    20c2:	00 00                	add    BYTE PTR [rax],al
    20c4:	5c                   	pop    rsp
    20c5:	00 00                	add    BYTE PTR [rax],al
    20c7:	00 98 ef ff ff 10    	add    BYTE PTR [rax+0x10ffffef],bl
	...
    20d5:	00 00                	add    BYTE PTR [rax],al
    20d7:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    20da:	00 00                	add    BYTE PTR [rax],al
    20dc:	74 00                	je     20de <__stack_chk_fail@plt+0x104e>
    20de:	00 00                	add    BYTE PTR [rax],al
    20e0:	90                   	nop
    20e1:	ef                   	out    dx,eax
    20e2:	ff                   	(bad)  
    20e3:	ff 30                	push   QWORD PTR [rax]
	...
    20ed:	00 00                	add    BYTE PTR [rax],al
    20ef:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    20f2:	00 00                	add    BYTE PTR [rax],al
    20f4:	8c 00                	mov    WORD PTR [rax],es
    20f6:	00 00                	add    BYTE PTR [rax],al
    20f8:	91                   	xchg   ecx,eax
    20f9:	f0 ff                	lock (bad) 
    20fb:	ff 62 00             	jmp    QWORD PTR [rdx+0x0]
    20fe:	00 00                	add    BYTE PTR [rax],al
    2100:	00 45 0e             	add    BYTE PTR [rbp+0xe],al
    2103:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2109:	02 59 0c             	add    bl,BYTE PTR [rcx+0xc]
    210c:	07                   	(bad)  
    210d:	08 00                	or     BYTE PTR [rax],al
    210f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    2112:	00 00                	add    BYTE PTR [rax],al
    2114:	ac                   	lods   al,BYTE PTR ds:[rsi]
    2115:	00 00                	add    BYTE PTR [rax],al
    2117:	00 d3                	add    bl,dl
    2119:	f0 ff                	lock (bad) 
    211b:	ff 2d 00 00 00 00    	jmp    FWORD PTR [rip+0x0]        # 2121 <__stack_chk_fail@plt+0x1091>
    2121:	45 0e                	rex.RB (bad) 
    2123:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2129:	64 0c 07             	fs or  al,0x7
    212c:	08 00                	or     BYTE PTR [rax],al
    212e:	00 00                	add    BYTE PTR [rax],al
    2130:	44 00 00             	add    BYTE PTR [rax],r8b
    2133:	00 cc                	add    ah,cl
    2135:	00 00                	add    BYTE PTR [rax],al
    2137:	00 e8                	add    al,ch
    2139:	f0 ff                	lock (bad) 
    213b:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
    213e:	00 00                	add    BYTE PTR [rax],al
    2140:	00 46 0e             	add    BYTE PTR [rsi+0xe],al
    2143:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
    2149:	8e 03                	mov    es,WORD PTR [rbx]
    214b:	45 0e                	rex.RB (bad) 
    214d:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
    2153:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff86302f9d <__stack_chk_fail@plt+0xffffffff86301f0d>
    2159:	06                   	(bad)  
    215a:	48 0e                	rex.W (bad) 
    215c:	38 83 07 47 0e 40    	cmp    BYTE PTR [rbx+0x400e4707],al
    2162:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2163:	0e                   	(bad)  
    2164:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
    2167:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
    216a:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
    216d:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
    2170:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
    2173:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
    2176:	08 00                	or     BYTE PTR [rax],al
    2178:	10 00                	adc    BYTE PTR [rax],al
    217a:	00 00                	add    BYTE PTR [rax],al
    217c:	14 01                	adc    al,0x1
    217e:	00 00                	add    BYTE PTR [rax],al
    2180:	10 f1                	adc    cl,dh
    2182:	ff                   	(bad)  
    2183:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 2189 <__stack_chk_fail@plt+0x10f9>
    2189:	00 00                	add    BYTE PTR [rax],al
    218b:	00 00                	add    BYTE PTR [rax],al
    218d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000003da8 <.init_array>:
    3da8:	80 11 00             	adc    BYTE PTR [rcx],0x0
    3dab:	00 00                	add    BYTE PTR [rax],al
    3dad:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000003db0 <.fini_array>:
    3db0:	40 11 00             	rex adc DWORD PTR [rax],eax
    3db3:	00 00                	add    BYTE PTR [rax],al
    3db5:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000003db8 <.dynamic>:
    3db8:	01 00                	add    DWORD PTR [rax],eax
    3dba:	00 00                	add    BYTE PTR [rax],al
    3dbc:	00 00                	add    BYTE PTR [rax],al
    3dbe:	00 00                	add    BYTE PTR [rax],al
    3dc0:	01 00                	add    DWORD PTR [rax],eax
    3dc2:	00 00                	add    BYTE PTR [rax],al
    3dc4:	00 00                	add    BYTE PTR [rax],al
    3dc6:	00 00                	add    BYTE PTR [rax],al
    3dc8:	0c 00                	or     al,0x0
    3dca:	00 00                	add    BYTE PTR [rax],al
    3dcc:	00 00                	add    BYTE PTR [rax],al
    3dce:	00 00                	add    BYTE PTR [rax],al
    3dd0:	00 10                	add    BYTE PTR [rax],dl
    3dd2:	00 00                	add    BYTE PTR [rax],al
    3dd4:	00 00                	add    BYTE PTR [rax],al
    3dd6:	00 00                	add    BYTE PTR [rax],al
    3dd8:	0d 00 00 00 00       	or     eax,0x0
    3ddd:	00 00                	add    BYTE PTR [rax],al
    3ddf:	00 98 12 00 00 00    	add    BYTE PTR [rax+0x12],bl
    3de5:	00 00                	add    BYTE PTR [rax],al
    3de7:	00 19                	add    BYTE PTR [rcx],bl
    3de9:	00 00                	add    BYTE PTR [rax],al
    3deb:	00 00                	add    BYTE PTR [rax],al
    3ded:	00 00                	add    BYTE PTR [rax],al
    3def:	00 a8 3d 00 00 00    	add    BYTE PTR [rax+0x3d],ch
    3df5:	00 00                	add    BYTE PTR [rax],al
    3df7:	00 1b                	add    BYTE PTR [rbx],bl
    3df9:	00 00                	add    BYTE PTR [rax],al
    3dfb:	00 00                	add    BYTE PTR [rax],al
    3dfd:	00 00                	add    BYTE PTR [rax],al
    3dff:	00 08                	add    BYTE PTR [rax],cl
    3e01:	00 00                	add    BYTE PTR [rax],al
    3e03:	00 00                	add    BYTE PTR [rax],al
    3e05:	00 00                	add    BYTE PTR [rax],al
    3e07:	00 1a                	add    BYTE PTR [rdx],bl
    3e09:	00 00                	add    BYTE PTR [rax],al
    3e0b:	00 00                	add    BYTE PTR [rax],al
    3e0d:	00 00                	add    BYTE PTR [rax],al
    3e0f:	00 b0 3d 00 00 00    	add    BYTE PTR [rax+0x3d],dh
    3e15:	00 00                	add    BYTE PTR [rax],al
    3e17:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    3e1a:	00 00                	add    BYTE PTR [rax],al
    3e1c:	00 00                	add    BYTE PTR [rax],al
    3e1e:	00 00                	add    BYTE PTR [rax],al
    3e20:	08 00                	or     BYTE PTR [rax],al
    3e22:	00 00                	add    BYTE PTR [rax],al
    3e24:	00 00                	add    BYTE PTR [rax],al
    3e26:	00 00                	add    BYTE PTR [rax],al
    3e28:	f5                   	cmc    
    3e29:	fe                   	(bad)  
    3e2a:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3e2d:	00 00                	add    BYTE PTR [rax],al
    3e2f:	00 a0 03 00 00 00    	add    BYTE PTR [rax+0x3],ah
    3e35:	00 00                	add    BYTE PTR [rax],al
    3e37:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 3e3d <__stack_chk_fail@plt+0x2dad>
    3e3d:	00 00                	add    BYTE PTR [rax],al
    3e3f:	00 a0 04 00 00 00    	add    BYTE PTR [rax+0x4],ah
    3e45:	00 00                	add    BYTE PTR [rax],al
    3e47:	00 06                	add    BYTE PTR [rsi],al
    3e49:	00 00                	add    BYTE PTR [rax],al
    3e4b:	00 00                	add    BYTE PTR [rax],al
    3e4d:	00 00                	add    BYTE PTR [rax],al
    3e4f:	00 c8                	add    al,cl
    3e51:	03 00                	add    eax,DWORD PTR [rax]
    3e53:	00 00                	add    BYTE PTR [rax],al
    3e55:	00 00                	add    BYTE PTR [rax],al
    3e57:	00 0a                	add    BYTE PTR [rdx],cl
    3e59:	00 00                	add    BYTE PTR [rax],al
    3e5b:	00 00                	add    BYTE PTR [rax],al
    3e5d:	00 00                	add    BYTE PTR [rax],al
    3e5f:	00 a4 00 00 00 00 00 	add    BYTE PTR [rax+rax*1+0x0],ah
    3e66:	00 00                	add    BYTE PTR [rax],al
    3e68:	0b 00                	or     eax,DWORD PTR [rax]
    3e6a:	00 00                	add    BYTE PTR [rax],al
    3e6c:	00 00                	add    BYTE PTR [rax],al
    3e6e:	00 00                	add    BYTE PTR [rax],al
    3e70:	18 00                	sbb    BYTE PTR [rax],al
    3e72:	00 00                	add    BYTE PTR [rax],al
    3e74:	00 00                	add    BYTE PTR [rax],al
    3e76:	00 00                	add    BYTE PTR [rax],al
    3e78:	15 00 00 00 00       	adc    eax,0x0
	...
    3e85:	00 00                	add    BYTE PTR [rax],al
    3e87:	00 03                	add    BYTE PTR [rbx],al
    3e89:	00 00                	add    BYTE PTR [rax],al
    3e8b:	00 00                	add    BYTE PTR [rax],al
    3e8d:	00 00                	add    BYTE PTR [rax],al
    3e8f:	00 a8 3f 00 00 00    	add    BYTE PTR [rax+0x3f],ch
    3e95:	00 00                	add    BYTE PTR [rax],al
    3e97:	00 02                	add    BYTE PTR [rdx],al
    3e99:	00 00                	add    BYTE PTR [rax],al
    3e9b:	00 00                	add    BYTE PTR [rax],al
    3e9d:	00 00                	add    BYTE PTR [rax],al
    3e9f:	00 48 00             	add    BYTE PTR [rax+0x0],cl
    3ea2:	00 00                	add    BYTE PTR [rax],al
    3ea4:	00 00                	add    BYTE PTR [rax],al
    3ea6:	00 00                	add    BYTE PTR [rax],al
    3ea8:	14 00                	adc    al,0x0
    3eaa:	00 00                	add    BYTE PTR [rax],al
    3eac:	00 00                	add    BYTE PTR [rax],al
    3eae:	00 00                	add    BYTE PTR [rax],al
    3eb0:	07                   	(bad)  
    3eb1:	00 00                	add    BYTE PTR [rax],al
    3eb3:	00 00                	add    BYTE PTR [rax],al
    3eb5:	00 00                	add    BYTE PTR [rax],al
    3eb7:	00 17                	add    BYTE PTR [rdi],dl
    3eb9:	00 00                	add    BYTE PTR [rax],al
    3ebb:	00 00                	add    BYTE PTR [rax],al
    3ebd:	00 00                	add    BYTE PTR [rax],al
    3ebf:	00 48 06             	add    BYTE PTR [rax+0x6],cl
    3ec2:	00 00                	add    BYTE PTR [rax],al
    3ec4:	00 00                	add    BYTE PTR [rax],al
    3ec6:	00 00                	add    BYTE PTR [rax],al
    3ec8:	07                   	(bad)  
    3ec9:	00 00                	add    BYTE PTR [rax],al
    3ecb:	00 00                	add    BYTE PTR [rax],al
    3ecd:	00 00                	add    BYTE PTR [rax],al
    3ecf:	00 88 05 00 00 00    	add    BYTE PTR [rax+0x5],cl
    3ed5:	00 00                	add    BYTE PTR [rax],al
    3ed7:	00 08                	add    BYTE PTR [rax],cl
    3ed9:	00 00                	add    BYTE PTR [rax],al
    3edb:	00 00                	add    BYTE PTR [rax],al
    3edd:	00 00                	add    BYTE PTR [rax],al
    3edf:	00 c0                	add    al,al
    3ee1:	00 00                	add    BYTE PTR [rax],al
    3ee3:	00 00                	add    BYTE PTR [rax],al
    3ee5:	00 00                	add    BYTE PTR [rax],al
    3ee7:	00 09                	add    BYTE PTR [rcx],cl
    3ee9:	00 00                	add    BYTE PTR [rax],al
    3eeb:	00 00                	add    BYTE PTR [rax],al
    3eed:	00 00                	add    BYTE PTR [rax],al
    3eef:	00 18                	add    BYTE PTR [rax],bl
    3ef1:	00 00                	add    BYTE PTR [rax],al
    3ef3:	00 00                	add    BYTE PTR [rax],al
    3ef5:	00 00                	add    BYTE PTR [rax],al
    3ef7:	00 1e                	add    BYTE PTR [rsi],bl
    3ef9:	00 00                	add    BYTE PTR [rax],al
    3efb:	00 00                	add    BYTE PTR [rax],al
    3efd:	00 00                	add    BYTE PTR [rax],al
    3eff:	00 08                	add    BYTE PTR [rax],cl
    3f01:	00 00                	add    BYTE PTR [rax],al
    3f03:	00 00                	add    BYTE PTR [rax],al
    3f05:	00 00                	add    BYTE PTR [rax],al
    3f07:	00 fb                	add    bl,bh
    3f09:	ff                   	(bad)  
    3f0a:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f0d:	00 00                	add    BYTE PTR [rax],al
    3f0f:	00 01                	add    BYTE PTR [rcx],al
    3f11:	00 00                	add    BYTE PTR [rax],al
    3f13:	08 00                	or     BYTE PTR [rax],al
    3f15:	00 00                	add    BYTE PTR [rax],al
    3f17:	00 fe                	add    dh,bh
    3f19:	ff                   	(bad)  
    3f1a:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f1d:	00 00                	add    BYTE PTR [rax],al
    3f1f:	00 58 05             	add    BYTE PTR [rax+0x5],bl
    3f22:	00 00                	add    BYTE PTR [rax],al
    3f24:	00 00                	add    BYTE PTR [rax],al
    3f26:	00 00                	add    BYTE PTR [rax],al
    3f28:	ff                   	(bad)  
    3f29:	ff                   	(bad)  
    3f2a:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f2d:	00 00                	add    BYTE PTR [rax],al
    3f2f:	00 01                	add    BYTE PTR [rcx],al
    3f31:	00 00                	add    BYTE PTR [rax],al
    3f33:	00 00                	add    BYTE PTR [rax],al
    3f35:	00 00                	add    BYTE PTR [rax],al
    3f37:	00 f0                	add    al,dh
    3f39:	ff                   	(bad)  
    3f3a:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f3d:	00 00                	add    BYTE PTR [rax],al
    3f3f:	00 44 05 00          	add    BYTE PTR [rbp+rax*1+0x0],al
    3f43:	00 00                	add    BYTE PTR [rax],al
    3f45:	00 00                	add    BYTE PTR [rax],al
    3f47:	00 f9                	add    cl,bh
    3f49:	ff                   	(bad)  
    3f4a:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f4d:	00 00                	add    BYTE PTR [rax],al
    3f4f:	00 03                	add    BYTE PTR [rbx],al
	...

Disassembly of section .got:

0000000000003fa8 <.got>:
    3fa8:	b8 3d 00 00 00       	mov    eax,0x3d
	...
    3fbd:	00 00                	add    BYTE PTR [rax],al
    3fbf:	00 30                	add    BYTE PTR [rax],dh
    3fc1:	10 00                	adc    BYTE PTR [rax],al
    3fc3:	00 00                	add    BYTE PTR [rax],al
    3fc5:	00 00                	add    BYTE PTR [rax],al
    3fc7:	00 40 10             	add    BYTE PTR [rax+0x10],al
    3fca:	00 00                	add    BYTE PTR [rax],al
    3fcc:	00 00                	add    BYTE PTR [rax],al
    3fce:	00 00                	add    BYTE PTR [rax],al
    3fd0:	50                   	push   rax
    3fd1:	10 00                	adc    BYTE PTR [rax],al
	...

Disassembly of section .data:

0000000000004000 <.data>:
	...
    4008:	08 40 00             	or     BYTE PTR [rax+0x0],al
    400b:	00 00                	add    BYTE PTR [rax],al
    400d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .bss:

0000000000004010 <.bss>:
	...

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	47                   	rex.RXB
   1:	43                   	rex.XB
   2:	43 3a 20             	rex.XB cmp spl,BYTE PTR [r8]
   5:	28 55 62             	sub    BYTE PTR [rbp+0x62],dl
   8:	75 6e                	jne    78 <__cxa_finalize@plt-0xfe8>
   a:	74 75                	je     81 <__cxa_finalize@plt-0xfdf>
   c:	20 39                	and    BYTE PTR [rcx],bh
   e:	2e 34 2e             	cs xor al,0x2e
  11:	30 2d 31 75 62 75    	xor    BYTE PTR [rip+0x75627531],ch        # 75627548 <__stack_chk_fail@plt+0x756264b8>
  17:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  18:	74 75                	je     8f <__cxa_finalize@plt-0xfd1>
  1a:	31 7e 32             	xor    DWORD PTR [rsi+0x32],edi
  1d:	30 2e                	xor    BYTE PTR [rsi],ch
  1f:	30 34 2e             	xor    BYTE PTR [rsi+rbp*1],dh
  22:	31 29                	xor    DWORD PTR [rcx],ebp
  24:	20 39                	and    BYTE PTR [rcx],bh
  26:	2e 34 2e             	cs xor al,0x2e
  29:	30 00                	xor    BYTE PTR [rax],al
