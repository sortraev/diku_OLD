
new_absalon:     file format elf64-x86-64


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
 367:	00 9c 87 f3 82 cf 93 	add    BYTE PTR [rdi+rax*4-0x6c307d0d],bl
 36e:	2f                   	(bad)
 36f:	18 9d 30 a1 67 1a    	sbb    BYTE PTR [rbp+0x1a67a130],bl
 375:	29 99 c1 f8 9e 97    	sub    DWORD PTR [rcx-0x6861073f],ebx
 37b:	86                   	.byte 0x86

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
 3a0:	03 00                	add    eax,DWORD PTR [rax]
 3a2:	00 00                	add    BYTE PTR [rax],al
 3a4:	16                   	(bad)
 3a5:	00 00                	add    BYTE PTR [rax],al
 3a7:	00 01                	add    BYTE PTR [rcx],al
 3a9:	00 00                	add    BYTE PTR [rax],al
 3ab:	00 06                	add    BYTE PTR [rsi],al
 3ad:	00 00                	add    BYTE PTR [rax],al
 3af:	00 00                	add    BYTE PTR [rax],al
 3b1:	01 a1 00 80 01 10    	add    DWORD PTR [rcx+0x10018000],esp
 3b7:	02 16                	add    dl,BYTE PTR [rsi]
 3b9:	00 00                	add    BYTE PTR [rax],al
 3bb:	00 18                	add    BYTE PTR [rax],bl
 3bd:	00 00                	add    BYTE PTR [rax],al
 3bf:	00 00                	add    BYTE PTR [rax],al
 3c1:	00 00                	add    BYTE PTR [rax],al
 3c3:	00 28                	add    BYTE PTR [rax],ch
 3c5:	1d 8c 1c d1 65       	sbb    eax,0x65d11c8c
 3ca:	ce                   	(bad)
 3cb:	6d                   	ins    DWORD PTR es:[rdi],dx
 3cc:	66 55                	push   bp
 3ce:	61                   	(bad)
 3cf:	10 39                	adc    BYTE PTR [rcx],bh
 3d1:	f2                   	repnz
 3d2:	8b                   	.byte 0x8b
 3d3:	1c                   	.byte 0x1c

Disassembly of section .dynsym:

00000000000003d8 <.dynsym>:
	...
 3f0:	bd 00 00 00 12       	mov    ebp,0x12000000
	...
 405:	00 00                	add    BYTE PTR [rax],al
 407:	00 e2                	add    dl,ah
 409:	00 00                	add    BYTE PTR [rax],al
 40b:	00 20                	add    BYTE PTR [rax],ah
	...
 41d:	00 00                	add    BYTE PTR [rax],al
 41f:	00 77 00             	add    BYTE PTR [rdi+0x0],dh
 422:	00 00                	add    BYTE PTR [rax],al
 424:	12 00                	adc    al,BYTE PTR [rax]
	...
 436:	00 00                	add    BYTE PTR [rax],al
 438:	33 00                	xor    eax,DWORD PTR [rax]
 43a:	00 00                	add    BYTE PTR [rax],al
 43c:	12 00                	adc    al,BYTE PTR [rax]
	...
 44e:	00 00                	add    BYTE PTR [rax],al
 450:	38 00                	cmp    BYTE PTR [rax],al
 452:	00 00                	add    BYTE PTR [rax],al
 454:	12 00                	adc    al,BYTE PTR [rax]
	...
 466:	00 00                	add    BYTE PTR [rax],al
 468:	57                   	push   rdi
 469:	00 00                	add    BYTE PTR [rax],al
 46b:	00 12                	add    BYTE PTR [rdx],dl
	...
 47d:	00 00                	add    BYTE PTR [rax],al
 47f:	00 5e 00             	add    BYTE PTR [rsi+0x0],bl
 482:	00 00                	add    BYTE PTR [rax],al
 484:	12 00                	adc    al,BYTE PTR [rax]
	...
 496:	00 00                	add    BYTE PTR [rax],al
 498:	72 00                	jb     49a <_init-0xb66>
 49a:	00 00                	add    BYTE PTR [rax],al
 49c:	12 00                	adc    al,BYTE PTR [rax]
	...
 4ae:	00 00                	add    BYTE PTR [rax],al
 4b0:	ab                   	stos   DWORD PTR es:[rdi],eax
 4b1:	00 00                	add    BYTE PTR [rax],al
 4b3:	00 12                	add    BYTE PTR [rdx],dl
	...
 4c5:	00 00                	add    BYTE PTR [rax],al
 4c7:	00 64 00 00          	add    BYTE PTR [rax+rax*1+0x0],ah
 4cb:	00 12                	add    BYTE PTR [rdx],dl
	...
 4dd:	00 00                	add    BYTE PTR [rax],al
 4df:	00 6a 00             	add    BYTE PTR [rdx+0x0],ch
 4e2:	00 00                	add    BYTE PTR [rax],al
 4e4:	12 00                	adc    al,BYTE PTR [rax]
	...
 4f6:	00 00                	add    BYTE PTR [rax],al
 4f8:	fe 00                	inc    BYTE PTR [rax]
 4fa:	00 00                	add    BYTE PTR [rax],al
 4fc:	20 00                	and    BYTE PTR [rax],al
	...
 50e:	00 00                	add    BYTE PTR [rax],al
 510:	86 00                	xchg   BYTE PTR [rax],al
 512:	00 00                	add    BYTE PTR [rax],al
 514:	12 00                	adc    al,BYTE PTR [rax]
	...
 526:	00 00                	add    BYTE PTR [rax],al
 528:	0b 00                	or     eax,DWORD PTR [rax]
 52a:	00 00                	add    BYTE PTR [rax],al
 52c:	12 00                	adc    al,BYTE PTR [rax]
	...
 53e:	00 00                	add    BYTE PTR [rax],al
 540:	49 00 00             	rex.WB add BYTE PTR [r8],al
 543:	00 12                	add    BYTE PTR [rdx],dl
	...
 555:	00 00                	add    BYTE PTR [rax],al
 557:	00 a3 00 00 00 12    	add    BYTE PTR [rbx+0x12000000],ah
	...
 56d:	00 00                	add    BYTE PTR [rax],al
 56f:	00 17                	add    BYTE PTR [rdi],dl
 571:	00 00                	add    BYTE PTR [rax],al
 573:	00 12                	add    BYTE PTR [rdx],dl
	...
 585:	00 00                	add    BYTE PTR [rax],al
 587:	00 1d 00 00 00 12    	add    BYTE PTR [rip+0x12000000],bl        # 1200058d <_end+0x11ffc52d>
	...
 59d:	00 00                	add    BYTE PTR [rax],al
 59f:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
 5a2:	00 00                	add    BYTE PTR [rax],al
 5a4:	12 00                	adc    al,BYTE PTR [rax]
	...
 5b6:	00 00                	add    BYTE PTR [rax],al
 5b8:	12 00                	adc    al,BYTE PTR [rax]
 5ba:	00 00                	add    BYTE PTR [rax],al
 5bc:	12 00                	adc    al,BYTE PTR [rax]
	...
 5ce:	00 00                	add    BYTE PTR [rax],al
 5d0:	0d 01 00 00 20       	or     eax,0x20000001
	...
 5e5:	00 00                	add    BYTE PTR [rax],al
 5e7:	00 7f 00             	add    BYTE PTR [rdi+0x0],bh
 5ea:	00 00                	add    BYTE PTR [rax],al
 5ec:	11 00                	adc    DWORD PTR [rax],eax
 5ee:	1a 00                	sbb    al,BYTE PTR [rax]
 5f0:	20 40 00             	and    BYTE PTR [rax+0x0],al
 5f3:	00 00                	add    BYTE PTR [rax],al
 5f5:	00 00                	add    BYTE PTR [rax],al
 5f7:	00 08                	add    BYTE PTR [rax],cl
 5f9:	00 00                	add    BYTE PTR [rax],al
 5fb:	00 00                	add    BYTE PTR [rax],al
 5fd:	00 00                	add    BYTE PTR [rax],al
 5ff:	00 94 00 00 00 22 00 	add    BYTE PTR [rax+rax*1+0x220000],dl
	...
 616:	00 00                	add    BYTE PTR [rax],al
 618:	51                   	push   rcx
 619:	00 00                	add    BYTE PTR [rax],al
 61b:	00 11                	add    BYTE PTR [rcx],dl
 61d:	00 1a                	add    BYTE PTR [rdx],bl
 61f:	00 30                	add    BYTE PTR [rax],dh
 621:	40 00 00             	rex add BYTE PTR [rax],al
 624:	00 00                	add    BYTE PTR [rax],al
 626:	00 00                	add    BYTE PTR [rax],al
 628:	08 00                	or     BYTE PTR [rax],al
 62a:	00 00                	add    BYTE PTR [rax],al
 62c:	00 00                	add    BYTE PTR [rax],al
 62e:	00 00                	add    BYTE PTR [rax],al
 630:	8d 00                	lea    eax,[rax]
 632:	00 00                	add    BYTE PTR [rax],al
 634:	11 00                	adc    DWORD PTR [rax],eax
 636:	1a 00                	sbb    al,BYTE PTR [rax]
 638:	40                   	rex
 639:	40 00 00             	rex add BYTE PTR [rax],al
 63c:	00 00                	add    BYTE PTR [rax],al
 63e:	00 00                	add    BYTE PTR [rax],al
 640:	08 00                	or     BYTE PTR [rax],al
 642:	00 00                	add    BYTE PTR [rax],al
 644:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynstr:

0000000000000648 <.dynstr>:
 648:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
 64c:	63 2e                	movsxd ebp,DWORD PTR [rsi]
 64e:	73 6f                	jae    6bf <_init-0x941>
 650:	2e 36 00 66 66       	cs ss add BYTE PTR [rsi+0x66],ah
 655:	6c                   	ins    BYTE PTR es:[rdi],dx
 656:	75 73                	jne    6cb <_init-0x935>
 658:	68 00 65 78 69       	push   0x69786500
 65d:	74 00                	je     65f <_init-0x9a1>
 65f:	66 6f                	outs   dx,WORD PTR ds:[rsi]
 661:	70 65                	jo     6c8 <_init-0x938>
 663:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 664:	00 70 65             	add    BYTE PTR [rax+0x65],dh
 667:	72 72                	jb     6db <_init-0x925>
 669:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 66a:	72 00                	jb     66c <_init-0x994>
 66c:	5f                   	pop    rdi
 66d:	5f                   	pop    rdi
 66e:	69 73 6f 63 39 39 5f 	imul   esi,DWORD PTR [rbx+0x6f],0x5f393963
 675:	73 63                	jae    6da <_init-0x926>
 677:	61                   	(bad)
 678:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 679:	66 00 70 75          	data16 add BYTE PTR [rax+0x75],dh
 67d:	74 73                	je     6f2 <_init-0x90e>
 67f:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 682:	73 74                	jae    6f8 <_init-0x908>
 684:	61                   	(bad)
 685:	63 6b 5f             	movsxd ebp,DWORD PTR [rbx+0x5f]
 688:	63 68 6b             	movsxd ebp,DWORD PTR [rax+0x6b]
 68b:	5f                   	pop    rdi
 68c:	66 61                	data16 (bad)
 68e:	69 6c 00 72 65 61 6c 	imul   ebp,DWORD PTR [rax+rax*1+0x72],0x6c6c6165
 695:	6c 
 696:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 697:	63 00                	movsxd eax,DWORD PTR [rax]
 699:	73 74                	jae    70f <_init-0x8f1>
 69b:	64 69 6e 00 70 72 69 	imul   ebp,DWORD PTR fs:[rsi+0x0],0x6e697270
 6a2:	6e 
 6a3:	74 66                	je     70b <_init-0x8f5>
 6a5:	00 66 67             	add    BYTE PTR [rsi+0x67],ah
 6a8:	65 74 63             	gs je  70e <_init-0x8f2>
 6ab:	00 66 67             	add    BYTE PTR [rsi+0x67],ah
 6ae:	65 74 73             	gs je  724 <_init-0x8dc>
 6b1:	00 67 65             	add    BYTE PTR [rdi+0x65],ah
 6b4:	74 63                	je     719 <_init-0x8e7>
 6b6:	68 61 72 00 72       	push   0x72007261
 6bb:	65 61                	gs (bad)
 6bd:	64 00 74 6f 75       	add    BYTE PTR fs:[rdi+rbp*2+0x75],dh
 6c2:	70 70                	jo     734 <_init-0x8cc>
 6c4:	65 72 00             	gs jb  6c7 <_init-0x939>
 6c7:	73 74                	jae    73d <_init-0x8c3>
 6c9:	64 6f                	outs   dx,DWORD PTR fs:[rsi]
 6cb:	75 74                	jne    741 <_init-0x8bf>
 6cd:	00 6d 61             	add    BYTE PTR [rbp+0x61],ch
 6d0:	6c                   	ins    BYTE PTR es:[rdi],dx
 6d1:	6c                   	ins    BYTE PTR es:[rdi],dx
 6d2:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 6d3:	63 00                	movsxd eax,DWORD PTR [rax]
 6d5:	73 74                	jae    74b <_init-0x8b5>
 6d7:	64 65 72 72          	fs gs jb 74d <_init-0x8b3>
 6db:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 6de:	63 78 61             	movsxd edi,DWORD PTR [rax+0x61]
 6e1:	5f                   	pop    rdi
 6e2:	66 69 6e 61 6c 69    	imul   bp,WORD PTR [rsi+0x61],0x696c
 6e8:	7a 65                	jp     74f <_init-0x8b1>
 6ea:	00 73 65             	add    BYTE PTR [rbx+0x65],dh
 6ed:	74 76                	je     765 <_init-0x89b>
 6ef:	62 75 66 00 5f 5f 6c 	vmaxsh xmm11,xmm19,WORD PTR [rdi+0xd8]
 6f6:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
 6fd:	72 74                	jb     773 <_init-0x88d>
 6ff:	5f                   	pop    rdi
 700:	6d                   	ins    DWORD PTR es:[rdi],dx
 701:	61                   	(bad)
 702:	69 6e 00 66 72 65 65 	imul   ebp,DWORD PTR [rsi+0x0],0x65657266
 709:	00 47 4c             	add    BYTE PTR [rdi+0x4c],al
 70c:	49                   	rex.WB
 70d:	42                   	rex.X
 70e:	43 5f                	rex.XB pop r15
 710:	32 2e                	xor    ch,BYTE PTR [rsi]
 712:	37                   	(bad)
 713:	00 47 4c             	add    BYTE PTR [rdi+0x4c],al
 716:	49                   	rex.WB
 717:	42                   	rex.X
 718:	43 5f                	rex.XB pop r15
 71a:	32 2e                	xor    ch,BYTE PTR [rsi]
 71c:	34 00                	xor    al,0x0
 71e:	47                   	rex.RXB
 71f:	4c                   	rex.WR
 720:	49                   	rex.WB
 721:	42                   	rex.X
 722:	43 5f                	rex.XB pop r15
 724:	32 2e                	xor    ch,BYTE PTR [rsi]
 726:	32 2e                	xor    ch,BYTE PTR [rsi]
 728:	35 00 5f 49 54       	xor    eax,0x54495f00
 72d:	4d 5f                	rex.WRB pop r15
 72f:	64 65 72 65          	fs gs jb 798 <_init-0x868>
 733:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 73a:	4d 
 73b:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 73d:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 73e:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 73f:	65 54                	gs push rsp
 741:	61                   	(bad)
 742:	62                   	(bad)
 743:	6c                   	ins    BYTE PTR es:[rdi],dx
 744:	65 00 5f 5f          	add    BYTE PTR gs:[rdi+0x5f],bl
 748:	67 6d                	ins    DWORD PTR es:[edi],dx
 74a:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 74b:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 74c:	5f                   	pop    rdi
 74d:	73 74                	jae    7c3 <_init-0x83d>
 74f:	61                   	(bad)
 750:	72 74                	jb     7c6 <_init-0x83a>
 752:	5f                   	pop    rdi
 753:	5f                   	pop    rdi
 754:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 757:	54                   	push   rsp
 758:	4d 5f                	rex.WRB pop r15
 75a:	72 65                	jb     7c1 <_init-0x83f>
 75c:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 763:	4d 
 764:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 766:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 767:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 768:	65 54                	gs push rsp
 76a:	61                   	(bad)
 76b:	62                   	.byte 0x62
 76c:	6c                   	ins    BYTE PTR es:[rdi],dx
 76d:	65                   	gs
	...

Disassembly of section .gnu.version:

0000000000000770 <.gnu.version>:
 770:	00 00                	add    BYTE PTR [rax],al
 772:	02 00                	add    al,BYTE PTR [rax]
 774:	00 00                	add    BYTE PTR [rax],al
 776:	02 00                	add    al,BYTE PTR [rax]
 778:	02 00                	add    al,BYTE PTR [rax]
 77a:	03 00                	add    eax,DWORD PTR [rax]
 77c:	02 00                	add    al,BYTE PTR [rax]
 77e:	02 00                	add    al,BYTE PTR [rax]
 780:	02 00                	add    al,BYTE PTR [rax]
 782:	02 00                	add    al,BYTE PTR [rax]
 784:	02 00                	add    al,BYTE PTR [rax]
 786:	02 00                	add    al,BYTE PTR [rax]
 788:	00 00                	add    BYTE PTR [rax],al
 78a:	02 00                	add    al,BYTE PTR [rax]
 78c:	02 00                	add    al,BYTE PTR [rax]
 78e:	02 00                	add    al,BYTE PTR [rax]
 790:	02 00                	add    al,BYTE PTR [rax]
 792:	02 00                	add    al,BYTE PTR [rax]
 794:	02 00                	add    al,BYTE PTR [rax]
 796:	04 00                	add    al,0x0
 798:	02 00                	add    al,BYTE PTR [rax]
 79a:	00 00                	add    BYTE PTR [rax],al
 79c:	02 00                	add    al,BYTE PTR [rax]
 79e:	02 00                	add    al,BYTE PTR [rax]
 7a0:	02 00                	add    al,BYTE PTR [rax]
 7a2:	02 00                	add    al,BYTE PTR [rax]

Disassembly of section .gnu.version_r:

00000000000007a8 <.gnu.version_r>:
 7a8:	01 00                	add    DWORD PTR [rax],eax
 7aa:	03 00                	add    eax,DWORD PTR [rax]
 7ac:	01 00                	add    DWORD PTR [rax],eax
 7ae:	00 00                	add    BYTE PTR [rax],al
 7b0:	10 00                	adc    BYTE PTR [rax],al
 7b2:	00 00                	add    BYTE PTR [rax],al
 7b4:	00 00                	add    BYTE PTR [rax],al
 7b6:	00 00                	add    BYTE PTR [rax],al
 7b8:	17                   	(bad)
 7b9:	69 69 0d 00 00 04 00 	imul   ebp,DWORD PTR [rcx+0xd],0x40000
 7c0:	c2 00 00             	ret    0x0
 7c3:	00 10                	add    BYTE PTR [rax],dl
 7c5:	00 00                	add    BYTE PTR [rax],al
 7c7:	00 14 69             	add    BYTE PTR [rcx+rbp*2],dl
 7ca:	69 0d 00 00 03 00 cc 	imul   ecx,DWORD PTR [rip+0x30000],0xcc        # 307d4 <_end+0x2c774>
 7d1:	00 00 00 
 7d4:	10 00                	adc    BYTE PTR [rax],al
 7d6:	00 00                	add    BYTE PTR [rax],al
 7d8:	75 1a                	jne    7f4 <_init-0x80c>
 7da:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
 7e0:	d6                   	(bad)
 7e1:	00 00                	add    BYTE PTR [rax],al
 7e3:	00 00                	add    BYTE PTR [rax],al
 7e5:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

00000000000007e8 <.rela.dyn>:
 7e8:	38 3d 00 00 00 00    	cmp    BYTE PTR [rip+0x0],bh        # 7ee <_init-0x812>
 7ee:	00 00                	add    BYTE PTR [rax],al
 7f0:	08 00                	or     BYTE PTR [rax],al
 7f2:	00 00                	add    BYTE PTR [rax],al
 7f4:	00 00                	add    BYTE PTR [rax],al
 7f6:	00 00                	add    BYTE PTR [rax],al
 7f8:	40 13 00             	rex adc eax,DWORD PTR [rax]
 7fb:	00 00                	add    BYTE PTR [rax],al
 7fd:	00 00                	add    BYTE PTR [rax],al
 7ff:	00 40 3d             	add    BYTE PTR [rax+0x3d],al
 802:	00 00                	add    BYTE PTR [rax],al
 804:	00 00                	add    BYTE PTR [rax],al
 806:	00 00                	add    BYTE PTR [rax],al
 808:	08 00                	or     BYTE PTR [rax],al
 80a:	00 00                	add    BYTE PTR [rax],al
 80c:	00 00                	add    BYTE PTR [rax],al
 80e:	00 00                	add    BYTE PTR [rax],al
 810:	00 13                	add    BYTE PTR [rbx],dl
 812:	00 00                	add    BYTE PTR [rax],al
 814:	00 00                	add    BYTE PTR [rax],al
 816:	00 00                	add    BYTE PTR [rax],al
 818:	08 40 00             	or     BYTE PTR [rax+0x0],al
 81b:	00 00                	add    BYTE PTR [rax],al
 81d:	00 00                	add    BYTE PTR [rax],al
 81f:	00 08                	add    BYTE PTR [rax],cl
 821:	00 00                	add    BYTE PTR [rax],al
 823:	00 00                	add    BYTE PTR [rax],al
 825:	00 00                	add    BYTE PTR [rax],al
 827:	00 08                	add    BYTE PTR [rax],cl
 829:	40 00 00             	rex add BYTE PTR [rax],al
 82c:	00 00                	add    BYTE PTR [rax],al
 82e:	00 00                	add    BYTE PTR [rax],al
 830:	d8 3f                	fdivr  DWORD PTR [rdi]
 832:	00 00                	add    BYTE PTR [rax],al
 834:	00 00                	add    BYTE PTR [rax],al
 836:	00 00                	add    BYTE PTR [rax],al
 838:	06                   	(bad)
 839:	00 00                	add    BYTE PTR [rax],al
 83b:	00 02                	add    BYTE PTR [rdx],al
	...
 845:	00 00                	add    BYTE PTR [rax],al
 847:	00 e0                	add    al,ah
 849:	3f                   	(bad)
 84a:	00 00                	add    BYTE PTR [rax],al
 84c:	00 00                	add    BYTE PTR [rax],al
 84e:	00 00                	add    BYTE PTR [rax],al
 850:	06                   	(bad)
 851:	00 00                	add    BYTE PTR [rax],al
 853:	00 09                	add    BYTE PTR [rcx],cl
	...
 85d:	00 00                	add    BYTE PTR [rax],al
 85f:	00 e8                	add    al,ch
 861:	3f                   	(bad)
 862:	00 00                	add    BYTE PTR [rax],al
 864:	00 00                	add    BYTE PTR [rax],al
 866:	00 00                	add    BYTE PTR [rax],al
 868:	06                   	(bad)
 869:	00 00                	add    BYTE PTR [rax],al
 86b:	00 0c 00             	add    BYTE PTR [rax+rax*1],cl
	...
 876:	00 00                	add    BYTE PTR [rax],al
 878:	f0 3f                	lock (bad)
 87a:	00 00                	add    BYTE PTR [rax],al
 87c:	00 00                	add    BYTE PTR [rax],al
 87e:	00 00                	add    BYTE PTR [rax],al
 880:	06                   	(bad)
 881:	00 00                	add    BYTE PTR [rax],al
 883:	00 15 00 00 00 00    	add    BYTE PTR [rip+0x0],dl        # 889 <_init-0x777>
 889:	00 00                	add    BYTE PTR [rax],al
 88b:	00 00                	add    BYTE PTR [rax],al
 88d:	00 00                	add    BYTE PTR [rax],al
 88f:	00 f8                	add    al,bh
 891:	3f                   	(bad)
 892:	00 00                	add    BYTE PTR [rax],al
 894:	00 00                	add    BYTE PTR [rax],al
 896:	00 00                	add    BYTE PTR [rax],al
 898:	06                   	(bad)
 899:	00 00                	add    BYTE PTR [rax],al
 89b:	00 17                	add    BYTE PTR [rdi],dl
	...
 8a5:	00 00                	add    BYTE PTR [rax],al
 8a7:	00 20                	add    BYTE PTR [rax],ah
 8a9:	40 00 00             	rex add BYTE PTR [rax],al
 8ac:	00 00                	add    BYTE PTR [rax],al
 8ae:	00 00                	add    BYTE PTR [rax],al
 8b0:	05 00 00 00 16       	add    eax,0x16000000
	...
 8bd:	00 00                	add    BYTE PTR [rax],al
 8bf:	00 30                	add    BYTE PTR [rax],dh
 8c1:	40 00 00             	rex add BYTE PTR [rax],al
 8c4:	00 00                	add    BYTE PTR [rax],al
 8c6:	00 00                	add    BYTE PTR [rax],al
 8c8:	05 00 00 00 18       	add    eax,0x18000000
	...
 8d5:	00 00                	add    BYTE PTR [rax],al
 8d7:	00 40 40             	add    BYTE PTR [rax+0x40],al
 8da:	00 00                	add    BYTE PTR [rax],al
 8dc:	00 00                	add    BYTE PTR [rax],al
 8de:	00 00                	add    BYTE PTR [rax],al
 8e0:	05 00 00 00 19       	add    eax,0x19000000
	...

Disassembly of section .rela.plt:

00000000000008f0 <.rela.plt>:
 8f0:	50                   	push   rax
 8f1:	3f                   	(bad)
 8f2:	00 00                	add    BYTE PTR [rax],al
 8f4:	00 00                	add    BYTE PTR [rax],al
 8f6:	00 00                	add    BYTE PTR [rax],al
 8f8:	07                   	(bad)
 8f9:	00 00                	add    BYTE PTR [rax],al
 8fb:	00 01                	add    BYTE PTR [rcx],al
	...
 905:	00 00                	add    BYTE PTR [rax],al
 907:	00 58 3f             	add    BYTE PTR [rax+0x3f],bl
 90a:	00 00                	add    BYTE PTR [rax],al
 90c:	00 00                	add    BYTE PTR [rax],al
 90e:	00 00                	add    BYTE PTR [rax],al
 910:	07                   	(bad)
 911:	00 00                	add    BYTE PTR [rax],al
 913:	00 03                	add    BYTE PTR [rbx],al
	...
 91d:	00 00                	add    BYTE PTR [rax],al
 91f:	00 60 3f             	add    BYTE PTR [rax+0x3f],ah
 922:	00 00                	add    BYTE PTR [rax],al
 924:	00 00                	add    BYTE PTR [rax],al
 926:	00 00                	add    BYTE PTR [rax],al
 928:	07                   	(bad)
 929:	00 00                	add    BYTE PTR [rax],al
 92b:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...
 936:	00 00                	add    BYTE PTR [rax],al
 938:	68 3f 00 00 00       	push   0x3f
 93d:	00 00                	add    BYTE PTR [rax],al
 93f:	00 07                	add    BYTE PTR [rdi],al
 941:	00 00                	add    BYTE PTR [rax],al
 943:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 949 <_init-0x6b7>
 949:	00 00                	add    BYTE PTR [rax],al
 94b:	00 00                	add    BYTE PTR [rax],al
 94d:	00 00                	add    BYTE PTR [rax],al
 94f:	00 70 3f             	add    BYTE PTR [rax+0x3f],dh
 952:	00 00                	add    BYTE PTR [rax],al
 954:	00 00                	add    BYTE PTR [rax],al
 956:	00 00                	add    BYTE PTR [rax],al
 958:	07                   	(bad)
 959:	00 00                	add    BYTE PTR [rax],al
 95b:	00 06                	add    BYTE PTR [rsi],al
	...
 965:	00 00                	add    BYTE PTR [rax],al
 967:	00 78 3f             	add    BYTE PTR [rax+0x3f],bh
 96a:	00 00                	add    BYTE PTR [rax],al
 96c:	00 00                	add    BYTE PTR [rax],al
 96e:	00 00                	add    BYTE PTR [rax],al
 970:	07                   	(bad)
 971:	00 00                	add    BYTE PTR [rax],al
 973:	00 07                	add    BYTE PTR [rdi],al
	...
 97d:	00 00                	add    BYTE PTR [rax],al
 97f:	00 80 3f 00 00 00    	add    BYTE PTR [rax+0x3f],al
 985:	00 00                	add    BYTE PTR [rax],al
 987:	00 07                	add    BYTE PTR [rdi],al
 989:	00 00                	add    BYTE PTR [rax],al
 98b:	00 08                	add    BYTE PTR [rax],cl
	...
 995:	00 00                	add    BYTE PTR [rax],al
 997:	00 88 3f 00 00 00    	add    BYTE PTR [rax+0x3f],cl
 99d:	00 00                	add    BYTE PTR [rax],al
 99f:	00 07                	add    BYTE PTR [rdi],al
 9a1:	00 00                	add    BYTE PTR [rax],al
 9a3:	00 0a                	add    BYTE PTR [rdx],cl
	...
 9ad:	00 00                	add    BYTE PTR [rax],al
 9af:	00 90 3f 00 00 00    	add    BYTE PTR [rax+0x3f],dl
 9b5:	00 00                	add    BYTE PTR [rax],al
 9b7:	00 07                	add    BYTE PTR [rdi],al
 9b9:	00 00                	add    BYTE PTR [rax],al
 9bb:	00 0b                	add    BYTE PTR [rbx],cl
	...
 9c5:	00 00                	add    BYTE PTR [rax],al
 9c7:	00 98 3f 00 00 00    	add    BYTE PTR [rax+0x3f],bl
 9cd:	00 00                	add    BYTE PTR [rax],al
 9cf:	00 07                	add    BYTE PTR [rdi],al
 9d1:	00 00                	add    BYTE PTR [rax],al
 9d3:	00 0d 00 00 00 00    	add    BYTE PTR [rip+0x0],cl        # 9d9 <_init-0x627>
 9d9:	00 00                	add    BYTE PTR [rax],al
 9db:	00 00                	add    BYTE PTR [rax],al
 9dd:	00 00                	add    BYTE PTR [rax],al
 9df:	00 a0 3f 00 00 00    	add    BYTE PTR [rax+0x3f],ah
 9e5:	00 00                	add    BYTE PTR [rax],al
 9e7:	00 07                	add    BYTE PTR [rdi],al
 9e9:	00 00                	add    BYTE PTR [rax],al
 9eb:	00 0e                	add    BYTE PTR [rsi],cl
	...
 9f5:	00 00                	add    BYTE PTR [rax],al
 9f7:	00 a8 3f 00 00 00    	add    BYTE PTR [rax+0x3f],ch
 9fd:	00 00                	add    BYTE PTR [rax],al
 9ff:	00 07                	add    BYTE PTR [rdi],al
 a01:	00 00                	add    BYTE PTR [rax],al
 a03:	00 0f                	add    BYTE PTR [rdi],cl
	...
 a0d:	00 00                	add    BYTE PTR [rax],al
 a0f:	00 b0 3f 00 00 00    	add    BYTE PTR [rax+0x3f],dh
 a15:	00 00                	add    BYTE PTR [rax],al
 a17:	00 07                	add    BYTE PTR [rdi],al
 a19:	00 00                	add    BYTE PTR [rax],al
 a1b:	00 10                	add    BYTE PTR [rax],dl
	...
 a25:	00 00                	add    BYTE PTR [rax],al
 a27:	00 b8 3f 00 00 00    	add    BYTE PTR [rax+0x3f],bh
 a2d:	00 00                	add    BYTE PTR [rax],al
 a2f:	00 07                	add    BYTE PTR [rdi],al
 a31:	00 00                	add    BYTE PTR [rax],al
 a33:	00 11                	add    BYTE PTR [rcx],dl
	...
 a3d:	00 00                	add    BYTE PTR [rax],al
 a3f:	00 c0                	add    al,al
 a41:	3f                   	(bad)
 a42:	00 00                	add    BYTE PTR [rax],al
 a44:	00 00                	add    BYTE PTR [rax],al
 a46:	00 00                	add    BYTE PTR [rax],al
 a48:	07                   	(bad)
 a49:	00 00                	add    BYTE PTR [rax],al
 a4b:	00 12                	add    BYTE PTR [rdx],dl
	...
 a55:	00 00                	add    BYTE PTR [rax],al
 a57:	00 c8                	add    al,cl
 a59:	3f                   	(bad)
 a5a:	00 00                	add    BYTE PTR [rax],al
 a5c:	00 00                	add    BYTE PTR [rax],al
 a5e:	00 00                	add    BYTE PTR [rax],al
 a60:	07                   	(bad)
 a61:	00 00                	add    BYTE PTR [rax],al
 a63:	00 13                	add    BYTE PTR [rbx],dl
	...
 a6d:	00 00                	add    BYTE PTR [rax],al
 a6f:	00 d0                	add    al,dl
 a71:	3f                   	(bad)
 a72:	00 00                	add    BYTE PTR [rax],al
 a74:	00 00                	add    BYTE PTR [rax],al
 a76:	00 00                	add    BYTE PTR [rax],al
 a78:	07                   	(bad)
 a79:	00 00                	add    BYTE PTR [rax],al
 a7b:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
	...

Disassembly of section .init:

0000000000001000 <_init>:
    1000:	f3 0f 1e fa          	endbr64
    1004:	48 83 ec 08          	sub    rsp,0x8
    1008:	48 8b 05 d9 2f 00 00 	mov    rax,QWORD PTR [rip+0x2fd9]        # 3fe8 <__gmon_start__>
    100f:	48 85 c0             	test   rax,rax
    1012:	74 02                	je     1016 <_init+0x16>
    1014:	ff d0                	call   rax
    1016:	48 83 c4 08          	add    rsp,0x8
    101a:	c3                   	ret

Disassembly of section .plt:

0000000000001020 <.plt>:
    1020:	ff 35 1a 2f 00 00    	push   QWORD PTR [rip+0x2f1a]        # 3f40 <_GLOBAL_OFFSET_TABLE_+0x8>
    1026:	f2 ff 25 1b 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f1b]        # 3f48 <_GLOBAL_OFFSET_TABLE_+0x10>
    102d:	0f 1f 00             	nop    DWORD PTR [rax]
    1030:	f3 0f 1e fa          	endbr64
    1034:	68 00 00 00 00       	push   0x0
    1039:	f2 e9 e1 ff ff ff    	bnd jmp 1020 <.plt>
    103f:	90                   	nop
    1040:	f3 0f 1e fa          	endbr64
    1044:	68 01 00 00 00       	push   0x1
    1049:	f2 e9 d1 ff ff ff    	bnd jmp 1020 <.plt>
    104f:	90                   	nop
    1050:	f3 0f 1e fa          	endbr64
    1054:	68 02 00 00 00       	push   0x2
    1059:	f2 e9 c1 ff ff ff    	bnd jmp 1020 <.plt>
    105f:	90                   	nop
    1060:	f3 0f 1e fa          	endbr64
    1064:	68 03 00 00 00       	push   0x3
    1069:	f2 e9 b1 ff ff ff    	bnd jmp 1020 <.plt>
    106f:	90                   	nop
    1070:	f3 0f 1e fa          	endbr64
    1074:	68 04 00 00 00       	push   0x4
    1079:	f2 e9 a1 ff ff ff    	bnd jmp 1020 <.plt>
    107f:	90                   	nop
    1080:	f3 0f 1e fa          	endbr64
    1084:	68 05 00 00 00       	push   0x5
    1089:	f2 e9 91 ff ff ff    	bnd jmp 1020 <.plt>
    108f:	90                   	nop
    1090:	f3 0f 1e fa          	endbr64
    1094:	68 06 00 00 00       	push   0x6
    1099:	f2 e9 81 ff ff ff    	bnd jmp 1020 <.plt>
    109f:	90                   	nop
    10a0:	f3 0f 1e fa          	endbr64
    10a4:	68 07 00 00 00       	push   0x7
    10a9:	f2 e9 71 ff ff ff    	bnd jmp 1020 <.plt>
    10af:	90                   	nop
    10b0:	f3 0f 1e fa          	endbr64
    10b4:	68 08 00 00 00       	push   0x8
    10b9:	f2 e9 61 ff ff ff    	bnd jmp 1020 <.plt>
    10bf:	90                   	nop
    10c0:	f3 0f 1e fa          	endbr64
    10c4:	68 09 00 00 00       	push   0x9
    10c9:	f2 e9 51 ff ff ff    	bnd jmp 1020 <.plt>
    10cf:	90                   	nop
    10d0:	f3 0f 1e fa          	endbr64
    10d4:	68 0a 00 00 00       	push   0xa
    10d9:	f2 e9 41 ff ff ff    	bnd jmp 1020 <.plt>
    10df:	90                   	nop
    10e0:	f3 0f 1e fa          	endbr64
    10e4:	68 0b 00 00 00       	push   0xb
    10e9:	f2 e9 31 ff ff ff    	bnd jmp 1020 <.plt>
    10ef:	90                   	nop
    10f0:	f3 0f 1e fa          	endbr64
    10f4:	68 0c 00 00 00       	push   0xc
    10f9:	f2 e9 21 ff ff ff    	bnd jmp 1020 <.plt>
    10ff:	90                   	nop
    1100:	f3 0f 1e fa          	endbr64
    1104:	68 0d 00 00 00       	push   0xd
    1109:	f2 e9 11 ff ff ff    	bnd jmp 1020 <.plt>
    110f:	90                   	nop
    1110:	f3 0f 1e fa          	endbr64
    1114:	68 0e 00 00 00       	push   0xe
    1119:	f2 e9 01 ff ff ff    	bnd jmp 1020 <.plt>
    111f:	90                   	nop
    1120:	f3 0f 1e fa          	endbr64
    1124:	68 0f 00 00 00       	push   0xf
    1129:	f2 e9 f1 fe ff ff    	bnd jmp 1020 <.plt>
    112f:	90                   	nop
    1130:	f3 0f 1e fa          	endbr64
    1134:	68 10 00 00 00       	push   0x10
    1139:	f2 e9 e1 fe ff ff    	bnd jmp 1020 <.plt>
    113f:	90                   	nop

Disassembly of section .plt.got:

0000000000001140 <__cxa_finalize@plt>:
    1140:	f3 0f 1e fa          	endbr64
    1144:	f2 ff 25 ad 2e 00 00 	bnd jmp QWORD PTR [rip+0x2ead]        # 3ff8 <__cxa_finalize@GLIBC_2.2.5>
    114b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .plt.sec:

0000000000001150 <free@plt>:
    1150:	f3 0f 1e fa          	endbr64
    1154:	f2 ff 25 f5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2df5]        # 3f50 <free@GLIBC_2.2.5>
    115b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001160 <toupper@plt>:
    1160:	f3 0f 1e fa          	endbr64
    1164:	f2 ff 25 ed 2d 00 00 	bnd jmp QWORD PTR [rip+0x2ded]        # 3f58 <toupper@GLIBC_2.2.5>
    116b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001170 <puts@plt>:
    1170:	f3 0f 1e fa          	endbr64
    1174:	f2 ff 25 e5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2de5]        # 3f60 <puts@GLIBC_2.2.5>
    117b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001180 <__stack_chk_fail@plt>:
    1180:	f3 0f 1e fa          	endbr64
    1184:	f2 ff 25 dd 2d 00 00 	bnd jmp QWORD PTR [rip+0x2ddd]        # 3f68 <__stack_chk_fail@GLIBC_2.4>
    118b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001190 <printf@plt>:
    1190:	f3 0f 1e fa          	endbr64
    1194:	f2 ff 25 d5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2dd5]        # 3f70 <printf@GLIBC_2.2.5>
    119b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011a0 <fgetc@plt>:
    11a0:	f3 0f 1e fa          	endbr64
    11a4:	f2 ff 25 cd 2d 00 00 	bnd jmp QWORD PTR [rip+0x2dcd]        # 3f78 <fgetc@GLIBC_2.2.5>
    11ab:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011b0 <read@plt>:
    11b0:	f3 0f 1e fa          	endbr64
    11b4:	f2 ff 25 c5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2dc5]        # 3f80 <read@GLIBC_2.2.5>
    11bb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011c0 <fgets@plt>:
    11c0:	f3 0f 1e fa          	endbr64
    11c4:	f2 ff 25 bd 2d 00 00 	bnd jmp QWORD PTR [rip+0x2dbd]        # 3f88 <fgets@GLIBC_2.2.5>
    11cb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011d0 <getchar@plt>:
    11d0:	f3 0f 1e fa          	endbr64
    11d4:	f2 ff 25 b5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2db5]        # 3f90 <getchar@GLIBC_2.2.5>
    11db:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011e0 <malloc@plt>:
    11e0:	f3 0f 1e fa          	endbr64
    11e4:	f2 ff 25 ad 2d 00 00 	bnd jmp QWORD PTR [rip+0x2dad]        # 3f98 <malloc@GLIBC_2.2.5>
    11eb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011f0 <fflush@plt>:
    11f0:	f3 0f 1e fa          	endbr64
    11f4:	f2 ff 25 a5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2da5]        # 3fa0 <fflush@GLIBC_2.2.5>
    11fb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001200 <realloc@plt>:
    1200:	f3 0f 1e fa          	endbr64
    1204:	f2 ff 25 9d 2d 00 00 	bnd jmp QWORD PTR [rip+0x2d9d]        # 3fa8 <realloc@GLIBC_2.2.5>
    120b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001210 <setvbuf@plt>:
    1210:	f3 0f 1e fa          	endbr64
    1214:	f2 ff 25 95 2d 00 00 	bnd jmp QWORD PTR [rip+0x2d95]        # 3fb0 <setvbuf@GLIBC_2.2.5>
    121b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001220 <fopen@plt>:
    1220:	f3 0f 1e fa          	endbr64
    1224:	f2 ff 25 8d 2d 00 00 	bnd jmp QWORD PTR [rip+0x2d8d]        # 3fb8 <fopen@GLIBC_2.2.5>
    122b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001230 <perror@plt>:
    1230:	f3 0f 1e fa          	endbr64
    1234:	f2 ff 25 85 2d 00 00 	bnd jmp QWORD PTR [rip+0x2d85]        # 3fc0 <perror@GLIBC_2.2.5>
    123b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001240 <__isoc99_scanf@plt>:
    1240:	f3 0f 1e fa          	endbr64
    1244:	f2 ff 25 7d 2d 00 00 	bnd jmp QWORD PTR [rip+0x2d7d]        # 3fc8 <__isoc99_scanf@GLIBC_2.7>
    124b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001250 <exit@plt>:
    1250:	f3 0f 1e fa          	endbr64
    1254:	f2 ff 25 75 2d 00 00 	bnd jmp QWORD PTR [rip+0x2d75]        # 3fd0 <exit@GLIBC_2.2.5>
    125b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .text:

0000000000001260 <_start>:
    1260:	f3 0f 1e fa          	endbr64
    1264:	31 ed                	xor    ebp,ebp
    1266:	49 89 d1             	mov    r9,rdx
    1269:	5e                   	pop    rsi
    126a:	48 89 e2             	mov    rdx,rsp
    126d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
    1271:	50                   	push   rax
    1272:	54                   	push   rsp
    1273:	4c 8d 05 f6 06 00 00 	lea    r8,[rip+0x6f6]        # 1970 <__libc_csu_fini>
    127a:	48 8d 0d 7f 06 00 00 	lea    rcx,[rip+0x67f]        # 1900 <__libc_csu_init>
    1281:	48 8d 3d 2b 06 00 00 	lea    rdi,[rip+0x62b]        # 18b3 <main>
    1288:	ff 15 52 2d 00 00    	call   QWORD PTR [rip+0x2d52]        # 3fe0 <__libc_start_main@GLIBC_2.2.5>
    128e:	f4                   	hlt
    128f:	90                   	nop

0000000000001290 <deregister_tm_clones>:
    1290:	48 8d 3d 79 2d 00 00 	lea    rdi,[rip+0x2d79]        # 4010 <__TMC_END__>
    1297:	48 8d 05 72 2d 00 00 	lea    rax,[rip+0x2d72]        # 4010 <__TMC_END__>
    129e:	48 39 f8             	cmp    rax,rdi
    12a1:	74 15                	je     12b8 <deregister_tm_clones+0x28>
    12a3:	48 8b 05 2e 2d 00 00 	mov    rax,QWORD PTR [rip+0x2d2e]        # 3fd8 <_ITM_deregisterTMCloneTable>
    12aa:	48 85 c0             	test   rax,rax
    12ad:	74 09                	je     12b8 <deregister_tm_clones+0x28>
    12af:	ff e0                	jmp    rax
    12b1:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    12b8:	c3                   	ret
    12b9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

00000000000012c0 <register_tm_clones>:
    12c0:	48 8d 3d 49 2d 00 00 	lea    rdi,[rip+0x2d49]        # 4010 <__TMC_END__>
    12c7:	48 8d 35 42 2d 00 00 	lea    rsi,[rip+0x2d42]        # 4010 <__TMC_END__>
    12ce:	48 29 fe             	sub    rsi,rdi
    12d1:	48 89 f0             	mov    rax,rsi
    12d4:	48 c1 ee 3f          	shr    rsi,0x3f
    12d8:	48 c1 f8 03          	sar    rax,0x3
    12dc:	48 01 c6             	add    rsi,rax
    12df:	48 d1 fe             	sar    rsi,1
    12e2:	74 14                	je     12f8 <register_tm_clones+0x38>
    12e4:	48 8b 05 05 2d 00 00 	mov    rax,QWORD PTR [rip+0x2d05]        # 3ff0 <_ITM_registerTMCloneTable>
    12eb:	48 85 c0             	test   rax,rax
    12ee:	74 08                	je     12f8 <register_tm_clones+0x38>
    12f0:	ff e0                	jmp    rax
    12f2:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]
    12f8:	c3                   	ret
    12f9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

0000000000001300 <__do_global_dtors_aux>:
    1300:	f3 0f 1e fa          	endbr64
    1304:	80 3d 3d 2d 00 00 00 	cmp    BYTE PTR [rip+0x2d3d],0x0        # 4048 <completed.8061>
    130b:	75 2b                	jne    1338 <__do_global_dtors_aux+0x38>
    130d:	55                   	push   rbp
    130e:	48 83 3d e2 2c 00 00 	cmp    QWORD PTR [rip+0x2ce2],0x0        # 3ff8 <__cxa_finalize@GLIBC_2.2.5>
    1315:	00 
    1316:	48 89 e5             	mov    rbp,rsp
    1319:	74 0c                	je     1327 <__do_global_dtors_aux+0x27>
    131b:	48 8b 3d e6 2c 00 00 	mov    rdi,QWORD PTR [rip+0x2ce6]        # 4008 <__dso_handle>
    1322:	e8 19 fe ff ff       	call   1140 <__cxa_finalize@plt>
    1327:	e8 64 ff ff ff       	call   1290 <deregister_tm_clones>
    132c:	c6 05 15 2d 00 00 01 	mov    BYTE PTR [rip+0x2d15],0x1        # 4048 <completed.8061>
    1333:	5d                   	pop    rbp
    1334:	c3                   	ret
    1335:	0f 1f 00             	nop    DWORD PTR [rax]
    1338:	c3                   	ret
    1339:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

0000000000001340 <frame_dummy>:
    1340:	f3 0f 1e fa          	endbr64
    1344:	e9 77 ff ff ff       	jmp    12c0 <register_tm_clones>

0000000000001349 <winner_function>:
    1349:	f3 0f 1e fa          	endbr64
    134d:	55                   	push   rbp
    134e:	48 89 e5             	mov    rbp,rsp
    1351:	48 83 ec 60          	sub    rsp,0x60
    1355:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    135c:	00 00 
    135e:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    1362:	31 c0                	xor    eax,eax
    1364:	48 8d 35 9d 0c 00 00 	lea    rsi,[rip+0xc9d]        # 2008 <_IO_stdin_used+0x8>
    136b:	48 8d 3d 98 0c 00 00 	lea    rdi,[rip+0xc98]        # 200a <_IO_stdin_used+0xa>
    1372:	e8 a9 fe ff ff       	call   1220 <fopen@plt>
    1377:	48 89 45 a8          	mov    QWORD PTR [rbp-0x58],rax
    137b:	48 83 7d a8 00       	cmp    QWORD PTR [rbp-0x58],0x0
    1380:	75 1b                	jne    139d <winner_function+0x54>
    1382:	48 8d 3d 8f 0c 00 00 	lea    rdi,[rip+0xc8f]        # 2018 <_IO_stdin_used+0x18>
    1389:	b8 00 00 00 00       	mov    eax,0x0
    138e:	e8 fd fd ff ff       	call   1190 <printf@plt>
    1393:	bf 00 00 00 00       	mov    edi,0x0
    1398:	e8 b3 fe ff ff       	call   1250 <exit@plt>
    139d:	48 8b 55 a8          	mov    rdx,QWORD PTR [rbp-0x58]
    13a1:	48 8d 45 b0          	lea    rax,[rbp-0x50]
    13a5:	be 40 00 00 00       	mov    esi,0x40
    13aa:	48 89 c7             	mov    rdi,rax
    13ad:	e8 0e fe ff ff       	call   11c0 <fgets@plt>
    13b2:	48 85 c0             	test   rax,rax
    13b5:	75 16                	jne    13cd <winner_function+0x84>
    13b7:	48 8d 3d a4 0c 00 00 	lea    rdi,[rip+0xca4]        # 2062 <_IO_stdin_used+0x62>
    13be:	e8 6d fe ff ff       	call   1230 <perror@plt>
    13c3:	bf 01 00 00 00       	mov    edi,0x1
    13c8:	e8 83 fe ff ff       	call   1250 <exit@plt>
    13cd:	48 8d 45 b0          	lea    rax,[rbp-0x50]
    13d1:	48 89 c7             	mov    rdi,rax
    13d4:	e8 97 fd ff ff       	call   1170 <puts@plt>
    13d9:	48 8b 05 40 2c 00 00 	mov    rax,QWORD PTR [rip+0x2c40]        # 4020 <stdout@GLIBC_2.2.5>
    13e0:	48 89 c7             	mov    rdi,rax
    13e3:	e8 08 fe ff ff       	call   11f0 <fflush@plt>
    13e8:	bf 00 00 00 00       	mov    edi,0x0
    13ed:	e8 5e fe ff ff       	call   1250 <exit@plt>

00000000000013f2 <slurp>:
    13f2:	f3 0f 1e fa          	endbr64
    13f6:	55                   	push   rbp
    13f7:	48 89 e5             	mov    rbp,rsp
    13fa:	48 83 ec 30          	sub    rsp,0x30
    13fe:	e8 cd fd ff ff       	call   11d0 <getchar@plt>
    1403:	bf 62 00 00 00       	mov    edi,0x62
    1408:	e8 d3 fd ff ff       	call   11e0 <malloc@plt>
    140d:	48 89 45 d8          	mov    QWORD PTR [rbp-0x28],rax
    1411:	48 8b 45 d8          	mov    rax,QWORD PTR [rbp-0x28]
    1415:	48 89 45 e0          	mov    QWORD PTR [rbp-0x20],rax
    1419:	48 c7 45 e8 62 00 00 	mov    QWORD PTR [rbp-0x18],0x62
    1420:	00 
    1421:	48 8b 45 e8          	mov    rax,QWORD PTR [rbp-0x18]
    1425:	48 89 45 f0          	mov    QWORD PTR [rbp-0x10],rax
    1429:	48 83 7d d8 00       	cmp    QWORD PTR [rbp-0x28],0x0
    142e:	75 0a                	jne    143a <slurp+0x48>
    1430:	b8 00 00 00 00       	mov    eax,0x0
    1435:	e9 af 00 00 00       	jmp    14e9 <slurp+0xf7>
    143a:	48 8b 05 ef 2b 00 00 	mov    rax,QWORD PTR [rip+0x2bef]        # 4030 <stdin@GLIBC_2.2.5>
    1441:	48 89 c7             	mov    rdi,rax
    1444:	e8 57 fd ff ff       	call   11a0 <fgetc@plt>
    1449:	89 45 d4             	mov    DWORD PTR [rbp-0x2c],eax
    144c:	83 7d d4 ff          	cmp    DWORD PTR [rbp-0x2c],0xffffffff
    1450:	0f 84 84 00 00 00    	je     14da <slurp+0xe8>
    1456:	48 83 6d f0 01       	sub    QWORD PTR [rbp-0x10],0x1
    145b:	48 83 7d f0 00       	cmp    QWORD PTR [rbp-0x10],0x0
    1460:	75 5b                	jne    14bd <slurp+0xcb>
    1462:	48 8b 45 e8          	mov    rax,QWORD PTR [rbp-0x18]
    1466:	48 89 45 f0          	mov    QWORD PTR [rbp-0x10],rax
    146a:	48 d1 65 e8          	shl    QWORD PTR [rbp-0x18],1
    146e:	48 8b 55 e8          	mov    rdx,QWORD PTR [rbp-0x18]
    1472:	48 8b 45 e0          	mov    rax,QWORD PTR [rbp-0x20]
    1476:	48 89 d6             	mov    rsi,rdx
    1479:	48 89 c7             	mov    rdi,rax
    147c:	e8 7f fd ff ff       	call   1200 <realloc@plt>
    1481:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    1485:	48 83 7d f8 00       	cmp    QWORD PTR [rbp-0x8],0x0
    148a:	75 13                	jne    149f <slurp+0xad>
    148c:	48 8b 45 e0          	mov    rax,QWORD PTR [rbp-0x20]
    1490:	48 89 c7             	mov    rdi,rax
    1493:	e8 b8 fc ff ff       	call   1150 <free@plt>
    1498:	b8 00 00 00 00       	mov    eax,0x0
    149d:	eb 4a                	jmp    14e9 <slurp+0xf7>
    149f:	48 8b 45 d8          	mov    rax,QWORD PTR [rbp-0x28]
    14a3:	48 2b 45 e0          	sub    rax,QWORD PTR [rbp-0x20]
    14a7:	48 89 c2             	mov    rdx,rax
    14aa:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    14ae:	48 01 d0             	add    rax,rdx
    14b1:	48 89 45 d8          	mov    QWORD PTR [rbp-0x28],rax
    14b5:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    14b9:	48 89 45 e0          	mov    QWORD PTR [rbp-0x20],rax
    14bd:	48 8b 45 d8          	mov    rax,QWORD PTR [rbp-0x28]
    14c1:	48 8d 50 01          	lea    rdx,[rax+0x1]
    14c5:	48 89 55 d8          	mov    QWORD PTR [rbp-0x28],rdx
    14c9:	8b 55 d4             	mov    edx,DWORD PTR [rbp-0x2c]
    14cc:	88 10                	mov    BYTE PTR [rax],dl
    14ce:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    14d1:	3c 0a                	cmp    al,0xa
    14d3:	74 08                	je     14dd <slurp+0xeb>
    14d5:	e9 60 ff ff ff       	jmp    143a <slurp+0x48>
    14da:	90                   	nop
    14db:	eb 01                	jmp    14de <slurp+0xec>
    14dd:	90                   	nop
    14de:	48 8b 45 d8          	mov    rax,QWORD PTR [rbp-0x28]
    14e2:	c6 00 00             	mov    BYTE PTR [rax],0x0
    14e5:	48 8b 45 e0          	mov    rax,QWORD PTR [rbp-0x20]
    14e9:	c9                   	leave
    14ea:	c3                   	ret

00000000000014eb <doAction>:
    14eb:	f3 0f 1e fa          	endbr64
    14ef:	55                   	push   rbp
    14f0:	48 89 e5             	mov    rbp,rsp
    14f3:	48 83 ec 10          	sub    rsp,0x10
    14f7:	48 89 7d f8          	mov    QWORD PTR [rbp-0x8],rdi
    14fb:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    14ff:	48 8b 10             	mov    rdx,QWORD PTR [rax]
    1502:	b8 00 00 00 00       	mov    eax,0x0
    1507:	ff d2                	call   rdx
    1509:	90                   	nop
    150a:	c9                   	leave
    150b:	c3                   	ret

000000000000150c <s>:
    150c:	f3 0f 1e fa          	endbr64
    1510:	55                   	push   rbp
    1511:	48 89 e5             	mov    rbp,rsp
    1514:	48 8d 35 2e fe ff ff 	lea    rsi,[rip+0xfffffffffffffe2e]        # 1349 <winner_function>
    151b:	48 8d 3d 57 0b 00 00 	lea    rdi,[rip+0xb57]        # 2079 <_IO_stdin_used+0x79>
    1522:	b8 00 00 00 00       	mov    eax,0x0
    1527:	e8 64 fc ff ff       	call   1190 <printf@plt>
    152c:	48 8d 3d 65 0b 00 00 	lea    rdi,[rip+0xb65]        # 2098 <_IO_stdin_used+0x98>
    1533:	e8 38 fc ff ff       	call   1170 <puts@plt>
    1538:	90                   	nop
    1539:	5d                   	pop    rbp
    153a:	c3                   	ret

000000000000153b <p>:
    153b:	f3 0f 1e fa          	endbr64
    153f:	55                   	push   rbp
    1540:	48 89 e5             	mov    rbp,rsp
    1543:	48 8d 3d 9c 0b 00 00 	lea    rdi,[rip+0xb9c]        # 20e6 <_IO_stdin_used+0xe6>
    154a:	e8 21 fc ff ff       	call   1170 <puts@plt>
    154f:	90                   	nop
    1550:	5d                   	pop    rbp
    1551:	c3                   	ret

0000000000001552 <m>:
    1552:	f3 0f 1e fa          	endbr64
    1556:	55                   	push   rbp
    1557:	48 89 e5             	mov    rbp,rsp
    155a:	48 8d 3d 99 0b 00 00 	lea    rdi,[rip+0xb99]        # 20fa <_IO_stdin_used+0xfa>
    1561:	e8 0a fc ff ff       	call   1170 <puts@plt>
    1566:	90                   	nop
    1567:	5d                   	pop    rbp
    1568:	c3                   	ret

0000000000001569 <leaveMessage>:
    1569:	f3 0f 1e fa          	endbr64
    156d:	55                   	push   rbp
    156e:	48 89 e5             	mov    rbp,rsp
    1571:	48 83 ec 10          	sub    rsp,0x10
    1575:	48 8d 3d 94 0b 00 00 	lea    rdi,[rip+0xb94]        # 2110 <_IO_stdin_used+0x110>
    157c:	e8 ef fb ff ff       	call   1170 <puts@plt>
    1581:	bf 08 00 00 00       	mov    edi,0x8
    1586:	e8 55 fc ff ff       	call   11e0 <malloc@plt>
    158b:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    158f:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    1593:	ba 08 00 00 00       	mov    edx,0x8
    1598:	48 89 c6             	mov    rsi,rax
    159b:	bf 00 00 00 00       	mov    edi,0x0
    15a0:	e8 0b fc ff ff       	call   11b0 <read@plt>
    15a5:	48 85 c0             	test   rax,rax
    15a8:	7f 16                	jg     15c0 <leaveMessage+0x57>
    15aa:	48 8d 3d a5 0b 00 00 	lea    rdi,[rip+0xba5]        # 2156 <_IO_stdin_used+0x156>
    15b1:	e8 ba fb ff ff       	call   1170 <puts@plt>
    15b6:	bf 01 00 00 00       	mov    edi,0x1
    15bb:	e8 90 fc ff ff       	call   1250 <exit@plt>
    15c0:	90                   	nop
    15c1:	c9                   	leave
    15c2:	c3                   	ret

00000000000015c3 <i>:
    15c3:	f3 0f 1e fa          	endbr64
    15c7:	55                   	push   rbp
    15c8:	48 89 e5             	mov    rbp,rsp
    15cb:	48 83 ec 10          	sub    rsp,0x10
    15cf:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    15d6:	00 00 
    15d8:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    15dc:	31 c0                	xor    eax,eax
    15de:	48 8d 3d 8d 0b 00 00 	lea    rdi,[rip+0xb8d]        # 2172 <_IO_stdin_used+0x172>
    15e5:	e8 86 fb ff ff       	call   1170 <puts@plt>
    15ea:	48 8d 45 f7          	lea    rax,[rbp-0x9]
    15ee:	48 89 c6             	mov    rsi,rax
    15f1:	48 8d 3d 97 0b 00 00 	lea    rdi,[rip+0xb97]        # 218f <_IO_stdin_used+0x18f>
    15f8:	b8 00 00 00 00       	mov    eax,0x0
    15fd:	e8 3e fc ff ff       	call   1240 <__isoc99_scanf@plt>
    1602:	83 f8 01             	cmp    eax,0x1
    1605:	74 16                	je     161d <i+0x5a>
    1607:	48 8d 3d 85 0b 00 00 	lea    rdi,[rip+0xb85]        # 2193 <_IO_stdin_used+0x193>
    160e:	e8 5d fb ff ff       	call   1170 <puts@plt>
    1613:	bf 01 00 00 00       	mov    edi,0x1
    1618:	e8 33 fc ff ff       	call   1250 <exit@plt>
    161d:	0f b6 45 f7          	movzx  eax,BYTE PTR [rbp-0x9]
    1621:	0f be c0             	movsx  eax,al
    1624:	89 c7                	mov    edi,eax
    1626:	e8 35 fb ff ff       	call   1160 <toupper@plt>
    162b:	83 f8 59             	cmp    eax,0x59
    162e:	75 1d                	jne    164d <i+0x8a>
    1630:	48 8d 3d 73 0b 00 00 	lea    rdi,[rip+0xb73]        # 21aa <_IO_stdin_used+0x1aa>
    1637:	e8 34 fb ff ff       	call   1170 <puts@plt>
    163c:	48 8b 05 15 2a 00 00 	mov    rax,QWORD PTR [rip+0x2a15]        # 4058 <student>
    1643:	48 89 c7             	mov    rdi,rax
    1646:	e8 05 fb ff ff       	call   1150 <free@plt>
    164b:	eb 0c                	jmp    1659 <i+0x96>
    164d:	48 8d 3d 5c 0b 00 00 	lea    rdi,[rip+0xb5c]        # 21b0 <_IO_stdin_used+0x1b0>
    1654:	e8 17 fb ff ff       	call   1170 <puts@plt>
    1659:	90                   	nop
    165a:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    165e:	64 48 33 04 25 28 00 	xor    rax,QWORD PTR fs:0x28
    1665:	00 00 
    1667:	74 05                	je     166e <i+0xab>
    1669:	e8 12 fb ff ff       	call   1180 <__stack_chk_fail@plt>
    166e:	c9                   	leave
    166f:	c3                   	ret

0000000000001670 <printMenu>:
    1670:	f3 0f 1e fa          	endbr64
    1674:	55                   	push   rbp
    1675:	48 89 e5             	mov    rbp,rsp
    1678:	48 8d 3d 65 0b 00 00 	lea    rdi,[rip+0xb65]        # 21e4 <_IO_stdin_used+0x1e4>
    167f:	e8 ec fa ff ff       	call   1170 <puts@plt>
    1684:	48 8d 3d 5a 0b 00 00 	lea    rdi,[rip+0xb5a]        # 21e5 <_IO_stdin_used+0x1e5>
    168b:	e8 e0 fa ff ff       	call   1170 <puts@plt>
    1690:	48 8d 3d 65 0b 00 00 	lea    rdi,[rip+0xb65]        # 21fc <_IO_stdin_used+0x1fc>
    1697:	e8 d4 fa ff ff       	call   1170 <puts@plt>
    169c:	48 8d 3d 70 0b 00 00 	lea    rdi,[rip+0xb70]        # 2213 <_IO_stdin_used+0x213>
    16a3:	e8 c8 fa ff ff       	call   1170 <puts@plt>
    16a8:	48 8d 3d 7c 0b 00 00 	lea    rdi,[rip+0xb7c]        # 222b <_IO_stdin_used+0x22b>
    16af:	e8 bc fa ff ff       	call   1170 <puts@plt>
    16b4:	48 8d 3d 8b 0b 00 00 	lea    rdi,[rip+0xb8b]        # 2246 <_IO_stdin_used+0x246>
    16bb:	e8 b0 fa ff ff       	call   1170 <puts@plt>
    16c0:	48 8d 3d 99 0b 00 00 	lea    rdi,[rip+0xb99]        # 2260 <_IO_stdin_used+0x260>
    16c7:	e8 a4 fa ff ff       	call   1170 <puts@plt>
    16cc:	48 8d 3d a5 0b 00 00 	lea    rdi,[rip+0xba5]        # 2278 <_IO_stdin_used+0x278>
    16d3:	e8 98 fa ff ff       	call   1170 <puts@plt>
    16d8:	48 8d 3d c8 0b 00 00 	lea    rdi,[rip+0xbc8]        # 22a7 <_IO_stdin_used+0x2a7>
    16df:	e8 8c fa ff ff       	call   1170 <puts@plt>
    16e4:	90                   	nop
    16e5:	5d                   	pop    rbp
    16e6:	c3                   	ret

00000000000016e7 <processInput>:
    16e7:	f3 0f 1e fa          	endbr64
    16eb:	55                   	push   rbp
    16ec:	48 89 e5             	mov    rbp,rsp
    16ef:	53                   	push   rbx
    16f0:	48 83 ec 08          	sub    rsp,0x8
    16f4:	48 8d 35 55 29 00 00 	lea    rsi,[rip+0x2955]        # 4050 <choice>
    16fb:	48 8d 3d 8d 0a 00 00 	lea    rdi,[rip+0xa8d]        # 218f <_IO_stdin_used+0x18f>
    1702:	b8 00 00 00 00       	mov    eax,0x0
    1707:	e8 34 fb ff ff       	call   1240 <__isoc99_scanf@plt>
    170c:	83 f8 01             	cmp    eax,0x1
    170f:	74 16                	je     1727 <processInput+0x40>
    1711:	48 8d 3d 7b 0a 00 00 	lea    rdi,[rip+0xa7b]        # 2193 <_IO_stdin_used+0x193>
    1718:	e8 53 fa ff ff       	call   1170 <puts@plt>
    171d:	bf 01 00 00 00       	mov    edi,0x1
    1722:	e8 29 fb ff ff       	call   1250 <exit@plt>
    1727:	0f b6 05 22 29 00 00 	movzx  eax,BYTE PTR [rip+0x2922]        # 4050 <choice>
    172e:	0f be c0             	movsx  eax,al
    1731:	89 c7                	mov    edi,eax
    1733:	e8 28 fa ff ff       	call   1160 <toupper@plt>
    1738:	88 05 12 29 00 00    	mov    BYTE PTR [rip+0x2912],al        # 4050 <choice>
    173e:	0f b6 05 0b 29 00 00 	movzx  eax,BYTE PTR [rip+0x290b]        # 4050 <choice>
    1745:	0f be c0             	movsx  eax,al
    1748:	83 e8 45             	sub    eax,0x45
    174b:	83 f8 0e             	cmp    eax,0xe
    174e:	0f 87 dc 00 00 00    	ja     1830 <processInput+0x149>
    1754:	89 c0                	mov    eax,eax
    1756:	48 8d 14 85 00 00 00 	lea    rdx,[rax*4+0x0]
    175d:	00 
    175e:	48 8d 05 cb 0b 00 00 	lea    rax,[rip+0xbcb]        # 2330 <_IO_stdin_used+0x330>
    1765:	8b 04 02             	mov    eax,DWORD PTR [rdx+rax*1]
    1768:	48 98                	cdqe
    176a:	48 8d 15 bf 0b 00 00 	lea    rdx,[rip+0xbbf]        # 2330 <_IO_stdin_used+0x330>
    1771:	48 01 d0             	add    rax,rdx
    1774:	3e ff e0             	notrack jmp rax
    1777:	48 8b 05 da 28 00 00 	mov    rax,QWORD PTR [rip+0x28da]        # 4058 <student>
    177e:	48 85 c0             	test   rax,rax
    1781:	74 16                	je     1799 <processInput+0xb2>
    1783:	48 8b 05 ce 28 00 00 	mov    rax,QWORD PTR [rip+0x28ce]        # 4058 <student>
    178a:	48 8d 15 7b fd ff ff 	lea    rdx,[rip+0xfffffffffffffd7b]        # 150c <s>
    1791:	48 89 10             	mov    QWORD PTR [rax],rdx
    1794:	e9 ad 00 00 00       	jmp    1846 <processInput+0x15f>
    1799:	48 8d 3d 0e 0b 00 00 	lea    rdi,[rip+0xb0e]        # 22ae <_IO_stdin_used+0x2ae>
    17a0:	e8 cb f9 ff ff       	call   1170 <puts@plt>
    17a5:	e9 9c 00 00 00       	jmp    1846 <processInput+0x15f>
    17aa:	48 8b 05 a7 28 00 00 	mov    rax,QWORD PTR [rip+0x28a7]        # 4058 <student>
    17b1:	48 8d 15 0b fe ff ff 	lea    rdx,[rip+0xfffffffffffffe0b]        # 15c3 <i>
    17b8:	48 89 10             	mov    QWORD PTR [rax],rdx
    17bb:	e9 86 00 00 00       	jmp    1846 <processInput+0x15f>
    17c0:	48 8b 05 91 28 00 00 	mov    rax,QWORD PTR [rip+0x2891]        # 4058 <student>
    17c7:	48 8d 15 84 fd ff ff 	lea    rdx,[rip+0xfffffffffffffd84]        # 1552 <m>
    17ce:	48 89 10             	mov    QWORD PTR [rax],rdx
    17d1:	48 8d 3d e8 0a 00 00 	lea    rdi,[rip+0xae8]        # 22c0 <_IO_stdin_used+0x2c0>
    17d8:	e8 93 f9 ff ff       	call   1170 <puts@plt>
    17dd:	48 8d 3d 04 0b 00 00 	lea    rdi,[rip+0xb04]        # 22e8 <_IO_stdin_used+0x2e8>
    17e4:	e8 87 f9 ff ff       	call   1170 <puts@plt>
    17e9:	48 8d 3d 1d 0b 00 00 	lea    rdi,[rip+0xb1d]        # 230d <_IO_stdin_used+0x30d>
    17f0:	e8 7b f9 ff ff       	call   1170 <puts@plt>
    17f5:	48 8b 1d 5c 28 00 00 	mov    rbx,QWORD PTR [rip+0x285c]        # 4058 <student>
    17fc:	e8 f1 fb ff ff       	call   13f2 <slurp>
    1801:	48 89 43 08          	mov    QWORD PTR [rbx+0x8],rax
    1805:	eb 3f                	jmp    1846 <processInput+0x15f>
    1807:	48 8b 05 4a 28 00 00 	mov    rax,QWORD PTR [rip+0x284a]        # 4058 <student>
    180e:	48 8d 15 26 fd ff ff 	lea    rdx,[rip+0xfffffffffffffd26]        # 153b <p>
    1815:	48 89 10             	mov    QWORD PTR [rax],rdx
    1818:	eb 2c                	jmp    1846 <processInput+0x15f>
    181a:	b8 00 00 00 00       	mov    eax,0x0
    181f:	e8 45 fd ff ff       	call   1569 <leaveMessage>
    1824:	eb 20                	jmp    1846 <processInput+0x15f>
    1826:	bf 00 00 00 00       	mov    edi,0x0
    182b:	e8 20 fa ff ff       	call   1250 <exit@plt>
    1830:	48 8d 3d e9 0a 00 00 	lea    rdi,[rip+0xae9]        # 2320 <_IO_stdin_used+0x320>
    1837:	e8 34 f9 ff ff       	call   1170 <puts@plt>
    183c:	bf 01 00 00 00       	mov    edi,0x1
    1841:	e8 0a fa ff ff       	call   1250 <exit@plt>
    1846:	90                   	nop
    1847:	48 83 c4 08          	add    rsp,0x8
    184b:	5b                   	pop    rbx
    184c:	5d                   	pop    rbp
    184d:	c3                   	ret

000000000000184e <setup>:
    184e:	f3 0f 1e fa          	endbr64
    1852:	55                   	push   rbp
    1853:	48 89 e5             	mov    rbp,rsp
    1856:	48 8b 05 d3 27 00 00 	mov    rax,QWORD PTR [rip+0x27d3]        # 4030 <stdin@GLIBC_2.2.5>
    185d:	b9 00 00 00 00       	mov    ecx,0x0
    1862:	ba 01 00 00 00       	mov    edx,0x1
    1867:	be 00 00 00 00       	mov    esi,0x0
    186c:	48 89 c7             	mov    rdi,rax
    186f:	e8 9c f9 ff ff       	call   1210 <setvbuf@plt>
    1874:	48 8b 05 a5 27 00 00 	mov    rax,QWORD PTR [rip+0x27a5]        # 4020 <stdout@GLIBC_2.2.5>
    187b:	b9 00 00 00 00       	mov    ecx,0x0
    1880:	ba 01 00 00 00       	mov    edx,0x1
    1885:	be 00 00 00 00       	mov    esi,0x0
    188a:	48 89 c7             	mov    rdi,rax
    188d:	e8 7e f9 ff ff       	call   1210 <setvbuf@plt>
    1892:	48 8b 05 a7 27 00 00 	mov    rax,QWORD PTR [rip+0x27a7]        # 4040 <stderr@GLIBC_2.2.5>
    1899:	b9 00 00 00 00       	mov    ecx,0x0
    189e:	ba 01 00 00 00       	mov    edx,0x1
    18a3:	be 00 00 00 00       	mov    esi,0x0
    18a8:	48 89 c7             	mov    rdi,rax
    18ab:	e8 60 f9 ff ff       	call   1210 <setvbuf@plt>
    18b0:	90                   	nop
    18b1:	5d                   	pop    rbp
    18b2:	c3                   	ret

00000000000018b3 <main>:
    18b3:	f3 0f 1e fa          	endbr64
    18b7:	55                   	push   rbp
    18b8:	48 89 e5             	mov    rbp,rsp
    18bb:	b8 00 00 00 00       	mov    eax,0x0
    18c0:	e8 89 ff ff ff       	call   184e <setup>
    18c5:	bf 08 00 00 00       	mov    edi,0x8
    18ca:	e8 11 f9 ff ff       	call   11e0 <malloc@plt>
    ; B HER
    18cf:	48 89 05 82 27 00 00 	mov    QWORD PTR [rip+0x2782],rax        # 4058 <student>
    18d6:	b8 00 00 00 00       	mov    eax,0x0
    18db:	e8 90 fd ff ff       	call   1670 <printMenu>
    18e0:	b8 00 00 00 00       	mov    eax,0x0
    18e5:	e8 fd fd ff ff       	call   16e7 <processInput>
    18ea:	48 8b 05 67 27 00 00 	mov    rax,QWORD PTR [rip+0x2767]        # 4058 <student>
    18f1:	48 89 c7             	mov    rdi,rax
    18f4:	e8 f2 fb ff ff       	call   14eb <doAction>
    18f9:	eb db                	jmp    18d6 <main+0x23>
    18fb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001900 <__libc_csu_init>:
    1900:	f3 0f 1e fa          	endbr64
    1904:	41 57                	push   r15
    1906:	4c 8d 3d 2b 24 00 00 	lea    r15,[rip+0x242b]        # 3d38 <__frame_dummy_init_array_entry>
    190d:	41 56                	push   r14
    190f:	49 89 d6             	mov    r14,rdx
    1912:	41 55                	push   r13
    1914:	49 89 f5             	mov    r13,rsi
    1917:	41 54                	push   r12
    1919:	41 89 fc             	mov    r12d,edi
    191c:	55                   	push   rbp
    191d:	48 8d 2d 1c 24 00 00 	lea    rbp,[rip+0x241c]        # 3d40 <__do_global_dtors_aux_fini_array_entry>
    1924:	53                   	push   rbx
    1925:	4c 29 fd             	sub    rbp,r15
    1928:	48 83 ec 08          	sub    rsp,0x8
    192c:	e8 cf f6 ff ff       	call   1000 <_init>
    1931:	48 c1 fd 03          	sar    rbp,0x3
    1935:	74 1f                	je     1956 <__libc_csu_init+0x56>
    1937:	31 db                	xor    ebx,ebx
    1939:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1940:	4c 89 f2             	mov    rdx,r14
    1943:	4c 89 ee             	mov    rsi,r13
    1946:	44 89 e7             	mov    edi,r12d
    1949:	41 ff 14 df          	call   QWORD PTR [r15+rbx*8]
    194d:	48 83 c3 01          	add    rbx,0x1
    1951:	48 39 dd             	cmp    rbp,rbx
    1954:	75 ea                	jne    1940 <__libc_csu_init+0x40>
    1956:	48 83 c4 08          	add    rsp,0x8
    195a:	5b                   	pop    rbx
    195b:	5d                   	pop    rbp
    195c:	41 5c                	pop    r12
    195e:	41 5d                	pop    r13
    1960:	41 5e                	pop    r14
    1962:	41 5f                	pop    r15
    1964:	c3                   	ret
    1965:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
    196c:	00 00 00 00 

0000000000001970 <__libc_csu_fini>:
    1970:	f3 0f 1e fa          	endbr64
    1974:	c3                   	ret

Disassembly of section .fini:

0000000000001978 <_fini>:
    1978:	f3 0f 1e fa          	endbr64
    197c:	48 83 ec 08          	sub    rsp,0x8
    1980:	48 83 c4 08          	add    rsp,0x8
    1984:	c3                   	ret

Disassembly of section .rodata:

0000000000002000 <_IO_stdin_used>:
    2000:	01 00                	add    DWORD PTR [rax],eax
    2002:	02 00                	add    al,BYTE PTR [rax]
    2004:	00 00                	add    BYTE PTR [rax],al
    2006:	00 00                	add    BYTE PTR [rax],al
    2008:	72 00                	jb     200a <_IO_stdin_used+0xa>
    200a:	66 6c                	data16 ins BYTE PTR es:[rdi],dx
    200c:	61                   	(bad)
    200d:	67 2e 74 78          	addr32 cs je 2089 <_IO_stdin_used+0x89>
    2011:	74 00                	je     2013 <_IO_stdin_used+0x13>
    2013:	00 00                	add    BYTE PTR [rax],al
    2015:	00 00                	add    BYTE PTR [rax],al
    2017:	00 50 6c             	add    BYTE PTR [rax+0x6c],dl
    201a:	65 61                	gs (bad)
    201c:	73 65                	jae    2083 <_IO_stdin_used+0x83>
    201e:	20 63 72             	and    BYTE PTR [rbx+0x72],ah
    2021:	65 61                	gs (bad)
    2023:	74 65                	je     208a <_IO_stdin_used+0x8a>
    2025:	20 27                	and    BYTE PTR [rdi],ah
    2027:	66 6c                	data16 ins BYTE PTR es:[rdi],dx
    2029:	61                   	(bad)
    202a:	67 2e 74 78          	addr32 cs je 20a6 <_IO_stdin_used+0xa6>
    202e:	74 27                	je     2057 <_IO_stdin_used+0x57>
    2030:	20 69 6e             	and    BYTE PTR [rcx+0x6e],ch
    2033:	20 74 68 69          	and    BYTE PTR [rax+rbp*2+0x69],dh
    2037:	73 20                	jae    2059 <_IO_stdin_used+0x59>
    2039:	64 69 72 65 63 74 6f 	imul   esi,DWORD PTR fs:[rdx+0x65],0x726f7463
    2040:	72 
    2041:	79 20                	jns    2063 <_IO_stdin_used+0x63>
    2043:	77 69                	ja     20ae <_IO_stdin_used+0xae>
    2045:	74 68                	je     20af <_IO_stdin_used+0xaf>
    2047:	20 79 6f             	and    BYTE PTR [rcx+0x6f],bh
    204a:	75 72                	jne    20be <_IO_stdin_used+0xbe>
    204c:	20 6f 77             	and    BYTE PTR [rdi+0x77],ch
    204f:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2050:	20 64 65 62          	and    BYTE PTR [rbp+riz*2+0x62],ah
    2054:	75 67                	jne    20bd <_IO_stdin_used+0xbd>
    2056:	67 69 6e 67 20 66 6c 	imul   ebp,DWORD PTR [esi+0x67],0x616c6620
    205d:	61 
    205e:	67 2e 0a 00          	cs or  al,BYTE PTR [eax]
    2062:	45 72 72             	rex.RB jb 20d7 <_IO_stdin_used+0xd7>
    2065:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2066:	72 20                	jb     2088 <_IO_stdin_used+0x88>
    2068:	72 65                	jb     20cf <_IO_stdin_used+0xcf>
    206a:	61                   	(bad)
    206b:	64 69 6e 67 20 66 6c 	imul   ebp,DWORD PTR fs:[rsi+0x67],0x616c6620
    2072:	61 
    2073:	67 2e 74 78          	addr32 cs je 20ef <_IO_stdin_used+0xef>
    2077:	74 00                	je     2079 <_IO_stdin_used+0x79>
    2079:	4f                   	rex.WRXB
    207a:	4f 50                	rex.WRXB push r8
    207c:	21 20                	and    DWORD PTR [rax],esp
    207e:	4c                   	rex.WR
    207f:	65 61                	gs (bad)
    2081:	6b 69 6e 67          	imul   ebp,DWORD PTR [rcx+0x6e],0x67
    2085:	20 73 65             	and    BYTE PTR [rbx+0x65],dh
    2088:	63 72 65             	movsxd esi,DWORD PTR [rdx+0x65]
    208b:	74 73                	je     2100 <_IO_stdin_used+0x100>
    208d:	2e 2e 2e 25 70 0a 00 	cs cs cs and eax,0xa70
    2094:	00 
    2095:	00 00                	add    BYTE PTR [rax],al
    2097:	00 54 68 61          	add    BYTE PTR [rax+rbp*2+0x61],dl
    209b:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    209c:	6b 73 20 66          	imul   esi,DWORD PTR [rbx+0x20],0x66
    20a0:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    20a1:	72 20                	jb     20c3 <_IO_stdin_used+0xc3>
    20a3:	73 75                	jae    211a <_IO_stdin_used+0x11a>
    20a5:	62 73                	(bad)
    20a7:	63 72 69             	movsxd esi,DWORD PTR [rdx+0x69]
    20aa:	62                   	(bad)
    20ab:	69 6e 67 21 20 49 20 	imul   ebp,DWORD PTR [rsi+0x67],0x20492021
    20b2:	72 65                	jb     2119 <_IO_stdin_used+0x119>
    20b4:	61                   	(bad)
    20b5:	6c                   	ins    BYTE PTR es:[rdi],dx
    20b6:	6c                   	ins    BYTE PTR es:[rdi],dx
    20b7:	79 20                	jns    20d9 <_IO_stdin_used+0xd9>
    20b9:	72 65                	jb     2120 <_IO_stdin_used+0x120>
    20bb:	63 6f 6d             	movsxd ebp,DWORD PTR [rdi+0x6d]
    20be:	6d                   	ins    DWORD PTR es:[rdi],dx
    20bf:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
    20c1:	64 20 62 65          	and    BYTE PTR fs:[rdx+0x65],ah
    20c5:	63 6f 6d             	movsxd ebp,DWORD PTR [rdi+0x6d]
    20c8:	69 6e 67 20 61 20 70 	imul   ebp,DWORD PTR [rsi+0x67],0x70206120
    20cf:	72 65                	jb     2136 <_IO_stdin_used+0x136>
    20d1:	6d                   	ins    DWORD PTR es:[rdi],dx
    20d2:	69 75 6d 20 73 74 75 	imul   esi,DWORD PTR [rbp+0x6d],0x75747320
    20d9:	64 65 6e             	fs outs dx,BYTE PTR gs:[rsi]
    20dc:	74 20                	je     20fe <_IO_stdin_used+0xfe>
    20de:	6d                   	ins    DWORD PTR es:[rdi],dx
    20df:	65 6d                	gs ins DWORD PTR es:[rdi],dx
    20e1:	62 65                	(bad)
    20e3:	72 21                	jb     2106 <_IO_stdin_used+0x106>
    20e5:	00 50 61             	add    BYTE PTR [rax+0x61],dl
    20e8:	79 6d                	jns    2157 <_IO_stdin_used+0x157>
    20ea:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
    20ec:	74 20                	je     210e <_IO_stdin_used+0x10e>
    20ee:	70 65                	jo     2155 <_IO_stdin_used+0x155>
    20f0:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    20f1:	64 69 6e 67 2e 2e 2e 	imul   ebp,DWORD PTR fs:[rsi+0x67],0x202e2e2e
    20f8:	20 
    20f9:	00 41 63             	add    BYTE PTR [rcx+0x63],al
    20fc:	63 6f 75             	movsxd ebp,DWORD PTR [rdi+0x75]
    20ff:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2100:	74 20                	je     2122 <_IO_stdin_used+0x122>
    2102:	63 72 65             	movsxd esi,DWORD PTR [rdx+0x65]
    2105:	61                   	(bad)
    2106:	74 65                	je     216d <_IO_stdin_used+0x16d>
    2108:	64 2e 00 00          	fs add BYTE PTR fs:[rax],al
    210c:	00 00                	add    BYTE PTR [rax],al
    210e:	00 00                	add    BYTE PTR [rax],al
    2110:	49 20 6f 6e          	rex.WB and BYTE PTR [r15+0x6e],bpl
    2114:	6c                   	ins    BYTE PTR es:[rdi],dx
    2115:	79 20                	jns    2137 <_IO_stdin_used+0x137>
    2117:	72 65                	jb     217e <_IO_stdin_used+0x17e>
    2119:	61                   	(bad)
    211a:	64 20 70 72          	and    BYTE PTR fs:[rax+0x72],dh
    211e:	65 6d                	gs ins DWORD PTR es:[rdi],dx
    2120:	69 75 6d 20 73 74 75 	imul   esi,DWORD PTR [rbp+0x6d],0x75747320
    2127:	64 65 6e             	fs outs dx,BYTE PTR gs:[rsi]
    212a:	74 20                	je     214c <_IO_stdin_used+0x14c>
    212c:	6d                   	ins    DWORD PTR es:[rdi],dx
    212d:	65 6d                	gs ins DWORD PTR es:[rdi],dx
    212f:	62 65                	(bad)
    2131:	72 20                	jb     2153 <_IO_stdin_used+0x153>
    2133:	6d                   	ins    DWORD PTR es:[rdi],dx
    2134:	65 73 73             	gs jae 21aa <_IO_stdin_used+0x1aa>
    2137:	61                   	(bad)
    2138:	67 65 73 20          	addr32 gs jae 215c <_IO_stdin_used+0x15c>
    213c:	62 75 74 20 79       	(bad)
    2141:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2142:	75 20                	jne    2164 <_IO_stdin_used+0x164>
    2144:	63 61 6e             	movsxd esp,DWORD PTR [rcx+0x6e]
    2147:	20 74 72 79          	and    BYTE PTR [rdx+rsi*2+0x79],dh
    214b:	20 61 6e             	and    BYTE PTR [rcx+0x6e],ah
    214e:	79 77                	jns    21c7 <_IO_stdin_used+0x1c7>
    2150:	61                   	(bad)
    2151:	79 73                	jns    21c6 <_IO_stdin_used+0x1c6>
    2153:	3a 20                	cmp    ah,BYTE PTR [rax]
    2155:	00 45 72             	add    BYTE PTR [rbp+0x72],al
    2158:	72 6f                	jb     21c9 <_IO_stdin_used+0x1c9>
    215a:	72 20                	jb     217c <_IO_stdin_used+0x17c>
    215c:	77 68                	ja     21c6 <_IO_stdin_used+0x1c6>
    215e:	69 6c 65 20 72 65 61 	imul   ebp,DWORD PTR [rbp+riz*2+0x20],0x64616572
    2165:	64 
    2166:	69 6e 67 20 6d 65 73 	imul   ebp,DWORD PTR [rsi+0x67],0x73656d20
    216d:	73 61                	jae    21d0 <_IO_stdin_used+0x1d0>
    216f:	67 65 00 59 6f       	add    BYTE PTR gs:[ecx+0x6f],bl
    2174:	75 27                	jne    219d <_IO_stdin_used+0x19d>
    2176:	72 65                	jb     21dd <_IO_stdin_used+0x1dd>
    2178:	20 6c 65 61          	and    BYTE PTR [rbp+riz*2+0x61],ch
    217c:	76 69                	jbe    21e7 <_IO_stdin_used+0x1e7>
    217e:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    217f:	67 20 61 6c          	and    BYTE PTR [ecx+0x6c],ah
    2183:	72 65                	jb     21ea <_IO_stdin_used+0x1ea>
    2185:	61                   	(bad)
    2186:	64 79 28             	fs jns 21b1 <_IO_stdin_used+0x1b1>
    2189:	59                   	pop    rcx
    218a:	2f                   	(bad)
    218b:	4e 29 3f             	rex.WRX sub QWORD PTR [rdi],r15
    218e:	00 20                	add    BYTE PTR [rax],ah
    2190:	25 63 00 49 20       	and    eax,0x20490063
    2195:	64 6f                	outs   dx,DWORD PTR fs:[rsi]
    2197:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2198:	27                   	(bad)
    2199:	74 20                	je     21bb <_IO_stdin_used+0x1bb>
    219b:	75 6e                	jne    220b <_IO_stdin_used+0x20b>
    219d:	64 65 72 73          	fs gs jb 2214 <_IO_stdin_used+0x214>
    21a1:	74 61                	je     2204 <_IO_stdin_used+0x204>
    21a3:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    21a4:	64 20 79 6f          	and    BYTE PTR fs:[rcx+0x6f],bh
    21a8:	75 00                	jne    21aa <_IO_stdin_used+0x1aa>
    21aa:	42 79 65             	rex.X jns 2212 <_IO_stdin_used+0x212>
    21ad:	21 00                	and    DWORD PTR [rax],eax
    21af:	00 4f 4b             	add    BYTE PTR [rdi+0x4b],cl
    21b2:	2e 20 52 65          	cs and BYTE PTR [rdx+0x65],dl
    21b6:	6d                   	ins    DWORD PTR es:[rdi],dx
    21b7:	65 6d                	gs ins DWORD PTR es:[rdi],dx
    21b9:	62 65                	(bad)
    21bb:	72 20                	jb     21dd <_IO_stdin_used+0x1dd>
    21bd:	74 6f                	je     222e <_IO_stdin_used+0x22e>
    21bf:	20 73 69             	and    BYTE PTR [rbx+0x69],dh
    21c2:	67 6e                	outs   dx,BYTE PTR ds:[esi]
    21c4:	20 75 70             	and    BYTE PTR [rbp+0x70],dh
    21c7:	20 66 6f             	and    BYTE PTR [rsi+0x6f],ah
    21ca:	72 20                	jb     21ec <_IO_stdin_used+0x1ec>
    21cc:	63 6f 75             	movsxd ebp,DWORD PTR [rdi+0x75]
    21cf:	72 73                	jb     2244 <_IO_stdin_used+0x244>
    21d1:	65 73 20             	gs jae 21f4 <_IO_stdin_used+0x1f4>
    21d4:	62 65 66 6f 72       	(bad)
    21d9:	65 20 64 65 61       	and    BYTE PTR gs:[rbp+riz*2+0x61],ah
    21de:	64 6c                	fs ins BYTE PTR es:[rdi],dx
    21e0:	69 6e 65 00 00 57 65 	imul   ebp,DWORD PTR [rsi+0x65],0x65570000
    21e7:	6c                   	ins    BYTE PTR es:[rdi],dx
    21e8:	63 6f 6d             	movsxd ebp,DWORD PTR [rdi+0x6d]
    21eb:	65 20 74 6f 20       	and    BYTE PTR gs:[rdi+rbp*2+0x20],dh
    21f0:	4e                   	rex.WRX
    21f1:	65 77 20             	gs ja  2214 <_IO_stdin_used+0x214>
    21f4:	41 62 73             	rex.B (bad)
    21f7:	61                   	(bad)
    21f8:	6c                   	ins    BYTE PTR es:[rdi],dx
    21f9:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    21fa:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    21fb:	00 3d 3d 3d 3d 3d    	add    BYTE PTR [rip+0x3d3d3d3d],bh        # 3d3d5f3e <_end+0x3d3d1ede>
    2201:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    2206:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    220b:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    2210:	3d 3d 00 28 53       	cmp    eax,0x5328003d
    2215:	29 75 62             	sub    DWORD PTR [rbp+0x62],esi
    2218:	73 63                	jae    227d <_IO_stdin_used+0x27d>
    221a:	72 69                	jb     2285 <_IO_stdin_used+0x285>
    221c:	62 65                	(bad)
    221e:	20 74 6f 20          	and    BYTE PTR [rdi+rbp*2+0x20],dh
    2222:	61                   	(bad)
    2223:	20 63 6f             	and    BYTE PTR [rbx+0x6f],ah
    2226:	75 72                	jne    229a <_IO_stdin_used+0x29a>
    2228:	73 65                	jae    228f <_IO_stdin_used+0x28f>
    222a:	00 28                	add    BYTE PTR [rax],ch
    222c:	49 29 6e 71          	sub    QWORD PTR [r14+0x71],rbp
    2230:	75 69                	jne    229b <_IO_stdin_used+0x29b>
    2232:	72 65                	jb     2299 <_IO_stdin_used+0x299>
    2234:	20 61 62             	and    BYTE PTR [rcx+0x62],ah
    2237:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2238:	75 74                	jne    22ae <_IO_stdin_used+0x2ae>
    223a:	20 67 72             	and    BYTE PTR [rdi+0x72],ah
    223d:	61                   	(bad)
    223e:	64 75 61             	fs jne 22a2 <_IO_stdin_used+0x2a2>
    2241:	74 69                	je     22ac <_IO_stdin_used+0x2ac>
    2243:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2244:	67 00 28             	add    BYTE PTR [eax],ch
    2247:	4d 29 61 6b          	sub    QWORD PTR [r9+0x6b],r12
    224b:	65 20 61 6e          	and    BYTE PTR gs:[rcx+0x6e],ah
    224f:	20 41 62             	and    BYTE PTR [rcx+0x62],al
    2252:	73 61                	jae    22b5 <_IO_stdin_used+0x2b5>
    2254:	6c                   	ins    BYTE PTR es:[rdi],dx
    2255:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2256:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2257:	20 61 63             	and    BYTE PTR [rcx+0x63],ah
    225a:	63 6f 75             	movsxd ebp,DWORD PTR [rdi+0x75]
    225d:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    225e:	74 00                	je     2260 <_IO_stdin_used+0x260>
    2260:	28 50 29             	sub    BYTE PTR [rax+0x29],dl
    2263:	61                   	(bad)
    2264:	79 20                	jns    2286 <_IO_stdin_used+0x286>
    2266:	66 6f                	outs   dx,WORD PTR ds:[rsi]
    2268:	72 20                	jb     228a <_IO_stdin_used+0x28a>
    226a:	68 69 67 68 65       	push   0x65686769
    226f:	72 20                	jb     2291 <_IO_stdin_used+0x291>
    2271:	67 72 61             	addr32 jb 22d5 <_IO_stdin_used+0x2d5>
    2274:	64 65 73 00          	fs gs jae 2278 <_IO_stdin_used+0x278>
    2278:	28 4c 29 65          	sub    BYTE PTR [rcx+rbp*1+0x65],cl
    227c:	61                   	(bad)
    227d:	76 65                	jbe    22e4 <_IO_stdin_used+0x2e4>
    227f:	20 61 20             	and    BYTE PTR [rcx+0x20],ah
    2282:	6d                   	ins    DWORD PTR es:[rdi],dx
    2283:	65 73 73             	gs jae 22f9 <_IO_stdin_used+0x2f9>
    2286:	61                   	(bad)
    2287:	67 65 20 28          	and    BYTE PTR gs:[eax],ch
    228b:	77 69                	ja     22f6 <_IO_stdin_used+0x2f6>
    228d:	74 68                	je     22f7 <_IO_stdin_used+0x2f7>
    228f:	20 6f 72             	and    BYTE PTR [rdi+0x72],ch
    2292:	20 77 69             	and    BYTE PTR [rdi+0x69],dh
    2295:	74 68                	je     22ff <_IO_stdin_used+0x2ff>
    2297:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2298:	75 74                	jne    230e <_IO_stdin_used+0x30e>
    229a:	20 6c 6f 67          	and    BYTE PTR [rdi+rbp*2+0x67],ch
    229e:	67 69 6e 67 20 69 6e 	imul   ebp,DWORD PTR [esi+0x67],0x296e6920
    22a5:	29 
    22a6:	00 28                	add    BYTE PTR [rax],ch
    22a8:	45 29 78 69          	sub    DWORD PTR [r8+0x69],r15d
    22ac:	74 00                	je     22ae <_IO_stdin_used+0x2ae>
    22ae:	4e 6f                	rex.WRX outs dx,DWORD PTR ds:[rsi]
    22b0:	74 20                	je     22d2 <_IO_stdin_used+0x2d2>
    22b2:	6c                   	ins    BYTE PTR es:[rdi],dx
    22b3:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    22b4:	67 67 65 64 20 69 6e 	addr32 gs and BYTE PTR fs:[ecx+0x6e],ch
    22bb:	21 00                	and    DWORD PTR [rax],eax
    22bd:	00 00                	add    BYTE PTR [rax],al
    22bf:	00 3d 3d 3d 3d 3d    	add    BYTE PTR [rip+0x3d3d3d3d],bh        # 3d3d6002 <_end+0x3d3d1fa2>
    22c5:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    22ca:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    22cf:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    22d4:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    22d9:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    22de:	3d 3d 3d 3d 3d       	cmp    eax,0x3d3d3d3d
    22e3:	3d 00 00 00 00       	cmp    eax,0x0
    22e8:	52                   	push   rdx
    22e9:	65 67 69 73 74 72 61 	imul   esi,DWORD PTR gs:[ebx+0x74],0x69746172
    22f0:	74 69 
    22f2:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    22f3:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    22f4:	3a 20                	cmp    ah,BYTE PTR [rax]
    22f6:	57                   	push   rdi
    22f7:	65 6c                	gs ins BYTE PTR es:[rdi],dx
    22f9:	63 6f 6d             	movsxd ebp,DWORD PTR [rdi+0x6d]
    22fc:	65 20 74 6f 20       	and    BYTE PTR gs:[rdi+rbp*2+0x20],dh
    2301:	4e                   	rex.WRX
    2302:	65 77 20             	gs ja  2325 <_IO_stdin_used+0x325>
    2305:	41 62 73             	rex.B (bad)
    2308:	61                   	(bad)
    2309:	6c                   	ins    BYTE PTR es:[rdi],dx
    230a:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    230b:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    230c:	00 45 6e             	add    BYTE PTR [rbp+0x6e],al
    230f:	74 65                	je     2376 <__GNU_EH_FRAME_HDR+0xa>
    2311:	72 20                	jb     2333 <_IO_stdin_used+0x333>
    2313:	79 6f                	jns    2384 <__GNU_EH_FRAME_HDR+0x18>
    2315:	75 72                	jne    2389 <__GNU_EH_FRAME_HDR+0x1d>
    2317:	20 4b 55             	and    BYTE PTR [rbx+0x55],cl
    231a:	20 49 44             	and    BYTE PTR [rcx+0x44],cl
    231d:	3a 20                	cmp    ah,BYTE PTR [rax]
    231f:	00 49 6e             	add    BYTE PTR [rcx+0x6e],cl
    2322:	76 61                	jbe    2385 <__GNU_EH_FRAME_HDR+0x19>
    2324:	6c                   	ins    BYTE PTR es:[rdi],dx
    2325:	69 64 20 6f 70 74 69 	imul   esp,DWORD PTR [rax+riz*1+0x6f],0x6f697470
    232c:	6f 
    232d:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    232e:	21 00                	and    DWORD PTR [rax],eax
    2330:	f6 f4                	div    ah
    2332:	ff                   	(bad)
    2333:	ff 00                	inc    DWORD PTR [rax]
    2335:	f5                   	cmc
    2336:	ff                   	(bad)
    2337:	ff 00                	inc    DWORD PTR [rax]
    2339:	f5                   	cmc
    233a:	ff                   	(bad)
    233b:	ff 00                	inc    DWORD PTR [rax]
    233d:	f5                   	cmc
    233e:	ff                   	(bad)
    233f:	ff                   	(bad)
    2340:	7a f4                	jp     2336 <_IO_stdin_used+0x336>
    2342:	ff                   	(bad)
    2343:	ff 00                	inc    DWORD PTR [rax]
    2345:	f5                   	cmc
    2346:	ff                   	(bad)
    2347:	ff 00                	inc    DWORD PTR [rax]
    2349:	f5                   	cmc
    234a:	ff                   	(bad)
    234b:	ff                   	(bad)
    234c:	ea                   	(bad)
    234d:	f4                   	hlt
    234e:	ff                   	(bad)
    234f:	ff 90 f4 ff ff 00    	call   QWORD PTR [rax+0xfffff4]
    2355:	f5                   	cmc
    2356:	ff                   	(bad)
    2357:	ff 00                	inc    DWORD PTR [rax]
    2359:	f5                   	cmc
    235a:	ff                   	(bad)
    235b:	ff d7                	call   rdi
    235d:	f4                   	hlt
    235e:	ff                   	(bad)
    235f:	ff 00                	inc    DWORD PTR [rax]
    2361:	f5                   	cmc
    2362:	ff                   	(bad)
    2363:	ff 00                	inc    DWORD PTR [rax]
    2365:	f5                   	cmc
    2366:	ff                   	(bad)
    2367:	ff 47 f4             	inc    DWORD PTR [rdi-0xc]
    236a:	ff                   	(bad)
    236b:	ff                   	.byte 0xff

Disassembly of section .eh_frame_hdr:

000000000000236c <__GNU_EH_FRAME_HDR>:
    236c:	01 1b                	add    DWORD PTR [rbx],ebx
    236e:	03 3b                	add    edi,DWORD PTR [rbx]
    2370:	98                   	cwde
    2371:	00 00                	add    BYTE PTR [rax],al
    2373:	00 12                	add    BYTE PTR [rdx],dl
    2375:	00 00                	add    BYTE PTR [rax],al
    2377:	00 b4 ec ff ff cc 00 	add    BYTE PTR [rsp+rbp*8+0xccffff],dh
    237e:	00 00                	add    BYTE PTR [rax],al
    2380:	d4                   	(bad)
    2381:	ed                   	in     eax,dx
    2382:	ff                   	(bad)
    2383:	ff f4                	push   rsp
    2385:	00 00                	add    BYTE PTR [rax],al
    2387:	00 e4                	add    ah,ah
    2389:	ed                   	in     eax,dx
    238a:	ff                   	(bad)
    238b:	ff 0c 01             	dec    DWORD PTR [rcx+rax*1]
    238e:	00 00                	add    BYTE PTR [rax],al
    2390:	f4                   	hlt
    2391:	ee                   	out    dx,al
    2392:	ff                   	(bad)
    2393:	ff b4 00 00 00 dd ef 	push   QWORD PTR [rax+rax*1-0x10230000]
    239a:	ff                   	(bad)
    239b:	ff 24 01             	jmp    QWORD PTR [rcx+rax*1]
    239e:	00 00                	add    BYTE PTR [rax],al
    23a0:	86 f0                	xchg   al,dh
    23a2:	ff                   	(bad)
    23a3:	ff 40 01             	inc    DWORD PTR [rax+0x1]
    23a6:	00 00                	add    BYTE PTR [rax],al
    23a8:	7f f1                	jg     239b <__GNU_EH_FRAME_HDR+0x2f>
    23aa:	ff                   	(bad)
    23ab:	ff 60 01             	jmp    QWORD PTR [rax+0x1]
    23ae:	00 00                	add    BYTE PTR [rax],al
    23b0:	a0 f1 ff ff 80 01 00 	movabs al,ds:0xcf00000180fffff1
    23b7:	00 cf 
    23b9:	f1                   	int1
    23ba:	ff                   	(bad)
    23bb:	ff a0 01 00 00 e6    	jmp    QWORD PTR [rax-0x19ffffff]
    23c1:	f1                   	int1
    23c2:	ff                   	(bad)
    23c3:	ff c0                	inc    eax
    23c5:	01 00                	add    DWORD PTR [rax],eax
    23c7:	00 fd                	add    ch,bh
    23c9:	f1                   	int1
    23ca:	ff                   	(bad)
    23cb:	ff e0                	jmp    rax
    23cd:	01 00                	add    DWORD PTR [rax],eax
    23cf:	00 57 f2             	add    BYTE PTR [rdi-0xe],dl
    23d2:	ff                   	(bad)
    23d3:	ff 00                	inc    DWORD PTR [rax]
    23d5:	02 00                	add    al,BYTE PTR [rax]
    23d7:	00 04 f3             	add    BYTE PTR [rbx+rsi*8],al
    23da:	ff                   	(bad)
    23db:	ff 20                	jmp    QWORD PTR [rax]
    23dd:	02 00                	add    al,BYTE PTR [rax]
    23df:	00 7b f3             	add    BYTE PTR [rbx-0xd],bh
    23e2:	ff                   	(bad)
    23e3:	ff 40 02             	inc    DWORD PTR [rax+0x2]
    23e6:	00 00                	add    BYTE PTR [rax],al
    23e8:	e2 f4                	loop   23de <__GNU_EH_FRAME_HDR+0x72>
    23ea:	ff                   	(bad)
    23eb:	ff 64 02 00          	jmp    QWORD PTR [rdx+rax*1+0x0]
    23ef:	00 47 f5             	add    BYTE PTR [rdi-0xb],al
    23f2:	ff                   	(bad)
    23f3:	ff 84 02 00 00 94 f5 	inc    DWORD PTR [rdx+rax*1-0xa6c0000]
    23fa:	ff                   	(bad)
    23fb:	ff a4 02 00 00 04 f6 	jmp    QWORD PTR [rdx+rax*1-0x9fc0000]
    2402:	ff                   	(bad)
    2403:	ff                   	(bad)
    2404:	ec                   	in     al,dx
    2405:	02 00                	add    al,BYTE PTR [rax]
	...

Disassembly of section .eh_frame:

0000000000002408 <__FRAME_END__-0x264>:
    2408:	14 00                	adc    al,0x0
    240a:	00 00                	add    BYTE PTR [rax],al
    240c:	00 00                	add    BYTE PTR [rax],al
    240e:	00 00                	add    BYTE PTR [rax],al
    2410:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
    2413:	00 01                	add    BYTE PTR [rcx],al
    2415:	78 10                	js     2427 <__GNU_EH_FRAME_HDR+0xbb>
    2417:	01 1b                	add    DWORD PTR [rbx],ebx
    2419:	0c 07                	or     al,0x7
    241b:	08 90 01 00 00 14    	or     BYTE PTR [rax+0x14000001],dl
    2421:	00 00                	add    BYTE PTR [rax],al
    2423:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    2426:	00 00                	add    BYTE PTR [rax],al
    2428:	38 ee                	cmp    dh,ch
    242a:	ff                   	(bad)
    242b:	ff 2f                	jmp    FWORD PTR [rdi]
    242d:	00 00                	add    BYTE PTR [rax],al
    242f:	00 00                	add    BYTE PTR [rax],al
    2431:	44 07                	rex.R (bad)
    2433:	10 00                	adc    BYTE PTR [rax],al
    2435:	00 00                	add    BYTE PTR [rax],al
    2437:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
    243a:	00 00                	add    BYTE PTR [rax],al
    243c:	34 00                	xor    al,0x0
    243e:	00 00                	add    BYTE PTR [rax],al
    2440:	e0 eb                	loopne 242d <__GNU_EH_FRAME_HDR+0xc1>
    2442:	ff                   	(bad)
    2443:	ff 20                	jmp    QWORD PTR [rax]
    2445:	01 00                	add    DWORD PTR [rax],eax
    2447:	00 00                	add    BYTE PTR [rax],al
    2449:	0e                   	(bad)
    244a:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
    244d:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
    2450:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
    2453:	80 00 3f             	add    BYTE PTR [rax],0x3f
    2456:	1a 3a                	sbb    bh,BYTE PTR [rdx]
    2458:	2a 33                	sub    dh,BYTE PTR [rbx]
    245a:	24 22                	and    al,0x22
    245c:	00 00                	add    BYTE PTR [rax],al
    245e:	00 00                	add    BYTE PTR [rax],al
    2460:	14 00                	adc    al,0x0
    2462:	00 00                	add    BYTE PTR [rax],al
    2464:	5c                   	pop    rsp
    2465:	00 00                	add    BYTE PTR [rax],al
    2467:	00 d8                	add    al,bl
    2469:	ec                   	in     al,dx
    246a:	ff                   	(bad)
    246b:	ff 10                	call   QWORD PTR [rax]
	...
    2475:	00 00                	add    BYTE PTR [rax],al
    2477:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    247a:	00 00                	add    BYTE PTR [rax],al
    247c:	74 00                	je     247e <__GNU_EH_FRAME_HDR+0x112>
    247e:	00 00                	add    BYTE PTR [rax],al
    2480:	d0 ec                	shr    ah,1
    2482:	ff                   	(bad)
    2483:	ff 10                	call   QWORD PTR [rax]
    2485:	01 00                	add    DWORD PTR [rax],eax
	...
    248f:	00 18                	add    BYTE PTR [rax],bl
    2491:	00 00                	add    BYTE PTR [rax],al
    2493:	00 8c 00 00 00 b1 ee 	add    BYTE PTR [rax+rax*1-0x114f0000],cl
    249a:	ff                   	(bad)
    249b:	ff a9 00 00 00 00    	jmp    FWORD PTR [rcx+0x0]
    24a1:	45 0e                	rex.RB (bad)
    24a3:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    24a9:	00 00                	add    BYTE PTR [rax],al
    24ab:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    24ae:	00 00                	add    BYTE PTR [rax],al
    24b0:	a8 00                	test   al,0x0
    24b2:	00 00                	add    BYTE PTR [rax],al
    24b4:	3e ef                	ds out dx,eax
    24b6:	ff                   	(bad)
    24b7:	ff                   	(bad)
    24b8:	f9                   	stc
    24b9:	00 00                	add    BYTE PTR [rax],al
    24bb:	00 00                	add    BYTE PTR [rax],al
    24bd:	45 0e                	rex.RB (bad)
    24bf:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    24c5:	02 f0                	add    dh,al
    24c7:	0c 07                	or     al,0x7
    24c9:	08 00                	or     BYTE PTR [rax],al
    24cb:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    24ce:	00 00                	add    BYTE PTR [rax],al
    24d0:	c8 00 00 00          	enter  0x0,0x0
    24d4:	17                   	(bad)
    24d5:	f0 ff                	lock (bad)
    24d7:	ff 21                	jmp    QWORD PTR [rcx]
    24d9:	00 00                	add    BYTE PTR [rax],al
    24db:	00 00                	add    BYTE PTR [rax],al
    24dd:	45 0e                	rex.RB (bad)
    24df:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    24e5:	58                   	pop    rax
    24e6:	0c 07                	or     al,0x7
    24e8:	08 00                	or     BYTE PTR [rax],al
    24ea:	00 00                	add    BYTE PTR [rax],al
    24ec:	1c 00                	sbb    al,0x0
    24ee:	00 00                	add    BYTE PTR [rax],al
    24f0:	e8 00 00 00 18       	call   180024f5 <_end+0x17ffe495>
    24f5:	f0 ff                	lock (bad)
    24f7:	ff 2f                	jmp    FWORD PTR [rdi]
    24f9:	00 00                	add    BYTE PTR [rax],al
    24fb:	00 00                	add    BYTE PTR [rax],al
    24fd:	45 0e                	rex.RB (bad)
    24ff:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2505:	66 0c 07             	data16 or al,0x7
    2508:	08 00                	or     BYTE PTR [rax],al
    250a:	00 00                	add    BYTE PTR [rax],al
    250c:	1c 00                	sbb    al,0x0
    250e:	00 00                	add    BYTE PTR [rax],al
    2510:	08 01                	or     BYTE PTR [rcx],al
    2512:	00 00                	add    BYTE PTR [rax],al
    2514:	27                   	(bad)
    2515:	f0 ff                	lock (bad)
    2517:	ff 17                	call   QWORD PTR [rdi]
    2519:	00 00                	add    BYTE PTR [rax],al
    251b:	00 00                	add    BYTE PTR [rax],al
    251d:	45 0e                	rex.RB (bad)
    251f:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2525:	4e 0c 07             	rex.WRX or al,0x7
    2528:	08 00                	or     BYTE PTR [rax],al
    252a:	00 00                	add    BYTE PTR [rax],al
    252c:	1c 00                	sbb    al,0x0
    252e:	00 00                	add    BYTE PTR [rax],al
    2530:	28 01                	sub    BYTE PTR [rcx],al
    2532:	00 00                	add    BYTE PTR [rax],al
    2534:	1e                   	(bad)
    2535:	f0 ff                	lock (bad)
    2537:	ff 17                	call   QWORD PTR [rdi]
    2539:	00 00                	add    BYTE PTR [rax],al
    253b:	00 00                	add    BYTE PTR [rax],al
    253d:	45 0e                	rex.RB (bad)
    253f:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2545:	4e 0c 07             	rex.WRX or al,0x7
    2548:	08 00                	or     BYTE PTR [rax],al
    254a:	00 00                	add    BYTE PTR [rax],al
    254c:	1c 00                	sbb    al,0x0
    254e:	00 00                	add    BYTE PTR [rax],al
    2550:	48 01 00             	add    QWORD PTR [rax],rax
    2553:	00 15 f0 ff ff 5a    	add    BYTE PTR [rip+0x5afffff0],dl        # 5b002549 <_end+0x5affe4e9>
    2559:	00 00                	add    BYTE PTR [rax],al
    255b:	00 00                	add    BYTE PTR [rax],al
    255d:	45 0e                	rex.RB (bad)
    255f:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2565:	02 51 0c             	add    dl,BYTE PTR [rcx+0xc]
    2568:	07                   	(bad)
    2569:	08 00                	or     BYTE PTR [rax],al
    256b:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    256e:	00 00                	add    BYTE PTR [rax],al
    2570:	68 01 00 00 4f       	push   0x4f000001
    2575:	f0 ff                	lock (bad)
    2577:	ff ad 00 00 00 00    	jmp    FWORD PTR [rbp+0x0]
    257d:	45 0e                	rex.RB (bad)
    257f:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2585:	02 a4 0c 07 08 00 00 	add    ah,BYTE PTR [rsp+rcx*1+0x807]
    258c:	1c 00                	sbb    al,0x0
    258e:	00 00                	add    BYTE PTR [rax],al
    2590:	88 01                	mov    BYTE PTR [rcx],al
    2592:	00 00                	add    BYTE PTR [rax],al
    2594:	dc f0                	fdivr  st(0),st
    2596:	ff                   	(bad)
    2597:	ff 77 00             	push   QWORD PTR [rdi+0x0]
    259a:	00 00                	add    BYTE PTR [rax],al
    259c:	00 45 0e             	add    BYTE PTR [rbp+0xe],al
    259f:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    25a5:	02 6e 0c             	add    ch,BYTE PTR [rsi+0xc]
    25a8:	07                   	(bad)
    25a9:	08 00                	or     BYTE PTR [rax],al
    25ab:	00 20                	add    BYTE PTR [rax],ah
    25ad:	00 00                	add    BYTE PTR [rax],al
    25af:	00 a8 01 00 00 33    	add    BYTE PTR [rax+0x33000001],ch
    25b5:	f1                   	int1
    25b6:	ff                   	(bad)
    25b7:	ff 67 01             	jmp    QWORD PTR [rdi+0x1]
    25ba:	00 00                	add    BYTE PTR [rax],al
    25bc:	00 45 0e             	add    BYTE PTR [rbp+0xe],al
    25bf:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    25c5:	45 83 03 03          	rex.RB add DWORD PTR [r11],0x3
    25c9:	59                   	pop    rcx
    25ca:	01 0c 07             	add    DWORD PTR [rdi+rax*1],ecx
    25cd:	08 00                	or     BYTE PTR [rax],al
    25cf:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    25d2:	00 00                	add    BYTE PTR [rax],al
    25d4:	cc                   	int3
    25d5:	01 00                	add    DWORD PTR [rax],eax
    25d7:	00 76 f2             	add    BYTE PTR [rsi-0xe],dh
    25da:	ff                   	(bad)
    25db:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
    25de:	00 00                	add    BYTE PTR [rax],al
    25e0:	00 45 0e             	add    BYTE PTR [rbp+0xe],al
    25e3:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    25e9:	02 5c 0c 07          	add    bl,BYTE PTR [rsp+rcx*1+0x7]
    25ed:	08 00                	or     BYTE PTR [rax],al
    25ef:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    25f2:	00 00                	add    BYTE PTR [rax],al
    25f4:	ec                   	in     al,dx
    25f5:	01 00                	add    DWORD PTR [rax],eax
    25f7:	00 bb f2 ff ff 48    	add    BYTE PTR [rbx+0x48fffff2],bh
    25fd:	00 00                	add    BYTE PTR [rax],al
    25ff:	00 00                	add    BYTE PTR [rax],al
    2601:	45 0e                	rex.RB (bad)
    2603:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2609:	00 00                	add    BYTE PTR [rax],al
    260b:	00 00                	add    BYTE PTR [rax],al
    260d:	00 00                	add    BYTE PTR [rax],al
    260f:	00 44 00 00          	add    BYTE PTR [rax+rax*1+0x0],al
    2613:	00 0c 02             	add    BYTE PTR [rdx+rax*1],cl
    2616:	00 00                	add    BYTE PTR [rax],al
    2618:	e8 f2 ff ff 65       	call   6600260f <_end+0x65ffe5af>
    261d:	00 00                	add    BYTE PTR [rax],al
    261f:	00 00                	add    BYTE PTR [rax],al
    2621:	46 0e                	rex.RX (bad)
    2623:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
    2629:	8e 03                	mov    es,WORD PTR [rbx]
    262b:	45 0e                	rex.RB (bad)
    262d:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
    2633:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff8630347d <_end+0xffffffff862ff41d>
    2639:	06                   	(bad)
    263a:	48 0e                	rex.W (bad)
    263c:	38 83 07 47 0e 40    	cmp    BYTE PTR [rbx+0x400e4707],al
    2642:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2643:	0e                   	(bad)
    2644:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
    2647:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
    264a:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
    264d:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
    2650:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
    2653:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
    2656:	08 00                	or     BYTE PTR [rax],al
    2658:	10 00                	adc    BYTE PTR [rax],al
    265a:	00 00                	add    BYTE PTR [rax],al
    265c:	54                   	push   rsp
    265d:	02 00                	add    al,BYTE PTR [rax]
    265f:	00 10                	add    BYTE PTR [rax],dl
    2661:	f3 ff                	repz (bad)
    2663:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 2669 <__GNU_EH_FRAME_HDR+0x2fd>
    2669:	00 00                	add    BYTE PTR [rax],al
	...

000000000000266c <__FRAME_END__>:
    266c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000003d38 <__frame_dummy_init_array_entry>:
    3d38:	40 13 00             	rex adc eax,DWORD PTR [rax]
    3d3b:	00 00                	add    BYTE PTR [rax],al
    3d3d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000003d40 <__do_global_dtors_aux_fini_array_entry>:
    3d40:	00 13                	add    BYTE PTR [rbx],dl
    3d42:	00 00                	add    BYTE PTR [rax],al
    3d44:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000003d48 <_DYNAMIC>:
    3d48:	01 00                	add    DWORD PTR [rax],eax
    3d4a:	00 00                	add    BYTE PTR [rax],al
    3d4c:	00 00                	add    BYTE PTR [rax],al
    3d4e:	00 00                	add    BYTE PTR [rax],al
    3d50:	01 00                	add    DWORD PTR [rax],eax
    3d52:	00 00                	add    BYTE PTR [rax],al
    3d54:	00 00                	add    BYTE PTR [rax],al
    3d56:	00 00                	add    BYTE PTR [rax],al
    3d58:	0c 00                	or     al,0x0
    3d5a:	00 00                	add    BYTE PTR [rax],al
    3d5c:	00 00                	add    BYTE PTR [rax],al
    3d5e:	00 00                	add    BYTE PTR [rax],al
    3d60:	00 10                	add    BYTE PTR [rax],dl
    3d62:	00 00                	add    BYTE PTR [rax],al
    3d64:	00 00                	add    BYTE PTR [rax],al
    3d66:	00 00                	add    BYTE PTR [rax],al
    3d68:	0d 00 00 00 00       	or     eax,0x0
    3d6d:	00 00                	add    BYTE PTR [rax],al
    3d6f:	00 78 19             	add    BYTE PTR [rax+0x19],bh
    3d72:	00 00                	add    BYTE PTR [rax],al
    3d74:	00 00                	add    BYTE PTR [rax],al
    3d76:	00 00                	add    BYTE PTR [rax],al
    3d78:	19 00                	sbb    DWORD PTR [rax],eax
    3d7a:	00 00                	add    BYTE PTR [rax],al
    3d7c:	00 00                	add    BYTE PTR [rax],al
    3d7e:	00 00                	add    BYTE PTR [rax],al
    3d80:	38 3d 00 00 00 00    	cmp    BYTE PTR [rip+0x0],bh        # 3d86 <_DYNAMIC+0x3e>
    3d86:	00 00                	add    BYTE PTR [rax],al
    3d88:	1b 00                	sbb    eax,DWORD PTR [rax]
    3d8a:	00 00                	add    BYTE PTR [rax],al
    3d8c:	00 00                	add    BYTE PTR [rax],al
    3d8e:	00 00                	add    BYTE PTR [rax],al
    3d90:	08 00                	or     BYTE PTR [rax],al
    3d92:	00 00                	add    BYTE PTR [rax],al
    3d94:	00 00                	add    BYTE PTR [rax],al
    3d96:	00 00                	add    BYTE PTR [rax],al
    3d98:	1a 00                	sbb    al,BYTE PTR [rax]
    3d9a:	00 00                	add    BYTE PTR [rax],al
    3d9c:	00 00                	add    BYTE PTR [rax],al
    3d9e:	00 00                	add    BYTE PTR [rax],al
    3da0:	40 3d 00 00 00 00    	rex cmp eax,0x0
    3da6:	00 00                	add    BYTE PTR [rax],al
    3da8:	1c 00                	sbb    al,0x0
    3daa:	00 00                	add    BYTE PTR [rax],al
    3dac:	00 00                	add    BYTE PTR [rax],al
    3dae:	00 00                	add    BYTE PTR [rax],al
    3db0:	08 00                	or     BYTE PTR [rax],al
    3db2:	00 00                	add    BYTE PTR [rax],al
    3db4:	00 00                	add    BYTE PTR [rax],al
    3db6:	00 00                	add    BYTE PTR [rax],al
    3db8:	f5                   	cmc
    3db9:	fe                   	(bad)
    3dba:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3dbd:	00 00                	add    BYTE PTR [rax],al
    3dbf:	00 a0 03 00 00 00    	add    BYTE PTR [rax+0x3],ah
    3dc5:	00 00                	add    BYTE PTR [rax],al
    3dc7:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 3dcd <_DYNAMIC+0x85>
    3dcd:	00 00                	add    BYTE PTR [rax],al
    3dcf:	00 48 06             	add    BYTE PTR [rax+0x6],cl
    3dd2:	00 00                	add    BYTE PTR [rax],al
    3dd4:	00 00                	add    BYTE PTR [rax],al
    3dd6:	00 00                	add    BYTE PTR [rax],al
    3dd8:	06                   	(bad)
    3dd9:	00 00                	add    BYTE PTR [rax],al
    3ddb:	00 00                	add    BYTE PTR [rax],al
    3ddd:	00 00                	add    BYTE PTR [rax],al
    3ddf:	00 d8                	add    al,bl
    3de1:	03 00                	add    eax,DWORD PTR [rax]
    3de3:	00 00                	add    BYTE PTR [rax],al
    3de5:	00 00                	add    BYTE PTR [rax],al
    3de7:	00 0a                	add    BYTE PTR [rdx],cl
    3de9:	00 00                	add    BYTE PTR [rax],al
    3deb:	00 00                	add    BYTE PTR [rax],al
    3ded:	00 00                	add    BYTE PTR [rax],al
    3def:	00 27                	add    BYTE PTR [rdi],ah
    3df1:	01 00                	add    DWORD PTR [rax],eax
    3df3:	00 00                	add    BYTE PTR [rax],al
    3df5:	00 00                	add    BYTE PTR [rax],al
    3df7:	00 0b                	add    BYTE PTR [rbx],cl
    3df9:	00 00                	add    BYTE PTR [rax],al
    3dfb:	00 00                	add    BYTE PTR [rax],al
    3dfd:	00 00                	add    BYTE PTR [rax],al
    3dff:	00 18                	add    BYTE PTR [rax],bl
    3e01:	00 00                	add    BYTE PTR [rax],al
    3e03:	00 00                	add    BYTE PTR [rax],al
    3e05:	00 00                	add    BYTE PTR [rax],al
    3e07:	00 15 00 00 00 00    	add    BYTE PTR [rip+0x0],dl        # 3e0d <_DYNAMIC+0xc5>
	...
    3e15:	00 00                	add    BYTE PTR [rax],al
    3e17:	00 03                	add    BYTE PTR [rbx],al
    3e19:	00 00                	add    BYTE PTR [rax],al
    3e1b:	00 00                	add    BYTE PTR [rax],al
    3e1d:	00 00                	add    BYTE PTR [rax],al
    3e1f:	00 38                	add    BYTE PTR [rax],bh
    3e21:	3f                   	(bad)
    3e22:	00 00                	add    BYTE PTR [rax],al
    3e24:	00 00                	add    BYTE PTR [rax],al
    3e26:	00 00                	add    BYTE PTR [rax],al
    3e28:	02 00                	add    al,BYTE PTR [rax]
    3e2a:	00 00                	add    BYTE PTR [rax],al
    3e2c:	00 00                	add    BYTE PTR [rax],al
    3e2e:	00 00                	add    BYTE PTR [rax],al
    3e30:	98                   	cwde
    3e31:	01 00                	add    DWORD PTR [rax],eax
    3e33:	00 00                	add    BYTE PTR [rax],al
    3e35:	00 00                	add    BYTE PTR [rax],al
    3e37:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    3e3a:	00 00                	add    BYTE PTR [rax],al
    3e3c:	00 00                	add    BYTE PTR [rax],al
    3e3e:	00 00                	add    BYTE PTR [rax],al
    3e40:	07                   	(bad)
    3e41:	00 00                	add    BYTE PTR [rax],al
    3e43:	00 00                	add    BYTE PTR [rax],al
    3e45:	00 00                	add    BYTE PTR [rax],al
    3e47:	00 17                	add    BYTE PTR [rdi],dl
    3e49:	00 00                	add    BYTE PTR [rax],al
    3e4b:	00 00                	add    BYTE PTR [rax],al
    3e4d:	00 00                	add    BYTE PTR [rax],al
    3e4f:	00 f0                	add    al,dh
    3e51:	08 00                	or     BYTE PTR [rax],al
    3e53:	00 00                	add    BYTE PTR [rax],al
    3e55:	00 00                	add    BYTE PTR [rax],al
    3e57:	00 07                	add    BYTE PTR [rdi],al
    3e59:	00 00                	add    BYTE PTR [rax],al
    3e5b:	00 00                	add    BYTE PTR [rax],al
    3e5d:	00 00                	add    BYTE PTR [rax],al
    3e5f:	00 e8                	add    al,ch
    3e61:	07                   	(bad)
    3e62:	00 00                	add    BYTE PTR [rax],al
    3e64:	00 00                	add    BYTE PTR [rax],al
    3e66:	00 00                	add    BYTE PTR [rax],al
    3e68:	08 00                	or     BYTE PTR [rax],al
    3e6a:	00 00                	add    BYTE PTR [rax],al
    3e6c:	00 00                	add    BYTE PTR [rax],al
    3e6e:	00 00                	add    BYTE PTR [rax],al
    3e70:	08 01                	or     BYTE PTR [rcx],al
    3e72:	00 00                	add    BYTE PTR [rax],al
    3e74:	00 00                	add    BYTE PTR [rax],al
    3e76:	00 00                	add    BYTE PTR [rax],al
    3e78:	09 00                	or     DWORD PTR [rax],eax
    3e7a:	00 00                	add    BYTE PTR [rax],al
    3e7c:	00 00                	add    BYTE PTR [rax],al
    3e7e:	00 00                	add    BYTE PTR [rax],al
    3e80:	18 00                	sbb    BYTE PTR [rax],al
    3e82:	00 00                	add    BYTE PTR [rax],al
    3e84:	00 00                	add    BYTE PTR [rax],al
    3e86:	00 00                	add    BYTE PTR [rax],al
    3e88:	1e                   	(bad)
    3e89:	00 00                	add    BYTE PTR [rax],al
    3e8b:	00 00                	add    BYTE PTR [rax],al
    3e8d:	00 00                	add    BYTE PTR [rax],al
    3e8f:	00 08                	add    BYTE PTR [rax],cl
    3e91:	00 00                	add    BYTE PTR [rax],al
    3e93:	00 00                	add    BYTE PTR [rax],al
    3e95:	00 00                	add    BYTE PTR [rax],al
    3e97:	00 fb                	add    bl,bh
    3e99:	ff                   	(bad)
    3e9a:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3e9d:	00 00                	add    BYTE PTR [rax],al
    3e9f:	00 01                	add    BYTE PTR [rcx],al
    3ea1:	00 00                	add    BYTE PTR [rax],al
    3ea3:	08 00                	or     BYTE PTR [rax],al
    3ea5:	00 00                	add    BYTE PTR [rax],al
    3ea7:	00 fe                	add    dh,bh
    3ea9:	ff                   	(bad)
    3eaa:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3ead:	00 00                	add    BYTE PTR [rax],al
    3eaf:	00 a8 07 00 00 00    	add    BYTE PTR [rax+0x7],ch
    3eb5:	00 00                	add    BYTE PTR [rax],al
    3eb7:	00 ff                	add    bh,bh
    3eb9:	ff                   	(bad)
    3eba:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3ebd:	00 00                	add    BYTE PTR [rax],al
    3ebf:	00 01                	add    BYTE PTR [rcx],al
    3ec1:	00 00                	add    BYTE PTR [rax],al
    3ec3:	00 00                	add    BYTE PTR [rax],al
    3ec5:	00 00                	add    BYTE PTR [rax],al
    3ec7:	00 f0                	add    al,dh
    3ec9:	ff                   	(bad)
    3eca:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3ecd:	00 00                	add    BYTE PTR [rax],al
    3ecf:	00 70 07             	add    BYTE PTR [rax+0x7],dh
    3ed2:	00 00                	add    BYTE PTR [rax],al
    3ed4:	00 00                	add    BYTE PTR [rax],al
    3ed6:	00 00                	add    BYTE PTR [rax],al
    3ed8:	f9                   	stc
    3ed9:	ff                   	(bad)
    3eda:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3edd:	00 00                	add    BYTE PTR [rax],al
    3edf:	00 03                	add    BYTE PTR [rbx],al
	...

Disassembly of section .got:

0000000000003f38 <_GLOBAL_OFFSET_TABLE_>:
    3f38:	48 3d 00 00 00 00    	cmp    rax,0x0
	...
    3f4e:	00 00                	add    BYTE PTR [rax],al
    3f50:	30 10                	xor    BYTE PTR [rax],dl
    3f52:	00 00                	add    BYTE PTR [rax],al
    3f54:	00 00                	add    BYTE PTR [rax],al
    3f56:	00 00                	add    BYTE PTR [rax],al
    3f58:	40 10 00             	rex adc BYTE PTR [rax],al
    3f5b:	00 00                	add    BYTE PTR [rax],al
    3f5d:	00 00                	add    BYTE PTR [rax],al
    3f5f:	00 50 10             	add    BYTE PTR [rax+0x10],dl
    3f62:	00 00                	add    BYTE PTR [rax],al
    3f64:	00 00                	add    BYTE PTR [rax],al
    3f66:	00 00                	add    BYTE PTR [rax],al
    3f68:	60                   	(bad)
    3f69:	10 00                	adc    BYTE PTR [rax],al
    3f6b:	00 00                	add    BYTE PTR [rax],al
    3f6d:	00 00                	add    BYTE PTR [rax],al
    3f6f:	00 70 10             	add    BYTE PTR [rax+0x10],dh
    3f72:	00 00                	add    BYTE PTR [rax],al
    3f74:	00 00                	add    BYTE PTR [rax],al
    3f76:	00 00                	add    BYTE PTR [rax],al
    3f78:	80 10 00             	adc    BYTE PTR [rax],0x0
    3f7b:	00 00                	add    BYTE PTR [rax],al
    3f7d:	00 00                	add    BYTE PTR [rax],al
    3f7f:	00 90 10 00 00 00    	add    BYTE PTR [rax+0x10],dl
    3f85:	00 00                	add    BYTE PTR [rax],al
    3f87:	00 a0 10 00 00 00    	add    BYTE PTR [rax+0x10],ah
    3f8d:	00 00                	add    BYTE PTR [rax],al
    3f8f:	00 b0 10 00 00 00    	add    BYTE PTR [rax+0x10],dh
    3f95:	00 00                	add    BYTE PTR [rax],al
    3f97:	00 c0                	add    al,al
    3f99:	10 00                	adc    BYTE PTR [rax],al
    3f9b:	00 00                	add    BYTE PTR [rax],al
    3f9d:	00 00                	add    BYTE PTR [rax],al
    3f9f:	00 d0                	add    al,dl
    3fa1:	10 00                	adc    BYTE PTR [rax],al
    3fa3:	00 00                	add    BYTE PTR [rax],al
    3fa5:	00 00                	add    BYTE PTR [rax],al
    3fa7:	00 e0                	add    al,ah
    3fa9:	10 00                	adc    BYTE PTR [rax],al
    3fab:	00 00                	add    BYTE PTR [rax],al
    3fad:	00 00                	add    BYTE PTR [rax],al
    3faf:	00 f0                	add    al,dh
    3fb1:	10 00                	adc    BYTE PTR [rax],al
    3fb3:	00 00                	add    BYTE PTR [rax],al
    3fb5:	00 00                	add    BYTE PTR [rax],al
    3fb7:	00 00                	add    BYTE PTR [rax],al
    3fb9:	11 00                	adc    DWORD PTR [rax],eax
    3fbb:	00 00                	add    BYTE PTR [rax],al
    3fbd:	00 00                	add    BYTE PTR [rax],al
    3fbf:	00 10                	add    BYTE PTR [rax],dl
    3fc1:	11 00                	adc    DWORD PTR [rax],eax
    3fc3:	00 00                	add    BYTE PTR [rax],al
    3fc5:	00 00                	add    BYTE PTR [rax],al
    3fc7:	00 20                	add    BYTE PTR [rax],ah
    3fc9:	11 00                	adc    DWORD PTR [rax],eax
    3fcb:	00 00                	add    BYTE PTR [rax],al
    3fcd:	00 00                	add    BYTE PTR [rax],al
    3fcf:	00 30                	add    BYTE PTR [rax],dh
    3fd1:	11 00                	adc    DWORD PTR [rax],eax
	...

Disassembly of section .data:

0000000000004000 <__data_start>:
	...

0000000000004008 <__dso_handle>:
    4008:	08 40 00             	or     BYTE PTR [rax+0x0],al
    400b:	00 00                	add    BYTE PTR [rax],al
    400d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .bss:

0000000000004020 <stdout@@GLIBC_2.2.5>:
	...

0000000000004030 <stdin@@GLIBC_2.2.5>:
	...

0000000000004040 <stderr@@GLIBC_2.2.5>:
	...

0000000000004048 <completed.8061>:
	...

0000000000004050 <choice>:
	...

0000000000004058 <student>:
	...

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	47                   	rex.RXB
   1:	43                   	rex.XB
   2:	43 3a 20             	rex.XB cmp spl,BYTE PTR [r8]
   5:	28 55 62             	sub    BYTE PTR [rbp+0x62],dl
   8:	75 6e                	jne    78 <_init-0xf88>
   a:	74 75                	je     81 <_init-0xf7f>
   c:	20 39                	and    BYTE PTR [rcx],bh
   e:	2e 34 2e             	cs xor al,0x2e
  11:	30 2d 31 75 62 75    	xor    BYTE PTR [rip+0x75627531],ch        # 75627548 <_end+0x756234e8>
  17:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  18:	74 75                	je     8f <_init-0xf71>
  1a:	31 7e 32             	xor    DWORD PTR [rsi+0x32],edi
  1d:	30 2e                	xor    BYTE PTR [rsi],ch
  1f:	30 34 2e             	xor    BYTE PTR [rsi+rbp*1],dh
  22:	31 29                	xor    DWORD PTR [rcx],ebp
  24:	20 39                	and    BYTE PTR [rcx],bh
  26:	2e 34 2e             	cs xor al,0x2e
  29:	30 00                	xor    BYTE PTR [rax],al

Disassembly of section .debug_aranges:

0000000000000000 <.debug_aranges>:
   0:	2c 00                	sub    al,0x0
   2:	00 00                	add    BYTE PTR [rax],al
   4:	02 00                	add    al,BYTE PTR [rax]
   6:	00 00                	add    BYTE PTR [rax],al
   8:	00 00                	add    BYTE PTR [rax],al
   a:	08 00                	or     BYTE PTR [rax],al
   c:	00 00                	add    BYTE PTR [rax],al
   e:	00 00                	add    BYTE PTR [rax],al
  10:	49 13 00             	adc    rax,QWORD PTR [r8]
  13:	00 00                	add    BYTE PTR [rax],al
  15:	00 00                	add    BYTE PTR [rax],al
  17:	00 b2 05 00 00 00    	add    BYTE PTR [rdx+0x5],dh
	...

Disassembly of section .debug_info:

0000000000000000 <.debug_info>:
   0:	c1 05 00 00 04 00 00 	rol    DWORD PTR [rip+0x40000],0x0        # 40007 <_end+0x3bfa7>
   7:	00 00                	add    BYTE PTR [rax],al
   9:	00 08                	add    BYTE PTR [rax],cl
   b:	01 82 02 00 00 0c    	add    DWORD PTR [rdx+0xc000002],eax
  11:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  12:	01 00                	add    DWORD PTR [rax],eax
  14:	00 a8 00 00 00 49    	add    BYTE PTR [rax+0x49000000],ch
  1a:	13 00                	adc    eax,DWORD PTR [rax]
  1c:	00 00                	add    BYTE PTR [rax],al
  1e:	00 00                	add    BYTE PTR [rax],al
  20:	00 b2 05 00 00 00    	add    BYTE PTR [rdx+0x5],dh
  26:	00 00                	add    BYTE PTR [rax],al
  28:	00 00                	add    BYTE PTR [rax],al
  2a:	00 00                	add    BYTE PTR [rax],al
  2c:	00 02                	add    BYTE PTR [rdx],al
  2e:	01 08                	add    DWORD PTR [rax],ecx
  30:	10 01                	adc    BYTE PTR [rcx],al
  32:	00 00                	add    BYTE PTR [rax],al
  34:	02 02                	add    al,BYTE PTR [rdx]
  36:	07                   	(bad)
  37:	c4 01 00 00          	(bad)
  3b:	02 04 07             	add    al,BYTE PTR [rdi+rax*1]
  3e:	31 01                	xor    DWORD PTR [rcx],eax
  40:	00 00                	add    BYTE PTR [rax],al
  42:	02 08                	add    cl,BYTE PTR [rax]
  44:	07                   	(bad)
  45:	2c 01                	sub    al,0x1
  47:	00 00                	add    BYTE PTR [rax],al
  49:	02 01                	add    al,BYTE PTR [rcx]
  4b:	06                   	(bad)
  4c:	12 01                	adc    al,BYTE PTR [rcx]
  4e:	00 00                	add    BYTE PTR [rax],al
  50:	02 02                	add    al,BYTE PTR [rdx]
  52:	05 3a 00 00 00       	add    eax,0x3a
  57:	03 04 05 69 6e 74 00 	add    eax,DWORD PTR [rax*1+0x746e69]
  5e:	02 08                	add    cl,BYTE PTR [rax]
  60:	05 d7 00 00 00       	add    eax,0xd7
  65:	04 25                	add    al,0x25
  67:	02 00                	add    al,BYTE PTR [rax]
  69:	00 02                	add    BYTE PTR [rdx],al
  6b:	98                   	cwde
  6c:	19 5e 00             	sbb    DWORD PTR [rsi+0x0],ebx
  6f:	00 00                	add    BYTE PTR [rax],al
  71:	04 ef                	add    al,0xef
  73:	01 00                	add    DWORD PTR [rax],eax
  75:	00 02                	add    BYTE PTR [rdx],al
  77:	99                   	cdq
  78:	1b 5e 00             	sbb    ebx,DWORD PTR [rsi+0x0]
  7b:	00 00                	add    BYTE PTR [rax],al
  7d:	05 08 06 08 85       	add    eax,0x85080608
  82:	00 00                	add    BYTE PTR [rax],al
  84:	00 02                	add    BYTE PTR [rdx],al
  86:	01 06                	add    DWORD PTR [rsi],eax
  88:	19 01                	sbb    DWORD PTR [rcx],eax
  8a:	00 00                	add    BYTE PTR [rax],al
  8c:	07                   	(bad)
  8d:	85 00                	test   DWORD PTR [rax],eax
  8f:	00 00                	add    BYTE PTR [rax],al
  91:	06                   	(bad)
  92:	08 8c 00 00 00 07 91 	or     BYTE PTR [rax+rax*1-0x6ef90000],cl
  99:	00 00                	add    BYTE PTR [rax],al
  9b:	00 04 9e             	add    BYTE PTR [rsi+rbx*4],al
  9e:	00 00                	add    BYTE PTR [rax],al
  a0:	00 03                	add    BYTE PTR [rbx],al
  a2:	5a                   	pop    rdx
  a3:	1b 42 00             	sbb    eax,DWORD PTR [rdx+0x0]
  a6:	00 00                	add    BYTE PTR [rax],al
  a8:	04 44                	add    al,0x44
  aa:	00 00                	add    BYTE PTR [rax],al
  ac:	00 04 d1             	add    BYTE PTR [rcx+rdx*8],al
  af:	17                   	(bad)
  b0:	42 00 00             	rex.X add BYTE PTR [rax],al
  b3:	00 08                	add    BYTE PTR [rax],cl
  b5:	14 00                	adc    al,0x0
  b7:	00 00                	add    BYTE PTR [rax],al
  b9:	d8 05 31 08 3b 02    	fadd   DWORD PTR [rip+0x23b0831]        # 23b08f0 <_end+0x23ac890>
  bf:	00 00                	add    BYTE PTR [rax],al
  c1:	09 5e 00             	or     DWORD PTR [rsi+0x0],ebx
  c4:	00 00                	add    BYTE PTR [rax],al
  c6:	05 33 07 57 00       	add    eax,0x570733
  cb:	00 00                	add    BYTE PTR [rax],al
  cd:	00 09                	add    BYTE PTR [rcx],cl
  cf:	49 03 00             	add    rax,QWORD PTR [r8]
  d2:	00 05 36 09 7f 00    	add    BYTE PTR [rip+0x7f0936],al        # 7f0a0e <_end+0x7ec9ae>
  d8:	00 00                	add    BYTE PTR [rax],al
  da:	08 09                	or     BYTE PTR [rcx],cl
  dc:	53                   	push   rbx
  dd:	01 00                	add    DWORD PTR [rax],eax
  df:	00 05 37 09 7f 00    	add    BYTE PTR [rip+0x7f0937],al        # 7f0a1c <_end+0x7ec9bc>
  e5:	00 00                	add    BYTE PTR [rax],al
  e7:	10 09                	adc    BYTE PTR [rcx],cl
  e9:	57                   	push   rdi
  ea:	02 00                	add    al,BYTE PTR [rax]
  ec:	00 05 38 09 7f 00    	add    BYTE PTR [rip+0x7f0938],al        # 7f0a2a <_end+0x7ec9ca>
  f2:	00 00                	add    BYTE PTR [rax],al
  f4:	18 09                	sbb    BYTE PTR [rcx],cl
  f6:	60                   	(bad)
  f7:	01 00                	add    DWORD PTR [rax],eax
  f9:	00 05 39 09 7f 00    	add    BYTE PTR [rip+0x7f0939],al        # 7f0a38 <_end+0x7ec9d8>
  ff:	00 00                	add    BYTE PTR [rax],al
 101:	20 09                	and    BYTE PTR [rcx],cl
 103:	50                   	push   rax
 104:	00 00                	add    BYTE PTR [rax],al
 106:	00 05 3a 09 7f 00    	add    BYTE PTR [rip+0x7f093a],al        # 7f0a46 <_end+0x7ec9e6>
 10c:	00 00                	add    BYTE PTR [rax],al
 10e:	28 09                	sub    BYTE PTR [rcx],cl
 110:	e1 01                	loope  113 <_init-0xeed>
 112:	00 00                	add    BYTE PTR [rax],al
 114:	05 3b 09 7f 00       	add    eax,0x7f093b
 119:	00 00                	add    BYTE PTR [rax],al
 11b:	30 09                	xor    BYTE PTR [rcx],cl
 11d:	65 00 00             	add    BYTE PTR gs:[rax],al
 120:	00 05 3c 09 7f 00    	add    BYTE PTR [rip+0x7f093c],al        # 7f0a62 <_end+0x7eca02>
 126:	00 00                	add    BYTE PTR [rax],al
 128:	38 09                	cmp    BYTE PTR [rcx],cl
 12a:	8b 01                	mov    eax,DWORD PTR [rcx]
 12c:	00 00                	add    BYTE PTR [rax],al
 12e:	05 3d 09 7f 00       	add    eax,0x7f093d
 133:	00 00                	add    BYTE PTR [rax],al
 135:	40 09 20             	rex or DWORD PTR [rax],esp
 138:	03 00                	add    eax,DWORD PTR [rax]
 13a:	00 05 40 09 7f 00    	add    BYTE PTR [rip+0x7f0940],al        # 7f0a80 <_end+0x7eca20>
 140:	00 00                	add    BYTE PTR [rax],al
 142:	48 09 2d 02 00 00 05 	or     QWORD PTR [rip+0x5000002],rbp        # 500014b <_end+0x4ffc0eb>
 149:	41 09 7f 00          	or     DWORD PTR [r15+0x0],edi
 14d:	00 00                	add    BYTE PTR [rax],al
 14f:	50                   	push   rax
 150:	09 2d 00 00 00 05    	or     DWORD PTR [rip+0x5000000],ebp        # 5000156 <_end+0x4ffc0f6>
 156:	42 09 7f 00          	rex.X or DWORD PTR [rdi+0x0],edi
 15a:	00 00                	add    BYTE PTR [rax],al
 15c:	58                   	pop    rax
 15d:	09 82 00 00 00 05    	or     DWORD PTR [rdx+0x5000000],eax
 163:	44 16                	rex.R (bad)
 165:	54                   	push   rsp
 166:	02 00                	add    al,BYTE PTR [rax]
 168:	00 60 09             	add    BYTE PTR [rax+0x9],ah
 16b:	02 02                	add    al,BYTE PTR [rdx]
 16d:	00 00                	add    BYTE PTR [rax],al
 16f:	05 46 14 5a 02       	add    eax,0x25a1446
 174:	00 00                	add    BYTE PTR [rax],al
 176:	68 09 41 03 00       	push   0x34109
 17b:	00 05 48 07 57 00    	add    BYTE PTR [rip+0x570748],al        # 5708c9 <_end+0x56c869>
 181:	00 00                	add    BYTE PTR [rax],al
 183:	70 09                	jo     18e <_init-0xe72>
 185:	43 02 00             	rex.XB add al,BYTE PTR [r8]
 188:	00 05 49 07 57 00    	add    BYTE PTR [rip+0x570749],al        # 5708d7 <_end+0x56c877>
 18e:	00 00                	add    BYTE PTR [rax],al
 190:	74 09                	je     19b <_init-0xe65>
 192:	04 01                	add    al,0x1
 194:	00 00                	add    BYTE PTR [rax],al
 196:	05 4a 0b 65 00       	add    eax,0x650b4a
 19b:	00 00                	add    BYTE PTR [rax],al
 19d:	78 09                	js     1a8 <_init-0xe58>
 19f:	b9 00 00 00 05       	mov    ecx,0x5000000
 1a4:	4d 12 34 00          	rex.WRB adc r14b,BYTE PTR [r8+rax*1]
 1a8:	00 00                	add    BYTE PTR [rax],al
 1aa:	80 09 65             	or     BYTE PTR [rcx],0x65
 1ad:	02 00                	add    al,BYTE PTR [rax]
 1af:	00 05 4e 0f 49 00    	add    BYTE PTR [rip+0x490f4e],al        # 491103 <_end+0x48d0a3>
 1b5:	00 00                	add    BYTE PTR [rax],al
 1b7:	82                   	(bad)
 1b8:	09 49 01             	or     DWORD PTR [rcx+0x1],ecx
 1bb:	00 00                	add    BYTE PTR [rax],al
 1bd:	05 4f 08 60 02       	add    eax,0x260084f
 1c2:	00 00                	add    BYTE PTR [rax],al
 1c4:	83 09 72             	or     DWORD PTR [rcx],0x72
 1c7:	00 00                	add    BYTE PTR [rax],al
 1c9:	00 05 51 0f 70 02    	add    BYTE PTR [rip+0x2700f51],al        # 2701120 <_end+0x26fd0c0>
 1cf:	00 00                	add    BYTE PTR [rax],al
 1d1:	88 09                	mov    BYTE PTR [rcx],cl
 1d3:	08 01                	or     BYTE PTR [rcx],al
 1d5:	00 00                	add    BYTE PTR [rax],al
 1d7:	05 59 0d 71 00       	add    eax,0x710d59
 1dc:	00 00                	add    BYTE PTR [rax],al
 1de:	90                   	nop
 1df:	09 4e 02             	or     DWORD PTR [rsi+0x2],ecx
 1e2:	00 00                	add    BYTE PTR [rax],al
 1e4:	05 5b 17 7b 02       	add    eax,0x27b175b
 1e9:	00 00                	add    BYTE PTR [rax],al
 1eb:	98                   	cwde
 1ec:	09 77 02             	or     DWORD PTR [rdi+0x2],esi
 1ef:	00 00                	add    BYTE PTR [rax],al
 1f1:	05 5c 19 86 02       	add    eax,0x286195c
 1f6:	00 00                	add    BYTE PTR [rax],al
 1f8:	a0 09 9e 01 00 00 05 	movabs al,ds:0x145d050000019e09
 1ff:	5d 14 
 201:	5a                   	pop    rdx
 202:	02 00                	add    al,BYTE PTR [rax]
 204:	00 a8 09 91 00 00    	add    BYTE PTR [rax+0x9109],ch
 20a:	00 05 5e 09 7d 00    	add    BYTE PTR [rip+0x7d095e],al        # 7d0b6e <_end+0x7ccb0e>
 210:	00 00                	add    BYTE PTR [rax],al
 212:	b0 09                	mov    al,0x9
 214:	84 01                	test   BYTE PTR [rcx],al
 216:	00 00                	add    BYTE PTR [rax],al
 218:	05 5f 0a a8 00       	add    eax,0xa80a5f
 21d:	00 00                	add    BYTE PTR [rax],al
 21f:	b8 09 1f 02 00       	mov    eax,0x21f09
 224:	00 05 60 07 57 00    	add    BYTE PTR [rip+0x570760],al        # 57098a <_end+0x56c92a>
 22a:	00 00                	add    BYTE PTR [rax],al
 22c:	c0 09 ac             	ror    BYTE PTR [rcx],0xac
 22f:	01 00                	add    DWORD PTR [rax],eax
 231:	00 05 62 08 8c 02    	add    BYTE PTR [rip+0x28c0862],al        # 28c0a99 <_end+0x28bca39>
 237:	00 00                	add    BYTE PTR [rax],al
 239:	c4                   	(bad)
 23a:	00 04 18             	add    BYTE PTR [rax+rbx*1],al
 23d:	00 00                	add    BYTE PTR [rax],al
 23f:	00 06                	add    BYTE PTR [rsi],al
 241:	07                   	(bad)
 242:	19 b4 00 00 00 0a 5d 	sbb    DWORD PTR [rax+rax*1+0x5d0a0000],esi
 249:	03 00                	add    eax,DWORD PTR [rax]
 24b:	00 05 2b 0e 0b 3e    	add    BYTE PTR [rip+0x3e0b0e2b],al        # 3e0b107c <_end+0x3e0ad01c>
 251:	01 00                	add    DWORD PTR [rax],eax
 253:	00 06                	add    BYTE PTR [rsi],al
 255:	08 4f 02             	or     BYTE PTR [rdi+0x2],cl
 258:	00 00                	add    BYTE PTR [rax],al
 25a:	06                   	(bad)
 25b:	08 b4 00 00 00 0c 85 	or     BYTE PTR [rax+rax*1-0x7af40000],dh
 262:	00 00                	add    BYTE PTR [rax],al
 264:	00 70 02             	add    BYTE PTR [rax+0x2],dh
 267:	00 00                	add    BYTE PTR [rax],al
 269:	0d 42 00 00 00       	or     eax,0x42
 26e:	00 00                	add    BYTE PTR [rax],al
 270:	06                   	(bad)
 271:	08 47 02             	or     BYTE PTR [rdi+0x2],al
 274:	00 00                	add    BYTE PTR [rax],al
 276:	0b 4b 02             	or     ecx,DWORD PTR [rbx+0x2]
 279:	00 00                	add    BYTE PTR [rax],al
 27b:	06                   	(bad)
 27c:	08 76 02             	or     BYTE PTR [rsi+0x2],dh
 27f:	00 00                	add    BYTE PTR [rax],al
 281:	0b 74 02 00          	or     esi,DWORD PTR [rdx+rax*1+0x0]
 285:	00 06                	add    BYTE PTR [rsi],al
 287:	08 81 02 00 00 0c    	or     BYTE PTR [rcx+0xc000002],al
 28d:	85 00                	test   DWORD PTR [rax],eax
 28f:	00 00                	add    BYTE PTR [rax],al
 291:	9c                   	pushf
 292:	02 00                	add    al,BYTE PTR [rax]
 294:	00 0d 42 00 00 00    	add    BYTE PTR [rip+0x42],cl        # 2dc <_init-0xd24>
 29a:	13 00                	adc    eax,DWORD PTR [rax]
 29c:	0e                   	(bad)
 29d:	3d 02 00 00 07       	cmp    eax,0x7000002
 2a2:	89 0e                	mov    DWORD PTR [rsi],ecx
 2a4:	a8 02                	test   al,0x2
 2a6:	00 00                	add    BYTE PTR [rax],al
 2a8:	06                   	(bad)
 2a9:	08 3b                	or     BYTE PTR [rbx],bh
 2ab:	02 00                	add    al,BYTE PTR [rax]
 2ad:	00 0e                	add    BYTE PTR [rsi],cl
 2af:	56                   	push   rsi
 2b0:	03 00                	add    eax,DWORD PTR [rax]
 2b2:	00 07                	add    BYTE PTR [rdi],al
 2b4:	8a 0e                	mov    cl,BYTE PTR [rsi]
 2b6:	a8 02                	test   al,0x2
 2b8:	00 00                	add    BYTE PTR [rax],al
 2ba:	0e                   	(bad)
 2bb:	cb                   	retf
 2bc:	00 00                	add    BYTE PTR [rax],al
 2be:	00 07                	add    BYTE PTR [rdi],al
 2c0:	8b 0e                	mov    ecx,DWORD PTR [rsi]
 2c2:	a8 02                	test   al,0x2
 2c4:	00 00                	add    BYTE PTR [rax],al
 2c6:	0e                   	(bad)
 2c7:	24 00                	and    al,0x0
 2c9:	00 00                	add    BYTE PTR [rax],al
 2cb:	08 1a                	or     BYTE PTR [rdx],bl
 2cd:	0c 57                	or     al,0x57
 2cf:	00 00                	add    BYTE PTR [rax],al
 2d1:	00 0c 97             	add    BYTE PTR [rdi+rdx*4],cl
 2d4:	00 00                	add    BYTE PTR [rax],al
 2d6:	00 dd                	add    ch,bl
 2d8:	02 00                	add    al,BYTE PTR [rax]
 2da:	00 0f                	add    BYTE PTR [rdi],cl
 2dc:	00 07                	add    BYTE PTR [rdi],al
 2de:	d2 02                	rol    BYTE PTR [rdx],cl
 2e0:	00 00                	add    BYTE PTR [rax],al
 2e2:	0e                   	(bad)
 2e3:	2e 03 00             	cs add eax,DWORD PTR [rax]
 2e6:	00 08                	add    BYTE PTR [rax],cl
 2e8:	1b 1a                	sbb    ebx,DWORD PTR [rdx]
 2ea:	dd 02                	fld    QWORD PTR [rdx]
 2ec:	00 00                	add    BYTE PTR [rax],al
 2ee:	02 08                	add    cl,BYTE PTR [rax]
 2f0:	05 d2 00 00 00       	add    eax,0xd2
 2f5:	02 08                	add    cl,BYTE PTR [rax]
 2f7:	07                   	(bad)
 2f8:	27                   	(bad)
 2f9:	01 00                	add    DWORD PTR [rax],eax
 2fb:	00 10                	add    BYTE PTR [rax],dl
 2fd:	78 00                	js     2ff <_init-0xd01>
 2ff:	00 00                	add    BYTE PTR [rax],al
 301:	09 1f                	or     DWORD PTR [rdi],ebx
 303:	02 0f                	add    cl,BYTE PTR [rdi]
 305:	09 03                	or     DWORD PTR [rbx],eax
 307:	00 00                	add    BYTE PTR [rax],al
 309:	06                   	(bad)
 30a:	08 7f 00             	or     BYTE PTR [rdi+0x0],bh
 30d:	00 00                	add    BYTE PTR [rax],al
 30f:	0e                   	(bad)
 310:	00 00                	add    BYTE PTR [rax],al
 312:	00 00                	add    BYTE PTR [rax],al
 314:	0a 24 0e             	or     ah,BYTE PTR [rsi+rcx*1]
 317:	7f 00                	jg     319 <_init-0xce7>
 319:	00 00                	add    BYTE PTR [rax],al
 31b:	0e                   	(bad)
 31c:	97                   	xchg   edi,eax
 31d:	01 00                	add    DWORD PTR [rax],eax
 31f:	00 0a                	add    BYTE PTR [rdx],cl
 321:	32 0c 57             	xor    cl,BYTE PTR [rdi+rdx*2]
 324:	00 00                	add    BYTE PTR [rax],al
 326:	00 0e                	add    BYTE PTR [rsi],cl
 328:	7d 01                	jge    32b <_init-0xcd5>
 32a:	00 00                	add    BYTE PTR [rax],al
 32c:	0a 37                	or     dh,BYTE PTR [rdi]
 32e:	0c 57                	or     al,0x57
 330:	00 00                	add    BYTE PTR [rax],al
 332:	00 0e                	add    BYTE PTR [rsi],cl
 334:	3a 03                	cmp    al,BYTE PTR [rbx]
 336:	00 00                	add    BYTE PTR [rax],al
 338:	0a 3b                	or     bh,BYTE PTR [rbx]
 33a:	0c 57                	or     al,0x57
 33c:	00 00                	add    BYTE PTR [rax],al
 33e:	00 11                	add    BYTE PTR [rcx],dl
 340:	10 01                	adc    BYTE PTR [rcx],al
 342:	09 09                	or     DWORD PTR [rcx],ecx
 344:	63 03                	movsxd eax,DWORD PTR [rbx]
 346:	00 00                	add    BYTE PTR [rax],al
 348:	09 e0                	or     eax,esp
 34a:	00 00                	add    BYTE PTR [rax],al
 34c:	00 01                	add    BYTE PTR [rcx],al
 34e:	0a 0f                	or     cl,BYTE PTR [rdi]
 350:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 351:	03 00                	add    eax,DWORD PTR [rax]
 353:	00 00                	add    BYTE PTR [rax],al
 355:	09 b4 00 00 00 01 0b 	or     DWORD PTR [rax+rax*1+0xb010000],esi
 35c:	09 7f 00             	or     DWORD PTR [rdi+0x0],edi
 35f:	00 00                	add    BYTE PTR [rax],al
 361:	08 00                	or     BYTE PTR [rax],al
 363:	12 9c 00 00 00 6e 03 	adc    bl,BYTE PTR [rax+rax*1+0x36e0000]
 36a:	00 00                	add    BYTE PTR [rax],al
 36c:	13 00                	adc    eax,DWORD PTR [rax]
 36e:	06                   	(bad)
 36f:	08 63 03             	or     BYTE PTR [rbx+0x3],ah
 372:	00 00                	add    BYTE PTR [rax],al
 374:	04 d7                	add    al,0xd7
 376:	01 00                	add    DWORD PTR [rax],eax
 378:	00 01                	add    BYTE PTR [rcx],al
 37a:	0c 03                	or     al,0x3
 37c:	3f                   	(bad)
 37d:	03 00                	add    eax,DWORD PTR [rax]
 37f:	00 14 1d 00 00 00 01 	add    BYTE PTR [rbx*1+0x1000000],dl
 386:	0e                   	(bad)
 387:	06                   	(bad)
 388:	85 00                	test   DWORD PTR [rax],eax
 38a:	00 00                	add    BYTE PTR [rax],al
 38c:	09 03                	or     DWORD PTR [rbx],eax
 38e:	50                   	push   rax
 38f:	40 00 00             	rex add BYTE PTR [rax],al
 392:	00 00                	add    BYTE PTR [rax],al
 394:	00 00                	add    BYTE PTR [rax],al
 396:	14 b5                	adc    al,0xb5
 398:	01 00                	add    DWORD PTR [rax],eax
 39a:	00 01                	add    BYTE PTR [rcx],al
 39c:	0f 0c                	(bad)
 39e:	ac                   	lods   al,BYTE PTR ds:[rsi]
 39f:	03 00                	add    eax,DWORD PTR [rax]
 3a1:	00 09                	add    BYTE PTR [rcx],cl
 3a3:	03 58 40             	add    ebx,DWORD PTR [rax+0x40]
 3a6:	00 00                	add    BYTE PTR [rax],al
 3a8:	00 00                	add    BYTE PTR [rax],al
 3aa:	00 00                	add    BYTE PTR [rax],al
 3ac:	06                   	(bad)
 3ad:	08 74 03 00          	or     BYTE PTR [rbx+rax*1+0x0],dh
 3b1:	00 15 4b 00 00 00    	add    BYTE PTR [rip+0x4b],dl        # 402 <_init-0xbfe>
 3b7:	01 a1 05 57 00 00    	add    DWORD PTR [rcx+0x5705],esp
 3bd:	00 b3 18 00 00 00    	add    BYTE PTR [rbx+0x18],dh
 3c3:	00 00                	add    BYTE PTR [rax],al
 3c5:	00 48 00             	add    BYTE PTR [rax+0x0],cl
 3c8:	00 00                	add    BYTE PTR [rax],al
 3ca:	00 00                	add    BYTE PTR [rax],al
 3cc:	00 00                	add    BYTE PTR [rax],al
 3ce:	01 9c 16 c5 00 00 00 	add    DWORD PTR [rsi+rdx*1+0xc5],ebx
 3d5:	01 9a 06 4e 18 00    	add    DWORD PTR [rdx+0x184e06],ebx
 3db:	00 00                	add    BYTE PTR [rax],al
 3dd:	00 00                	add    BYTE PTR [rax],al
 3df:	00 65 00             	add    BYTE PTR [rbp+0x0],ah
 3e2:	00 00                	add    BYTE PTR [rax],al
 3e4:	00 00                	add    BYTE PTR [rax],al
 3e6:	00 00                	add    BYTE PTR [rax],al
 3e8:	01 9c 16 f7 00 00 00 	add    DWORD PTR [rsi+rdx*1+0xf7],ebx
 3ef:	01 73 06             	add    DWORD PTR [rbx+0x6],esi
 3f2:	e7 16                	out    0x16,eax
 3f4:	00 00                	add    BYTE PTR [rax],al
 3f6:	00 00                	add    BYTE PTR [rax],al
 3f8:	00 00                	add    BYTE PTR [rax],al
 3fa:	67 01 00             	add    DWORD PTR [eax],eax
 3fd:	00 00                	add    BYTE PTR [rax],al
 3ff:	00 00                	add    BYTE PTR [rax],al
 401:	00 01                	add    BYTE PTR [rcx],al
 403:	9c                   	pushf
 404:	16                   	(bad)
 405:	15 02 00 00 01       	adc    eax,0x1000002
 40a:	67 06                	addr32 (bad)
 40c:	70 16                	jo     424 <_init-0xbdc>
 40e:	00 00                	add    BYTE PTR [rax],al
 410:	00 00                	add    BYTE PTR [rax],al
 412:	00 00                	add    BYTE PTR [rax],al
 414:	77 00                	ja     416 <_init-0xbea>
 416:	00 00                	add    BYTE PTR [rax],al
 418:	00 00                	add    BYTE PTR [rax],al
 41a:	00 00                	add    BYTE PTR [rax],al
 41c:	01 9c 17 69 00 01 58 	add    DWORD PTR [rdi+rdx*1+0x58010069],ebx
 423:	06                   	(bad)
 424:	c3                   	ret
 425:	15 00 00 00 00       	adc    eax,0x0
 42a:	00 00                	add    BYTE PTR [rax],al
 42c:	ad                   	lods   eax,DWORD PTR ds:[rsi]
 42d:	00 00                	add    BYTE PTR [rax],al
 42f:	00 00                	add    BYTE PTR [rax],al
 431:	00 00                	add    BYTE PTR [rax],al
 433:	00 01                	add    BYTE PTR [rcx],al
 435:	9c                   	pushf
 436:	4a 04 00             	rex.WX add al,0x0
 439:	00 18                	add    BYTE PTR [rax],bl
 43b:	f9                   	stc
 43c:	01 00                	add    DWORD PTR [rax],eax
 43e:	00 01                	add    BYTE PTR [rcx],al
 440:	59                   	pop    rcx
 441:	08 85 00 00 00 02    	or     BYTE PTR [rbp+0x2000000],al
 447:	91                   	xchg   ecx,eax
 448:	67 00 19             	add    BYTE PTR [ecx],bl
 44b:	07                   	(bad)
 44c:	00 00                	add    BYTE PTR [rax],al
 44e:	00 01                	add    BYTE PTR [rcx],al
 450:	4f 06                	rex.WRXB (bad)
 452:	69 15 00 00 00 00 00 	imul   edx,DWORD PTR [rip+0x0],0x5a0000        # 45c <_init-0xba4>
 459:	00 5a 00 
 45c:	00 00                	add    BYTE PTR [rax],al
 45e:	00 00                	add    BYTE PTR [rax],al
 460:	00 00                	add    BYTE PTR [rax],al
 462:	01 9c 78 04 00 00 1a 	add    DWORD PTR [rax+rdi*2+0x1a000004],ebx
 469:	6d                   	ins    DWORD PTR es:[rdi],dx
 46a:	73 67                	jae    4d3 <_init-0xb2d>
 46c:	00 01                	add    BYTE PTR [rcx],al
 46e:	51                   	push   rcx
 46f:	09 7f 00             	or     DWORD PTR [rdi+0x0],edi
 472:	00 00                	add    BYTE PTR [rax],al
 474:	02 91 68 00 1b 6d    	add    dl,BYTE PTR [rcx+0x6d1b0068]
 47a:	00 01                	add    BYTE PTR [rcx],al
 47c:	4d 06                	rex.WRB (bad)
 47e:	52                   	push   rdx
 47f:	15 00 00 00 00       	adc    eax,0x0
 484:	00 00                	add    BYTE PTR [rax],al
 486:	17                   	(bad)
 487:	00 00                	add    BYTE PTR [rax],al
 489:	00 00                	add    BYTE PTR [rax],al
 48b:	00 00                	add    BYTE PTR [rax],al
 48d:	00 01                	add    BYTE PTR [rcx],al
 48f:	9c                   	pushf
 490:	1b 70 00             	sbb    esi,DWORD PTR [rax+0x0]
 493:	01 49 06             	add    DWORD PTR [rcx+0x6],ecx
 496:	3b 15 00 00 00 00    	cmp    edx,DWORD PTR [rip+0x0]        # 49c <_init-0xb64>
 49c:	00 00                	add    BYTE PTR [rax],al
 49e:	17                   	(bad)
 49f:	00 00                	add    BYTE PTR [rax],al
 4a1:	00 00                	add    BYTE PTR [rax],al
 4a3:	00 00                	add    BYTE PTR [rax],al
 4a5:	00 01                	add    BYTE PTR [rcx],al
 4a7:	9c                   	pushf
 4a8:	1b 73 00             	sbb    esi,DWORD PTR [rbx+0x0]
 4ab:	01 44 06 0c          	add    DWORD PTR [rsi+rax*1+0xc],eax
 4af:	15 00 00 00 00       	adc    eax,0x0
 4b4:	00 00                	add    BYTE PTR [rax],al
 4b6:	2f                   	(bad)
 4b7:	00 00                	add    BYTE PTR [rax],al
 4b9:	00 00                	add    BYTE PTR [rax],al
 4bb:	00 00                	add    BYTE PTR [rax],al
 4bd:	00 01                	add    BYTE PTR [rcx],al
 4bf:	9c                   	pushf
 4c0:	1c 1e                	sbb    al,0x1e
 4c2:	01 00                	add    DWORD PTR [rax],eax
 4c4:	00 01                	add    BYTE PTR [rcx],al
 4c6:	42 06                	rex.X (bad)
 4c8:	eb 14                	jmp    4de <_init-0xb22>
 4ca:	00 00                	add    BYTE PTR [rax],al
 4cc:	00 00                	add    BYTE PTR [rax],al
 4ce:	00 00                	add    BYTE PTR [rax],al
 4d0:	21 00                	and    DWORD PTR [rax],eax
 4d2:	00 00                	add    BYTE PTR [rax],al
 4d4:	00 00                	add    BYTE PTR [rax],al
 4d6:	00 00                	add    BYTE PTR [rax],al
 4d8:	01 9c ee 04 00 00 1d 	add    DWORD PTR [rsi+rbp*8+0x1d000004],ebx
 4df:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 4e0:	62                   	(bad)
 4e1:	6a 00                	push   0x0
 4e3:	01 42 1a             	add    DWORD PTR [rdx+0x1a],eax
 4e6:	ac                   	lods   al,BYTE PTR ds:[rsi]
 4e7:	03 00                	add    eax,DWORD PTR [rax]
 4e9:	00 02                	add    BYTE PTR [rdx],al
 4eb:	91                   	xchg   ecx,eax
 4ec:	68 00 1e 8b 00       	push   0x8b1e00
 4f1:	00 00                	add    BYTE PTR [rax],al
 4f3:	01 24 07             	add    DWORD PTR [rdi+rax*1],esp
 4f6:	7f 00                	jg     4f8 <_init-0xb08>
 4f8:	00 00                	add    BYTE PTR [rax],al
 4fa:	f2 13 00             	repnz adc eax,DWORD PTR [rax]
 4fd:	00 00                	add    BYTE PTR [rax],al
 4ff:	00 00                	add    BYTE PTR [rax],al
 501:	00 f9                	add    cl,bh
 503:	00 00                	add    BYTE PTR [rax],al
 505:	00 00                	add    BYTE PTR [rax],al
 507:	00 00                	add    BYTE PTR [rax],al
 509:	00 01                	add    BYTE PTR [rcx],al
 50b:	9c                   	pushf
 50c:	7b 05                	jnp    513 <_init-0xaed>
 50e:	00 00                	add    BYTE PTR [rax],al
 510:	18 af 00 00 00 01    	sbb    BYTE PTR [rdi+0x1000000],ch
 516:	26 09 7f 00          	es or  DWORD PTR [rdi+0x0],edi
 51a:	00 00                	add    BYTE PTR [rax],al
 51c:	02 91 48 18 0f 02    	add    dl,BYTE PTR [rcx+0x20f1848]
 522:	00 00                	add    BYTE PTR [rax],al
 524:	01 26                	add    DWORD PTR [rsi],esp
 526:	1d 7f 00 00 00       	sbb    eax,0x7f
 52b:	02 91 50 18 bd 01    	add    dl,BYTE PTR [rcx+0x1bd1850]
 531:	00 00                	add    BYTE PTR [rax],al
 533:	01 27                	add    DWORD PTR [rdi],esp
 535:	0a a8 00 00 00 02    	or     ch,BYTE PTR [rax+0x2000000]
 53b:	91                   	xchg   ecx,eax
 53c:	58                   	pop    rax
 53d:	1a 6c 65 6e          	sbb    ch,BYTE PTR [rbp+riz*2+0x6e]
 541:	00 01                	add    BYTE PTR [rcx],al
 543:	27                   	(bad)
 544:	17                   	(bad)
 545:	a8 00                	test   al,0x0
 547:	00 00                	add    BYTE PTR [rax],al
 549:	02 91 60 1a 63 00    	add    dl,BYTE PTR [rcx+0x631a60]
 54f:	01 28                	add    DWORD PTR [rax],ebp
 551:	07                   	(bad)
 552:	57                   	push   rdi
 553:	00 00                	add    BYTE PTR [rax],al
 555:	00 02                	add    BYTE PTR [rdx],al
 557:	91                   	xchg   ecx,eax
 558:	44 1f                	rex.R (bad)
 55a:	62                   	(bad)
 55b:	14 00                	adc    al,0x0
 55d:	00 00                	add    BYTE PTR [rax],al
 55f:	00 00                	add    BYTE PTR [rax],al
 561:	00 5b 00             	add    BYTE PTR [rbx+0x0],bl
 564:	00 00                	add    BYTE PTR [rax],al
 566:	00 00                	add    BYTE PTR [rax],al
 568:	00 00                	add    BYTE PTR [rax],al
 56a:	18 09                	sbb    BYTE PTR [rcx],cl
 56c:	02 00                	add    al,BYTE PTR [rax]
 56e:	00 01                	add    BYTE PTR [rcx],al
 570:	31 0d 7f 00 00 00    	xor    DWORD PTR [rip+0x7f],ecx        # 5f5 <_init-0xa0b>
 576:	02 91 68 00 00 19    	add    dl,BYTE PTR [rcx+0x19000068]
 57c:	e7 00                	out    0x0,eax
 57e:	00 00                	add    BYTE PTR [rax],al
 580:	01 12                	add    DWORD PTR [rdx],edx
 582:	06                   	(bad)
 583:	49 13 00             	adc    rax,QWORD PTR [r8]
 586:	00 00                	add    BYTE PTR [rax],al
 588:	00 00                	add    BYTE PTR [rax],al
 58a:	00 a9 00 00 00 00    	add    BYTE PTR [rcx+0x0],ch
 590:	00 00                	add    BYTE PTR [rax],al
 592:	00 01                	add    BYTE PTR [rcx],al
 594:	9c                   	pushf
 595:	b8 05 00 00 1a       	mov    eax,0x1a000005
 59a:	62 75 66 00 01       	(bad)
 59f:	13 08                	adc    ecx,DWORD PTR [rax]
 5a1:	b8 05 00 00 03       	mov    eax,0x3000005
 5a6:	91                   	xchg   ecx,eax
 5a7:	a0 7f 1a 66 00 01 14 	movabs al,ds:0xa809140100661a7f
 5ae:	09 a8 
 5b0:	02 00                	add    al,BYTE PTR [rax]
 5b2:	00 03                	add    BYTE PTR [rbx],al
 5b4:	91                   	xchg   ecx,eax
 5b5:	98                   	cwde
 5b6:	7f 00                	jg     5b8 <_init-0xa48>
 5b8:	20 85 00 00 00 0d    	and    BYTE PTR [rbp+0xd000000],al
 5be:	42 00 00             	rex.X add BYTE PTR [rax],al
 5c1:	00 3f                	add    BYTE PTR [rdi],bh
	...

Disassembly of section .debug_abbrev:

0000000000000000 <.debug_abbrev>:
   0:	01 11                	add    DWORD PTR [rcx],edx
   2:	01 25 0e 13 0b 03    	add    DWORD PTR [rip+0x30b130e],esp        # 30b1316 <_end+0x30ad2b6>
   8:	0e                   	(bad)
   9:	1b 0e                	sbb    ecx,DWORD PTR [rsi]
   b:	11 01                	adc    DWORD PTR [rcx],eax
   d:	12 07                	adc    al,BYTE PTR [rdi]
   f:	10 17                	adc    BYTE PTR [rdi],dl
  11:	00 00                	add    BYTE PTR [rax],al
  13:	02 24 00             	add    ah,BYTE PTR [rax+rax*1]
  16:	0b 0b                	or     ecx,DWORD PTR [rbx]
  18:	3e 0b 03             	ds or  eax,DWORD PTR [rbx]
  1b:	0e                   	(bad)
  1c:	00 00                	add    BYTE PTR [rax],al
  1e:	03 24 00             	add    esp,DWORD PTR [rax+rax*1]
  21:	0b 0b                	or     ecx,DWORD PTR [rbx]
  23:	3e 0b 03             	ds or  eax,DWORD PTR [rbx]
  26:	08 00                	or     BYTE PTR [rax],al
  28:	00 04 16             	add    BYTE PTR [rsi+rdx*1],al
  2b:	00 03                	add    BYTE PTR [rbx],al
  2d:	0e                   	(bad)
  2e:	3a 0b                	cmp    cl,BYTE PTR [rbx]
  30:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
  32:	39 0b                	cmp    DWORD PTR [rbx],ecx
  34:	49 13 00             	adc    rax,QWORD PTR [r8]
  37:	00 05 0f 00 0b 0b    	add    BYTE PTR [rip+0xb0b000f],al        # b0b004c <_end+0xb0abfec>
  3d:	00 00                	add    BYTE PTR [rax],al
  3f:	06                   	(bad)
  40:	0f 00 0b             	str    WORD PTR [rbx]
  43:	0b 49 13             	or     ecx,DWORD PTR [rcx+0x13]
  46:	00 00                	add    BYTE PTR [rax],al
  48:	07                   	(bad)
  49:	26 00 49 13          	es add BYTE PTR [rcx+0x13],cl
  4d:	00 00                	add    BYTE PTR [rax],al
  4f:	08 13                	or     BYTE PTR [rbx],dl
  51:	01 03                	add    DWORD PTR [rbx],eax
  53:	0e                   	(bad)
  54:	0b 0b                	or     ecx,DWORD PTR [rbx]
  56:	3a 0b                	cmp    cl,BYTE PTR [rbx]
  58:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
  5a:	39 0b                	cmp    DWORD PTR [rbx],ecx
  5c:	01 13                	add    DWORD PTR [rbx],edx
  5e:	00 00                	add    BYTE PTR [rax],al
  60:	09 0d 00 03 0e 3a    	or     DWORD PTR [rip+0x3a0e0300],ecx        # 3a0e0366 <_end+0x3a0dc306>
  66:	0b 3b                	or     edi,DWORD PTR [rbx]
  68:	0b 39                	or     edi,DWORD PTR [rcx]
  6a:	0b 49 13             	or     ecx,DWORD PTR [rcx+0x13]
  6d:	38 0b                	cmp    BYTE PTR [rbx],cl
  6f:	00 00                	add    BYTE PTR [rax],al
  71:	0a 16                	or     dl,BYTE PTR [rsi]
  73:	00 03                	add    BYTE PTR [rbx],al
  75:	0e                   	(bad)
  76:	3a 0b                	cmp    cl,BYTE PTR [rbx]
  78:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
  7a:	39 0b                	cmp    DWORD PTR [rbx],ecx
  7c:	00 00                	add    BYTE PTR [rax],al
  7e:	0b 13                	or     edx,DWORD PTR [rbx]
  80:	00 03                	add    BYTE PTR [rbx],al
  82:	0e                   	(bad)
  83:	3c 19                	cmp    al,0x19
  85:	00 00                	add    BYTE PTR [rax],al
  87:	0c 01                	or     al,0x1
  89:	01 49 13             	add    DWORD PTR [rcx+0x13],ecx
  8c:	01 13                	add    DWORD PTR [rbx],edx
  8e:	00 00                	add    BYTE PTR [rax],al
  90:	0d 21 00 49 13       	or     eax,0x13490021
  95:	2f                   	(bad)
  96:	0b 00                	or     eax,DWORD PTR [rax]
  98:	00 0e                	add    BYTE PTR [rsi],cl
  9a:	34 00                	xor    al,0x0
  9c:	03 0e                	add    ecx,DWORD PTR [rsi]
  9e:	3a 0b                	cmp    cl,BYTE PTR [rbx]
  a0:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
  a2:	39 0b                	cmp    DWORD PTR [rbx],ecx
  a4:	49 13 3f             	adc    rdi,QWORD PTR [r15]
  a7:	19 3c 19             	sbb    DWORD PTR [rcx+rbx*1],edi
  aa:	00 00                	add    BYTE PTR [rax],al
  ac:	0f 21 00             	mov    rax,dr0
  af:	00 00                	add    BYTE PTR [rax],al
  b1:	10 34 00             	adc    BYTE PTR [rax+rax*1],dh
  b4:	03 0e                	add    ecx,DWORD PTR [rsi]
  b6:	3a 0b                	cmp    cl,BYTE PTR [rbx]
  b8:	3b 05 39 0b 49 13    	cmp    eax,DWORD PTR [rip+0x13490b39]        # 13490bf7 <_end+0x1348cb97>
  be:	3f                   	(bad)
  bf:	19 3c 19             	sbb    DWORD PTR [rcx+rbx*1],edi
  c2:	00 00                	add    BYTE PTR [rax],al
  c4:	11 13                	adc    DWORD PTR [rbx],edx
  c6:	01 0b                	add    DWORD PTR [rbx],ecx
  c8:	0b 3a                	or     edi,DWORD PTR [rdx]
  ca:	0b 3b                	or     edi,DWORD PTR [rbx]
  cc:	0b 39                	or     edi,DWORD PTR [rcx]
  ce:	0b 01                	or     eax,DWORD PTR [rcx]
  d0:	13 00                	adc    eax,DWORD PTR [rax]
  d2:	00 12                	add    BYTE PTR [rdx],dl
  d4:	15 01 49 13 01       	adc    eax,0x1134901
  d9:	13 00                	adc    eax,DWORD PTR [rax]
  db:	00 13                	add    BYTE PTR [rbx],dl
  dd:	18 00                	sbb    BYTE PTR [rax],al
  df:	00 00                	add    BYTE PTR [rax],al
  e1:	14 34                	adc    al,0x34
  e3:	00 03                	add    BYTE PTR [rbx],al
  e5:	0e                   	(bad)
  e6:	3a 0b                	cmp    cl,BYTE PTR [rbx]
  e8:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
  ea:	39 0b                	cmp    DWORD PTR [rbx],ecx
  ec:	49 13 3f             	adc    rdi,QWORD PTR [r15]
  ef:	19 02                	sbb    DWORD PTR [rdx],eax
  f1:	18 00                	sbb    BYTE PTR [rax],al
  f3:	00 15 2e 00 3f 19    	add    BYTE PTR [rip+0x193f002e],dl        # 193f0127 <_end+0x193ec0c7>
  f9:	03 0e                	add    ecx,DWORD PTR [rsi]
  fb:	3a 0b                	cmp    cl,BYTE PTR [rbx]
  fd:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
  ff:	39 0b                	cmp    DWORD PTR [rbx],ecx
 101:	49 13 11             	adc    rdx,QWORD PTR [r9]
 104:	01 12                	add    DWORD PTR [rdx],edx
 106:	07                   	(bad)
 107:	40 18 96 42 19 00 00 	rex sbb BYTE PTR [rsi+0x1942],dl
 10e:	16                   	(bad)
 10f:	2e 00 3f             	cs add BYTE PTR [rdi],bh
 112:	19 03                	sbb    DWORD PTR [rbx],eax
 114:	0e                   	(bad)
 115:	3a 0b                	cmp    cl,BYTE PTR [rbx]
 117:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
 119:	39 0b                	cmp    DWORD PTR [rbx],ecx
 11b:	11 01                	adc    DWORD PTR [rcx],eax
 11d:	12 07                	adc    al,BYTE PTR [rdi]
 11f:	40 18 96 42 19 00 00 	rex sbb BYTE PTR [rsi+0x1942],dl
 126:	17                   	(bad)
 127:	2e 01 3f             	cs add DWORD PTR [rdi],edi
 12a:	19 03                	sbb    DWORD PTR [rbx],eax
 12c:	08 3a                	or     BYTE PTR [rdx],bh
 12e:	0b 3b                	or     edi,DWORD PTR [rbx]
 130:	0b 39                	or     edi,DWORD PTR [rcx]
 132:	0b 11                	or     edx,DWORD PTR [rcx]
 134:	01 12                	add    DWORD PTR [rdx],edx
 136:	07                   	(bad)
 137:	40 18 96 42 19 01 13 	rex sbb BYTE PTR [rsi+0x13011942],dl
 13e:	00 00                	add    BYTE PTR [rax],al
 140:	18 34 00             	sbb    BYTE PTR [rax+rax*1],dh
 143:	03 0e                	add    ecx,DWORD PTR [rsi]
 145:	3a 0b                	cmp    cl,BYTE PTR [rbx]
 147:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
 149:	39 0b                	cmp    DWORD PTR [rbx],ecx
 14b:	49 13 02             	adc    rax,QWORD PTR [r10]
 14e:	18 00                	sbb    BYTE PTR [rax],al
 150:	00 19                	add    BYTE PTR [rcx],bl
 152:	2e 01 3f             	cs add DWORD PTR [rdi],edi
 155:	19 03                	sbb    DWORD PTR [rbx],eax
 157:	0e                   	(bad)
 158:	3a 0b                	cmp    cl,BYTE PTR [rbx]
 15a:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
 15c:	39 0b                	cmp    DWORD PTR [rbx],ecx
 15e:	11 01                	adc    DWORD PTR [rcx],eax
 160:	12 07                	adc    al,BYTE PTR [rdi]
 162:	40 18 96 42 19 01 13 	rex sbb BYTE PTR [rsi+0x13011942],dl
 169:	00 00                	add    BYTE PTR [rax],al
 16b:	1a 34 00             	sbb    dh,BYTE PTR [rax+rax*1]
 16e:	03 08                	add    ecx,DWORD PTR [rax]
 170:	3a 0b                	cmp    cl,BYTE PTR [rbx]
 172:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
 174:	39 0b                	cmp    DWORD PTR [rbx],ecx
 176:	49 13 02             	adc    rax,QWORD PTR [r10]
 179:	18 00                	sbb    BYTE PTR [rax],al
 17b:	00 1b                	add    BYTE PTR [rbx],bl
 17d:	2e 00 3f             	cs add BYTE PTR [rdi],bh
 180:	19 03                	sbb    DWORD PTR [rbx],eax
 182:	08 3a                	or     BYTE PTR [rdx],bh
 184:	0b 3b                	or     edi,DWORD PTR [rbx]
 186:	0b 39                	or     edi,DWORD PTR [rcx]
 188:	0b 11                	or     edx,DWORD PTR [rcx]
 18a:	01 12                	add    DWORD PTR [rdx],edx
 18c:	07                   	(bad)
 18d:	40 18 96 42 19 00 00 	rex sbb BYTE PTR [rsi+0x1942],dl
 194:	1c 2e                	sbb    al,0x2e
 196:	01 3f                	add    DWORD PTR [rdi],edi
 198:	19 03                	sbb    DWORD PTR [rbx],eax
 19a:	0e                   	(bad)
 19b:	3a 0b                	cmp    cl,BYTE PTR [rbx]
 19d:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
 19f:	39 0b                	cmp    DWORD PTR [rbx],ecx
 1a1:	27                   	(bad)
 1a2:	19 11                	sbb    DWORD PTR [rcx],edx
 1a4:	01 12                	add    DWORD PTR [rdx],edx
 1a6:	07                   	(bad)
 1a7:	40 18 96 42 19 01 13 	rex sbb BYTE PTR [rsi+0x13011942],dl
 1ae:	00 00                	add    BYTE PTR [rax],al
 1b0:	1d 05 00 03 08       	sbb    eax,0x8030005
 1b5:	3a 0b                	cmp    cl,BYTE PTR [rbx]
 1b7:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
 1b9:	39 0b                	cmp    DWORD PTR [rbx],ecx
 1bb:	49 13 02             	adc    rax,QWORD PTR [r10]
 1be:	18 00                	sbb    BYTE PTR [rax],al
 1c0:	00 1e                	add    BYTE PTR [rsi],bl
 1c2:	2e 01 3f             	cs add DWORD PTR [rdi],edi
 1c5:	19 03                	sbb    DWORD PTR [rbx],eax
 1c7:	0e                   	(bad)
 1c8:	3a 0b                	cmp    cl,BYTE PTR [rbx]
 1ca:	3b 0b                	cmp    ecx,DWORD PTR [rbx]
 1cc:	39 0b                	cmp    DWORD PTR [rbx],ecx
 1ce:	27                   	(bad)
 1cf:	19 49 13             	sbb    DWORD PTR [rcx+0x13],ecx
 1d2:	11 01                	adc    DWORD PTR [rcx],eax
 1d4:	12 07                	adc    al,BYTE PTR [rdi]
 1d6:	40 18 96 42 19 01 13 	rex sbb BYTE PTR [rsi+0x13011942],dl
 1dd:	00 00                	add    BYTE PTR [rax],al
 1df:	1f                   	(bad)
 1e0:	0b 01                	or     eax,DWORD PTR [rcx]
 1e2:	11 01                	adc    DWORD PTR [rcx],eax
 1e4:	12 07                	adc    al,BYTE PTR [rdi]
 1e6:	00 00                	add    BYTE PTR [rax],al
 1e8:	20 01                	and    BYTE PTR [rcx],al
 1ea:	01 49 13             	add    DWORD PTR [rcx+0x13],ecx
 1ed:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .debug_line:

0000000000000000 <.debug_line>:
   0:	af                   	scas   eax,DWORD PTR es:[rdi]
   1:	02 00                	add    al,BYTE PTR [rax]
   3:	00 03                	add    BYTE PTR [rbx],al
   5:	00 1c 01             	add    BYTE PTR [rcx+rax*1],bl
   8:	00 00                	add    BYTE PTR [rax],al
   a:	01 01                	add    DWORD PTR [rcx],eax
   c:	fb                   	sti
   d:	0e                   	(bad)
   e:	0d 00 01 01 01       	or     eax,0x1010100
  13:	01 00                	add    DWORD PTR [rax],eax
  15:	00 00                	add    BYTE PTR [rax],al
  17:	01 00                	add    DWORD PTR [rax],eax
  19:	00 01                	add    BYTE PTR [rcx],al
  1b:	2f                   	(bad)
  1c:	75 73                	jne    91 <_init-0xf6f>
  1e:	72 2f                	jb     4f <_init-0xfb1>
  20:	69 6e 63 6c 75 64 65 	imul   ebp,DWORD PTR [rsi+0x63],0x6564756c
  27:	2f                   	(bad)
  28:	78 38                	js     62 <_init-0xf9e>
  2a:	36 5f                	ss pop rdi
  2c:	36 34 2d             	ss xor al,0x2d
  2f:	6c                   	ins    BYTE PTR es:[rdi],dx
  30:	69 6e 75 78 2d 67 6e 	imul   ebp,DWORD PTR [rsi+0x75],0x6e672d78
  37:	75 2f                	jne    68 <_init-0xf98>
  39:	62                   	(bad)
  3a:	69 74 73 00 2f 75 73 	imul   esi,DWORD PTR [rbx+rsi*2+0x0],0x7273752f
  41:	72 
  42:	2f                   	(bad)
  43:	69 6e 63 6c 75 64 65 	imul   ebp,DWORD PTR [rsi+0x63],0x6564756c
  4a:	00 2f                	add    BYTE PTR [rdi],ch
  4c:	75 73                	jne    c1 <_init-0xf3f>
  4e:	72 2f                	jb     7f <_init-0xf81>
  50:	6c                   	ins    BYTE PTR es:[rdi],dx
  51:	69 62 2f 67 63 63 2f 	imul   esp,DWORD PTR [rdx+0x2f],0x2f636367
  58:	78 38                	js     92 <_init-0xf6e>
  5a:	36 5f                	ss pop rdi
  5c:	36 34 2d             	ss xor al,0x2d
  5f:	6c                   	ins    BYTE PTR es:[rdi],dx
  60:	69 6e 75 78 2d 67 6e 	imul   ebp,DWORD PTR [rsi+0x75],0x6e672d78
  67:	75 2f                	jne    98 <_init-0xf68>
  69:	39 2f                	cmp    DWORD PTR [rdi],ebp
  6b:	69 6e 63 6c 75 64 65 	imul   ebp,DWORD PTR [rsi+0x63],0x6564756c
  72:	00 2f                	add    BYTE PTR [rdi],ch
  74:	75 73                	jne    e9 <_init-0xf17>
  76:	72 2f                	jb     a7 <_init-0xf59>
  78:	69 6e 63 6c 75 64 65 	imul   ebp,DWORD PTR [rsi+0x63],0x6564756c
  7f:	2f                   	(bad)
  80:	78 38                	js     ba <_init-0xf46>
  82:	36 5f                	ss pop rdi
  84:	36 34 2d             	ss xor al,0x2d
  87:	6c                   	ins    BYTE PTR es:[rdi],dx
  88:	69 6e 75 78 2d 67 6e 	imul   ebp,DWORD PTR [rsi+0x75],0x6e672d78
  8f:	75 2f                	jne    c0 <_init-0xf40>
  91:	62                   	(bad)
  92:	69 74 73 2f 74 79 70 	imul   esi,DWORD PTR [rbx+rsi*2+0x2f],0x65707974
  99:	65 
  9a:	73 00                	jae    9c <_init-0xf64>
  9c:	00 6e 65             	add    BYTE PTR [rsi+0x65],ch
  9f:	77 5f                	ja     100 <_init-0xf00>
  a1:	61                   	(bad)
  a2:	62 73                	(bad)
  a4:	61                   	(bad)
  a5:	6c                   	ins    BYTE PTR es:[rdi],dx
  a6:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  a7:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  a8:	2e 63 00             	cs movsxd eax,DWORD PTR [rax]
  ab:	00 00                	add    BYTE PTR [rax],al
  ad:	00 74 79 70          	add    BYTE PTR [rcx+rdi*2+0x70],dh
  b1:	65 73 2e             	gs jae e2 <_init-0xf1e>
  b4:	68 00 01 00 00       	push   0x100
  b9:	73 74                	jae    12f <_init-0xed1>
  bb:	64 69 6e 74 2e 68 00 	imul   ebp,DWORD PTR fs:[rsi+0x74],0x200682e
  c2:	02 
  c3:	00 00                	add    BYTE PTR [rax],al
  c5:	73 74                	jae    13b <_init-0xec5>
  c7:	64 64 65 66 2e 68 00 	fs fs gs cs pushw 0x300
  ce:	03 
  cf:	00 00                	add    BYTE PTR [rax],al
  d1:	73 74                	jae    147 <_init-0xeb9>
  d3:	72 75                	jb     14a <_init-0xeb6>
  d5:	63 74 5f 46          	movsxd esi,DWORD PTR [rdi+rbx*2+0x46]
  d9:	49                   	rex.WB
  da:	4c                   	rex.WR
  db:	45                   	rex.RB
  dc:	2e 68 00 04 00 00    	cs push 0x400
  e2:	46                   	rex.RX
  e3:	49                   	rex.WB
  e4:	4c                   	rex.WR
  e5:	45                   	rex.RB
  e6:	2e 68 00 04 00 00    	cs push 0x400
  ec:	73 74                	jae    162 <_init-0xe9e>
  ee:	64 69 6f 2e 68 00 02 	imul   ebp,DWORD PTR fs:[rdi+0x2e],0x20068
  f5:	00 
  f6:	00 73 79             	add    BYTE PTR [rbx+0x79],dh
  f9:	73 5f                	jae    15a <_init-0xea6>
  fb:	65 72 72             	gs jb  170 <_init-0xe90>
  fe:	6c                   	ins    BYTE PTR es:[rdi],dx
  ff:	69 73 74 2e 68 00 01 	imul   esi,DWORD PTR [rbx+0x74],0x100682e
 106:	00 00                	add    BYTE PTR [rax],al
 108:	75 6e                	jne    178 <_init-0xe88>
 10a:	69 73 74 64 2e 68 00 	imul   esi,DWORD PTR [rbx+0x74],0x682e64
 111:	02 00                	add    al,BYTE PTR [rax]
 113:	00 67 65             	add    BYTE PTR [rdi+0x65],ah
 116:	74 6f                	je     187 <_init-0xe79>
 118:	70 74                	jo     18e <_init-0xe72>
 11a:	5f                   	pop    rdi
 11b:	63 6f 72             	movsxd ebp,DWORD PTR [rdi+0x72]
 11e:	65 2e 68 00 01 00 00 	gs cs push 0x100
 125:	00 05 18 00 09 02    	add    BYTE PTR [rip+0x2090018],al        # 2090143 <_end+0x208c0e3>
 12b:	49 13 00             	adc    rax,QWORD PTR [r8]
 12e:	00 00                	add    BYTE PTR [rax],al
 130:	00 00                	add    BYTE PTR [rax],al
 132:	00 03                	add    BYTE PTR [rbx],al
 134:	11 01                	adc    DWORD PTR [rcx],eax
 136:	ba 05 0d e6 05       	mov    edx,0x5e60d05
 13b:	06                   	(bad)
 13c:	08 67 05             	or     BYTE PTR [rdi+0x5],ah
 13f:	05 75 08 14 05       	add    eax,0x5140875
 144:	08 a1 05 06 08 4a    	or     BYTE PTR [rcx+0x4a080605],ah
 14a:	05 05 59 bb 05       	add    eax,0x5bb5905
 14f:	03 a0 bb e5 05 13    	add    esp,DWORD PTR [rax+0x1305e5bb]
 155:	a1 05 03 bb 05 10 59 	movabs eax,ds:0x1d05591005bb0305
 15c:	05 1d 
 15e:	d6                   	(bad)
 15f:	05 0a 83 05 17       	add    eax,0x1705830a
 164:	82                   	(bad)
 165:	05 06 84 05 0c       	add    eax,0xc058406
 16a:	75 05                	jne    171 <_init-0xe8f>
 16c:	09 a0 05 08 08 21    	or     DWORD PTR [rax+0x21080805],esp
 172:	a0 05 0b bb 05 15 83 	movabs al,ds:0xa05831505bb0b05
 179:	05 0a 
 17b:	08 a0 05 09 75 05    	or     BYTE PTR [rax+0x5750905],ah
 181:	10 bb 05 1c 76 05    	adc    BYTE PTR [rbx+0x5761c05],bh
 187:	0c ac                	or     al,0xac
 189:	05 0d ad 05 0f       	add    eax,0xf05ad0d
 18e:	85 05 12 ba 05 0a    	test   DWORD PTR [rip+0xa05ba12],eax        # a05bba6 <_end+0xa057b46>
 194:	58                   	pop    rax
 195:	05 08 3c 05 07       	add    eax,0x7053c08
 19a:	03 71 4a             	add    esi,DWORD PTR [rcx+0x4a]
 19d:	5a                   	pop    rdx
 19e:	03 0e                	add    ecx,DWORD PTR [rsi]
 1a0:	3c 05                	cmp    al,0x5
 1a2:	09 22                	or     DWORD PTR [rdx],esp
 1a4:	05 0a 75 05 01       	add    eax,0x105750a
 1a9:	4b 05 1f 30 05 22    	rex.WXB add rax,0x2205301f
 1af:	f2 05 33 d6 05 0a    	repnz add eax,0xa05d633
 1b5:	3e 05 03 83 08 75    	ds add eax,0x75088303
 1bb:	05 01 bb 05 0a       	add    eax,0xa05bb01
 1c0:	3e 05 03 83 05 01    	ds add eax,0x1058303
 1c6:	bb 05 0a 3e 05       	mov    ebx,0x53e0a05
 1cb:	0c 82                	or     al,0x82
 1cd:	05 26 ba 05 15       	add    eax,0x1505ba26
 1d2:	3e 05 03 bb 05 17    	ds add eax,0x1705bb03
 1d8:	bb 05 07 d7 05       	mov    ebx,0x5d70705
 1dd:	06                   	(bad)
 1de:	08 58 05             	or     BYTE PTR [rax+0x5],bl
 1e1:	05 59 bb 05 01       	add    eax,0x105bb59
 1e6:	a0 05 0a 3e ba 05 03 	movabs al,ds:0x5e60305ba3e0a05
 1ed:	e6 05 
 1ef:	07                   	(bad)
 1f0:	bb 05 06 08 74       	mov    ebx,0x74080605
 1f5:	05 05 59 bb 05       	add    eax,0x5bb5905
 1fa:	07                   	(bad)
 1fb:	a0 05 06 d6 05 05 59 	movabs al,ds:0x5bb590505d60605
 202:	bb 05 
 204:	01 e8                	add    eax,ebp
 206:	05 05 2c 05 01       	add    eax,0x1052c05
 20b:	bc 05 12 08 68       	mov    esp,0x68081205
 210:	05 03 83 bb bb       	add    eax,0xbbbb8303
 215:	bb bb bb bb bb       	mov    ebx,0xbbbbbbbb
 21a:	bb 05 01 bb 05       	mov    ebx,0x5bb0105
 21f:	15 3e 05 07 c9       	adc    eax,0xc907053e
 224:	05 06 08 74 05       	add    eax,0x5740806
 229:	05 59 bb 05 0c       	add    eax,0xc05bb59
 22e:	a0 05 0a 08 12 05 03 	movabs al,ds:0x567030512080a05
 235:	67 05 
 237:	09 02                	or     DWORD PTR [rdx],eax
 239:	39 14 05 08 74 05 0e 	cmp    DWORD PTR [rax*1+0xe057408],edx
 240:	59                   	pop    rcx
 241:	05 17 74 05 05       	add    eax,0x5057417
 246:	a2 05 07 56 05 05 bc 	movabs ds:0xc05bc0505560705,al
 24d:	05 0c 
 24f:	5a                   	pop    rdx
 250:	05 15 74 05 05       	add    eax,0x5057415
 255:	9f                   	lahf
 256:	05 0c 5a 05 15       	add    eax,0x15055a0c
 25b:	74 05                	je     262 <_init-0xd9e>
 25d:	05 9f bb bb 05       	add    eax,0x5bbbb9f
 262:	0c bb                	or     al,0xbb
 264:	05 15 74 05 13       	add    eax,0x13057415
 269:	58                   	pop    rax
 26a:	05 05 4b 05 0c       	add    eax,0xc054b05
 26f:	30 05 15 74 05 05    	xor    BYTE PTR [rip+0x5057415],al        # 505768a <_end+0x505362a>
 275:	9f                   	lahf
 276:	30 9f 30 a0 bb 05    	xor    BYTE PTR [rdi+0x5bba030],bl
 27c:	01 a1 05 0e 84 05    	add    DWORD PTR [rcx+0x5840e05],esp
 282:	03 84 08 c9 08 c9 05 	add    eax,DWORD PTR [rax+rcx*1+0x5c908c9]
 289:	01 08                	add    DWORD PTR [rax],ecx
 28b:	c9                   	leave
 28c:	05 0c 3e 05 03       	add    eax,0x3053e0c
 291:	83 05 1b 9f 05 0b 9e 	add    DWORD PTR [rip+0xb059f1b],0xffffff9e        # b05a1b3 <_end+0xb056153>
 298:	05 05 00 02 04       	add    eax,0x4020005
 29d:	01 76 00             	add    DWORD PTR [rsi+0x0],esi
 2a0:	02 04 01             	add    al,BYTE PTR [rcx+rax*1]
 2a3:	9f                   	lahf
 2a4:	00 02                	add    BYTE PTR [rdx],al
 2a6:	04 01                	add    al,0x1
 2a8:	a0 00 02 04 01 e1 02 	movabs al,ds:0x202e101040200
 2af:	02 00 
 2b1:	01 01                	add    DWORD PTR [rcx],eax

Disassembly of section .debug_str:

0000000000000000 <.debug_str>:
   0:	6f                   	outs   dx,DWORD PTR ds:[rsi]
   1:	70 74                	jo     77 <_init-0xf89>
   3:	61                   	(bad)
   4:	72 67                	jb     6d <_init-0xf93>
   6:	00 6c 65 61          	add    BYTE PTR [rbp+riz*2+0x61],ch
   a:	76 65                	jbe    71 <_init-0xf8f>
   c:	4d                   	rex.WRB
   d:	65 73 73             	gs jae 83 <_init-0xf7d>
  10:	61                   	(bad)
  11:	67 65 00 5f 49       	add    BYTE PTR gs:[edi+0x49],bl
  16:	4f 5f                	rex.WRXB pop r15
  18:	46                   	rex.RX
  19:	49                   	rex.WB
  1a:	4c                   	rex.WR
  1b:	45 00 63 68          	add    BYTE PTR [r11+0x68],r12b
  1f:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  20:	69 63 65 00 73 79 73 	imul   esp,DWORD PTR [rbx+0x65],0x73797300
  27:	5f                   	pop    rdi
  28:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  29:	65 72 72             	gs jb  9e <_init-0xf62>
  2c:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
  2f:	4f 5f                	rex.WRXB pop r15
  31:	73 61                	jae    94 <_init-0xf6c>
  33:	76 65                	jbe    9a <_init-0xf66>
  35:	5f                   	pop    rdi
  36:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
  38:	64 00 73 68          	add    BYTE PTR fs:[rbx+0x68],dh
  3c:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  3d:	72 74                	jb     b3 <_init-0xf4d>
  3f:	20 69 6e             	and    BYTE PTR [rcx+0x6e],ch
  42:	74 00                	je     44 <_init-0xfbc>
  44:	73 69                	jae    af <_init-0xf51>
  46:	7a 65                	jp     ad <_init-0xf53>
  48:	5f                   	pop    rdi
  49:	74 00                	je     4b <_init-0xfb5>
  4b:	6d                   	ins    DWORD PTR es:[rdi],dx
  4c:	61                   	(bad)
  4d:	69 6e 00 5f 49 4f 5f 	imul   ebp,DWORD PTR [rsi+0x0],0x5f4f495f
  54:	77 72                	ja     c8 <_init-0xf38>
  56:	69 74 65 5f 70 74 72 	imul   esi,DWORD PTR [rbp+riz*2+0x5f],0x727470
  5d:	00 
  5e:	5f                   	pop    rdi
  5f:	66 6c                	data16 ins BYTE PTR es:[rdi],dx
  61:	61                   	(bad)
  62:	67 73 00             	addr32 jae 65 <_init-0xf9b>
  65:	5f                   	pop    rdi
  66:	49                   	rex.WB
  67:	4f 5f                	rex.WRXB pop r15
  69:	62 75 66 5f 62       	(bad)
  6e:	61                   	(bad)
  6f:	73 65                	jae    d6 <_init-0xf2a>
  71:	00 5f 6c             	add    BYTE PTR [rdi+0x6c],bl
  74:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  75:	63 6b 00             	movsxd ebp,DWORD PTR [rbx+0x0]
  78:	5f                   	pop    rdi
  79:	5f                   	pop    rdi
  7a:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
  7c:	76 69                	jbe    e7 <_init-0xf19>
  7e:	72 6f                	jb     ef <_init-0xf11>
  80:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  81:	00 5f 6d             	add    BYTE PTR [rdi+0x6d],bl
  84:	61                   	(bad)
  85:	72 6b                	jb     f2 <_init-0xf0e>
  87:	65 72 73             	gs jb  fd <_init-0xf03>
  8a:	00 73 6c             	add    BYTE PTR [rbx+0x6c],dh
  8d:	75 72                	jne    101 <_init-0xeff>
  8f:	70 00                	jo     91 <_init-0xf6f>
  91:	5f                   	pop    rdi
  92:	66 72 65             	data16 jb fa <_init-0xf06>
  95:	65 72 65             	gs jb  fd <_init-0xf03>
  98:	73 5f                	jae    f9 <_init-0xf07>
  9a:	62 75 66 00 75       	(bad)
  9f:	69 6e 74 70 74 72 5f 	imul   ebp,DWORD PTR [rsi+0x74],0x5f727470
  a6:	74 00                	je     a8 <_init-0xf58>
  a8:	2f                   	(bad)
  a9:	62 75                	(bad)
  ab:	69 6c 64 00 6c 69 6e 	imul   ebp,DWORD PTR [rsp+riz*2+0x0],0x656e696c
  b2:	65 
  b3:	00 6b 75             	add    BYTE PTR [rbx+0x75],ch
  b6:	69 64 00 5f 63 75 72 	imul   esp,DWORD PTR [rax+rax*1+0x5f],0x5f727563
  bd:	5f 
  be:	63 6f 6c             	movsxd ebp,DWORD PTR [rdi+0x6c]
  c1:	75 6d                	jne    130 <_init-0xed0>
  c3:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  c4:	00 73 65             	add    BYTE PTR [rbx+0x65],dh
  c7:	74 75                	je     13e <_init-0xec2>
  c9:	70 00                	jo     cb <_init-0xf35>
  cb:	73 74                	jae    141 <_init-0xebf>
  cd:	64 65 72 72          	fs gs jb 143 <_init-0xebd>
  d1:	00 6c 6f 6e          	add    BYTE PTR [rdi+rbp*2+0x6e],ch
  d5:	67 20 6c 6f 6e       	and    BYTE PTR [edi+ebp*2+0x6e],ch
  da:	67 20 69 6e          	and    BYTE PTR [ecx+0x6e],ch
  de:	74 00                	je     e0 <_init-0xf20>
  e0:	61                   	(bad)
  e1:	63 74 69 6f          	movsxd esi,DWORD PTR [rcx+rbp*2+0x6f]
  e5:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  e6:	00 77 69             	add    BYTE PTR [rdi+0x69],dh
  e9:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  ea:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  eb:	65 72 5f             	gs jb  14d <_init-0xeb3>
  ee:	66 75 6e             	data16 jne 15f <_init-0xea1>
  f1:	63 74 69 6f          	movsxd esi,DWORD PTR [rcx+rbp*2+0x6f]
  f5:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  f6:	00 70 72             	add    BYTE PTR [rax+0x72],dh
  f9:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  fa:	63 65 73             	movsxd esp,DWORD PTR [rbp+0x73]
  fd:	73 49                	jae    148 <_init-0xeb8>
  ff:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 100:	70 75                	jo     177 <_init-0xe89>
 102:	74 00                	je     104 <_init-0xefc>
 104:	5f                   	pop    rdi
 105:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 106:	6c                   	ins    BYTE PTR es:[rdi],dx
 107:	64 5f                	fs pop rdi
 109:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 10a:	66 66 73 65          	data16 data16 jae 173 <_init-0xe8d>
 10e:	74 00                	je     110 <_init-0xef0>
 110:	75 6e                	jne    180 <_init-0xe80>
 112:	73 69                	jae    17d <_init-0xe83>
 114:	67 6e                	outs   dx,BYTE PTR ds:[esi]
 116:	65 64 20 63 68       	gs and BYTE PTR fs:[rbx+0x68],ah
 11b:	61                   	(bad)
 11c:	72 00                	jb     11e <_init-0xee2>
 11e:	64 6f                	outs   dx,DWORD PTR fs:[rsi]
 120:	41 63 74 69 6f       	movsxd esi,DWORD PTR [r9+rbp*2+0x6f]
 125:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 126:	00 6c 6f 6e          	add    BYTE PTR [rdi+rbp*2+0x6e],ch
 12a:	67 20 6c 6f 6e       	and    BYTE PTR [edi+ebp*2+0x6e],ch
 12f:	67 20 75 6e          	and    BYTE PTR [ebp+0x6e],dh
 133:	73 69                	jae    19e <_init-0xe62>
 135:	67 6e                	outs   dx,BYTE PTR ds:[esi]
 137:	65 64 20 69 6e       	gs and BYTE PTR fs:[rcx+0x6e],ch
 13c:	74 00                	je     13e <_init-0xec2>
 13e:	5f                   	pop    rdi
 13f:	49                   	rex.WB
 140:	4f 5f                	rex.WRXB pop r15
 142:	6d                   	ins    DWORD PTR es:[rdi],dx
 143:	61                   	(bad)
 144:	72 6b                	jb     1b1 <_init-0xe4f>
 146:	65 72 00             	gs jb  149 <_init-0xeb7>
 149:	5f                   	pop    rdi
 14a:	73 68                	jae    1b4 <_init-0xe4c>
 14c:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 14d:	72 74                	jb     1c3 <_init-0xe3d>
 14f:	62 75 66 00 5f 49 4f 	vmaxsh xmm9,xmm19,WORD PTR [rcx+0x9e]
 156:	5f                   	pop    rdi
 157:	72 65                	jb     1be <_init-0xe42>
 159:	61                   	(bad)
 15a:	64 5f                	fs pop rdi
 15c:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
 15e:	64 00 5f 49          	add    BYTE PTR fs:[rdi+0x49],bl
 162:	4f 5f                	rex.WRXB pop r15
 164:	77 72                	ja     1d8 <_init-0xe28>
 166:	69 74 65 5f 62 61 73 	imul   esi,DWORD PTR [rbp+riz*2+0x5f],0x65736162
 16d:	65 
 16e:	00 6e 65             	add    BYTE PTR [rsi+0x65],ch
 171:	77 5f                	ja     1d2 <_init-0xe2e>
 173:	61                   	(bad)
 174:	62 73                	(bad)
 176:	61                   	(bad)
 177:	6c                   	ins    BYTE PTR es:[rdi],dx
 178:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 179:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 17a:	2e 63 00             	cs movsxd eax,DWORD PTR [rax]
 17d:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 17e:	70 74                	jo     1f4 <_init-0xe0c>
 180:	65 72 72             	gs jb  1f5 <_init-0xe0b>
 183:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 186:	70 61                	jo     1e9 <_init-0xe17>
 188:	64 35 00 5f 49 4f    	fs xor eax,0x4f495f00
 18e:	5f                   	pop    rdi
 18f:	62 75 66 5f 65       	(bad)
 194:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 195:	64 00 6f 70          	add    BYTE PTR fs:[rdi+0x70],ch
 199:	74 69                	je     204 <_init-0xdfc>
 19b:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 19c:	64 00 5f 66          	add    BYTE PTR fs:[rdi+0x66],bl
 1a0:	72 65                	jb     207 <_init-0xdf9>
 1a2:	65 72 65             	gs jb  20a <_init-0xdf6>
 1a5:	73 5f                	jae    206 <_init-0xdfa>
 1a7:	6c                   	ins    BYTE PTR es:[rdi],dx
 1a8:	69 73 74 00 5f 75 6e 	imul   esi,DWORD PTR [rbx+0x74],0x6e755f00
 1af:	75 73                	jne    224 <_init-0xddc>
 1b1:	65 64 32 00          	gs xor al,BYTE PTR fs:[rax]
 1b5:	73 74                	jae    22b <_init-0xdd5>
 1b7:	75 64                	jne    21d <_init-0xde3>
 1b9:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
 1bb:	74 00                	je     1bd <_init-0xe43>
 1bd:	6c                   	ins    BYTE PTR es:[rdi],dx
 1be:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
 1c0:	6d                   	ins    DWORD PTR es:[rdi],dx
 1c1:	61                   	(bad)
 1c2:	78 00                	js     1c4 <_init-0xe3c>
 1c4:	73 68                	jae    22e <_init-0xdd2>
 1c6:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 1c7:	72 74                	jb     23d <_init-0xdc3>
 1c9:	20 75 6e             	and    BYTE PTR [rbp+0x6e],dh
 1cc:	73 69                	jae    237 <_init-0xdc9>
 1ce:	67 6e                	outs   dx,BYTE PTR ds:[esi]
 1d0:	65 64 20 69 6e       	gs and BYTE PTR fs:[rcx+0x6e],ch
 1d5:	74 00                	je     1d7 <_init-0xe29>
 1d7:	73 74                	jae    24d <_init-0xdb3>
 1d9:	75 64                	jne    23f <_init-0xdc1>
 1db:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
 1dd:	74 5f                	je     23e <_init-0xdc2>
 1df:	74 00                	je     1e1 <_init-0xe1f>
 1e1:	5f                   	pop    rdi
 1e2:	49                   	rex.WB
 1e3:	4f 5f                	rex.WRXB pop r15
 1e5:	77 72                	ja     259 <_init-0xda7>
 1e7:	69 74 65 5f 65 6e 64 	imul   esi,DWORD PTR [rbp+riz*2+0x5f],0x646e65
 1ee:	00 
 1ef:	5f                   	pop    rdi
 1f0:	5f                   	pop    rdi
 1f1:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 1f2:	66 66 36 34 5f       	data16 data16 ss xor al,0x5f
 1f7:	74 00                	je     1f9 <_init-0xe07>
 1f9:	72 65                	jb     260 <_init-0xda0>
 1fb:	73 70                	jae    26d <_init-0xd93>
 1fd:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 1fe:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 1ff:	73 65                	jae    266 <_init-0xd9a>
 201:	00 5f 63             	add    BYTE PTR [rdi+0x63],bl
 204:	68 61 69 6e 00       	push   0x6e6961
 209:	6c                   	ins    BYTE PTR es:[rdi],dx
 20a:	69 6e 65 6e 00 6c 69 	imul   ebp,DWORD PTR [rsi+0x65],0x696c006e
 211:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 212:	65 70 00             	gs jo  215 <_init-0xdeb>
 215:	70 72                	jo     289 <_init-0xd77>
 217:	69 6e 74 4d 65 6e 75 	imul   ebp,DWORD PTR [rsi+0x74],0x756e654d
 21e:	00 5f 6d             	add    BYTE PTR [rdi+0x6d],bl
 221:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 222:	64 65 00 5f 5f       	fs add BYTE PTR gs:[rdi+0x5f],bl
 227:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 228:	66 66 5f             	data16 pop di
 22b:	74 00                	je     22d <_init-0xdd3>
 22d:	5f                   	pop    rdi
 22e:	49                   	rex.WB
 22f:	4f 5f                	rex.WRXB pop r15
 231:	62 61                	(bad)
 233:	63 6b 75             	movsxd ebp,DWORD PTR [rbx+0x75]
 236:	70 5f                	jo     297 <_init-0xd69>
 238:	62 61                	(bad)
 23a:	73 65                	jae    2a1 <_init-0xd5f>
 23c:	00 73 74             	add    BYTE PTR [rbx+0x74],dh
 23f:	64 69 6e 00 5f 66 6c 	imul   ebp,DWORD PTR fs:[rsi+0x0],0x616c665f
 246:	61 
 247:	67 73 32             	addr32 jae 27c <_init-0xd84>
 24a:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 24d:	4f 5f                	rex.WRXB pop r15
 24f:	63 6f 64             	movsxd ebp,DWORD PTR [rdi+0x64]
 252:	65 63 76 74          	movsxd esi,DWORD PTR gs:[rsi+0x74]
 256:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 259:	4f 5f                	rex.WRXB pop r15
 25b:	72 65                	jb     2c2 <_init-0xd3e>
 25d:	61                   	(bad)
 25e:	64 5f                	fs pop rdi
 260:	62 61                	(bad)
 262:	73 65                	jae    2c9 <_init-0xd37>
 264:	00 5f 76             	add    BYTE PTR [rdi+0x76],bl
 267:	74 61                	je     2ca <_init-0xd36>
 269:	62                   	(bad)
 26a:	6c                   	ins    BYTE PTR es:[rdi],dx
 26b:	65 5f                	gs pop rdi
 26d:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 26e:	66 66 73 65          	data16 data16 jae 2d7 <_init-0xd29>
 272:	74 00                	je     274 <_init-0xd8c>
 274:	5f                   	pop    rdi
 275:	49                   	rex.WB
 276:	4f 5f                	rex.WRXB pop r15
 278:	77 69                	ja     2e3 <_init-0xd1d>
 27a:	64 65 5f             	fs gs pop rdi
 27d:	64 61                	fs (bad)
 27f:	74 61                	je     2e2 <_init-0xd1e>
 281:	00 47 4e             	add    BYTE PTR [rdi+0x4e],al
 284:	55                   	push   rbp
 285:	20 43 31             	and    BYTE PTR [rbx+0x31],al
 288:	37                   	(bad)
 289:	20 39                	and    BYTE PTR [rcx],bh
 28b:	2e 34 2e             	cs xor al,0x2e
 28e:	30 20                	xor    BYTE PTR [rax],ah
 290:	2d 6d 74 75 6e       	sub    eax,0x6e75746d
 295:	65 3d 67 65 6e 65    	gs cmp eax,0x656e6567
 29b:	72 69                	jb     306 <_init-0xcfa>
 29d:	63 20                	movsxd esp,DWORD PTR [rax]
 29f:	2d 6d 61 72 63       	sub    eax,0x6372616d
 2a4:	68 3d 78 38 36       	push   0x3638783d
 2a9:	2d 36 34 20 2d       	sub    eax,0x2d203436
 2ae:	67 20 2d 4f 30 20 2d 	and    BYTE PTR [eip+0x2d20304f],ch        # 2d203304 <_end+0x2d1ff2a4>
 2b5:	66 6e                	data16 outs dx,BYTE PTR ds:[rsi]
 2b7:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 2b8:	2d 62 75 69 6c       	sub    eax,0x6c697562
 2bd:	74 69                	je     328 <_init-0xcd8>
 2bf:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 2c0:	20 2d 66 61 73 79    	and    BYTE PTR [rip+0x79736166],ch        # 7973642c <_end+0x797323cc>
 2c6:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 2c7:	63 68 72             	movsxd ebp,DWORD PTR [rax+0x72]
 2ca:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 2cb:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 2cc:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 2cd:	75 73                	jne    342 <_init-0xcbe>
 2cf:	2d 75 6e 77 69       	sub    eax,0x69776e75
 2d4:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 2d5:	64 2d 74 61 62 6c    	fs sub eax,0x6c626174
 2db:	65 73 20             	gs jae 2fe <_init-0xd02>
 2de:	2d 66 73 74 61       	sub    eax,0x61747366
 2e3:	63 6b 2d             	movsxd ebp,DWORD PTR [rbx+0x2d]
 2e6:	70 72                	jo     35a <_init-0xca6>
 2e8:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 2e9:	74 65                	je     350 <_init-0xcb0>
 2eb:	63 74 6f 72          	movsxd esi,DWORD PTR [rdi+rbp*2+0x72]
 2ef:	2d 73 74 72 6f       	sub    eax,0x6f727473
 2f4:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 2f5:	67 20 2d 66 73 74 61 	and    BYTE PTR [eip+0x61747366],ch        # 61747662 <_end+0x61743602>
 2fc:	63 6b 2d             	movsxd ebp,DWORD PTR [rbx+0x2d]
 2ff:	63 6c 61 73          	movsxd ebp,DWORD PTR [rcx+riz*2+0x73]
 303:	68 2d 70 72 6f       	push   0x6f72702d
 308:	74 65                	je     36f <_init-0xc91>
 30a:	63 74 69 6f          	movsxd esi,DWORD PTR [rcx+rbp*2+0x6f]
 30e:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 30f:	20 2d 66 63 66 2d    	and    BYTE PTR [rip+0x2d666366],ch        # 2d66667b <_end+0x2d66261b>
 315:	70 72                	jo     389 <_init-0xc77>
 317:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 318:	74 65                	je     37f <_init-0xc81>
 31a:	63 74 69 6f          	movsxd esi,DWORD PTR [rcx+rbp*2+0x6f]
 31e:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 31f:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 322:	4f 5f                	rex.WRXB pop r15
 324:	73 61                	jae    387 <_init-0xc79>
 326:	76 65                	jbe    38d <_init-0xc73>
 328:	5f                   	pop    rdi
 329:	62 61                	(bad)
 32b:	73 65                	jae    392 <_init-0xc6e>
 32d:	00 73 79             	add    BYTE PTR [rbx+0x79],dh
 330:	73 5f                	jae    391 <_init-0xc6f>
 332:	65 72 72             	gs jb  3a7 <_init-0xc59>
 335:	6c                   	ins    BYTE PTR es:[rdi],dx
 336:	69 73 74 00 6f 70 74 	imul   esi,DWORD PTR [rbx+0x74],0x74706f00
 33d:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 33e:	70 74                	jo     3b4 <_init-0xc4c>
 340:	00 5f 66             	add    BYTE PTR [rdi+0x66],bl
 343:	69 6c 65 6e 6f 00 5f 	imul   ebp,DWORD PTR [rbp+riz*2+0x6e],0x495f006f
 34a:	49 
 34b:	4f 5f                	rex.WRXB pop r15
 34d:	72 65                	jb     3b4 <_init-0xc4c>
 34f:	61                   	(bad)
 350:	64 5f                	fs pop rdi
 352:	70 74                	jo     3c8 <_init-0xc38>
 354:	72 00                	jb     356 <_init-0xcaa>
 356:	73 74                	jae    3cc <_init-0xc34>
 358:	64 6f                	outs   dx,DWORD PTR fs:[rsi]
 35a:	75 74                	jne    3d0 <_init-0xc30>
 35c:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 35f:	4f 5f                	rex.WRXB pop r15
 361:	6c                   	ins    BYTE PTR es:[rdi],dx
 362:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 363:	63 6b 5f             	movsxd ebp,DWORD PTR [rbx+0x5f]
 366:	74 00                	je     368 <_init-0xc98>
