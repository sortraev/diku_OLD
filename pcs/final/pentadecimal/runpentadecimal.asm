
runpentadecimal:     file format elf64-x86-64


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
 367:	00 f0                	add    al,dh
 369:	fa                   	cli
 36a:	2c 04                	sub    al,0x4
 36c:	d1 f4                	shl    esp,1
 36e:	ec                   	in     al,dx
 36f:	f2 b1 44             	repnz mov cl,0x44
 372:	d2 e7                	shl    bh,cl
 374:	d3 c9                	ror    ecx,cl
 376:	7a ac                	jp     324 <_init-0xcdc>
 378:	a3                   	.byte 0xa3
 379:	83 0e 05             	or     DWORD PTR [rsi],0x5

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
 3a4:	13 00                	adc    eax,DWORD PTR [rax]
 3a6:	00 00                	add    BYTE PTR [rax],al
 3a8:	01 00                	add    DWORD PTR [rax],eax
 3aa:	00 00                	add    BYTE PTR [rax],al
 3ac:	06                   	(bad)
 3ad:	00 00                	add    BYTE PTR [rax],al
 3af:	00 00                	add    BYTE PTR [rax],al
 3b1:	01 81 00 00 00 00    	add    DWORD PTR [rcx+0x0],eax
 3b7:	02 13                	add    dl,BYTE PTR [rbx]
 3b9:	00 00                	add    BYTE PTR [rax],al
 3bb:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
 3be:	00 00                	add    BYTE PTR [rax],al
 3c0:	d1 65 ce             	shl    DWORD PTR [rbp-0x32],1
 3c3:	6d                   	ins    DWORD PTR es:[rdi],dx
 3c4:	39 f2                	cmp    edx,esi
 3c6:	8b                   	.byte 0x8b
 3c7:	1c                   	.byte 0x1c

Disassembly of section .dynsym:

00000000000003c8 <.dynsym>:
	...
 3e0:	8f 00                	pop    QWORD PTR [rax]
 3e2:	00 00                	add    BYTE PTR [rax],al
 3e4:	12 00                	adc    al,BYTE PTR [rax]
	...
 3f6:	00 00                	add    BYTE PTR [rax],al
 3f8:	be 00 00 00 20       	mov    esi,0x20000000
	...
 40d:	00 00                	add    BYTE PTR [rax],al
 40f:	00 12                	add    BYTE PTR [rdx],dl
 411:	00 00                	add    BYTE PTR [rax],al
 413:	00 12                	add    BYTE PTR [rdx],dl
	...
 425:	00 00                	add    BYTE PTR [rax],al
 427:	00 2b                	add    BYTE PTR [rbx],ch
 429:	00 00                	add    BYTE PTR [rax],al
 42b:	00 12                	add    BYTE PTR [rdx],dl
	...
 43d:	00 00                	add    BYTE PTR [rax],al
 43f:	00 54 00 00          	add    BYTE PTR [rax+rax*1+0x0],dl
 443:	00 12                	add    BYTE PTR [rdx],dl
	...
 455:	00 00                	add    BYTE PTR [rax],al
 457:	00 7d 00             	add    BYTE PTR [rbp+0x0],bh
 45a:	00 00                	add    BYTE PTR [rax],al
 45c:	12 00                	adc    al,BYTE PTR [rax]
	...
 46e:	00 00                	add    BYTE PTR [rax],al
 470:	76 00                	jbe    472 <_init-0xb8e>
 472:	00 00                	add    BYTE PTR [rax],al
 474:	12 00                	adc    al,BYTE PTR [rax]
	...
 486:	00 00                	add    BYTE PTR [rax],al
 488:	30 00                	xor    BYTE PTR [rax],al
 48a:	00 00                	add    BYTE PTR [rax],al
 48c:	12 00                	adc    al,BYTE PTR [rax]
	...
 49e:	00 00                	add    BYTE PTR [rax],al
 4a0:	5f                   	pop    rdi
 4a1:	00 00                	add    BYTE PTR [rax],al
 4a3:	00 12                	add    BYTE PTR [rdx],dl
	...
 4b5:	00 00                	add    BYTE PTR [rax],al
 4b7:	00 da                	add    dl,bl
 4b9:	00 00                	add    BYTE PTR [rax],al
 4bb:	00 20                	add    BYTE PTR [rax],ah
	...
 4cd:	00 00                	add    BYTE PTR [rax],al
 4cf:	00 38                	add    BYTE PTR [rax],bh
 4d1:	00 00                	add    BYTE PTR [rax],al
 4d3:	00 12                	add    BYTE PTR [rdx],dl
	...
 4e5:	00 00                	add    BYTE PTR [rax],al
 4e7:	00 3f                	add    BYTE PTR [rdi],bh
 4e9:	00 00                	add    BYTE PTR [rax],al
 4eb:	00 12                	add    BYTE PTR [rdx],dl
	...
 4fd:	00 00                	add    BYTE PTR [rax],al
 4ff:	00 94 00 00 00 12 00 	add    BYTE PTR [rax+rax*1+0x120000],dl
	...
 516:	00 00                	add    BYTE PTR [rax],al
 518:	23 00                	and    eax,DWORD PTR [rax]
 51a:	00 00                	add    BYTE PTR [rax],al
 51c:	12 00                	adc    al,BYTE PTR [rax]
	...
 52e:	00 00                	add    BYTE PTR [rax],al
 530:	5a                   	pop    rdx
 531:	00 00                	add    BYTE PTR [rax],al
 533:	00 12                	add    BYTE PTR [rdx],dl
	...
 545:	00 00                	add    BYTE PTR [rax],al
 547:	00 0b                	add    BYTE PTR [rbx],cl
 549:	00 00                	add    BYTE PTR [rax],al
 54b:	00 12                	add    BYTE PTR [rdx],dl
	...
 55d:	00 00                	add    BYTE PTR [rax],al
 55f:	00 4d 00             	add    BYTE PTR [rbp+0x0],cl
 562:	00 00                	add    BYTE PTR [rax],al
 564:	12 00                	adc    al,BYTE PTR [rax]
	...
 576:	00 00                	add    BYTE PTR [rax],al
 578:	e9 00 00 00 20       	jmp    2000057d <_end+0x1fffc54d>
	...
 58d:	00 00                	add    BYTE PTR [rax],al
 58f:	00 67 00             	add    BYTE PTR [rdi+0x0],ah
 592:	00 00                	add    BYTE PTR [rax],al
 594:	22 00                	and    al,BYTE PTR [rax]
	...
 5a6:	00 00                	add    BYTE PTR [rax],al
 5a8:	46 00 00             	rex.RX add BYTE PTR [rax],r8b
 5ab:	00 11                	add    BYTE PTR [rcx],dl
 5ad:	00 1a                	add    BYTE PTR [rdx],bl
 5af:	00 20                	add    BYTE PTR [rax],ah
 5b1:	40 00 00             	rex add BYTE PTR [rax],al
 5b4:	00 00                	add    BYTE PTR [rax],al
 5b6:	00 00                	add    BYTE PTR [rax],al
 5b8:	08 00                	or     BYTE PTR [rax],al
 5ba:	00 00                	add    BYTE PTR [rax],al
 5bc:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynstr:

00000000000005c0 <.dynstr>:
 5c0:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
 5c4:	63 2e                	movsxd ebp,DWORD PTR [rsi]
 5c6:	73 6f                	jae    637 <_init-0x9c9>
 5c8:	2e 36 00 70 65       	cs ss add BYTE PTR [rax+0x65],dh
 5cd:	72 72                	jb     641 <_init-0x9bf>
 5cf:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 5d0:	72 00                	jb     5d2 <_init-0xa2e>
 5d2:	5f                   	pop    rdi
 5d3:	5f                   	pop    rdi
 5d4:	73 74                	jae    64a <_init-0x9b6>
 5d6:	61                   	(bad)
 5d7:	63 6b 5f             	movsxd ebp,DWORD PTR [rbx+0x5f]
 5da:	63 68 6b             	movsxd ebp,DWORD PTR [rax+0x6b]
 5dd:	5f                   	pop    rdi
 5de:	66 61                	data16 (bad)
 5e0:	69 6c 00 72 65 61 6c 	imul   ebp,DWORD PTR [rax+rax*1+0x72],0x6c6c6165
 5e7:	6c 
 5e8:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 5e9:	63 00                	movsxd eax,DWORD PTR [rax]
 5eb:	6d                   	ins    DWORD PTR es:[rdi],dx
 5ec:	6d                   	ins    DWORD PTR es:[rdi],dx
 5ed:	61                   	(bad)
 5ee:	70 00                	jo     5f0 <_init-0xa10>
 5f0:	67 65 74 63          	addr32 gs je 657 <_init-0x9a9>
 5f4:	68 61 72 00 6d       	push   0x6d007261
 5f9:	65 6d                	gs ins DWORD PTR es:[rdi],dx
 5fb:	63 70 79             	movsxd esi,DWORD PTR [rax+0x79]
 5fe:	00 6d 61             	add    BYTE PTR [rbp+0x61],ch
 601:	6c                   	ins    BYTE PTR es:[rdi],dx
 602:	6c                   	ins    BYTE PTR es:[rdi],dx
 603:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 604:	63 00                	movsxd eax,DWORD PTR [rax]
 606:	73 74                	jae    67c <_init-0x984>
 608:	64 65 72 72          	fs gs jb 67e <_init-0x982>
 60c:	00 66 77             	add    BYTE PTR [rsi+0x77],ah
 60f:	72 69                	jb     67a <_init-0x986>
 611:	74 65                	je     678 <_init-0x988>
 613:	00 63 6c             	add    BYTE PTR [rbx+0x6c],ah
 616:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 617:	73 65                	jae    67e <_init-0x982>
 619:	00 6f 70             	add    BYTE PTR [rdi+0x70],ch
 61c:	65 6e                	outs   dx,BYTE PTR gs:[rsi]
 61e:	00 66 70             	add    BYTE PTR [rsi+0x70],ah
 621:	72 69                	jb     68c <_init-0x974>
 623:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 624:	74 66                	je     68c <_init-0x974>
 626:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 629:	63 78 61             	movsxd edi,DWORD PTR [rax+0x61]
 62c:	5f                   	pop    rdi
 62d:	66 69 6e 61 6c 69    	imul   bp,WORD PTR [rsi+0x61],0x696c
 633:	7a 65                	jp     69a <_init-0x966>
 635:	00 73 74             	add    BYTE PTR [rbx+0x74],dh
 638:	72 63                	jb     69d <_init-0x963>
 63a:	6d                   	ins    DWORD PTR es:[rdi],dx
 63b:	70 00                	jo     63d <_init-0x9c3>
 63d:	5f                   	pop    rdi
 63e:	5f                   	pop    rdi
 63f:	6c                   	ins    BYTE PTR es:[rdi],dx
 640:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
 647:	72 74                	jb     6bd <_init-0x943>
 649:	5f                   	pop    rdi
 64a:	6d                   	ins    DWORD PTR es:[rdi],dx
 64b:	61                   	(bad)
 64c:	69 6e 00 66 72 65 65 	imul   ebp,DWORD PTR [rsi+0x0],0x65657266
 653:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 656:	66 78 73             	data16 js 6cc <_init-0x934>
 659:	74 61                	je     6bc <_init-0x944>
 65b:	74 00                	je     65d <_init-0x9a3>
 65d:	47                   	rex.RXB
 65e:	4c                   	rex.WR
 65f:	49                   	rex.WB
 660:	42                   	rex.X
 661:	43 5f                	rex.XB pop r15
 663:	32 2e                	xor    ch,BYTE PTR [rsi]
 665:	31 34 00             	xor    DWORD PTR [rax+rax*1],esi
 668:	47                   	rex.RXB
 669:	4c                   	rex.WR
 66a:	49                   	rex.WB
 66b:	42                   	rex.X
 66c:	43 5f                	rex.XB pop r15
 66e:	32 2e                	xor    ch,BYTE PTR [rsi]
 670:	34 00                	xor    al,0x0
 672:	47                   	rex.RXB
 673:	4c                   	rex.WR
 674:	49                   	rex.WB
 675:	42                   	rex.X
 676:	43 5f                	rex.XB pop r15
 678:	32 2e                	xor    ch,BYTE PTR [rsi]
 67a:	32 2e                	xor    ch,BYTE PTR [rsi]
 67c:	35 00 5f 49 54       	xor    eax,0x54495f00
 681:	4d 5f                	rex.WRB pop r15
 683:	64 65 72 65          	fs gs jb 6ec <_init-0x914>
 687:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 68e:	4d 
 68f:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 691:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 692:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 693:	65 54                	gs push rsp
 695:	61                   	(bad)
 696:	62                   	(bad)
 697:	6c                   	ins    BYTE PTR es:[rdi],dx
 698:	65 00 5f 5f          	add    BYTE PTR gs:[rdi+0x5f],bl
 69c:	67 6d                	ins    DWORD PTR es:[edi],dx
 69e:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 69f:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 6a0:	5f                   	pop    rdi
 6a1:	73 74                	jae    717 <_init-0x8e9>
 6a3:	61                   	(bad)
 6a4:	72 74                	jb     71a <_init-0x8e6>
 6a6:	5f                   	pop    rdi
 6a7:	5f                   	pop    rdi
 6a8:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 6ab:	54                   	push   rsp
 6ac:	4d 5f                	rex.WRB pop r15
 6ae:	72 65                	jb     715 <_init-0x8eb>
 6b0:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 6b7:	4d 
 6b8:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 6ba:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 6bb:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 6bc:	65 54                	gs push rsp
 6be:	61                   	(bad)
 6bf:	62                   	.byte 0x62
 6c0:	6c                   	ins    BYTE PTR es:[rdi],dx
 6c1:	65                   	gs
	...

Disassembly of section .gnu.version:

00000000000006c4 <.gnu.version>:
 6c4:	00 00                	add    BYTE PTR [rax],al
 6c6:	02 00                	add    al,BYTE PTR [rax]
 6c8:	00 00                	add    BYTE PTR [rax],al
 6ca:	03 00                	add    eax,DWORD PTR [rax]
 6cc:	02 00                	add    al,BYTE PTR [rax]
 6ce:	02 00                	add    al,BYTE PTR [rax]
 6d0:	02 00                	add    al,BYTE PTR [rax]
 6d2:	02 00                	add    al,BYTE PTR [rax]
 6d4:	02 00                	add    al,BYTE PTR [rax]
 6d6:	02 00                	add    al,BYTE PTR [rax]
 6d8:	00 00                	add    BYTE PTR [rax],al
 6da:	04 00                	add    al,0x0
 6dc:	02 00                	add    al,BYTE PTR [rax]
 6de:	02 00                	add    al,BYTE PTR [rax]
 6e0:	02 00                	add    al,BYTE PTR [rax]
 6e2:	02 00                	add    al,BYTE PTR [rax]
 6e4:	02 00                	add    al,BYTE PTR [rax]
 6e6:	02 00                	add    al,BYTE PTR [rax]
 6e8:	00 00                	add    BYTE PTR [rax],al
 6ea:	02 00                	add    al,BYTE PTR [rax]
 6ec:	02 00                	add    al,BYTE PTR [rax]

Disassembly of section .gnu.version_r:

00000000000006f0 <.gnu.version_r>:
 6f0:	01 00                	add    DWORD PTR [rax],eax
 6f2:	03 00                	add    eax,DWORD PTR [rax]
 6f4:	01 00                	add    DWORD PTR [rax],eax
 6f6:	00 00                	add    BYTE PTR [rax],al
 6f8:	10 00                	adc    BYTE PTR [rax],al
 6fa:	00 00                	add    BYTE PTR [rax],al
 6fc:	00 00                	add    BYTE PTR [rax],al
 6fe:	00 00                	add    BYTE PTR [rax],al
 700:	94                   	xchg   esp,eax
 701:	91                   	xchg   ecx,eax
 702:	96                   	xchg   esi,eax
 703:	06                   	(bad)
 704:	00 00                	add    BYTE PTR [rax],al
 706:	04 00                	add    al,0x0
 708:	9d                   	popf
 709:	00 00                	add    BYTE PTR [rax],al
 70b:	00 10                	add    BYTE PTR [rax],dl
 70d:	00 00                	add    BYTE PTR [rax],al
 70f:	00 14 69             	add    BYTE PTR [rcx+rbp*2],dl
 712:	69 0d 00 00 03 00 a8 	imul   ecx,DWORD PTR [rip+0x30000],0xa8        # 3071c <_end+0x2c6ec>
 719:	00 00 00 
 71c:	10 00                	adc    BYTE PTR [rax],al
 71e:	00 00                	add    BYTE PTR [rax],al
 720:	75 1a                	jne    73c <_init-0x8c4>
 722:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
 728:	b2 00                	mov    dl,0x0
 72a:	00 00                	add    BYTE PTR [rax],al
 72c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000000730 <.rela.dyn>:
 730:	50                   	push   rax
 731:	3d 00 00 00 00       	cmp    eax,0x0
 736:	00 00                	add    BYTE PTR [rax],al
 738:	08 00                	or     BYTE PTR [rax],al
 73a:	00 00                	add    BYTE PTR [rax],al
 73c:	00 00                	add    BYTE PTR [rax],al
 73e:	00 00                	add    BYTE PTR [rax],al
 740:	e0 12                	loopne 754 <_init-0x8ac>
 742:	00 00                	add    BYTE PTR [rax],al
 744:	00 00                	add    BYTE PTR [rax],al
 746:	00 00                	add    BYTE PTR [rax],al
 748:	58                   	pop    rax
 749:	3d 00 00 00 00       	cmp    eax,0x0
 74e:	00 00                	add    BYTE PTR [rax],al
 750:	08 00                	or     BYTE PTR [rax],al
 752:	00 00                	add    BYTE PTR [rax],al
 754:	00 00                	add    BYTE PTR [rax],al
 756:	00 00                	add    BYTE PTR [rax],al
 758:	a0 12 00 00 00 00 00 	movabs al,ds:0x800000000000012
 75f:	00 08 
 761:	40 00 00             	rex add BYTE PTR [rax],al
 764:	00 00                	add    BYTE PTR [rax],al
 766:	00 00                	add    BYTE PTR [rax],al
 768:	08 00                	or     BYTE PTR [rax],al
 76a:	00 00                	add    BYTE PTR [rax],al
 76c:	00 00                	add    BYTE PTR [rax],al
 76e:	00 00                	add    BYTE PTR [rax],al
 770:	08 40 00             	or     BYTE PTR [rax+0x0],al
 773:	00 00                	add    BYTE PTR [rax],al
 775:	00 00                	add    BYTE PTR [rax],al
 777:	00 d8                	add    al,bl
 779:	3f                   	(bad)
 77a:	00 00                	add    BYTE PTR [rax],al
 77c:	00 00                	add    BYTE PTR [rax],al
 77e:	00 00                	add    BYTE PTR [rax],al
 780:	06                   	(bad)
 781:	00 00                	add    BYTE PTR [rax],al
 783:	00 02                	add    BYTE PTR [rdx],al
	...
 78d:	00 00                	add    BYTE PTR [rax],al
 78f:	00 e0                	add    al,ah
 791:	3f                   	(bad)
 792:	00 00                	add    BYTE PTR [rax],al
 794:	00 00                	add    BYTE PTR [rax],al
 796:	00 00                	add    BYTE PTR [rax],al
 798:	06                   	(bad)
 799:	00 00                	add    BYTE PTR [rax],al
 79b:	00 06                	add    BYTE PTR [rsi],al
	...
 7a5:	00 00                	add    BYTE PTR [rax],al
 7a7:	00 e8                	add    al,ch
 7a9:	3f                   	(bad)
 7aa:	00 00                	add    BYTE PTR [rax],al
 7ac:	00 00                	add    BYTE PTR [rax],al
 7ae:	00 00                	add    BYTE PTR [rax],al
 7b0:	06                   	(bad)
 7b1:	00 00                	add    BYTE PTR [rax],al
 7b3:	00 0a                	add    BYTE PTR [rdx],cl
	...
 7bd:	00 00                	add    BYTE PTR [rax],al
 7bf:	00 f0                	add    al,dh
 7c1:	3f                   	(bad)
 7c2:	00 00                	add    BYTE PTR [rax],al
 7c4:	00 00                	add    BYTE PTR [rax],al
 7c6:	00 00                	add    BYTE PTR [rax],al
 7c8:	06                   	(bad)
 7c9:	00 00                	add    BYTE PTR [rax],al
 7cb:	00 12                	add    BYTE PTR [rdx],dl
	...
 7d5:	00 00                	add    BYTE PTR [rax],al
 7d7:	00 f8                	add    al,bh
 7d9:	3f                   	(bad)
 7da:	00 00                	add    BYTE PTR [rax],al
 7dc:	00 00                	add    BYTE PTR [rax],al
 7de:	00 00                	add    BYTE PTR [rax],al
 7e0:	06                   	(bad)
 7e1:	00 00                	add    BYTE PTR [rax],al
 7e3:	00 13                	add    BYTE PTR [rbx],dl
	...
 7ed:	00 00                	add    BYTE PTR [rax],al
 7ef:	00 20                	add    BYTE PTR [rax],ah
 7f1:	40 00 00             	rex add BYTE PTR [rax],al
 7f4:	00 00                	add    BYTE PTR [rax],al
 7f6:	00 00                	add    BYTE PTR [rax],al
 7f8:	05 00 00 00 14       	add    eax,0x14000000
	...

Disassembly of section .rela.plt:

0000000000000808 <.rela.plt>:
 808:	68 3f 00 00 00       	push   0x3f
 80d:	00 00                	add    BYTE PTR [rax],al
 80f:	00 07                	add    BYTE PTR [rdi],al
 811:	00 00                	add    BYTE PTR [rax],al
 813:	00 01                	add    BYTE PTR [rcx],al
	...
 81d:	00 00                	add    BYTE PTR [rax],al
 81f:	00 70 3f             	add    BYTE PTR [rax+0x3f],dh
 822:	00 00                	add    BYTE PTR [rax],al
 824:	00 00                	add    BYTE PTR [rax],al
 826:	00 00                	add    BYTE PTR [rax],al
 828:	07                   	(bad)
 829:	00 00                	add    BYTE PTR [rax],al
 82b:	00 03                	add    BYTE PTR [rbx],al
	...
 835:	00 00                	add    BYTE PTR [rax],al
 837:	00 78 3f             	add    BYTE PTR [rax+0x3f],bh
 83a:	00 00                	add    BYTE PTR [rax],al
 83c:	00 00                	add    BYTE PTR [rax],al
 83e:	00 00                	add    BYTE PTR [rax],al
 840:	07                   	(bad)
 841:	00 00                	add    BYTE PTR [rax],al
 843:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...
 84e:	00 00                	add    BYTE PTR [rax],al
 850:	80 3f 00             	cmp    BYTE PTR [rdi],0x0
 853:	00 00                	add    BYTE PTR [rax],al
 855:	00 00                	add    BYTE PTR [rax],al
 857:	00 07                	add    BYTE PTR [rdi],al
 859:	00 00                	add    BYTE PTR [rax],al
 85b:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 861 <_init-0x79f>
 861:	00 00                	add    BYTE PTR [rax],al
 863:	00 00                	add    BYTE PTR [rax],al
 865:	00 00                	add    BYTE PTR [rax],al
 867:	00 88 3f 00 00 00    	add    BYTE PTR [rax+0x3f],cl
 86d:	00 00                	add    BYTE PTR [rax],al
 86f:	00 07                	add    BYTE PTR [rdi],al
 871:	00 00                	add    BYTE PTR [rax],al
 873:	00 07                	add    BYTE PTR [rdi],al
	...
 87d:	00 00                	add    BYTE PTR [rax],al
 87f:	00 90 3f 00 00 00    	add    BYTE PTR [rax+0x3f],dl
 885:	00 00                	add    BYTE PTR [rax],al
 887:	00 07                	add    BYTE PTR [rdi],al
 889:	00 00                	add    BYTE PTR [rax],al
 88b:	00 08                	add    BYTE PTR [rax],cl
	...
 895:	00 00                	add    BYTE PTR [rax],al
 897:	00 98 3f 00 00 00    	add    BYTE PTR [rax+0x3f],bl
 89d:	00 00                	add    BYTE PTR [rax],al
 89f:	00 07                	add    BYTE PTR [rdi],al
 8a1:	00 00                	add    BYTE PTR [rax],al
 8a3:	00 09                	add    BYTE PTR [rcx],cl
	...
 8ad:	00 00                	add    BYTE PTR [rax],al
 8af:	00 a0 3f 00 00 00    	add    BYTE PTR [rax+0x3f],ah
 8b5:	00 00                	add    BYTE PTR [rax],al
 8b7:	00 07                	add    BYTE PTR [rdi],al
 8b9:	00 00                	add    BYTE PTR [rax],al
 8bb:	00 0b                	add    BYTE PTR [rbx],cl
	...
 8c5:	00 00                	add    BYTE PTR [rax],al
 8c7:	00 a8 3f 00 00 00    	add    BYTE PTR [rax+0x3f],ch
 8cd:	00 00                	add    BYTE PTR [rax],al
 8cf:	00 07                	add    BYTE PTR [rdi],al
 8d1:	00 00                	add    BYTE PTR [rax],al
 8d3:	00 0c 00             	add    BYTE PTR [rax+rax*1],cl
	...
 8de:	00 00                	add    BYTE PTR [rax],al
 8e0:	b0 3f                	mov    al,0x3f
 8e2:	00 00                	add    BYTE PTR [rax],al
 8e4:	00 00                	add    BYTE PTR [rax],al
 8e6:	00 00                	add    BYTE PTR [rax],al
 8e8:	07                   	(bad)
 8e9:	00 00                	add    BYTE PTR [rax],al
 8eb:	00 0d 00 00 00 00    	add    BYTE PTR [rip+0x0],cl        # 8f1 <_init-0x70f>
 8f1:	00 00                	add    BYTE PTR [rax],al
 8f3:	00 00                	add    BYTE PTR [rax],al
 8f5:	00 00                	add    BYTE PTR [rax],al
 8f7:	00 b8 3f 00 00 00    	add    BYTE PTR [rax+0x3f],bh
 8fd:	00 00                	add    BYTE PTR [rax],al
 8ff:	00 07                	add    BYTE PTR [rdi],al
 901:	00 00                	add    BYTE PTR [rax],al
 903:	00 0e                	add    BYTE PTR [rsi],cl
	...
 90d:	00 00                	add    BYTE PTR [rax],al
 90f:	00 c0                	add    al,al
 911:	3f                   	(bad)
 912:	00 00                	add    BYTE PTR [rax],al
 914:	00 00                	add    BYTE PTR [rax],al
 916:	00 00                	add    BYTE PTR [rax],al
 918:	07                   	(bad)
 919:	00 00                	add    BYTE PTR [rax],al
 91b:	00 0f                	add    BYTE PTR [rdi],cl
	...
 925:	00 00                	add    BYTE PTR [rax],al
 927:	00 c8                	add    al,cl
 929:	3f                   	(bad)
 92a:	00 00                	add    BYTE PTR [rax],al
 92c:	00 00                	add    BYTE PTR [rax],al
 92e:	00 00                	add    BYTE PTR [rax],al
 930:	07                   	(bad)
 931:	00 00                	add    BYTE PTR [rax],al
 933:	00 10                	add    BYTE PTR [rax],dl
	...
 93d:	00 00                	add    BYTE PTR [rax],al
 93f:	00 d0                	add    al,dl
 941:	3f                   	(bad)
 942:	00 00                	add    BYTE PTR [rax],al
 944:	00 00                	add    BYTE PTR [rax],al
 946:	00 00                	add    BYTE PTR [rax],al
 948:	07                   	(bad)
 949:	00 00                	add    BYTE PTR [rax],al
 94b:	00 11                	add    BYTE PTR [rcx],dl
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
    1020:	ff 35 32 2f 00 00    	push   QWORD PTR [rip+0x2f32]        # 3f58 <_GLOBAL_OFFSET_TABLE_+0x8>
    1026:	f2 ff 25 33 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f33]        # 3f60 <_GLOBAL_OFFSET_TABLE_+0x10>
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

Disassembly of section .plt.got:

0000000000001110 <__cxa_finalize@plt>:
    1110:	f3 0f 1e fa          	endbr64
    1114:	f2 ff 25 dd 2e 00 00 	bnd jmp QWORD PTR [rip+0x2edd]        # 3ff8 <__cxa_finalize@GLIBC_2.2.5>
    111b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .plt.sec:

0000000000001120 <free@plt>:
    1120:	f3 0f 1e fa          	endbr64
    1124:	f2 ff 25 3d 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e3d]        # 3f68 <free@GLIBC_2.2.5>
    112b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001130 <__stack_chk_fail@plt>:
    1130:	f3 0f 1e fa          	endbr64
    1134:	f2 ff 25 35 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e35]        # 3f70 <__stack_chk_fail@GLIBC_2.4>
    113b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001140 <mmap@plt>:
    1140:	f3 0f 1e fa          	endbr64
    1144:	f2 ff 25 2d 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e2d]        # 3f78 <mmap@GLIBC_2.2.5>
    114b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001150 <close@plt>:
    1150:	f3 0f 1e fa          	endbr64
    1154:	f2 ff 25 25 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e25]        # 3f80 <close@GLIBC_2.2.5>
    115b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001160 <strcmp@plt>:
    1160:	f3 0f 1e fa          	endbr64
    1164:	f2 ff 25 1d 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e1d]        # 3f88 <strcmp@GLIBC_2.2.5>
    116b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001170 <getchar@plt>:
    1170:	f3 0f 1e fa          	endbr64
    1174:	f2 ff 25 15 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e15]        # 3f90 <getchar@GLIBC_2.2.5>
    117b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001180 <fprintf@plt>:
    1180:	f3 0f 1e fa          	endbr64
    1184:	f2 ff 25 0d 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e0d]        # 3f98 <fprintf@GLIBC_2.2.5>
    118b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001190 <memcpy@plt>:
    1190:	f3 0f 1e fa          	endbr64
    1194:	f2 ff 25 05 2e 00 00 	bnd jmp QWORD PTR [rip+0x2e05]        # 3fa0 <memcpy@GLIBC_2.14>
    119b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011a0 <malloc@plt>:
    11a0:	f3 0f 1e fa          	endbr64
    11a4:	f2 ff 25 fd 2d 00 00 	bnd jmp QWORD PTR [rip+0x2dfd]        # 3fa8 <malloc@GLIBC_2.2.5>
    11ab:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011b0 <__fxstat@plt>:
    11b0:	f3 0f 1e fa          	endbr64
    11b4:	f2 ff 25 f5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2df5]        # 3fb0 <__fxstat@GLIBC_2.2.5>
    11bb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011c0 <realloc@plt>:
    11c0:	f3 0f 1e fa          	endbr64
    11c4:	f2 ff 25 ed 2d 00 00 	bnd jmp QWORD PTR [rip+0x2ded]        # 3fb8 <realloc@GLIBC_2.2.5>
    11cb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011d0 <open@plt>:
    11d0:	f3 0f 1e fa          	endbr64
    11d4:	f2 ff 25 e5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2de5]        # 3fc0 <open@GLIBC_2.2.5>
    11db:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011e0 <perror@plt>:
    11e0:	f3 0f 1e fa          	endbr64
    11e4:	f2 ff 25 dd 2d 00 00 	bnd jmp QWORD PTR [rip+0x2ddd]        # 3fc8 <perror@GLIBC_2.2.5>
    11eb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000000011f0 <fwrite@plt>:
    11f0:	f3 0f 1e fa          	endbr64
    11f4:	f2 ff 25 d5 2d 00 00 	bnd jmp QWORD PTR [rip+0x2dd5]        # 3fd0 <fwrite@GLIBC_2.2.5>
    11fb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .text:

0000000000001200 <_start>:
    1200:	f3 0f 1e fa          	endbr64
    1204:	31 ed                	xor    ebp,ebp
    1206:	49 89 d1             	mov    r9,rdx
    1209:	5e                   	pop    rsi
    120a:	48 89 e2             	mov    rdx,rsp
    120d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
    1211:	50                   	push   rax
    1212:	54                   	push   rsp
    1213:	4c 8d 05 56 06 00 00 	lea    r8,[rip+0x656]        # 1870 <__libc_csu_fini>
    121a:	48 8d 0d df 05 00 00 	lea    rcx,[rip+0x5df]        # 1800 <__libc_csu_init>
    1221:	48 8d 3d c1 00 00 00 	lea    rdi,[rip+0xc1]        # 12e9 <main>
    1228:	ff 15 b2 2d 00 00    	call   QWORD PTR [rip+0x2db2]        # 3fe0 <__libc_start_main@GLIBC_2.2.5>
    122e:	f4                   	hlt
    122f:	90                   	nop

0000000000001230 <deregister_tm_clones>:
    1230:	48 8d 3d d9 2d 00 00 	lea    rdi,[rip+0x2dd9]        # 4010 <__TMC_END__>
    1237:	48 8d 05 d2 2d 00 00 	lea    rax,[rip+0x2dd2]        # 4010 <__TMC_END__>
    123e:	48 39 f8             	cmp    rax,rdi
    1241:	74 15                	je     1258 <deregister_tm_clones+0x28>
    1243:	48 8b 05 8e 2d 00 00 	mov    rax,QWORD PTR [rip+0x2d8e]        # 3fd8 <_ITM_deregisterTMCloneTable>
    124a:	48 85 c0             	test   rax,rax
    124d:	74 09                	je     1258 <deregister_tm_clones+0x28>
    124f:	ff e0                	jmp    rax
    1251:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1258:	c3                   	ret
    1259:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

0000000000001260 <register_tm_clones>:
    1260:	48 8d 3d a9 2d 00 00 	lea    rdi,[rip+0x2da9]        # 4010 <__TMC_END__>
    1267:	48 8d 35 a2 2d 00 00 	lea    rsi,[rip+0x2da2]        # 4010 <__TMC_END__>
    126e:	48 29 fe             	sub    rsi,rdi
    1271:	48 89 f0             	mov    rax,rsi
    1274:	48 c1 ee 3f          	shr    rsi,0x3f
    1278:	48 c1 f8 03          	sar    rax,0x3
    127c:	48 01 c6             	add    rsi,rax
    127f:	48 d1 fe             	sar    rsi,1
    1282:	74 14                	je     1298 <register_tm_clones+0x38>
    1284:	48 8b 05 65 2d 00 00 	mov    rax,QWORD PTR [rip+0x2d65]        # 3ff0 <_ITM_registerTMCloneTable>
    128b:	48 85 c0             	test   rax,rax
    128e:	74 08                	je     1298 <register_tm_clones+0x38>
    1290:	ff e0                	jmp    rax
    1292:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]
    1298:	c3                   	ret
    1299:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

00000000000012a0 <__do_global_dtors_aux>:
    12a0:	f3 0f 1e fa          	endbr64
    12a4:	80 3d 7d 2d 00 00 00 	cmp    BYTE PTR [rip+0x2d7d],0x0        # 4028 <completed.8061>
    12ab:	75 2b                	jne    12d8 <__do_global_dtors_aux+0x38>
    12ad:	55                   	push   rbp
    12ae:	48 83 3d 42 2d 00 00 	cmp    QWORD PTR [rip+0x2d42],0x0        # 3ff8 <__cxa_finalize@GLIBC_2.2.5>
    12b5:	00 
    12b6:	48 89 e5             	mov    rbp,rsp
    12b9:	74 0c                	je     12c7 <__do_global_dtors_aux+0x27>
    12bb:	48 8b 3d 46 2d 00 00 	mov    rdi,QWORD PTR [rip+0x2d46]        # 4008 <__dso_handle>
    12c2:	e8 49 fe ff ff       	call   1110 <__cxa_finalize@plt>
    12c7:	e8 64 ff ff ff       	call   1230 <deregister_tm_clones>
    12cc:	c6 05 55 2d 00 00 01 	mov    BYTE PTR [rip+0x2d55],0x1        # 4028 <completed.8061>
    12d3:	5d                   	pop    rbp
    12d4:	c3                   	ret
    12d5:	0f 1f 00             	nop    DWORD PTR [rax]
    12d8:	c3                   	ret
    12d9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

00000000000012e0 <frame_dummy>:
    12e0:	f3 0f 1e fa          	endbr64
    12e4:	e9 77 ff ff ff       	jmp    1260 <register_tm_clones>

00000000000012e9 <main>:
    12e9:	f3 0f 1e fa          	endbr64
    12ed:	55                   	push   rbp
    12ee:	48 89 e5             	mov    rbp,rsp
    12f1:	4c 8d 9c 24 00 80 ff 	lea    r11,[rsp-0x8000]
    12f8:	ff 
    12f9:	48 81 ec 00 10 00 00 	sub    rsp,0x1000
    1300:	48 83 0c 24 00       	or     QWORD PTR [rsp],0x0
    1305:	4c 39 dc             	cmp    rsp,r11
    1308:	75 ef                	jne    12f9 <main+0x10>
    130a:	48 83 ec 50          	sub    rsp,0x50
    130e:	89 bd bc 7f ff ff    	mov    DWORD PTR [rbp-0x8044],edi
    1314:	48 89 b5 b0 7f ff ff 	mov    QWORD PTR [rbp-0x8050],rsi
    131b:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    1322:	00 00 
    1324:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    1328:	31 c0                	xor    eax,eax
    132a:	83 bd bc 7f ff ff 01 	cmp    DWORD PTR [rbp-0x8044],0x1
    1331:	7f 2f                	jg     1362 <main+0x79>
    1333:	48 8b 85 b0 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8050]
    133a:	48 8b 10             	mov    rdx,QWORD PTR [rax]
    133d:	48 8b 05 dc 2c 00 00 	mov    rax,QWORD PTR [rip+0x2cdc]        # 4020 <stderr@GLIBC_2.2.5>
    1344:	48 8d 35 bd 0c 00 00 	lea    rsi,[rip+0xcbd]        # 2008 <_IO_stdin_used+0x8>
    134b:	48 89 c7             	mov    rdi,rax
    134e:	b8 00 00 00 00       	mov    eax,0x0
    1353:	e8 28 fe ff ff       	call   1180 <fprintf@plt>
    1358:	b8 01 00 00 00       	mov    eax,0x1
    135d:	e9 83 04 00 00       	jmp    17e5 <main+0x4fc>
    1362:	48 8b 85 b0 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8050]
    1369:	48 83 c0 08          	add    rax,0x8
    136d:	48 8b 00             	mov    rax,QWORD PTR [rax]
    1370:	48 8d 35 ab 0c 00 00 	lea    rsi,[rip+0xcab]        # 2022 <_IO_stdin_used+0x22>
    1377:	48 89 c7             	mov    rdi,rax
    137a:	e8 e1 fd ff ff       	call   1160 <strcmp@plt>
    137f:	85 c0                	test   eax,eax
    1381:	0f 85 80 01 00 00    	jne    1507 <main+0x21e>
    1387:	c7 85 d0 7f ff ff 00 	mov    DWORD PTR [rbp-0x8030],0x1000
    138e:	10 00 00 
    1391:	8b 85 d0 7f ff ff    	mov    eax,DWORD PTR [rbp-0x8030]
    1397:	48 98                	cdqe
    1399:	48 89 c7             	mov    rdi,rax
    139c:	e8 ff fd ff ff       	call   11a0 <malloc@plt>
    13a1:	48 89 85 e8 7f ff ff 	mov    QWORD PTR [rbp-0x8018],rax
    13a8:	c7 85 cc 7f ff ff 00 	mov    DWORD PTR [rbp-0x8034],0x0
    13af:	00 00 00 
    13b2:	c7 85 c8 7f ff ff 00 	mov    DWORD PTR [rbp-0x8038],0x0
    13b9:	00 00 00 
    13bc:	c7 85 d4 7f ff ff 00 	mov    DWORD PTR [rbp-0x802c],0x0
    13c3:	00 00 00 
    13c6:	e9 9c 00 00 00       	jmp    1467 <main+0x17e>
    13cb:	8b 85 d4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x802c]
    13d1:	3b 85 d0 7f ff ff    	cmp    eax,DWORD PTR [rbp-0x8030]
    13d7:	75 2c                	jne    1405 <main+0x11c>
    13d9:	81 85 d0 7f ff ff 00 	add    DWORD PTR [rbp-0x8030],0x1000
    13e0:	10 00 00 
    13e3:	8b 85 d0 7f ff ff    	mov    eax,DWORD PTR [rbp-0x8030]
    13e9:	48 63 d0             	movsxd rdx,eax
    13ec:	48 8b 85 e8 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8018]
    13f3:	48 89 d6             	mov    rsi,rdx
    13f6:	48 89 c7             	mov    rdi,rax
    13f9:	e8 c2 fd ff ff       	call   11c0 <realloc@plt>
    13fe:	48 89 85 e8 7f ff ff 	mov    QWORD PTR [rbp-0x8018],rax
    1405:	8b 85 d4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x802c]
    140b:	8d 50 01             	lea    edx,[rax+0x1]
    140e:	89 95 d4 7f ff ff    	mov    DWORD PTR [rbp-0x802c],edx
    1414:	48 63 d0             	movsxd rdx,eax
    1417:	48 8b 85 e8 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8018]
    141e:	48 01 d0             	add    rax,rdx
    1421:	8b 95 dc 7f ff ff    	mov    edx,DWORD PTR [rbp-0x8024]
    1427:	88 10                	mov    BYTE PTR [rax],dl
    1429:	83 bd dc 7f ff ff 00 	cmp    DWORD PTR [rbp-0x8024],0x0
    1430:	75 07                	jne    1439 <main+0x150>
    1432:	83 85 cc 7f ff ff 01 	add    DWORD PTR [rbp-0x8034],0x1
    1439:	8b 85 dc 7f ff ff    	mov    eax,DWORD PTR [rbp-0x8024]
    143f:	83 e0 0f             	and    eax,0xf
    1442:	83 f8 0f             	cmp    eax,0xf
    1445:	75 07                	jne    144e <main+0x165>
    1447:	83 85 c8 7f ff ff 01 	add    DWORD PTR [rbp-0x8038],0x1
    144e:	8b 85 dc 7f ff ff    	mov    eax,DWORD PTR [rbp-0x8024]
    1454:	25 f0 00 00 00       	and    eax,0xf0
    1459:	3d f0 00 00 00       	cmp    eax,0xf0
    145e:	75 07                	jne    1467 <main+0x17e>
    1460:	83 85 c8 7f ff ff 01 	add    DWORD PTR [rbp-0x8038],0x1
    1467:	e8 04 fd ff ff       	call   1170 <getchar@plt>
    146c:	89 85 dc 7f ff ff    	mov    DWORD PTR [rbp-0x8024],eax
    1472:	83 bd dc 7f ff ff ff 	cmp    DWORD PTR [rbp-0x8024],0xffffffff
    1479:	0f 85 4c ff ff ff    	jne    13cb <main+0xe2>
    147f:	8b 85 d4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x802c]
    1485:	48 98                	cdqe
    1487:	41 b9 00 00 00 00    	mov    r9d,0x0
    148d:	41 b8 ff ff ff ff    	mov    r8d,0xffffffff
    1493:	b9 22 00 00 00       	mov    ecx,0x22
    1498:	ba 07 00 00 00       	mov    edx,0x7
    149d:	48 89 c6             	mov    rsi,rax
    14a0:	bf 00 00 00 00       	mov    edi,0x0
    14a5:	e8 96 fc ff ff       	call   1140 <mmap@plt>
    14aa:	48 89 85 e0 7f ff ff 	mov    QWORD PTR [rbp-0x8020],rax
    14b1:	48 83 bd e0 7f ff ff 	cmp    QWORD PTR [rbp-0x8020],0xffffffffffffffff
    14b8:	ff 
    14b9:	75 16                	jne    14d1 <main+0x1e8>
    14bb:	48 8d 3d 62 0b 00 00 	lea    rdi,[rip+0xb62]        # 2024 <_IO_stdin_used+0x24>
    14c2:	e8 19 fd ff ff       	call   11e0 <perror@plt>
    14c7:	b8 01 00 00 00       	mov    eax,0x1
    14cc:	e9 14 03 00 00       	jmp    17e5 <main+0x4fc>
    14d1:	8b 85 d4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x802c]
    14d7:	48 63 d0             	movsxd rdx,eax
    14da:	48 8b 8d e8 7f ff ff 	mov    rcx,QWORD PTR [rbp-0x8018]
    14e1:	48 8b 85 e0 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8020]
    14e8:	48 89 ce             	mov    rsi,rcx
    14eb:	48 89 c7             	mov    rdi,rax
    14ee:	e8 9d fc ff ff       	call   1190 <memcpy@plt>
    14f3:	48 8b 85 e8 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8018]
    14fa:	48 89 c7             	mov    rdi,rax
    14fd:	e8 1e fc ff ff       	call   1120 <free@plt>
    1502:	e9 a7 01 00 00       	jmp    16ae <main+0x3c5>
    1507:	48 8b 85 b0 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8050]
    150e:	48 83 c0 08          	add    rax,0x8
    1512:	48 8b 00             	mov    rax,QWORD PTR [rax]
    1515:	be 00 00 00 00       	mov    esi,0x0
    151a:	48 89 c7             	mov    rdi,rax
    151d:	b8 00 00 00 00       	mov    eax,0x0
    1522:	e8 a9 fc ff ff       	call   11d0 <open@plt>
    1527:	89 85 d8 7f ff ff    	mov    DWORD PTR [rbp-0x8028],eax
    152d:	83 bd d8 7f ff ff 00 	cmp    DWORD PTR [rbp-0x8028],0x0
    1534:	79 16                	jns    154c <main+0x263>
    1536:	48 8d 3d f5 0a 00 00 	lea    rdi,[rip+0xaf5]        # 2032 <_IO_stdin_used+0x32>
    153d:	e8 9e fc ff ff       	call   11e0 <perror@plt>
    1542:	b8 01 00 00 00       	mov    eax,0x1
    1547:	e9 99 02 00 00       	jmp    17e5 <main+0x4fc>
    154c:	48 8d 95 f0 7f ff ff 	lea    rdx,[rbp-0x8010]
    1553:	8b 85 d8 7f ff ff    	mov    eax,DWORD PTR [rbp-0x8028]
    1559:	48 89 d6             	mov    rsi,rdx
    155c:	89 c7                	mov    edi,eax
    155e:	e8 1d 03 00 00       	call   1880 <__fstat>
    1563:	85 c0                	test   eax,eax
    1565:	74 16                	je     157d <main+0x294>
    1567:	48 8d 3d d2 0a 00 00 	lea    rdi,[rip+0xad2]        # 2040 <_IO_stdin_used+0x40>
    156e:	e8 6d fc ff ff       	call   11e0 <perror@plt>
    1573:	b8 01 00 00 00       	mov    eax,0x1
    1578:	e9 68 02 00 00       	jmp    17e5 <main+0x4fc>
    157d:	48 8b 85 20 80 ff ff 	mov    rax,QWORD PTR [rbp-0x7fe0]
    1584:	48 89 c6             	mov    rsi,rax
    1587:	8b 85 d8 7f ff ff    	mov    eax,DWORD PTR [rbp-0x8028]
    158d:	41 b9 00 00 00 00    	mov    r9d,0x0
    1593:	41 89 c0             	mov    r8d,eax
    1596:	b9 02 00 00 00       	mov    ecx,0x2
    159b:	ba 07 00 00 00       	mov    edx,0x7
    15a0:	bf 00 00 00 00       	mov    edi,0x0
    15a5:	e8 96 fb ff ff       	call   1140 <mmap@plt>
    15aa:	48 89 85 e0 7f ff ff 	mov    QWORD PTR [rbp-0x8020],rax
    15b1:	48 83 bd e0 7f ff ff 	cmp    QWORD PTR [rbp-0x8020],0xffffffffffffffff
    15b8:	ff 
    15b9:	75 16                	jne    15d1 <main+0x2e8>
    15bb:	48 8d 3d 62 0a 00 00 	lea    rdi,[rip+0xa62]        # 2024 <_IO_stdin_used+0x24>
    15c2:	e8 19 fc ff ff       	call   11e0 <perror@plt>
    15c7:	b8 01 00 00 00       	mov    eax,0x1
    15cc:	e9 14 02 00 00       	jmp    17e5 <main+0x4fc>
    15d1:	8b 85 d8 7f ff ff    	mov    eax,DWORD PTR [rbp-0x8028]
    15d7:	89 c7                	mov    edi,eax
    15d9:	e8 72 fb ff ff       	call   1150 <close@plt>
    15de:	c7 85 c4 7f ff ff 00 	mov    DWORD PTR [rbp-0x803c],0x0
    15e5:	00 00 00 
    15e8:	c7 85 c8 7f ff ff 00 	mov    DWORD PTR [rbp-0x8038],0x0
    15ef:	00 00 00 
    15f2:	c7 85 cc 7f ff ff 00 	mov    DWORD PTR [rbp-0x8034],0x0
    15f9:	00 00 00 
    15fc:	48 8b 85 e0 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8020]
    1603:	48 89 85 e8 7f ff ff 	mov    QWORD PTR [rbp-0x8018],rax
    160a:	eb 7c                	jmp    1688 <main+0x39f>
    160c:	8b 85 c4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x803c]
    1612:	48 63 d0             	movsxd rdx,eax
    1615:	48 8b 85 e8 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8018]
    161c:	48 01 d0             	add    rax,rdx
    161f:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    1622:	84 c0                	test   al,al
    1624:	75 07                	jne    162d <main+0x344>
    1626:	83 85 cc 7f ff ff 01 	add    DWORD PTR [rbp-0x8034],0x1
    162d:	8b 85 c4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x803c]
    1633:	48 63 d0             	movsxd rdx,eax
    1636:	48 8b 85 e8 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8018]
    163d:	48 01 d0             	add    rax,rdx
    1640:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    1643:	0f b6 c0             	movzx  eax,al
    1646:	83 e0 0f             	and    eax,0xf
    1649:	83 f8 0f             	cmp    eax,0xf
    164c:	75 07                	jne    1655 <main+0x36c>
    164e:	83 85 c8 7f ff ff 01 	add    DWORD PTR [rbp-0x8038],0x1
    1655:	8b 85 c4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x803c]
    165b:	48 63 d0             	movsxd rdx,eax
    165e:	48 8b 85 e8 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8018]
    1665:	48 01 d0             	add    rax,rdx
    1668:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    166b:	0f b6 c0             	movzx  eax,al
    166e:	25 f0 00 00 00       	and    eax,0xf0
    1673:	3d f0 00 00 00       	cmp    eax,0xf0
    1678:	75 07                	jne    1681 <main+0x398>
    167a:	83 85 c8 7f ff ff 01 	add    DWORD PTR [rbp-0x8038],0x1
    1681:	83 85 c4 7f ff ff 01 	add    DWORD PTR [rbp-0x803c],0x1
    1688:	8b 85 c4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x803c]
    168e:	48 63 d0             	movsxd rdx,eax
    1691:	48 8b 85 20 80 ff ff 	mov    rax,QWORD PTR [rbp-0x7fe0]
    1698:	48 39 c2             	cmp    rdx,rax
    169b:	0f 8c 6b ff ff ff    	jl     160c <main+0x323>
    16a1:	48 8b 85 20 80 ff ff 	mov    rax,QWORD PTR [rbp-0x7fe0]
    16a8:	89 85 d4 7f ff ff    	mov    DWORD PTR [rbp-0x802c],eax
    16ae:	83 ad bc 7f ff ff 01 	sub    DWORD PTR [rbp-0x8044],0x1
    16b5:	c7 85 c4 7f ff ff 00 	mov    DWORD PTR [rbp-0x803c],0x0
    16bc:	00 00 00 
    16bf:	eb 46                	jmp    1707 <main+0x41e>
    16c1:	8b 85 c4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x803c]
    16c7:	48 98                	cdqe
    16c9:	48 83 c0 01          	add    rax,0x1
    16cd:	48 8d 14 c5 00 00 00 	lea    rdx,[rax*8+0x0]
    16d4:	00 
    16d5:	48 8b 85 b0 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8050]
    16dc:	48 01 d0             	add    rax,rdx
    16df:	8b 95 c4 7f ff ff    	mov    edx,DWORD PTR [rbp-0x803c]
    16e5:	48 63 d2             	movsxd rdx,edx
    16e8:	48 8d 0c d5 00 00 00 	lea    rcx,[rdx*8+0x0]
    16ef:	00 
    16f0:	48 8b 95 b0 7f ff ff 	mov    rdx,QWORD PTR [rbp-0x8050]
    16f7:	48 01 ca             	add    rdx,rcx
    16fa:	48 8b 00             	mov    rax,QWORD PTR [rax]
    16fd:	48 89 02             	mov    QWORD PTR [rdx],rax
    1700:	83 85 c4 7f ff ff 01 	add    DWORD PTR [rbp-0x803c],0x1
    1707:	8b 85 c4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x803c]
    170d:	3b 85 bc 7f ff ff    	cmp    eax,DWORD PTR [rbp-0x8044]
    1713:	7c ac                	jl     16c1 <main+0x3d8>
    1715:	48 8b 05 04 29 00 00 	mov    rax,QWORD PTR [rip+0x2904]        # 4020 <stderr@GLIBC_2.2.5>
    171c:	8b 95 d4 7f ff ff    	mov    edx,DWORD PTR [rbp-0x802c]
    1722:	48 8d 35 27 09 00 00 	lea    rsi,[rip+0x927]        # 2050 <_IO_stdin_used+0x50>
    1729:	48 89 c7             	mov    rdi,rax
    172c:	b8 00 00 00 00       	mov    eax,0x0
    1731:	e8 4a fa ff ff       	call   1180 <fprintf@plt>
    1736:	83 bd d4 7f ff ff 32 	cmp    DWORD PTR [rbp-0x802c],0x32
    173d:	7e 24                	jle    1763 <main+0x47a>
    173f:	8b 85 d4 7f ff ff    	mov    eax,DWORD PTR [rbp-0x802c]
    1745:	8d 50 ce             	lea    edx,[rax-0x32]
    1748:	48 8b 05 d1 28 00 00 	mov    rax,QWORD PTR [rip+0x28d1]        # 4020 <stderr@GLIBC_2.2.5>
    174f:	48 8d 35 22 09 00 00 	lea    rsi,[rip+0x922]        # 2078 <_IO_stdin_used+0x78>
    1756:	48 89 c7             	mov    rdi,rax
    1759:	b8 00 00 00 00       	mov    eax,0x0
    175e:	e8 1d fa ff ff       	call   1180 <fprintf@plt>
    1763:	83 bd cc 7f ff ff 00 	cmp    DWORD PTR [rbp-0x8034],0x0
    176a:	7e 21                	jle    178d <main+0x4a4>
    176c:	48 8b 05 ad 28 00 00 	mov    rax,QWORD PTR [rip+0x28ad]        # 4020 <stderr@GLIBC_2.2.5>
    1773:	8b 95 cc 7f ff ff    	mov    edx,DWORD PTR [rbp-0x8034]
    1779:	48 8d 35 20 09 00 00 	lea    rsi,[rip+0x920]        # 20a0 <_IO_stdin_used+0xa0>
    1780:	48 89 c7             	mov    rdi,rax
    1783:	b8 00 00 00 00       	mov    eax,0x0
    1788:	e8 f3 f9 ff ff       	call   1180 <fprintf@plt>
    178d:	83 bd c8 7f ff ff 00 	cmp    DWORD PTR [rbp-0x8038],0x0
    1794:	7e 21                	jle    17b7 <main+0x4ce>
    1796:	48 8b 05 83 28 00 00 	mov    rax,QWORD PTR [rip+0x2883]        # 4020 <stderr@GLIBC_2.2.5>
    179d:	8b 95 c8 7f ff ff    	mov    edx,DWORD PTR [rbp-0x8038]
    17a3:	48 8d 35 1e 09 00 00 	lea    rsi,[rip+0x91e]        # 20c8 <_IO_stdin_used+0xc8>
    17aa:	48 89 c7             	mov    rdi,rax
    17ad:	b8 00 00 00 00       	mov    eax,0x0
    17b2:	e8 c9 f9 ff ff       	call   1180 <fprintf@plt>
    17b7:	48 8b 05 62 28 00 00 	mov    rax,QWORD PTR [rip+0x2862]        # 4020 <stderr@GLIBC_2.2.5>
    17be:	48 89 c1             	mov    rcx,rax
    17c1:	ba 1a 00 00 00       	mov    edx,0x1a
    17c6:	be 01 00 00 00       	mov    esi,0x1
    17cb:	48 8d 3d 17 09 00 00 	lea    rdi,[rip+0x917]        # 20e9 <_IO_stdin_used+0xe9>
    17d2:	e8 19 fa ff ff       	call   11f0 <fwrite@plt>
    17d7:	48 8b 85 e0 7f ff ff 	mov    rax,QWORD PTR [rbp-0x8020]
    17de:	ff d0                	call   rax
    17e0:	b8 01 00 00 00       	mov    eax,0x1
    17e5:	48 8b 4d f8          	mov    rcx,QWORD PTR [rbp-0x8]
    17e9:	64 48 33 0c 25 28 00 	xor    rcx,QWORD PTR fs:0x28
    17f0:	00 00 
    17f2:	74 05                	je     17f9 <main+0x510>
    17f4:	e8 37 f9 ff ff       	call   1130 <__stack_chk_fail@plt>
    17f9:	c9                   	leave
    17fa:	c3                   	ret
    17fb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001800 <__libc_csu_init>:
    1800:	f3 0f 1e fa          	endbr64
    1804:	41 57                	push   r15
    1806:	4c 8d 3d 43 25 00 00 	lea    r15,[rip+0x2543]        # 3d50 <__frame_dummy_init_array_entry>
    180d:	41 56                	push   r14
    180f:	49 89 d6             	mov    r14,rdx
    1812:	41 55                	push   r13
    1814:	49 89 f5             	mov    r13,rsi
    1817:	41 54                	push   r12
    1819:	41 89 fc             	mov    r12d,edi
    181c:	55                   	push   rbp
    181d:	48 8d 2d 34 25 00 00 	lea    rbp,[rip+0x2534]        # 3d58 <__do_global_dtors_aux_fini_array_entry>
    1824:	53                   	push   rbx
    1825:	4c 29 fd             	sub    rbp,r15
    1828:	48 83 ec 08          	sub    rsp,0x8
    182c:	e8 cf f7 ff ff       	call   1000 <_init>
    1831:	48 c1 fd 03          	sar    rbp,0x3
    1835:	74 1f                	je     1856 <__libc_csu_init+0x56>
    1837:	31 db                	xor    ebx,ebx
    1839:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1840:	4c 89 f2             	mov    rdx,r14
    1843:	4c 89 ee             	mov    rsi,r13
    1846:	44 89 e7             	mov    edi,r12d
    1849:	41 ff 14 df          	call   QWORD PTR [r15+rbx*8]
    184d:	48 83 c3 01          	add    rbx,0x1
    1851:	48 39 dd             	cmp    rbp,rbx
    1854:	75 ea                	jne    1840 <__libc_csu_init+0x40>
    1856:	48 83 c4 08          	add    rsp,0x8
    185a:	5b                   	pop    rbx
    185b:	5d                   	pop    rbp
    185c:	41 5c                	pop    r12
    185e:	41 5d                	pop    r13
    1860:	41 5e                	pop    r14
    1862:	41 5f                	pop    r15
    1864:	c3                   	ret
    1865:	66 66 2e 0f 1f 84 00 	data16 cs nop WORD PTR [rax+rax*1+0x0]
    186c:	00 00 00 00 

0000000000001870 <__libc_csu_fini>:
    1870:	f3 0f 1e fa          	endbr64
    1874:	c3                   	ret
    1875:	66 2e 0f 1f 84 00 00 	cs nop WORD PTR [rax+rax*1+0x0]
    187c:	00 00 00 
    187f:	90                   	nop

0000000000001880 <__fstat>:
    1880:	f3 0f 1e fa          	endbr64
    1884:	48 89 f2             	mov    rdx,rsi
    1887:	89 fe                	mov    esi,edi
    1889:	bf 01 00 00 00       	mov    edi,0x1
    188e:	e9 1d f9 ff ff       	jmp    11b0 <__fxstat@plt>

Disassembly of section .fini:

0000000000001894 <_fini>:
    1894:	f3 0f 1e fa          	endbr64
    1898:	48 83 ec 08          	sub    rsp,0x8
    189c:	48 83 c4 08          	add    rsp,0x8
    18a0:	c3                   	ret

Disassembly of section .rodata:

0000000000002000 <_IO_stdin_used>:
    2000:	01 00                	add    DWORD PTR [rax],eax
    2002:	02 00                	add    al,BYTE PTR [rax]
    2004:	00 00                	add    BYTE PTR [rax],al
    2006:	00 00                	add    BYTE PTR [rax],al
    2008:	55                   	push   rbp
    2009:	73 61                	jae    206c <_IO_stdin_used+0x6c>
    200b:	67 65 3a 20          	cmp    ah,BYTE PTR gs:[eax]
    200f:	25 73 20 28 3c       	and    eax,0x3c282073
    2014:	66 69 6c 65 6e 61 6d 	imul   bp,WORD PTR [rbp+riz*2+0x6e],0x6d61
    201b:	65 3e 7c 2d          	gs ds jl 204c <_IO_stdin_used+0x4c>
    201f:	29 0a                	sub    DWORD PTR [rdx],ecx
    2021:	00 2d 00 6d 6d 61    	add    BYTE PTR [rip+0x616d6d00],ch        # 616d8d27 <_end+0x616d4cf7>
    2027:	70 28                	jo     2051 <_IO_stdin_used+0x51>
    2029:	29 20                	sub    DWORD PTR [rax],esp
    202b:	66 61                	data16 (bad)
    202d:	69 6c 65 64 00 6f 70 	imul   ebp,DWORD PTR [rbp+riz*2+0x64],0x65706f00
    2034:	65 
    2035:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2036:	28 29                	sub    BYTE PTR [rcx],ch
    2038:	20 66 61             	and    BYTE PTR [rsi+0x61],ah
    203b:	69 6c 65 64 00 66 73 	imul   ebp,DWORD PTR [rbp+riz*2+0x64],0x74736600
    2042:	74 
    2043:	61                   	(bad)
    2044:	74 28                	je     206e <_IO_stdin_used+0x6e>
    2046:	29 20                	sub    DWORD PTR [rax],esp
    2048:	66 61                	data16 (bad)
    204a:	69 6c 65 64 00 00 52 	imul   ebp,DWORD PTR [rbp+riz*2+0x64],0x65520000
    2051:	65 
    2052:	61                   	(bad)
    2053:	64 20 25 64 20 62 79 	and    BYTE PTR fs:[rip+0x79622064],ah        # 796240be <_end+0x7962008e>
    205a:	74 65                	je     20c1 <_IO_stdin_used+0xc1>
    205c:	73 20                	jae    207e <_IO_stdin_used+0x7e>
    205e:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    205f:	66 20 73 68          	data16 and BYTE PTR [rbx+0x68],dh
    2063:	65 6c                	gs ins BYTE PTR es:[rdi],dx
    2065:	6c                   	ins    BYTE PTR es:[rdi],dx
    2066:	63 6f 64             	movsxd ebp,DWORD PTR [rdi+0x64]
    2069:	65 2e 20 48 65       	gs and BYTE PTR gs:[rax+0x65],cl
    206e:	72 65                	jb     20d5 <_IO_stdin_used+0xd5>
    2070:	20 67 6f             	and    BYTE PTR [rdi+0x6f],ah
    2073:	65 73 2e             	gs jae 20a4 <_IO_stdin_used+0xa4>
    2076:	0a 00                	or     al,BYTE PTR [rax]
    2078:	53                   	push   rbx
    2079:	68 65 6c 6c 63       	push   0x636c6c65
    207e:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    207f:	64 65 20 63 6f       	fs and BYTE PTR gs:[rbx+0x6f],ah
    2084:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    2085:	74 61                	je     20e8 <_IO_stdin_used+0xe8>
    2087:	69 6e 73 20 25 64 20 	imul   ebp,DWORD PTR [rsi+0x73],0x20642520
    208e:	62                   	(bad)
    208f:	79 74                	jns    2105 <__GNU_EH_FRAME_HDR+0x1>
    2091:	65 73 20             	gs jae 20b4 <_IO_stdin_used+0xb4>
    2094:	74 6f                	je     2105 <__GNU_EH_FRAME_HDR+0x1>
    2096:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    2097:	20 6d 61             	and    BYTE PTR [rbp+0x61],ch
    209a:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    209b:	79 2e                	jns    20cb <_IO_stdin_used+0xcb>
    209d:	0a 00                	or     al,BYTE PTR [rax]
    209f:	00 53 68             	add    BYTE PTR [rbx+0x68],dl
    20a2:	65 6c                	gs ins BYTE PTR es:[rdi],dx
    20a4:	6c                   	ins    BYTE PTR es:[rdi],dx
    20a5:	63 6f 64             	movsxd ebp,DWORD PTR [rdi+0x64]
    20a8:	65 20 63 6f          	and    BYTE PTR gs:[rbx+0x6f],ah
    20ac:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    20ad:	74 61                	je     2110 <__GNU_EH_FRAME_HDR+0xc>
    20af:	69 6e 73 20 25 64 20 	imul   ebp,DWORD PTR [rsi+0x73],0x20642520
    20b6:	7a 65                	jp     211d <__GNU_EH_FRAME_HDR+0x19>
    20b8:	72 6f                	jb     2129 <__GNU_EH_FRAME_HDR+0x25>
    20ba:	20 62 79             	and    BYTE PTR [rdx+0x79],ah
    20bd:	74 65                	je     2124 <__GNU_EH_FRAME_HDR+0x20>
    20bf:	73 2e                	jae    20ef <_IO_stdin_used+0xef>
    20c1:	0a 00                	or     al,BYTE PTR [rax]
    20c3:	00 00                	add    BYTE PTR [rax],al
    20c5:	00 00                	add    BYTE PTR [rax],al
    20c7:	00 53 68             	add    BYTE PTR [rbx+0x68],dl
    20ca:	65 6c                	gs ins BYTE PTR es:[rdi],dx
    20cc:	6c                   	ins    BYTE PTR es:[rdi],dx
    20cd:	63 6f 64             	movsxd ebp,DWORD PTR [rdi+0x64]
    20d0:	65 20 63 6f          	and    BYTE PTR gs:[rbx+0x6f],ah
    20d4:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    20d5:	74 61                	je     2138 <__GNU_EH_FRAME_HDR+0x34>
    20d7:	69 6e 73 20 25 64 20 	imul   ebp,DWORD PTR [rsi+0x73],0x20642520
    20de:	46 20 64 69 67       	and    BYTE PTR [rcx+r13*2+0x67],r12b
    20e3:	69 74 73 2e 0a 00 7e 	imul   esi,DWORD PTR [rbx+rsi*2+0x2e],0x7e7e000a
    20ea:	7e 
    20eb:	7e 20                	jle    210d <__GNU_EH_FRAME_HDR+0x9>
    20ed:	52                   	push   rdx
    20ee:	75 6e                	jne    215e <__GNU_EH_FRAME_HDR+0x5a>
    20f0:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    20f1:	69 6e 67 20 73 68 65 	imul   ebp,DWORD PTR [rsi+0x67],0x65687320
    20f8:	6c                   	ins    BYTE PTR es:[rdi],dx
    20f9:	6c                   	ins    BYTE PTR es:[rdi],dx
    20fa:	63 6f 64             	movsxd ebp,DWORD PTR [rdi+0x64]
    20fd:	65 20 7e 7e          	and    BYTE PTR gs:[rsi+0x7e],bh
    2101:	7e 0a                	jle    210d <__GNU_EH_FRAME_HDR+0x9>
	...

Disassembly of section .eh_frame_hdr:

0000000000002104 <__GNU_EH_FRAME_HDR>:
    2104:	01 1b                	add    DWORD PTR [rbx],ebx
    2106:	03 3b                	add    edi,DWORD PTR [rbx]
    2108:	48 00 00             	rex.W add BYTE PTR [rax],al
    210b:	00 08                	add    BYTE PTR [rax],cl
    210d:	00 00                	add    BYTE PTR [rax],al
    210f:	00 1c ef             	add    BYTE PTR [rdi+rbp*8],bl
    2112:	ff                   	(bad)
    2113:	ff                   	(bad)
    2114:	7c 00                	jl     2116 <__GNU_EH_FRAME_HDR+0x12>
    2116:	00 00                	add    BYTE PTR [rax],al
    2118:	0c f0                	or     al,0xf0
    211a:	ff                   	(bad)
    211b:	ff a4 00 00 00 1c f0 	jmp    QWORD PTR [rax+rax*1-0xfe40000]
    2122:	ff                   	(bad)
    2123:	ff                   	(bad)
    2124:	bc 00 00 00 fc       	mov    esp,0xfc000000
    2129:	f0 ff                	lock (bad)
    212b:	ff 64 00 00          	jmp    QWORD PTR [rax+rax*1+0x0]
    212f:	00 e5                	add    ch,ah
    2131:	f1                   	int1
    2132:	ff                   	(bad)
    2133:	ff d4                	call   rsp
    2135:	00 00                	add    BYTE PTR [rax],al
    2137:	00 fc                	add    ah,bh
    2139:	f6 ff                	idiv   bh
    213b:	ff f4                	push   rsp
    213d:	00 00                	add    BYTE PTR [rax],al
    213f:	00 6c f7 ff          	add    BYTE PTR [rdi+rsi*8-0x1],ch
    2143:	ff                   	(bad)
    2144:	3c 01                	cmp    al,0x1
    2146:	00 00                	add    BYTE PTR [rax],al
    2148:	7c f7                	jl     2141 <__GNU_EH_FRAME_HDR+0x3d>
    214a:	ff                   	(bad)
    214b:	ff 54 01 00          	call   QWORD PTR [rcx+rax*1+0x0]
	...

Disassembly of section .eh_frame:

0000000000002150 <__FRAME_END__-0x11c>:
    2150:	14 00                	adc    al,0x0
    2152:	00 00                	add    BYTE PTR [rax],al
    2154:	00 00                	add    BYTE PTR [rax],al
    2156:	00 00                	add    BYTE PTR [rax],al
    2158:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
    215b:	00 01                	add    BYTE PTR [rcx],al
    215d:	78 10                	js     216f <__GNU_EH_FRAME_HDR+0x6b>
    215f:	01 1b                	add    DWORD PTR [rbx],ebx
    2161:	0c 07                	or     al,0x7
    2163:	08 90 01 00 00 14    	or     BYTE PTR [rax+0x14000001],dl
    2169:	00 00                	add    BYTE PTR [rax],al
    216b:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    216e:	00 00                	add    BYTE PTR [rax],al
    2170:	90                   	nop
    2171:	f0 ff                	lock (bad)
    2173:	ff 2f                	jmp    FWORD PTR [rdi]
    2175:	00 00                	add    BYTE PTR [rax],al
    2177:	00 00                	add    BYTE PTR [rax],al
    2179:	44 07                	rex.R (bad)
    217b:	10 00                	adc    BYTE PTR [rax],al
    217d:	00 00                	add    BYTE PTR [rax],al
    217f:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
    2182:	00 00                	add    BYTE PTR [rax],al
    2184:	34 00                	xor    al,0x0
    2186:	00 00                	add    BYTE PTR [rax],al
    2188:	98                   	cwde
    2189:	ee                   	out    dx,al
    218a:	ff                   	(bad)
    218b:	ff f0                	push   rax
    218d:	00 00                	add    BYTE PTR [rax],al
    218f:	00 00                	add    BYTE PTR [rax],al
    2191:	0e                   	(bad)
    2192:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
    2195:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
    2198:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
    219b:	80 00 3f             	add    BYTE PTR [rax],0x3f
    219e:	1a 3a                	sbb    bh,BYTE PTR [rdx]
    21a0:	2a 33                	sub    dh,BYTE PTR [rbx]
    21a2:	24 22                	and    al,0x22
    21a4:	00 00                	add    BYTE PTR [rax],al
    21a6:	00 00                	add    BYTE PTR [rax],al
    21a8:	14 00                	adc    al,0x0
    21aa:	00 00                	add    BYTE PTR [rax],al
    21ac:	5c                   	pop    rsp
    21ad:	00 00                	add    BYTE PTR [rax],al
    21af:	00 60 ef             	add    BYTE PTR [rax-0x11],ah
    21b2:	ff                   	(bad)
    21b3:	ff 10                	call   QWORD PTR [rax]
	...
    21bd:	00 00                	add    BYTE PTR [rax],al
    21bf:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    21c2:	00 00                	add    BYTE PTR [rax],al
    21c4:	74 00                	je     21c6 <__GNU_EH_FRAME_HDR+0xc2>
    21c6:	00 00                	add    BYTE PTR [rax],al
    21c8:	58                   	pop    rax
    21c9:	ef                   	out    dx,eax
    21ca:	ff                   	(bad)
    21cb:	ff e0                	jmp    rax
	...
    21d5:	00 00                	add    BYTE PTR [rax],al
    21d7:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    21da:	00 00                	add    BYTE PTR [rax],al
    21dc:	8c 00                	mov    WORD PTR [rax],es
    21de:	00 00                	add    BYTE PTR [rax],al
    21e0:	09 f1                	or     ecx,esi
    21e2:	ff                   	(bad)
    21e3:	ff 12                	call   QWORD PTR [rdx]
    21e5:	05 00 00 00 45       	add    eax,0x45000000
    21ea:	0e                   	(bad)
    21eb:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    21f1:	03 09                	add    ecx,DWORD PTR [rcx]
    21f3:	05 0c 07 08 00       	add    eax,0x8070c
    21f8:	44 00 00             	add    BYTE PTR [rax],r8b
    21fb:	00 ac 00 00 00 00 f6 	add    BYTE PTR [rax+rax*1-0xa000000],ch
    2202:	ff                   	(bad)
    2203:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
    2206:	00 00                	add    BYTE PTR [rax],al
    2208:	00 46 0e             	add    BYTE PTR [rsi+0xe],al
    220b:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
    2211:	8e 03                	mov    es,WORD PTR [rbx]
    2213:	45 0e                	rex.RB (bad)
    2215:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
    221b:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff86303065 <_end+0xffffffff862ff035>
    2221:	06                   	(bad)
    2222:	48 0e                	rex.W (bad)
    2224:	38 83 07 47 0e 40    	cmp    BYTE PTR [rbx+0x400e4707],al
    222a:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    222b:	0e                   	(bad)
    222c:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
    222f:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
    2232:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
    2235:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
    2238:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
    223b:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
    223e:	08 00                	or     BYTE PTR [rax],al
    2240:	14 00                	adc    al,0x0
    2242:	00 00                	add    BYTE PTR [rax],al
    2244:	f4                   	hlt
    2245:	00 00                	add    BYTE PTR [rax],al
    2247:	00 28                	add    BYTE PTR [rax],ch
    2249:	f6 ff                	idiv   bh
    224b:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 2251 <__GNU_EH_FRAME_HDR+0x14d>
    2251:	00 00                	add    BYTE PTR [rax],al
    2253:	00 00                	add    BYTE PTR [rax],al
    2255:	00 00                	add    BYTE PTR [rax],al
    2257:	00 10                	add    BYTE PTR [rax],dl
    2259:	00 00                	add    BYTE PTR [rax],al
    225b:	00 0c 01             	add    BYTE PTR [rcx+rax*1],cl
    225e:	00 00                	add    BYTE PTR [rax],al
    2260:	20 f6                	and    dh,dh
    2262:	ff                   	(bad)
    2263:	ff 13                	call   QWORD PTR [rbx]
    2265:	00 00                	add    BYTE PTR [rax],al
    2267:	00 00                	add    BYTE PTR [rax],al
    2269:	00 00                	add    BYTE PTR [rax],al
	...

000000000000226c <__FRAME_END__>:
    226c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000003d50 <__frame_dummy_init_array_entry>:
    3d50:	e0 12                	loopne 3d64 <_DYNAMIC+0x4>
    3d52:	00 00                	add    BYTE PTR [rax],al
    3d54:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000003d58 <__do_global_dtors_aux_fini_array_entry>:
    3d58:	a0                   	.byte 0xa0
    3d59:	12 00                	adc    al,BYTE PTR [rax]
    3d5b:	00 00                	add    BYTE PTR [rax],al
    3d5d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000003d60 <_DYNAMIC>:
    3d60:	01 00                	add    DWORD PTR [rax],eax
    3d62:	00 00                	add    BYTE PTR [rax],al
    3d64:	00 00                	add    BYTE PTR [rax],al
    3d66:	00 00                	add    BYTE PTR [rax],al
    3d68:	01 00                	add    DWORD PTR [rax],eax
    3d6a:	00 00                	add    BYTE PTR [rax],al
    3d6c:	00 00                	add    BYTE PTR [rax],al
    3d6e:	00 00                	add    BYTE PTR [rax],al
    3d70:	0c 00                	or     al,0x0
    3d72:	00 00                	add    BYTE PTR [rax],al
    3d74:	00 00                	add    BYTE PTR [rax],al
    3d76:	00 00                	add    BYTE PTR [rax],al
    3d78:	00 10                	add    BYTE PTR [rax],dl
    3d7a:	00 00                	add    BYTE PTR [rax],al
    3d7c:	00 00                	add    BYTE PTR [rax],al
    3d7e:	00 00                	add    BYTE PTR [rax],al
    3d80:	0d 00 00 00 00       	or     eax,0x0
    3d85:	00 00                	add    BYTE PTR [rax],al
    3d87:	00 94 18 00 00 00 00 	add    BYTE PTR [rax+rbx*1+0x0],dl
    3d8e:	00 00                	add    BYTE PTR [rax],al
    3d90:	19 00                	sbb    DWORD PTR [rax],eax
    3d92:	00 00                	add    BYTE PTR [rax],al
    3d94:	00 00                	add    BYTE PTR [rax],al
    3d96:	00 00                	add    BYTE PTR [rax],al
    3d98:	50                   	push   rax
    3d99:	3d 00 00 00 00       	cmp    eax,0x0
    3d9e:	00 00                	add    BYTE PTR [rax],al
    3da0:	1b 00                	sbb    eax,DWORD PTR [rax]
    3da2:	00 00                	add    BYTE PTR [rax],al
    3da4:	00 00                	add    BYTE PTR [rax],al
    3da6:	00 00                	add    BYTE PTR [rax],al
    3da8:	08 00                	or     BYTE PTR [rax],al
    3daa:	00 00                	add    BYTE PTR [rax],al
    3dac:	00 00                	add    BYTE PTR [rax],al
    3dae:	00 00                	add    BYTE PTR [rax],al
    3db0:	1a 00                	sbb    al,BYTE PTR [rax]
    3db2:	00 00                	add    BYTE PTR [rax],al
    3db4:	00 00                	add    BYTE PTR [rax],al
    3db6:	00 00                	add    BYTE PTR [rax],al
    3db8:	58                   	pop    rax
    3db9:	3d 00 00 00 00       	cmp    eax,0x0
    3dbe:	00 00                	add    BYTE PTR [rax],al
    3dc0:	1c 00                	sbb    al,0x0
    3dc2:	00 00                	add    BYTE PTR [rax],al
    3dc4:	00 00                	add    BYTE PTR [rax],al
    3dc6:	00 00                	add    BYTE PTR [rax],al
    3dc8:	08 00                	or     BYTE PTR [rax],al
    3dca:	00 00                	add    BYTE PTR [rax],al
    3dcc:	00 00                	add    BYTE PTR [rax],al
    3dce:	00 00                	add    BYTE PTR [rax],al
    3dd0:	f5                   	cmc
    3dd1:	fe                   	(bad)
    3dd2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3dd5:	00 00                	add    BYTE PTR [rax],al
    3dd7:	00 a0 03 00 00 00    	add    BYTE PTR [rax+0x3],ah
    3ddd:	00 00                	add    BYTE PTR [rax],al
    3ddf:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 3de5 <_DYNAMIC+0x85>
    3de5:	00 00                	add    BYTE PTR [rax],al
    3de7:	00 c0                	add    al,al
    3de9:	05 00 00 00 00       	add    eax,0x0
    3dee:	00 00                	add    BYTE PTR [rax],al
    3df0:	06                   	(bad)
    3df1:	00 00                	add    BYTE PTR [rax],al
    3df3:	00 00                	add    BYTE PTR [rax],al
    3df5:	00 00                	add    BYTE PTR [rax],al
    3df7:	00 c8                	add    al,cl
    3df9:	03 00                	add    eax,DWORD PTR [rax]
    3dfb:	00 00                	add    BYTE PTR [rax],al
    3dfd:	00 00                	add    BYTE PTR [rax],al
    3dff:	00 0a                	add    BYTE PTR [rdx],cl
    3e01:	00 00                	add    BYTE PTR [rax],al
    3e03:	00 00                	add    BYTE PTR [rax],al
    3e05:	00 00                	add    BYTE PTR [rax],al
    3e07:	00 03                	add    BYTE PTR [rbx],al
    3e09:	01 00                	add    DWORD PTR [rax],eax
    3e0b:	00 00                	add    BYTE PTR [rax],al
    3e0d:	00 00                	add    BYTE PTR [rax],al
    3e0f:	00 0b                	add    BYTE PTR [rbx],cl
    3e11:	00 00                	add    BYTE PTR [rax],al
    3e13:	00 00                	add    BYTE PTR [rax],al
    3e15:	00 00                	add    BYTE PTR [rax],al
    3e17:	00 18                	add    BYTE PTR [rax],bl
    3e19:	00 00                	add    BYTE PTR [rax],al
    3e1b:	00 00                	add    BYTE PTR [rax],al
    3e1d:	00 00                	add    BYTE PTR [rax],al
    3e1f:	00 15 00 00 00 00    	add    BYTE PTR [rip+0x0],dl        # 3e25 <_DYNAMIC+0xc5>
	...
    3e2d:	00 00                	add    BYTE PTR [rax],al
    3e2f:	00 03                	add    BYTE PTR [rbx],al
    3e31:	00 00                	add    BYTE PTR [rax],al
    3e33:	00 00                	add    BYTE PTR [rax],al
    3e35:	00 00                	add    BYTE PTR [rax],al
    3e37:	00 50 3f             	add    BYTE PTR [rax+0x3f],dl
    3e3a:	00 00                	add    BYTE PTR [rax],al
    3e3c:	00 00                	add    BYTE PTR [rax],al
    3e3e:	00 00                	add    BYTE PTR [rax],al
    3e40:	02 00                	add    al,BYTE PTR [rax]
    3e42:	00 00                	add    BYTE PTR [rax],al
    3e44:	00 00                	add    BYTE PTR [rax],al
    3e46:	00 00                	add    BYTE PTR [rax],al
    3e48:	50                   	push   rax
    3e49:	01 00                	add    DWORD PTR [rax],eax
    3e4b:	00 00                	add    BYTE PTR [rax],al
    3e4d:	00 00                	add    BYTE PTR [rax],al
    3e4f:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    3e52:	00 00                	add    BYTE PTR [rax],al
    3e54:	00 00                	add    BYTE PTR [rax],al
    3e56:	00 00                	add    BYTE PTR [rax],al
    3e58:	07                   	(bad)
    3e59:	00 00                	add    BYTE PTR [rax],al
    3e5b:	00 00                	add    BYTE PTR [rax],al
    3e5d:	00 00                	add    BYTE PTR [rax],al
    3e5f:	00 17                	add    BYTE PTR [rdi],dl
    3e61:	00 00                	add    BYTE PTR [rax],al
    3e63:	00 00                	add    BYTE PTR [rax],al
    3e65:	00 00                	add    BYTE PTR [rax],al
    3e67:	00 08                	add    BYTE PTR [rax],cl
    3e69:	08 00                	or     BYTE PTR [rax],al
    3e6b:	00 00                	add    BYTE PTR [rax],al
    3e6d:	00 00                	add    BYTE PTR [rax],al
    3e6f:	00 07                	add    BYTE PTR [rdi],al
    3e71:	00 00                	add    BYTE PTR [rax],al
    3e73:	00 00                	add    BYTE PTR [rax],al
    3e75:	00 00                	add    BYTE PTR [rax],al
    3e77:	00 30                	add    BYTE PTR [rax],dh
    3e79:	07                   	(bad)
    3e7a:	00 00                	add    BYTE PTR [rax],al
    3e7c:	00 00                	add    BYTE PTR [rax],al
    3e7e:	00 00                	add    BYTE PTR [rax],al
    3e80:	08 00                	or     BYTE PTR [rax],al
    3e82:	00 00                	add    BYTE PTR [rax],al
    3e84:	00 00                	add    BYTE PTR [rax],al
    3e86:	00 00                	add    BYTE PTR [rax],al
    3e88:	d8 00                	fadd   DWORD PTR [rax]
    3e8a:	00 00                	add    BYTE PTR [rax],al
    3e8c:	00 00                	add    BYTE PTR [rax],al
    3e8e:	00 00                	add    BYTE PTR [rax],al
    3e90:	09 00                	or     DWORD PTR [rax],eax
    3e92:	00 00                	add    BYTE PTR [rax],al
    3e94:	00 00                	add    BYTE PTR [rax],al
    3e96:	00 00                	add    BYTE PTR [rax],al
    3e98:	18 00                	sbb    BYTE PTR [rax],al
    3e9a:	00 00                	add    BYTE PTR [rax],al
    3e9c:	00 00                	add    BYTE PTR [rax],al
    3e9e:	00 00                	add    BYTE PTR [rax],al
    3ea0:	1e                   	(bad)
    3ea1:	00 00                	add    BYTE PTR [rax],al
    3ea3:	00 00                	add    BYTE PTR [rax],al
    3ea5:	00 00                	add    BYTE PTR [rax],al
    3ea7:	00 08                	add    BYTE PTR [rax],cl
    3ea9:	00 00                	add    BYTE PTR [rax],al
    3eab:	00 00                	add    BYTE PTR [rax],al
    3ead:	00 00                	add    BYTE PTR [rax],al
    3eaf:	00 fb                	add    bl,bh
    3eb1:	ff                   	(bad)
    3eb2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3eb5:	00 00                	add    BYTE PTR [rax],al
    3eb7:	00 01                	add    BYTE PTR [rcx],al
    3eb9:	00 00                	add    BYTE PTR [rax],al
    3ebb:	08 00                	or     BYTE PTR [rax],al
    3ebd:	00 00                	add    BYTE PTR [rax],al
    3ebf:	00 fe                	add    dh,bh
    3ec1:	ff                   	(bad)
    3ec2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3ec5:	00 00                	add    BYTE PTR [rax],al
    3ec7:	00 f0                	add    al,dh
    3ec9:	06                   	(bad)
    3eca:	00 00                	add    BYTE PTR [rax],al
    3ecc:	00 00                	add    BYTE PTR [rax],al
    3ece:	00 00                	add    BYTE PTR [rax],al
    3ed0:	ff                   	(bad)
    3ed1:	ff                   	(bad)
    3ed2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3ed5:	00 00                	add    BYTE PTR [rax],al
    3ed7:	00 01                	add    BYTE PTR [rcx],al
    3ed9:	00 00                	add    BYTE PTR [rax],al
    3edb:	00 00                	add    BYTE PTR [rax],al
    3edd:	00 00                	add    BYTE PTR [rax],al
    3edf:	00 f0                	add    al,dh
    3ee1:	ff                   	(bad)
    3ee2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3ee5:	00 00                	add    BYTE PTR [rax],al
    3ee7:	00 c4                	add    ah,al
    3ee9:	06                   	(bad)
    3eea:	00 00                	add    BYTE PTR [rax],al
    3eec:	00 00                	add    BYTE PTR [rax],al
    3eee:	00 00                	add    BYTE PTR [rax],al
    3ef0:	f9                   	stc
    3ef1:	ff                   	(bad)
    3ef2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3ef5:	00 00                	add    BYTE PTR [rax],al
    3ef7:	00 03                	add    BYTE PTR [rbx],al
	...

Disassembly of section .got:

0000000000003f50 <_GLOBAL_OFFSET_TABLE_>:
    3f50:	60                   	(bad)
    3f51:	3d 00 00 00 00       	cmp    eax,0x0
	...
    3f66:	00 00                	add    BYTE PTR [rax],al
    3f68:	30 10                	xor    BYTE PTR [rax],dl
    3f6a:	00 00                	add    BYTE PTR [rax],al
    3f6c:	00 00                	add    BYTE PTR [rax],al
    3f6e:	00 00                	add    BYTE PTR [rax],al
    3f70:	40 10 00             	rex adc BYTE PTR [rax],al
    3f73:	00 00                	add    BYTE PTR [rax],al
    3f75:	00 00                	add    BYTE PTR [rax],al
    3f77:	00 50 10             	add    BYTE PTR [rax+0x10],dl
    3f7a:	00 00                	add    BYTE PTR [rax],al
    3f7c:	00 00                	add    BYTE PTR [rax],al
    3f7e:	00 00                	add    BYTE PTR [rax],al
    3f80:	60                   	(bad)
    3f81:	10 00                	adc    BYTE PTR [rax],al
    3f83:	00 00                	add    BYTE PTR [rax],al
    3f85:	00 00                	add    BYTE PTR [rax],al
    3f87:	00 70 10             	add    BYTE PTR [rax+0x10],dh
    3f8a:	00 00                	add    BYTE PTR [rax],al
    3f8c:	00 00                	add    BYTE PTR [rax],al
    3f8e:	00 00                	add    BYTE PTR [rax],al
    3f90:	80 10 00             	adc    BYTE PTR [rax],0x0
    3f93:	00 00                	add    BYTE PTR [rax],al
    3f95:	00 00                	add    BYTE PTR [rax],al
    3f97:	00 90 10 00 00 00    	add    BYTE PTR [rax+0x10],dl
    3f9d:	00 00                	add    BYTE PTR [rax],al
    3f9f:	00 a0 10 00 00 00    	add    BYTE PTR [rax+0x10],ah
    3fa5:	00 00                	add    BYTE PTR [rax],al
    3fa7:	00 b0 10 00 00 00    	add    BYTE PTR [rax+0x10],dh
    3fad:	00 00                	add    BYTE PTR [rax],al
    3faf:	00 c0                	add    al,al
    3fb1:	10 00                	adc    BYTE PTR [rax],al
    3fb3:	00 00                	add    BYTE PTR [rax],al
    3fb5:	00 00                	add    BYTE PTR [rax],al
    3fb7:	00 d0                	add    al,dl
    3fb9:	10 00                	adc    BYTE PTR [rax],al
    3fbb:	00 00                	add    BYTE PTR [rax],al
    3fbd:	00 00                	add    BYTE PTR [rax],al
    3fbf:	00 e0                	add    al,ah
    3fc1:	10 00                	adc    BYTE PTR [rax],al
    3fc3:	00 00                	add    BYTE PTR [rax],al
    3fc5:	00 00                	add    BYTE PTR [rax],al
    3fc7:	00 f0                	add    al,dh
    3fc9:	10 00                	adc    BYTE PTR [rax],al
    3fcb:	00 00                	add    BYTE PTR [rax],al
    3fcd:	00 00                	add    BYTE PTR [rax],al
    3fcf:	00 00                	add    BYTE PTR [rax],al
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

0000000000004020 <stderr@@GLIBC_2.2.5>:
	...

0000000000004028 <completed.8061>:
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
  11:	30 2d 31 75 62 75    	xor    BYTE PTR [rip+0x75627531],ch        # 75627548 <_end+0x75623518>
  17:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  18:	74 75                	je     8f <_init-0xf71>
  1a:	31 7e 32             	xor    DWORD PTR [rsi+0x32],edi
  1d:	30 2e                	xor    BYTE PTR [rsi],ch
  1f:	30 34 2e             	xor    BYTE PTR [rsi+rbp*1],dh
  22:	31 29                	xor    DWORD PTR [rcx],ebp
  24:	20 39                	and    BYTE PTR [rcx],bh
  26:	2e 34 2e             	cs xor al,0x2e
  29:	30 00                	xor    BYTE PTR [rax],al
