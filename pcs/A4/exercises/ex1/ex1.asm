
ex1_no_protection:     file format elf64-x86-64


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
 369:	77 05                	ja     370 <_init-0xc90>
 36b:	08 43 b5             	or     BYTE PTR [rbx-0x4b],al
 36e:	30 22                	xor    BYTE PTR [rdx],ah
 370:	56                   	push   rsi
 371:	1b 04 ff             	sbb    eax,DWORD PTR [rdi+rdi*8]
 374:	6a 58                	push   0x58
 376:	d1 26                	shl    DWORD PTR [rsi],1
 378:	01 65 16             	add    DWORD PTR [rbp+0x16],esp
 37b:	dd                   	.byte 0xdd

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
 3a4:	07                   	(bad)  
 3a5:	00 00                	add    BYTE PTR [rax],al
 3a7:	00 01                	add    BYTE PTR [rcx],al
 3a9:	00 00                	add    BYTE PTR [rax],al
 3ab:	00 06                	add    BYTE PTR [rsi],al
 3ad:	00 00                	add    BYTE PTR [rax],al
 3af:	00 00                	add    BYTE PTR [rax],al
 3b1:	00 81 00 00 00 00    	add    BYTE PTR [rcx+0x0],al
 3b7:	00 07                	add    BYTE PTR [rdi],al
 3b9:	00 00                	add    BYTE PTR [rax],al
 3bb:	00 00                	add    BYTE PTR [rax],al
 3bd:	00 00                	add    BYTE PTR [rax],al
 3bf:	00 d1                	add    cl,dl
 3c1:	65 ce                	gs (bad) 
 3c3:	6d                   	ins    DWORD PTR es:[rdi],dx

Disassembly of section .dynsym:

00000000000003c8 <.dynsym>:
	...
 3e0:	44 00 00             	add    BYTE PTR [rax],r8b
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
 427:	00 26                	add    BYTE PTR [rsi],ah
 429:	00 00                	add    BYTE PTR [rax],al
 42b:	00 12                	add    BYTE PTR [rdx],dl
	...
 43d:	00 00                	add    BYTE PTR [rax],al
 43f:	00 60 00             	add    BYTE PTR [rax+0x0],ah
 442:	00 00                	add    BYTE PTR [rax],al
 444:	20 00                	and    BYTE PTR [rax],al
	...
 456:	00 00                	add    BYTE PTR [rax],al
 458:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 459:	00 00                	add    BYTE PTR [rax],al
 45b:	00 20                	add    BYTE PTR [rax],ah
	...
 46d:	00 00                	add    BYTE PTR [rax],al
 46f:	00 17                	add    BYTE PTR [rdi],dl
 471:	00 00                	add    BYTE PTR [rax],al
 473:	00 22                	add    BYTE PTR [rdx],ah
	...

Disassembly of section .dynstr:

0000000000000488 <.dynstr>:
 488:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
 48c:	63 2e                	movsxd ebp,DWORD PTR [rsi]
 48e:	73 6f                	jae    4ff <_init-0xb01>
 490:	2e 36 00 73 74       	cs add BYTE PTR ss:[rbx+0x74],dh
 495:	72 63                	jb     4fa <_init-0xb06>
 497:	70 79                	jo     512 <_init-0xaee>
 499:	00 70 75             	add    BYTE PTR [rax+0x75],dh
 49c:	74 73                	je     511 <_init-0xaef>
 49e:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 4a1:	63 78 61             	movsxd edi,DWORD PTR [rax+0x61]
 4a4:	5f                   	pop    rdi
 4a5:	66 69 6e 61 6c 69    	imul   bp,WORD PTR [rsi+0x61],0x696c
 4ab:	7a 65                	jp     512 <_init-0xaee>
 4ad:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
 4b0:	6c                   	ins    BYTE PTR es:[rdi],dx
 4b1:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
 4b8:	72 74                	jb     52e <_init-0xad2>
 4ba:	5f                   	pop    rdi
 4bb:	6d                   	ins    DWORD PTR es:[rdi],dx
 4bc:	61                   	(bad)  
 4bd:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
 4c4:	43 5f                	rex.XB pop r15
 4c6:	32 2e                	xor    ch,BYTE PTR [rsi]
 4c8:	32 2e                	xor    ch,BYTE PTR [rsi]
 4ca:	35 00 5f 49 54       	xor    eax,0x54495f00
 4cf:	4d 5f                	rex.WRB pop r15
 4d1:	64 65 72 65          	fs gs jb 53a <_init-0xac6>
 4d5:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 4dc:	4d 
 4dd:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 4df:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 4e0:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 4e1:	65 54                	gs push rsp
 4e3:	61                   	(bad)  
 4e4:	62                   	(bad)  
 4e5:	6c                   	ins    BYTE PTR es:[rdi],dx
 4e6:	65 00 5f 5f          	add    BYTE PTR gs:[rdi+0x5f],bl
 4ea:	67 6d                	ins    DWORD PTR es:[edi],dx
 4ec:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 4ed:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 4ee:	5f                   	pop    rdi
 4ef:	73 74                	jae    565 <_init-0xa9b>
 4f1:	61                   	(bad)  
 4f2:	72 74                	jb     568 <_init-0xa98>
 4f4:	5f                   	pop    rdi
 4f5:	5f                   	pop    rdi
 4f6:	00 5f 49             	add    BYTE PTR [rdi+0x49],bl
 4f9:	54                   	push   rsp
 4fa:	4d 5f                	rex.WRB pop r15
 4fc:	72 65                	jb     563 <_init-0xa9d>
 4fe:	67 69 73 74 65 72 54 	imul   esi,DWORD PTR [ebx+0x74],0x4d547265
 505:	4d 
 506:	43 6c                	rex.XB ins BYTE PTR es:[rdi],dx
 508:	6f                   	outs   dx,DWORD PTR ds:[rsi]
 509:	6e                   	outs   dx,BYTE PTR ds:[rsi]
 50a:	65 54                	gs push rsp
 50c:	61                   	(bad)  
 50d:	62                   	.byte 0x62
 50e:	6c                   	ins    BYTE PTR es:[rdi],dx
 50f:	65                   	gs
	...

Disassembly of section .gnu.version:

0000000000000512 <.gnu.version>:
 512:	00 00                	add    BYTE PTR [rax],al
 514:	00 00                	add    BYTE PTR [rax],al
 516:	02 00                	add    al,BYTE PTR [rax]
 518:	02 00                	add    al,BYTE PTR [rax]
 51a:	02 00                	add    al,BYTE PTR [rax]
 51c:	00 00                	add    BYTE PTR [rax],al
 51e:	00 00                	add    BYTE PTR [rax],al
 520:	02 00                	add    al,BYTE PTR [rax]

Disassembly of section .gnu.version_r:

0000000000000528 <.gnu.version_r>:
 528:	01 00                	add    DWORD PTR [rax],eax
 52a:	01 00                	add    DWORD PTR [rax],eax
 52c:	01 00                	add    DWORD PTR [rax],eax
 52e:	00 00                	add    BYTE PTR [rax],al
 530:	10 00                	adc    BYTE PTR [rax],al
 532:	00 00                	add    BYTE PTR [rax],al
 534:	00 00                	add    BYTE PTR [rax],al
 536:	00 00                	add    BYTE PTR [rax],al
 538:	75 1a                	jne    554 <_init-0xaac>
 53a:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
 540:	38 00                	cmp    BYTE PTR [rax],al
 542:	00 00                	add    BYTE PTR [rax],al
 544:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000000548 <.rela.dyn>:
 548:	b0 3d                	mov    al,0x3d
 54a:	00 00                	add    BYTE PTR [rax],al
 54c:	00 00                	add    BYTE PTR [rax],al
 54e:	00 00                	add    BYTE PTR [rax],al
 550:	08 00                	or     BYTE PTR [rax],al
 552:	00 00                	add    BYTE PTR [rax],al
 554:	00 00                	add    BYTE PTR [rax],al
 556:	00 00                	add    BYTE PTR [rax],al
 558:	60                   	(bad)  
 559:	11 00                	adc    DWORD PTR [rax],eax
 55b:	00 00                	add    BYTE PTR [rax],al
 55d:	00 00                	add    BYTE PTR [rax],al
 55f:	00 b8 3d 00 00 00    	add    BYTE PTR [rax+0x3d],bh
 565:	00 00                	add    BYTE PTR [rax],al
 567:	00 08                	add    BYTE PTR [rax],cl
 569:	00 00                	add    BYTE PTR [rax],al
 56b:	00 00                	add    BYTE PTR [rax],al
 56d:	00 00                	add    BYTE PTR [rax],al
 56f:	00 20                	add    BYTE PTR [rax],ah
 571:	11 00                	adc    DWORD PTR [rax],eax
 573:	00 00                	add    BYTE PTR [rax],al
 575:	00 00                	add    BYTE PTR [rax],al
 577:	00 08                	add    BYTE PTR [rax],cl
 579:	40 00 00             	add    BYTE PTR [rax],al
 57c:	00 00                	add    BYTE PTR [rax],al
 57e:	00 00                	add    BYTE PTR [rax],al
 580:	08 00                	or     BYTE PTR [rax],al
 582:	00 00                	add    BYTE PTR [rax],al
 584:	00 00                	add    BYTE PTR [rax],al
 586:	00 00                	add    BYTE PTR [rax],al
 588:	08 40 00             	or     BYTE PTR [rax+0x0],al
 58b:	00 00                	add    BYTE PTR [rax],al
 58d:	00 00                	add    BYTE PTR [rax],al
 58f:	00 d8                	add    al,bl
 591:	3f                   	(bad)  
 592:	00 00                	add    BYTE PTR [rax],al
 594:	00 00                	add    BYTE PTR [rax],al
 596:	00 00                	add    BYTE PTR [rax],al
 598:	06                   	(bad)  
 599:	00 00                	add    BYTE PTR [rax],al
 59b:	00 01                	add    BYTE PTR [rcx],al
	...
 5a5:	00 00                	add    BYTE PTR [rax],al
 5a7:	00 e0                	add    al,ah
 5a9:	3f                   	(bad)  
 5aa:	00 00                	add    BYTE PTR [rax],al
 5ac:	00 00                	add    BYTE PTR [rax],al
 5ae:	00 00                	add    BYTE PTR [rax],al
 5b0:	06                   	(bad)  
 5b1:	00 00                	add    BYTE PTR [rax],al
 5b3:	00 04 00             	add    BYTE PTR [rax+rax*1],al
	...
 5be:	00 00                	add    BYTE PTR [rax],al
 5c0:	e8 3f 00 00 00       	call   604 <_init-0x9fc>
 5c5:	00 00                	add    BYTE PTR [rax],al
 5c7:	00 06                	add    BYTE PTR [rsi],al
 5c9:	00 00                	add    BYTE PTR [rax],al
 5cb:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 5d1 <_init-0xa2f>
 5d1:	00 00                	add    BYTE PTR [rax],al
 5d3:	00 00                	add    BYTE PTR [rax],al
 5d5:	00 00                	add    BYTE PTR [rax],al
 5d7:	00 f0                	add    al,dh
 5d9:	3f                   	(bad)  
 5da:	00 00                	add    BYTE PTR [rax],al
 5dc:	00 00                	add    BYTE PTR [rax],al
 5de:	00 00                	add    BYTE PTR [rax],al
 5e0:	06                   	(bad)  
 5e1:	00 00                	add    BYTE PTR [rax],al
 5e3:	00 06                	add    BYTE PTR [rsi],al
	...
 5ed:	00 00                	add    BYTE PTR [rax],al
 5ef:	00 f8                	add    al,bh
 5f1:	3f                   	(bad)  
 5f2:	00 00                	add    BYTE PTR [rax],al
 5f4:	00 00                	add    BYTE PTR [rax],al
 5f6:	00 00                	add    BYTE PTR [rax],al
 5f8:	06                   	(bad)  
 5f9:	00 00                	add    BYTE PTR [rax],al
 5fb:	00 07                	add    BYTE PTR [rdi],al
	...

Disassembly of section .rela.plt:

0000000000000608 <.rela.plt>:
 608:	c8 3f 00 00          	enter  0x3f,0x0
 60c:	00 00                	add    BYTE PTR [rax],al
 60e:	00 00                	add    BYTE PTR [rax],al
 610:	07                   	(bad)  
 611:	00 00                	add    BYTE PTR [rax],al
 613:	00 02                	add    BYTE PTR [rdx],al
	...
 61d:	00 00                	add    BYTE PTR [rax],al
 61f:	00 d0                	add    al,dl
 621:	3f                   	(bad)  
 622:	00 00                	add    BYTE PTR [rax],al
 624:	00 00                	add    BYTE PTR [rax],al
 626:	00 00                	add    BYTE PTR [rax],al
 628:	07                   	(bad)  
 629:	00 00                	add    BYTE PTR [rax],al
 62b:	00 03                	add    BYTE PTR [rbx],al
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
    1020:	ff 35 92 2f 00 00    	push   QWORD PTR [rip+0x2f92]        # 3fb8 <_GLOBAL_OFFSET_TABLE_+0x8>
    1026:	f2 ff 25 93 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f93]        # 3fc0 <_GLOBAL_OFFSET_TABLE_+0x10>
    102d:	0f 1f 00             	nop    DWORD PTR [rax]
    1030:	f3 0f 1e fa          	endbr64 
    1034:	68 00 00 00 00       	push   0x0
    1039:	f2 e9 e1 ff ff ff    	bnd jmp 1020 <.plt>
    103f:	90                   	nop
    1040:	f3 0f 1e fa          	endbr64 
    1044:	68 01 00 00 00       	push   0x1
    1049:	f2 e9 d1 ff ff ff    	bnd jmp 1020 <.plt>
    104f:	90                   	nop

Disassembly of section .plt.got:

0000000000001050 <__cxa_finalize@plt>:
    1050:	f3 0f 1e fa          	endbr64 
    1054:	f2 ff 25 9d 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f9d]        # 3ff8 <__cxa_finalize@GLIBC_2.2.5>
    105b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .plt.sec:

0000000000001060 <strcpy@plt>:
    1060:	f3 0f 1e fa          	endbr64 
    1064:	f2 ff 25 5d 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f5d]        # 3fc8 <strcpy@GLIBC_2.2.5>
    106b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

0000000000001070 <puts@plt>:
    1070:	f3 0f 1e fa          	endbr64 
    1074:	f2 ff 25 55 2f 00 00 	bnd jmp QWORD PTR [rip+0x2f55]        # 3fd0 <puts@GLIBC_2.2.5>
    107b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

Disassembly of section .text:

0000000000001080 <_start>:
    1080:	f3 0f 1e fa          	endbr64 
    1084:	31 ed                	xor    ebp,ebp
    1086:	49 89 d1             	mov    r9,rdx
    1089:	5e                   	pop    rsi
    108a:	48 89 e2             	mov    rdx,rsp
    108d:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
    1091:	50                   	push   rax
    1092:	54                   	push   rsp
    1093:	4c 8d 05 b6 01 00 00 	lea    r8,[rip+0x1b6]        # 1250 <__libc_csu_fini>
    109a:	48 8d 0d 3f 01 00 00 	lea    rcx,[rip+0x13f]        # 11e0 <__libc_csu_init>
    10a1:	48 8d 3d 00 01 00 00 	lea    rdi,[rip+0x100]        # 11a8 <main>
    10a8:	ff 15 32 2f 00 00    	call   QWORD PTR [rip+0x2f32]        # 3fe0 <__libc_start_main@GLIBC_2.2.5>
    10ae:	f4                   	hlt    
    10af:	90                   	nop

00000000000010b0 <deregister_tm_clones>:
    10b0:	48 8d 3d 59 2f 00 00 	lea    rdi,[rip+0x2f59]        # 4010 <__TMC_END__>
    10b7:	48 8d 05 52 2f 00 00 	lea    rax,[rip+0x2f52]        # 4010 <__TMC_END__>
    10be:	48 39 f8             	cmp    rax,rdi
    10c1:	74 15                	je     10d8 <deregister_tm_clones+0x28>
    10c3:	48 8b 05 0e 2f 00 00 	mov    rax,QWORD PTR [rip+0x2f0e]        # 3fd8 <_ITM_deregisterTMCloneTable>
    10ca:	48 85 c0             	test   rax,rax
    10cd:	74 09                	je     10d8 <deregister_tm_clones+0x28>
    10cf:	ff e0                	jmp    rax
    10d1:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    10d8:	c3                   	ret    
    10d9:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

00000000000010e0 <register_tm_clones>:
    10e0:	48 8d 3d 29 2f 00 00 	lea    rdi,[rip+0x2f29]        # 4010 <__TMC_END__>
    10e7:	48 8d 35 22 2f 00 00 	lea    rsi,[rip+0x2f22]        # 4010 <__TMC_END__>
    10ee:	48 29 fe             	sub    rsi,rdi
    10f1:	48 89 f0             	mov    rax,rsi
    10f4:	48 c1 ee 3f          	shr    rsi,0x3f
    10f8:	48 c1 f8 03          	sar    rax,0x3
    10fc:	48 01 c6             	add    rsi,rax
    10ff:	48 d1 fe             	sar    rsi,1
    1102:	74 14                	je     1118 <register_tm_clones+0x38>
    1104:	48 8b 05 e5 2e 00 00 	mov    rax,QWORD PTR [rip+0x2ee5]        # 3ff0 <_ITM_registerTMCloneTable>
    110b:	48 85 c0             	test   rax,rax
    110e:	74 08                	je     1118 <register_tm_clones+0x38>
    1110:	ff e0                	jmp    rax
    1112:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]
    1118:	c3                   	ret    
    1119:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

0000000000001120 <__do_global_dtors_aux>:
    1120:	f3 0f 1e fa          	endbr64 
    1124:	80 3d e5 2e 00 00 00 	cmp    BYTE PTR [rip+0x2ee5],0x0        # 4010 <__TMC_END__>
    112b:	75 2b                	jne    1158 <__do_global_dtors_aux+0x38>
    112d:	55                   	push   rbp
    112e:	48 83 3d c2 2e 00 00 	cmp    QWORD PTR [rip+0x2ec2],0x0        # 3ff8 <__cxa_finalize@GLIBC_2.2.5>
    1135:	00 
    1136:	48 89 e5             	mov    rbp,rsp
    1139:	74 0c                	je     1147 <__do_global_dtors_aux+0x27>
    113b:	48 8b 3d c6 2e 00 00 	mov    rdi,QWORD PTR [rip+0x2ec6]        # 4008 <__dso_handle>
    1142:	e8 09 ff ff ff       	call   1050 <__cxa_finalize@plt>
    1147:	e8 64 ff ff ff       	call   10b0 <deregister_tm_clones>
    114c:	c6 05 bd 2e 00 00 01 	mov    BYTE PTR [rip+0x2ebd],0x1        # 4010 <__TMC_END__>
    1153:	5d                   	pop    rbp
    1154:	c3                   	ret    
    1155:	0f 1f 00             	nop    DWORD PTR [rax]
    1158:	c3                   	ret    
    1159:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]

0000000000001160 <frame_dummy>:
    1160:	f3 0f 1e fa          	endbr64 
    1164:	e9 77 ff ff ff       	jmp    10e0 <register_tm_clones>

0000000000001169 <exploit_me>:
    1169:	f3 0f 1e fa          	endbr64 
    116d:	55                   	push   rbp
    116e:	48 89 e5             	mov    rbp,rsp
    1171:	48 83 ec 40          	sub    rsp,0x40
    1175:	48 89 7d c8          	mov    QWORD PTR [rbp-0x38],rdi
    1179:	c7 45 fc 00 00 00 00 	mov    DWORD PTR [rbp-0x4],0x0
    1180:	48 8b 55 c8          	mov    rdx,QWORD PTR [rbp-0x38]
    1184:	48 8d 45 d0          	lea    rax,[rbp-0x30]
    1188:	48 89 d6             	mov    rsi,rdx
    118b:	48 89 c7             	mov    rdi,rax
    118e:	e8 cd fe ff ff       	call   1060 <strcpy@plt>
    1193:	83 7d fc 00          	cmp    DWORD PTR [rbp-0x4],0x0
    1197:	74 0c                	je     11a5 <exploit_me+0x3c>
    1199:	48 8d 3d 64 0e 00 00 	lea    rdi,[rip+0xe64]        # 2004 <_IO_stdin_used+0x4>
    11a0:	e8 cb fe ff ff       	call   1070 <puts@plt>
    11a5:	90                   	nop
    11a6:	c9                   	leave  
    11a7:	c3                   	ret    

00000000000011a8 <main>:
    11a8:	f3 0f 1e fa          	endbr64 
    11ac:	55                   	push   rbp
    11ad:	48 89 e5             	mov    rbp,rsp
    11b0:	48 83 ec 10          	sub    rsp,0x10
    11b4:	89 7d fc             	mov    DWORD PTR [rbp-0x4],edi
    11b7:	48 89 75 f0          	mov    QWORD PTR [rbp-0x10],rsi
    11bb:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
    11bf:	48 83 c0 08          	add    rax,0x8
    11c3:	48 8b 00             	mov    rax,QWORD PTR [rax]
    11c6:	48 89 c7             	mov    rdi,rax
    11c9:	e8 9b ff ff ff       	call   1169 <exploit_me>
    11ce:	b8 00 00 00 00       	mov    eax,0x0
    11d3:	c9                   	leave  
    11d4:	c3                   	ret    
    11d5:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
    11dc:	00 00 00 
    11df:	90                   	nop

00000000000011e0 <__libc_csu_init>:
    11e0:	f3 0f 1e fa          	endbr64 
    11e4:	41 57                	push   r15
    11e6:	4c 8d 3d c3 2b 00 00 	lea    r15,[rip+0x2bc3]        # 3db0 <__frame_dummy_init_array_entry>
    11ed:	41 56                	push   r14
    11ef:	49 89 d6             	mov    r14,rdx
    11f2:	41 55                	push   r13
    11f4:	49 89 f5             	mov    r13,rsi
    11f7:	41 54                	push   r12
    11f9:	41 89 fc             	mov    r12d,edi
    11fc:	55                   	push   rbp
    11fd:	48 8d 2d b4 2b 00 00 	lea    rbp,[rip+0x2bb4]        # 3db8 <__do_global_dtors_aux_fini_array_entry>
    1204:	53                   	push   rbx
    1205:	4c 29 fd             	sub    rbp,r15
    1208:	48 83 ec 08          	sub    rsp,0x8
    120c:	e8 ef fd ff ff       	call   1000 <_init>
    1211:	48 c1 fd 03          	sar    rbp,0x3
    1215:	74 1f                	je     1236 <__libc_csu_init+0x56>
    1217:	31 db                	xor    ebx,ebx
    1219:	0f 1f 80 00 00 00 00 	nop    DWORD PTR [rax+0x0]
    1220:	4c 89 f2             	mov    rdx,r14
    1223:	4c 89 ee             	mov    rsi,r13
    1226:	44 89 e7             	mov    edi,r12d
    1229:	41 ff 14 df          	call   QWORD PTR [r15+rbx*8]
    122d:	48 83 c3 01          	add    rbx,0x1
    1231:	48 39 dd             	cmp    rbp,rbx
    1234:	75 ea                	jne    1220 <__libc_csu_init+0x40>
    1236:	48 83 c4 08          	add    rsp,0x8
    123a:	5b                   	pop    rbx
    123b:	5d                   	pop    rbp
    123c:	41 5c                	pop    r12
    123e:	41 5d                	pop    r13
    1240:	41 5e                	pop    r14
    1242:	41 5f                	pop    r15
    1244:	c3                   	ret    
    1245:	66 66 2e 0f 1f 84 00 	data16 nop WORD PTR cs:[rax+rax*1+0x0]
    124c:	00 00 00 00 

0000000000001250 <__libc_csu_fini>:
    1250:	f3 0f 1e fa          	endbr64 
    1254:	c3                   	ret    

Disassembly of section .fini:

0000000000001258 <_fini>:
    1258:	f3 0f 1e fa          	endbr64 
    125c:	48 83 ec 08          	sub    rsp,0x8
    1260:	48 83 c4 08          	add    rsp,0x8
    1264:	c3                   	ret    

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
    201c:	4c 00 00             	rex.WR add BYTE PTR [rax],r8b
    201f:	00 08                	add    BYTE PTR [rax],cl
    2021:	00 00                	add    BYTE PTR [rax],al
    2023:	00 08                	add    BYTE PTR [rax],cl
    2025:	f0 ff                	lock (bad) 
    2027:	ff 80 00 00 00 38    	inc    DWORD PTR [rax+0x38000000]
    202d:	f0 ff                	lock (bad) 
    202f:	ff a8 00 00 00 48    	jmp    FWORD PTR [rax+0x48000000]
    2035:	f0 ff                	lock (bad) 
    2037:	ff c0                	inc    eax
    2039:	00 00                	add    BYTE PTR [rax],al
    203b:	00 68 f0             	add    BYTE PTR [rax-0x10],ch
    203e:	ff                   	(bad)  
    203f:	ff 68 00             	jmp    FWORD PTR [rax+0x0]
    2042:	00 00                	add    BYTE PTR [rax],al
    2044:	51                   	push   rcx
    2045:	f1                   	icebp  
    2046:	ff                   	(bad)  
    2047:	ff                   	(bad)  
    2048:	d8 00                	fadd   DWORD PTR [rax]
    204a:	00 00                	add    BYTE PTR [rax],al
    204c:	90                   	nop
    204d:	f1                   	icebp  
    204e:	ff                   	(bad)  
    204f:	ff                   	(bad)  
    2050:	f8                   	clc    
    2051:	00 00                	add    BYTE PTR [rax],al
    2053:	00 c8                	add    al,cl
    2055:	f1                   	icebp  
    2056:	ff                   	(bad)  
    2057:	ff 18                	call   FWORD PTR [rax]
    2059:	01 00                	add    DWORD PTR [rax],eax
    205b:	00 38                	add    BYTE PTR [rax],bh
    205d:	f2 ff                	repnz (bad) 
    205f:	ff 60 01             	jmp    QWORD PTR [rax+0x1]
	...

Disassembly of section .eh_frame:

0000000000002068 <__FRAME_END__-0x124>:
    2068:	14 00                	adc    al,0x0
    206a:	00 00                	add    BYTE PTR [rax],al
    206c:	00 00                	add    BYTE PTR [rax],al
    206e:	00 00                	add    BYTE PTR [rax],al
    2070:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
    2073:	00 01                	add    BYTE PTR [rcx],al
    2075:	78 10                	js     2087 <__GNU_EH_FRAME_HDR+0x6f>
    2077:	01 1b                	add    DWORD PTR [rbx],ebx
    2079:	0c 07                	or     al,0x7
    207b:	08 90 01 00 00 14    	or     BYTE PTR [rax+0x14000001],dl
    2081:	00 00                	add    BYTE PTR [rax],al
    2083:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    2086:	00 00                	add    BYTE PTR [rax],al
    2088:	f8                   	clc    
    2089:	ef                   	out    dx,eax
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
    20a3:	ff 30                	push   QWORD PTR [rax]
    20a5:	00 00                	add    BYTE PTR [rax],al
    20a7:	00 00                	add    BYTE PTR [rax],al
    20a9:	0e                   	(bad)  
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
    20c7:	00 88 ef ff ff 10    	add    BYTE PTR [rax+0x10ffffef],cl
	...
    20d5:	00 00                	add    BYTE PTR [rax],al
    20d7:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    20da:	00 00                	add    BYTE PTR [rax],al
    20dc:	74 00                	je     20de <__GNU_EH_FRAME_HDR+0xc6>
    20de:	00 00                	add    BYTE PTR [rax],al
    20e0:	80 ef ff             	sub    bh,0xff
    20e3:	ff 20                	jmp    QWORD PTR [rax]
	...
    20ed:	00 00                	add    BYTE PTR [rax],al
    20ef:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    20f2:	00 00                	add    BYTE PTR [rax],al
    20f4:	8c 00                	mov    WORD PTR [rax],es
    20f6:	00 00                	add    BYTE PTR [rax],al
    20f8:	71 f0                	jno    20ea <__GNU_EH_FRAME_HDR+0xd2>
    20fa:	ff                   	(bad)  
    20fb:	ff                   	(bad)  
    20fc:	3f                   	(bad)  
    20fd:	00 00                	add    BYTE PTR [rax],al
    20ff:	00 00                	add    BYTE PTR [rax],al
    2101:	45 0e                	rex.RB (bad) 
    2103:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2109:	76 0c                	jbe    2117 <__GNU_EH_FRAME_HDR+0xff>
    210b:	07                   	(bad)  
    210c:	08 00                	or     BYTE PTR [rax],al
    210e:	00 00                	add    BYTE PTR [rax],al
    2110:	1c 00                	sbb    al,0x0
    2112:	00 00                	add    BYTE PTR [rax],al
    2114:	ac                   	lods   al,BYTE PTR ds:[rsi]
    2115:	00 00                	add    BYTE PTR [rax],al
    2117:	00 90 f0 ff ff 2d    	add    BYTE PTR [rax+0x2dfffff0],dl
    211d:	00 00                	add    BYTE PTR [rax],al
    211f:	00 00                	add    BYTE PTR [rax],al
    2121:	45 0e                	rex.RB (bad) 
    2123:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    2129:	64 0c 07             	fs or  al,0x7
    212c:	08 00                	or     BYTE PTR [rax],al
    212e:	00 00                	add    BYTE PTR [rax],al
    2130:	44 00 00             	add    BYTE PTR [rax],r8b
    2133:	00 cc                	add    ah,cl
    2135:	00 00                	add    BYTE PTR [rax],al
    2137:	00 a8 f0 ff ff 65    	add    BYTE PTR [rax+0x65fffff0],ch
    213d:	00 00                	add    BYTE PTR [rax],al
    213f:	00 00                	add    BYTE PTR [rax],al
    2141:	46 0e                	rex.RX (bad) 
    2143:	10 8f 02 49 0e 18    	adc    BYTE PTR [rdi+0x180e4902],cl
    2149:	8e 03                	mov    es,WORD PTR [rbx]
    214b:	45 0e                	rex.RB (bad) 
    214d:	20 8d 04 45 0e 28    	and    BYTE PTR [rbp+0x280e4504],cl
    2153:	8c 05 44 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e44],es        # ffffffff86302f9d <_end+0xffffffff862fef85>
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
    2180:	d0 f0                	shl    al,1
    2182:	ff                   	(bad)  
    2183:	ff 05 00 00 00 00    	inc    DWORD PTR [rip+0x0]        # 2189 <__GNU_EH_FRAME_HDR+0x171>
    2189:	00 00                	add    BYTE PTR [rax],al
	...

000000000000218c <__FRAME_END__>:
    218c:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000003db0 <__frame_dummy_init_array_entry>:
    3db0:	60                   	(bad)  
    3db1:	11 00                	adc    DWORD PTR [rax],eax
    3db3:	00 00                	add    BYTE PTR [rax],al
    3db5:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000003db8 <__do_global_dtors_aux_fini_array_entry>:
    3db8:	20 11                	and    BYTE PTR [rcx],dl
    3dba:	00 00                	add    BYTE PTR [rax],al
    3dbc:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000003dc0 <_DYNAMIC>:
    3dc0:	01 00                	add    DWORD PTR [rax],eax
    3dc2:	00 00                	add    BYTE PTR [rax],al
    3dc4:	00 00                	add    BYTE PTR [rax],al
    3dc6:	00 00                	add    BYTE PTR [rax],al
    3dc8:	01 00                	add    DWORD PTR [rax],eax
    3dca:	00 00                	add    BYTE PTR [rax],al
    3dcc:	00 00                	add    BYTE PTR [rax],al
    3dce:	00 00                	add    BYTE PTR [rax],al
    3dd0:	0c 00                	or     al,0x0
    3dd2:	00 00                	add    BYTE PTR [rax],al
    3dd4:	00 00                	add    BYTE PTR [rax],al
    3dd6:	00 00                	add    BYTE PTR [rax],al
    3dd8:	00 10                	add    BYTE PTR [rax],dl
    3dda:	00 00                	add    BYTE PTR [rax],al
    3ddc:	00 00                	add    BYTE PTR [rax],al
    3dde:	00 00                	add    BYTE PTR [rax],al
    3de0:	0d 00 00 00 00       	or     eax,0x0
    3de5:	00 00                	add    BYTE PTR [rax],al
    3de7:	00 58 12             	add    BYTE PTR [rax+0x12],bl
    3dea:	00 00                	add    BYTE PTR [rax],al
    3dec:	00 00                	add    BYTE PTR [rax],al
    3dee:	00 00                	add    BYTE PTR [rax],al
    3df0:	19 00                	sbb    DWORD PTR [rax],eax
    3df2:	00 00                	add    BYTE PTR [rax],al
    3df4:	00 00                	add    BYTE PTR [rax],al
    3df6:	00 00                	add    BYTE PTR [rax],al
    3df8:	b0 3d                	mov    al,0x3d
    3dfa:	00 00                	add    BYTE PTR [rax],al
    3dfc:	00 00                	add    BYTE PTR [rax],al
    3dfe:	00 00                	add    BYTE PTR [rax],al
    3e00:	1b 00                	sbb    eax,DWORD PTR [rax]
    3e02:	00 00                	add    BYTE PTR [rax],al
    3e04:	00 00                	add    BYTE PTR [rax],al
    3e06:	00 00                	add    BYTE PTR [rax],al
    3e08:	08 00                	or     BYTE PTR [rax],al
    3e0a:	00 00                	add    BYTE PTR [rax],al
    3e0c:	00 00                	add    BYTE PTR [rax],al
    3e0e:	00 00                	add    BYTE PTR [rax],al
    3e10:	1a 00                	sbb    al,BYTE PTR [rax]
    3e12:	00 00                	add    BYTE PTR [rax],al
    3e14:	00 00                	add    BYTE PTR [rax],al
    3e16:	00 00                	add    BYTE PTR [rax],al
    3e18:	b8 3d 00 00 00       	mov    eax,0x3d
    3e1d:	00 00                	add    BYTE PTR [rax],al
    3e1f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    3e22:	00 00                	add    BYTE PTR [rax],al
    3e24:	00 00                	add    BYTE PTR [rax],al
    3e26:	00 00                	add    BYTE PTR [rax],al
    3e28:	08 00                	or     BYTE PTR [rax],al
    3e2a:	00 00                	add    BYTE PTR [rax],al
    3e2c:	00 00                	add    BYTE PTR [rax],al
    3e2e:	00 00                	add    BYTE PTR [rax],al
    3e30:	f5                   	cmc    
    3e31:	fe                   	(bad)  
    3e32:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3e35:	00 00                	add    BYTE PTR [rax],al
    3e37:	00 a0 03 00 00 00    	add    BYTE PTR [rax+0x3],ah
    3e3d:	00 00                	add    BYTE PTR [rax],al
    3e3f:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 3e45 <_DYNAMIC+0x85>
    3e45:	00 00                	add    BYTE PTR [rax],al
    3e47:	00 88 04 00 00 00    	add    BYTE PTR [rax+0x4],cl
    3e4d:	00 00                	add    BYTE PTR [rax],al
    3e4f:	00 06                	add    BYTE PTR [rsi],al
    3e51:	00 00                	add    BYTE PTR [rax],al
    3e53:	00 00                	add    BYTE PTR [rax],al
    3e55:	00 00                	add    BYTE PTR [rax],al
    3e57:	00 c8                	add    al,cl
    3e59:	03 00                	add    eax,DWORD PTR [rax]
    3e5b:	00 00                	add    BYTE PTR [rax],al
    3e5d:	00 00                	add    BYTE PTR [rax],al
    3e5f:	00 0a                	add    BYTE PTR [rdx],cl
    3e61:	00 00                	add    BYTE PTR [rax],al
    3e63:	00 00                	add    BYTE PTR [rax],al
    3e65:	00 00                	add    BYTE PTR [rax],al
    3e67:	00 89 00 00 00 00    	add    BYTE PTR [rcx+0x0],cl
    3e6d:	00 00                	add    BYTE PTR [rax],al
    3e6f:	00 0b                	add    BYTE PTR [rbx],cl
    3e71:	00 00                	add    BYTE PTR [rax],al
    3e73:	00 00                	add    BYTE PTR [rax],al
    3e75:	00 00                	add    BYTE PTR [rax],al
    3e77:	00 18                	add    BYTE PTR [rax],bl
    3e79:	00 00                	add    BYTE PTR [rax],al
    3e7b:	00 00                	add    BYTE PTR [rax],al
    3e7d:	00 00                	add    BYTE PTR [rax],al
    3e7f:	00 15 00 00 00 00    	add    BYTE PTR [rip+0x0],dl        # 3e85 <_DYNAMIC+0xc5>
	...
    3e8d:	00 00                	add    BYTE PTR [rax],al
    3e8f:	00 03                	add    BYTE PTR [rbx],al
    3e91:	00 00                	add    BYTE PTR [rax],al
    3e93:	00 00                	add    BYTE PTR [rax],al
    3e95:	00 00                	add    BYTE PTR [rax],al
    3e97:	00 b0 3f 00 00 00    	add    BYTE PTR [rax+0x3f],dh
    3e9d:	00 00                	add    BYTE PTR [rax],al
    3e9f:	00 02                	add    BYTE PTR [rdx],al
    3ea1:	00 00                	add    BYTE PTR [rax],al
    3ea3:	00 00                	add    BYTE PTR [rax],al
    3ea5:	00 00                	add    BYTE PTR [rax],al
    3ea7:	00 30                	add    BYTE PTR [rax],dh
    3ea9:	00 00                	add    BYTE PTR [rax],al
    3eab:	00 00                	add    BYTE PTR [rax],al
    3ead:	00 00                	add    BYTE PTR [rax],al
    3eaf:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    3eb2:	00 00                	add    BYTE PTR [rax],al
    3eb4:	00 00                	add    BYTE PTR [rax],al
    3eb6:	00 00                	add    BYTE PTR [rax],al
    3eb8:	07                   	(bad)  
    3eb9:	00 00                	add    BYTE PTR [rax],al
    3ebb:	00 00                	add    BYTE PTR [rax],al
    3ebd:	00 00                	add    BYTE PTR [rax],al
    3ebf:	00 17                	add    BYTE PTR [rdi],dl
    3ec1:	00 00                	add    BYTE PTR [rax],al
    3ec3:	00 00                	add    BYTE PTR [rax],al
    3ec5:	00 00                	add    BYTE PTR [rax],al
    3ec7:	00 08                	add    BYTE PTR [rax],cl
    3ec9:	06                   	(bad)  
    3eca:	00 00                	add    BYTE PTR [rax],al
    3ecc:	00 00                	add    BYTE PTR [rax],al
    3ece:	00 00                	add    BYTE PTR [rax],al
    3ed0:	07                   	(bad)  
    3ed1:	00 00                	add    BYTE PTR [rax],al
    3ed3:	00 00                	add    BYTE PTR [rax],al
    3ed5:	00 00                	add    BYTE PTR [rax],al
    3ed7:	00 48 05             	add    BYTE PTR [rax+0x5],cl
    3eda:	00 00                	add    BYTE PTR [rax],al
    3edc:	00 00                	add    BYTE PTR [rax],al
    3ede:	00 00                	add    BYTE PTR [rax],al
    3ee0:	08 00                	or     BYTE PTR [rax],al
    3ee2:	00 00                	add    BYTE PTR [rax],al
    3ee4:	00 00                	add    BYTE PTR [rax],al
    3ee6:	00 00                	add    BYTE PTR [rax],al
    3ee8:	c0 00 00             	rol    BYTE PTR [rax],0x0
    3eeb:	00 00                	add    BYTE PTR [rax],al
    3eed:	00 00                	add    BYTE PTR [rax],al
    3eef:	00 09                	add    BYTE PTR [rcx],cl
    3ef1:	00 00                	add    BYTE PTR [rax],al
    3ef3:	00 00                	add    BYTE PTR [rax],al
    3ef5:	00 00                	add    BYTE PTR [rax],al
    3ef7:	00 18                	add    BYTE PTR [rax],bl
    3ef9:	00 00                	add    BYTE PTR [rax],al
    3efb:	00 00                	add    BYTE PTR [rax],al
    3efd:	00 00                	add    BYTE PTR [rax],al
    3eff:	00 1e                	add    BYTE PTR [rsi],bl
    3f01:	00 00                	add    BYTE PTR [rax],al
    3f03:	00 00                	add    BYTE PTR [rax],al
    3f05:	00 00                	add    BYTE PTR [rax],al
    3f07:	00 08                	add    BYTE PTR [rax],cl
    3f09:	00 00                	add    BYTE PTR [rax],al
    3f0b:	00 00                	add    BYTE PTR [rax],al
    3f0d:	00 00                	add    BYTE PTR [rax],al
    3f0f:	00 fb                	add    bl,bh
    3f11:	ff                   	(bad)  
    3f12:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f15:	00 00                	add    BYTE PTR [rax],al
    3f17:	00 01                	add    BYTE PTR [rcx],al
    3f19:	00 00                	add    BYTE PTR [rax],al
    3f1b:	08 00                	or     BYTE PTR [rax],al
    3f1d:	00 00                	add    BYTE PTR [rax],al
    3f1f:	00 fe                	add    dh,bh
    3f21:	ff                   	(bad)  
    3f22:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f25:	00 00                	add    BYTE PTR [rax],al
    3f27:	00 28                	add    BYTE PTR [rax],ch
    3f29:	05 00 00 00 00       	add    eax,0x0
    3f2e:	00 00                	add    BYTE PTR [rax],al
    3f30:	ff                   	(bad)  
    3f31:	ff                   	(bad)  
    3f32:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f35:	00 00                	add    BYTE PTR [rax],al
    3f37:	00 01                	add    BYTE PTR [rcx],al
    3f39:	00 00                	add    BYTE PTR [rax],al
    3f3b:	00 00                	add    BYTE PTR [rax],al
    3f3d:	00 00                	add    BYTE PTR [rax],al
    3f3f:	00 f0                	add    al,dh
    3f41:	ff                   	(bad)  
    3f42:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f45:	00 00                	add    BYTE PTR [rax],al
    3f47:	00 12                	add    BYTE PTR [rdx],dl
    3f49:	05 00 00 00 00       	add    eax,0x0
    3f4e:	00 00                	add    BYTE PTR [rax],al
    3f50:	f9                   	stc    
    3f51:	ff                   	(bad)  
    3f52:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    3f55:	00 00                	add    BYTE PTR [rax],al
    3f57:	00 03                	add    BYTE PTR [rbx],al
	...

Disassembly of section .got:

0000000000003fb0 <_GLOBAL_OFFSET_TABLE_>:
    3fb0:	c0 3d 00 00 00 00 00 	sar    BYTE PTR [rip+0x0],0x0        # 3fb7 <_GLOBAL_OFFSET_TABLE_+0x7>
	...
    3fc7:	00 30                	add    BYTE PTR [rax],dh
    3fc9:	10 00                	adc    BYTE PTR [rax],al
    3fcb:	00 00                	add    BYTE PTR [rax],al
    3fcd:	00 00                	add    BYTE PTR [rax],al
    3fcf:	00 40 10             	add    BYTE PTR [rax+0x10],al
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

0000000000004010 <completed.8061>:
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
  11:	30 2d 31 75 62 75    	xor    BYTE PTR [rip+0x75627531],ch        # 75627548 <_end+0x75623530>
  17:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  18:	74 75                	je     8f <_init-0xf71>
  1a:	31 7e 32             	xor    DWORD PTR [rsi+0x32],edi
  1d:	30 2e                	xor    BYTE PTR [rsi],ch
  1f:	30 34 2e             	xor    BYTE PTR [rsi+rbp*1],dh
  22:	31 29                	xor    DWORD PTR [rcx],ebp
  24:	20 39                	and    BYTE PTR [rcx],bh
  26:	2e 34 2e             	cs xor al,0x2e
  29:	30 00                	xor    BYTE PTR [rax],al
