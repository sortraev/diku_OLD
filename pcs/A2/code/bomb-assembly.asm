00000000000026ff <phase_1>:
    ; void phase_1(char *line) {
    ;   do {
    ;     char a = *(line++);
    ;     char b = *(pw++);
    ;     if (a != b)
    ;       explode();
    ;   } while (*pw != '\0');
    ;   say("phase 1 defused");
    ;   return;
    ; }
    26ff:	55                   	push   rbp
    2700:	48 89 e5             	mov    rbp,rsp
    2703:	48 83 ec 20          	sub    rsp,0x20
    2707:	48 89 7d e8          	mov    QWORD PTR [line_addr],rdi

                                    ; char *pw = "Why'd you leave the keys upon the table?";
    270b:	48 8d 05 8e 0c 00 00 	lea    rax,[rip+0xc8e]        # 33a0 <phase_6+0x5bf>
    2712:	48 89 45 f8          	mov    QWORD PTR [pw_addr],rax

                                    ; increment the line pointer and load a byte
                                    ; from the previously stored line pointer.
    2716:	48 8b 45 e8          	mov    rax,QWORD PTR [line_addr]
    271a:	48 8d 50 01          	lea    rdx,[rax+0x1]
    271e:	48 89 55 e8          	mov    QWORD PTR [line_addr],rdx
    2722:	0f b6 08             	movzx  ecx,BYTE PTR [rax]

                                    ; same for the password pointer.
    2725:	48 8b 45 f8          	mov    rax,QWORD PTR [pw_addr]
    2729:	48 8d 50 01          	lea    rdx,[rax+0x1]
    272d:	48 89 55 f8          	mov    QWORD PTR [pw_addr],rdx
    2731:	0f b6 00             	movzx  eax,BYTE PTR [rax]

                                    ; compare the two read bytes.
                                    ; explode if not equal.
    2734:	38 c1                	cmp    cl,al
    2736:	74 0a                	je     2742 <phase_1+0x43>
    2738:	b8 00 00 00 00       	mov    eax,0x0
    273d:	e8 f5 eb ff ff       	call   1337 <explode>

                                    ; do-while loop condition: repeat if the two
                                    ; bytes read from either string are equal.
    2742:	48 8b 45 f8          	mov    rax,QWORD PTR [pw_addr]
    2746:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    2749:	84 c0                	test   al,al
    274b:	75 c9                	jne    2716 <phase_1+0x17>

                                    ; say("Phase 1 defused");
    274d:	48 8d 3d 75 0c 00 00 	lea    rdi,[rip+0xc75]        # 33c9 <phase_6+0x5e8>
    2754:	e8 60 eb ff ff       	call   12b9 <say>
    2759:	90                   	nop
    275a:	c9                   	leave
    275b:	c3                   	ret

000000000000275c <phase_2>:

    ; this phase reads two floats from the input line, and makes two seemingly
    ; identical computations on those floats. The bomb explodes if the two
    ; results are equal, which at first seems wrong!
    ;
    ; However, the two expressions are only equal if we assume commutativity of
    ; the maxss instruction, and the documentation clearly states that if either
    ; operand to maxss is nan, then the second operand is returned. This eg.
    ; means that max(nan, 2) = 2, but max(2, nan) = nan.
    ;
    ; However, the value 0x803f0000 is intentionally chosen such that there
    ; exists a non-nan value x such that isnan(0x803f0000 ^ x) is true. 
    ; From a deep-dive into the bits, we find that x = 3.
    ; 
    ; Let max(x, y) be a max operator on floats which returns y if *either*
    ; operand is NAN, and xor(x, y) be an XOR operation on the raw bits of two
    ; values.
    ;
    ; then the decompiled function is this:
    ;
    ; void phase_2(char *line) {
    ;   int weird_val = 0x803f0000;
    ;   float a, b;
    ;   int n_read = sscanf(line, "%f %f", &a, &b);
    ;   if (n_read <= 1)
    ;     explode();
    ;
    ;   float x = xor(max(b, a), weird_val);
    ;   float y = xor(max(a, b), weird_val);
    ;
    ;   if (x == y)
    ;     explode();
    ;   say("Phase 2 defused");
    ;   return;
    ; }
    275c:	48 83 ec 18          	sub    rsp,0x18
    2760:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28

    2767:	00 00
    2769:	48 89 44 24 08       	mov    QWORD PTR [rsp+0x8],rax
    276e:	31 c0                	xor    eax,eax
    2770:	48 89 e2             	mov    rdx,rsp

    2773:	48 8d 4c 24 04       	lea    rcx,[rsp+0x4]
    2778:	48 8d 35 64 0c 00 00 	lea    rsi,[fmt_str_addr]        # 33e3 <phase_6+0x602>

                                    ; read two floats using sscanf with "%f %f"
                                    ; into variables a and b. explode if less
                                    ; than 2 values read.
    277f:	e8 cc e9 ff ff       	call   1150 <__isoc99_sscanf@plt>
    2784:	83 f8 01             	cmp    eax,0x1
    2787:	7e 4a                	jle    27d3 <phase_2+0x77>

    2789:	f3 0f 10 04 24       	movss  xmm0,DWORD PTR [rsp]      ; xmm0 = a
    278e:	f3 0f 10 54 24 04    	movss  xmm2,DWORD PTR [rsp+0x4]  ; xmm2 = b
    2794:	0f 28 ca             	movaps xmm1,xmm2                 ; xmm1 = b

                                    ; xmm1 = max(b, a);
    2797:	f3 0f 5f c8          	maxss  xmm1,xmm0

                                    ; int weird_val = 0x803f0000;
    279b:	f3 0f 10 1d 7d 0c 00 	movss  xmm3,DWORD PTR [rip+0xc7d] # 3420 <phase_6+0x63f>
    27a2:	00
                                    ; float x = xor(max(b, a), weird_val);
    27a3:	0f 57 cb             	xorps  xmm1,xmm3

                                    ; float y = xor(max(a, b), weird_val);
    27a6:	f3 0f 5f c2          	maxss  xmm0,xmm2 ; xmm0 = max(a, b)
    27aa:	0f 57 c3             	xorps  xmm0,xmm3 ; xmm0 = xor(max(a, b), weird_val)

                                    ; if the two results are equal, explode!
    27ad:	0f 2f c1             	comiss xmm0,xmm1
    27b0:	74 2d                	je     27df <phase_2+0x83>
    27b2:	48 8d 3d 37 0c 00 00 	lea    rdi,[rip+0xc37]        # 33f0 <phase_6+0x60f>
    27b9:	e8 fb ea ff ff       	call   12b9 <say>
    27be:	48 8b 44 24 08       	mov    rax,QWORD PTR [rsp+0x8]
    27c3:	64 48 33 04 25 28 00 	xor    rax,QWORD PTR fs:0x28
    27ca:	00 00
    27cc:	75 1d                	jne    27eb <phase_2+0x8f>
    27ce:	48 83 c4 18          	add    rsp,0x18
    27d2:	c3                   	ret

    27d3:	b8 00 00 00 00       	mov    eax,0x0
    27d8:	e8 5a eb ff ff       	call   1337 <explode>
    27dd:	eb aa                	jmp    2789 <phase_2+0x2d> ; never reached!!! lol
    27df:	b8 00 00 00 00       	mov    eax,0x0
    27e4:	e8 4e eb ff ff       	call   1337 <explode>
    27e9:	eb c7                	jmp    27b2 <phase_2+0x56> ; also never reached
    27eb:	e8 90 e8 ff ff       	call   1080 <__stack_chk_fail@plt>

00000000000027f0 <phase_3>:
    ; phase 3 reads two long ints x and y from the input line using sscanf
    ; and checks that:
    ;
    ; 0) number of ints read is 2;
    ; 1) x is greater than 0x1dcd64fff == 7999999999;
    ; 2) y is greater than 0x12a05f1ff == 4999999999;
    ; 3) for each number, we have n & (n - 1) == 0, meaning powers of 2;
    ; 4) y and x are different.
    ;
    ; hence we simply pick the first power of two greater than 7999999999, set x
    ; to this value, and choose y = 2 * x. easy! :p

    ; the pseudocode for this phase is:

    ; void phase_3(char *line) {
    ;   int x, y;
    ;   int n_read = sscanf(line, "%lld %lld", &x, &y);
    ;   if (n_read <= 1)
    ;     explode();
    ;   if (x <= 7999999999)
    ;     explode();
    ;   if (y <= 4999999999)
    ;     explode();
    ;   if (x & (x - 1) != 0)
    ;     explode();
    ;   if (y & (y - 1) != 0)
    ;     explode();
    ;   say("phase 3 defused");
    ;   return;
    ; }
    27f0:	55                   	push   rbp
    27f1:	48 89 e5             	mov    rbp,rsp
    27f4:	48 83 ec 30          	sub    rsp,0x30
    27f8:	48 89 7d d8          	mov    QWORD PTR [line_addr],rdi
    27fc:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    2803:	00 00
    2805:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    2809:	31 c0                	xor    eax,eax
    280b:	48 8d 4d f0          	lea    rcx,[x_addr]
    280f:	48 8d 55 e8          	lea    rdx,[b_addr]
    2813:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]
    2817:	48 8d 35 12 0c 00 00 	lea    rsi,[fmt_str_addr]   ; = "%lld %lld".
    281e:	48 89 c7             	mov    rdi,rax
    2821:	b8 00 00 00 00       	mov    eax,0x0

                                    ; read two long ints from input line
    2826:	e8 25 e9 ff ff       	call   1150 <__isoc99_sscanf@plt>
    282b:	83 f8 01             	cmp    eax,0x1

                                    ; if 1 or less ints read, explode
    282e:	7f 0a                	jg     283a <phase_3+0x4a>
    2830:	b8 00 00 00 00       	mov    eax,0x0
    2835:	e8 fd ea ff ff       	call   1337 <explode>


                                    ; check x is greater than 7999999999
    283a:	48 8b 45 e8          	mov    rax,QWORD PTR [b_addr]
    283e:	48 ba ff 4f d6 dc 01 	movabs rdx,0x1dcd64fff
    2848:	48 39 d0             	cmp    rax,rdx
    284b:	76 48                	jbe    2895 <phase_3+0xa5>
                                    ; check y is greater than 4999999999
    284d:	48 8b 45 f0          	mov    rax,QWORD PTR [x_addr]
    2851:	48 ba ff f1 05 2a 01 	movabs rdx,0x12a05f1ff
    2858:	00 00 00
    285b:	48 39 d0             	cmp    rax,rdx
    285e:	76 35                	jbe    2895 <phase_3+0xa5>

                                    ; check x is power of two
    2860:	48 8b 45 e8          	mov    rax,QWORD PTR [b_addr]
    2864:	48 8d 50 ff          	lea    rdx,[rax-0x1]
    2868:	48 8b 45 e8          	mov    rax,QWORD PTR [b_addr]
    286c:	48 21 d0             	and    rax,rdx
    286f:	48 85 c0             	test   rax,rax
    2872:	75 21                	jne    2895 <phase_3+0xa5>

                                    ; check y is power of two
    2874:	48 8b 45 f0          	mov    rax,QWORD PTR [x_addr]
    2878:	48 8d 50 ff          	lea    rdx,[rax-0x1]
    287c:	48 8b 45 f0          	mov    rax,QWORD PTR [x_addr]
    2880:	48 21 d0             	and    rax,rdx
    2883:	48 85 c0             	test   rax,rax
    2886:	75 0d                	jne    2895 <phase_3+0xa5>

                                    ; check x != y
    2888:	48 8b 55 e8          	mov    rdx,QWORD PTR [b_addr]
    288c:	48 8b 45 f0          	mov    rax,QWORD PTR [x_addr]
    2890:	48 39 c2             	cmp    rdx,rax
    2893:	75 0a                	jne    289f <phase_3+0xaf>
    2895:	b8 00 00 00 00       	mov    eax,0x0
    289a:	e8 98 ea ff ff       	call   1337 <explode>


                                    ; say("phase 3 defused");
    289f:	48 8d 3d 94 0b 00 00 	lea    rdi,[rip+0xb94]        # 343a <phase_6+0x659>
    28a6:	e8 0e ea ff ff       	call   12b9 <say>
    28ab:	90                   	nop
    28ac:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    28b0:	64 48 33 04 25 28 00 	xor    rax,QWORD PTR fs:0x28
    28b7:	00 00
    28b9:	74 05                	je     28c0 <phase_3+0xd0>
    28bb:	e8 c0 e7 ff ff       	call   1080 <__stack_chk_fail@plt>
    28c0:	c9                   	leave
    28c1:	c3                   	ret

00000000000028c2 <phase_4>:
    ; for this phase, we can immediately see in the disassembly that the
    ; expected password is "IT'S OVER! I HAVE THE HIGH GROUND". However,
    ; as we look in gdb, we see that our input is garbled into something else,
    ; hence we must find the input which garbles into "IT'S OVER ..." to
    ; complete the phase (actually, there are multiple valid inputs).

    ; the string is garbled using the key string "HUGTI'NOVRADES! "; each
    ; character in the input line is AND'ed with 15 (ie. 00...001111), and
    ; the resulting number is used to index the key string (which happens to be
    ; 16 chars long).

    ; void phase_4(char *line) {
    ;   const char *key = "HUGTI'NOVRADES! ";
    ;   const char *pw = "IT'S OVER! I HAVE THE HIGH GROUND";
    ;   const char tmp[34] = { 0 };
    ;   for (int i = 0; i <= 33; i++)
    ;     tmp[i] = key[line[i] & 0xf];
    ;
    ;   printf("result = %s\n", line);
    ;
    ;   if (strcmp(tmp, pw) != 0)
    ;     explode("wrong input!");
    ;
    ;   printf("phase 4 defused\n");
    ;   return;
    ; }

    28c2:	55                   	push   rbp
    28c3:	48 89 e5             	mov    rbp,rsp
    28c6:	48 83 ec 50          	sub    rsp,0x50
    28ca:	48 89 7d b8          	mov    QWORD PTR [line_addr],rdi
    28ce:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    28d5:	00 00
    28d7:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    28db:	31 c0                	xor    eax,eax
    28dd:	48 c7 45 d0 00 00 00 	mov    QWORD PTR [garbled_str_addr],0x0
    28e4:	00
    28e5:	48 c7 45 d8 00 00 00 	mov    QWORD PTR [rbp-0x28],0x0
    28ec:	00
    28ed:	48 c7 45 e0 00 00 00 	mov    QWORD PTR [rbp-0x20],0x0
    28f4:	00
    28f5:	48 c7 45 e8 00 00 00 	mov    QWORD PTR [rbp-0x18],0x0
    28fc:	00
    28fd:	66 c7 45 f0 00 00    	mov    WORD PTR [rbp-0x10],0x0
    2903:	c6 45 f2 00          	mov    BYTE PTR [rbp-0xe],0x0

                                    ; the garbling is likely implemented using
                                    ; a for-loop, since the loop condition is
                                    ; a check between loop variable and the number
                                    ; 0x21 == 33.

                                    ; int i = 0; // loop variable
    2907:	c7 45 cc 00 00 00 00 	mov    DWORD PTR [i_addr],0x0
    290e:	eb 30                	jmp    2940 <phase_4+0x7e>

                                    ; load next byte from the input line.
    2910:	8b 45 cc             	mov    eax,DWORD PTR [i_addr]
    2913:	48 63 d0             	movsxd rdx,eax
    2916:	48 8b 45 b8          	mov    rax,QWORD PTR [line_addr]
    291a:	48 01 d0             	add    rax,rdx

                                    ; AND the byte with 15 to get the key index.
    291d:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    2920:	0f be c0             	movsx  eax,al
    2923:	83 e0 0f             	and    eax,0xf
    2926:	48 98                	cdqe

                                    ; use key index to get the garbled byte.
                                    ; key[line[i] & 0xf];
    2928:	48 8d 15 81 41 00 00 	lea    rdx,[key_str_addr] ; "HUGTI'LOVRADES! "
    292f:	0f b6 14 10          	movzx  edx,BYTE PTR [rax+rdx*1]
    2933:	8b 45 cc             	mov    eax,DWORD PTR [i_addr]
    2936:	48 98                	cdqe
                                    ; write the garbled byte back to the line string!
    2938:	88 54 05 d0          	mov    BYTE PTR [rbp+rax*1-0x30],dl
    293c:	83 45 cc 01          	add    DWORD PTR [i_addr],0x1
    2940:	83 7d cc 21          	cmp    DWORD PTR [i_addr],0x21
    2944:	7e ca                	jle    2910 <phase_4+0x4e>

                                    ; assert that the garbled input matches the
                                    ; password string!
    2946:	48 8d 45 d0          	lea    rax,[garbled_str_addr]
    294a:	48 8d 35 07 0b 00 00 	lea    rsi,[pw_addr] ; "IT'S OVER ..."
    2951:	48 89 c7             	mov    rdi,rax
    2954:	e8 a7 e7 ff ff       	call   1100 <strcmp@plt>
    2959:	85 c0                	test   eax,eax
    295b:	74 0a                	je     2967 <phase_4+0xa5>
    295d:	b8 00 00 00 00       	mov    eax,0x0
    2962:	e8 d0 e9 ff ff       	call   1337 <explode>
                                    ; say("phase 4 defused");
    2967:	48 8d 3d 12 0b 00 00 	lea    rdi,[rip+0xb12]        # 3480 <phase_6+0x69f>
    296e:	e8 46 e9 ff ff       	call   12b9 <say>
    2973:	90                   	nop
    2974:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    2978:	64 48 33 04 25 28 00 	xor    rax,QWORD PTR fs:0x28
    297f:	00 00
    2981:	74 05                	je     2988 <phase_4+0xc6>
    2983:	e8 f8 e6 ff ff       	call   1080 <__stack_chk_fail@plt>
    2988:	c9                   	leave
    2989:	c3                   	ret



    298a:	55                   	push   rbp
    298b:	48 89 e5             	mov    rbp,rsp
    298e:	48 83 ec 10          	sub    rsp,0x10
    2992:	48 89 7d f8          	mov    QWORD PTR [rbp-0x8],rdi
    2996:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    299a:	48 89 c7             	mov    rdi,rax
    299d:	e8 17 e9 ff ff       	call   12b9 <say>
    29a2:	b8 00 00 00 00       	mov    eax,0x0
    29a7:	e8 8b e9 ff ff       	call   1337 <explode>
    29ac:	90                   	nop
    29ad:	c9                   	leave
    29ae:	c3                   	ret

00000000000029af <push>:
    ; this function simply pushes a monkey (or non-monkey value as a result of a
    ; computation) to the stack.
    ;
    ; void push(int val) {
    ;   if (i <= 0x13) // 0x13 == 19
    ;     stack[i++] = val;
    ;   else
    ;     explode("too high\n");
    ; }
    29af:	55                   	push   rbp
    29b0:	48 89 e5             	mov    rbp,rsp
    29b3:	48 83 ec 10          	sub    rsp,0x10

                                    ; check current size of stack
    29b7:	48 89 7d f8          	mov    QWORD PTR [new_monkey_addr],rdi
    29bb:	0f b7 05 1e 4a 00 00 	movzx  eax,WORD PTR [stack_size_addr]        # 73e0 <node5+0x420>
    29c2:	66 83 f8 13          	cmp    ax,0x13

                                    ; if <= 19, good to go!
    29c6:	7e 0c                	jle    29d4 <phase_4+0x112>
                                    ; else explode with message "too high"
    29c8:	48 8d 3d 13 0b 00 00 	lea    rdi,[rip+0xb13]        ; "too high" # 34e2 <phase_6+0x701>
    29cf:	e8 b6 ff ff ff       	call   298a <phase_4+0xc8>

                                    ; increment global stack size variable
    29d4:	0f b7 05 05 4a 00 00 	movzx  eax,WORD PTR [stack_size_addr]        # 73e0 <node5+0x420>
    29db:	89 c2                	mov    edx,eax
    29dd:	83 c2 01             	add    edx,0x1
    29e0:	66 89 15 f9 49 00 00 	mov    WORD PTR [stack_size_addr],dx        # 73e0 <node5+0x420>

    29e7:	98                   	cwde
    29e8:	48 98                	cdqe

                                    ; push the new monkey to the stack :)
    29ea:	48 8d 0c c5 00 00 00 	lea    rcx,[rax*8+0x0]
    29f1:	00
    29f2:	48 8d 15 07 4a 00 00 	lea    rdx,[monkey_stack_base_addr]        # 7400 <node5+0x440>
    29f9:	48 8b 45 f8          	mov    rax,QWORD PTR [new_monkey_addr]
    29fd:	48 89 04 11          	mov    QWORD PTR [monkey_stack_base_addr+rdx*1],rax
    2a01:	90                   	nop
    2a02:	c9                   	leave
    2a03:	c3                   	ret

0000000000002a04 <pop>:
    ; pop a value from the stack.
    ;
    ; void pop() {
    ;   if (i == 0)
    ;     explode("never sink so low\n");
    ;   return stack[--i]:
    ; }
    2a04:	55                   	push   rbp
    2a05:	48 89 e5             	mov    rbp,rsp
    2a08:	0f b7 05 d1 49 00 00 	movzx  eax,WORD PTR [stack_size_addr]        # 73e0 <node5+0x420>
    2a0f:	66 85 c0             	test   ax,ax
    2a12:	75 0c                	jne    2a20 <phase_4+0x15e>
    2a14:	48 8d 3d de 0a 00 00 	lea    rdi,[rip+0xade]        ; "never sink so low" # 34f9 <phase_6+0x718>
    2a1b:	e8 6a ff ff ff       	call   298a <phase_4+0xc8>
    2a20:	0f b7 05 b9 49 00 00 	movzx  eax,WORD PTR [stack_size_addr]        # 73e0 <node5+0x420>
    2a27:	83 e8 01             	sub    eax,0x1
    2a2a:	66 89 05 af 49 00 00 	mov    WORD PTR [stack_size_addr],ax        # 73e0 <node5+0x420>
    2a31:	0f b7 05 a8 49 00 00 	movzx  eax,WORD PTR [stack_size_addr]       # 73e0 <node5+0x420>
    2a38:	98                   	cwde
    2a39:	48 98                	cdqe
    2a3b:	48 8d 14 c5 00 00 00 	lea    rdx,[rax*8]
    2a42:	00
    2a43:	48 8d 05 b6 49 00 00 	lea    rax,[monkey_stack_base_addr]        # 7400 <node5+0x440>

    2a4a:	48 8b 04 02          	mov    rax,QWORD PTR [rdx+monkey_stack_base_addr*1]
    2a4e:	5d                   	pop    rbp
    2a4f:	c3                   	ret

0000000000002a50 <funcHP48>:
    ; funcHP48 is essentially an evaluator of RPN expressions,
    ; where the possible values are three unicode monkeys representing
    ; the numbers 2, -1, and 3, and the possible operators are addition,
    ; subtraction, and multiplication. However, there is a small "gotcha" in
    ; that the symbols for addition and multiplication are switched!

    ; "monkeys" are pushed and popped from the stack using the push and
    ; pop functions declared above. a maximum of 0x13 monkeys can be on
    ; the stack at a time; if more are popped, or if pop is called on an empty
    ; stack, the bomb explodes.
    ; if an expression is valid but contains less than 5 tokens, the bomb
    ; explodes.

    ; #define MONKEY1 "\xf0\x9f\x99\x88"
    ; #define MONKEY2 "\xf0\x9f\x99\x89"
    ; #define MONKEY3 "\xf0\x9f\x99\x8a"
    ; #define MAX_LEN 19
    ; static int monkey_stack[...] = { 0 };
    ; static int i = 0;
    ; int funcHP48(char *line) {
    ;   int num_tokens = 0;
    ;
    ;   const char *delim = " ";
    ;   char       *token = strtok(line, delim);
    ;
    ;   while (token != NULL) {
    ;     num_tokens++;
    ;
    ;     if (strcmp(token, MONKEY1) == 0)
    ;       push(2);
    ;     else if (strcmp(token, MONKEY2) == 0)
    ;       push(-1);
    ;     else if (strcmp(token, MONKEY3) == 0)
    ;       push(3);
    ;     else {
    ;       if (*token == '+') {
    ;         int a = pop(); // note the order of operands. this does not matter for *
    ;         int b = pop(); // and +, but is made like this to follow the disassembly.
    ;         push(b * a);
    ;       }
    ;       else if (*token == '-') {
    ;         int a = pop(); // here the order actually matters. first operand needs to
    ;         int b = pop(); // be last monkey popped!
    ;         push(b - a);
    ;       }
    ;       else if (*token == '*') {
    ;         int a = pop();
    ;         int b = pop();
    ;         push(b + a);
    ;       }
    ;       else
    ;         explode("unknown operator");
    ;     }
    ;
    ;     token = strtok(NULL, delim);
    ;   }
    ;
    ;   if (i != 1)
    ;     explode("stack leftover");
    ;   if (num_tokens < 4)
    ;     explode("complexity too low");
    ;
    ;   return monkey_stack[0];
    ; }

    2a50:	55                   	push   rbp
    2a51:	48 89 e5             	mov    rbp,rsp
    2a54:	48 83 ec 30          	sub    rsp,0x30
    2a58:	48 89 7d d8          	mov    QWORD PTR [line_addr],rdi
    2a5c:	c7 45 e4 00 00 00 00 	mov    DWORD PTR [token_count_addr],0x0

                                    ; load " " delimiter
    2a63:	48 8d 05 a7 0a 00 00 	lea    rax,[rip+0xaa7]        # 3511 <phase_6+0x730>
    2a6a:	48 89 45 e8          	mov    QWORD PTR [delim_addr],rax
    2a6e:	48 8b 55 e8          	mov    rdx,QWORD PTR [delim_addr]
    2a72:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]

                                    ; actually, the line pointer is overwritten with the returned
                                    ; token, but the source code would likely look more like this:
                                    ; char *token = strtok(line, " ");
    2a76:	48 89 d6             	mov    rsi,rdx
    2a79:	48 89 c7             	mov    rdi,rax
    2a7c:	e8 ff e6 ff ff       	call   1180 <strtok@plt>
    2a81:	48 89 45 d8          	mov    QWORD PTR [line_addr],rax

                                    ; loop over tokens in the input string.
                                    ; in the asm, we jump to a loop condition further down,
                                    ; but the source code would likely be a while loop like this.
                                    ; while (token != NULL)
    2a85:	e9 4b 01 00 00       	jmp    2bd5 <funcHP48+0x185>

                                    ; compare the token with MONKEY1 (as defined above).
                                    ; if (strcmp(token, MONKEY1) == 0)
                                    ;   push(2);
    2a8a:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]
    2a8e:	48 89 c6             	mov    rsi,rax
    2a91:	48 8d 3d 3b 0a 00 00 	lea    rdi,[monkey1_addr]
    2a98:	e8 63 e6 ff ff       	call   1100 <strcmp@plt>
    2a9d:	85 c0                	test   eax,eax
    2a9f:	75 0f                	jne    2ab0 <funcHP48+0x60>
    2aa1:	bf 02 00 00 00       	mov    edi,0x2
    2aa6:	e8 04 ff ff ff       	call   29af <phase_4+0xed> ; here the actual pus is made.
    2aab:	e9 0c 01 00 00       	jmp    2bbc <funcHP48+0x16c>

                                    ; the next paragraph repeats the same procedure, but
                                    ; with MONKEY2 and the value -1.
                                    ; else if (strcmp(token, MONKEY2) == 0)
                                    ;   push(-1);
    2ab0:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]
    2ab4:	48 89 c6             	mov    rsi,rax
    2ab7:	48 8d 3d 1a 0a 00 00 	lea    rdi,[monkey2_addr]        # 34d8 <phase_6+0x6f7>
    2abe:	e8 3d e6 ff ff       	call   1100 <strcmp@plt> ; strcmp monkey2
    2ac3:	85 c0                	test   eax,eax
    2ac5:	75 11                	jne    2ad8 <funcHP48+0x88>
    2ac7:	48 c7 c7 ff ff ff ff 	mov    rdi,0xffffffffffffffff
    2ace:	e8 dc fe ff ff       	call   29af <phase_4+0xed>
    2ad3:	e9 e4 00 00 00       	jmp    2bbc <funcHP48+0x16c>

                                    ; and finally, MONKEY3 and the value 3.
                                    ; else if (strcmp(token, MONKEY2) == 0)
                                    ;   push(-1);
    2ad8:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]
    2adc:	48 89 c6             	mov    rsi,rax
    2adf:	48 8d 3d f7 09 00 00 	lea    rdi,[monkey3_addr]    # 34dd <phase_6+0x6fc>
    2ae6:	e8 15 e6 ff ff       	call   1100 <strcmp@plt>  ; strcmp monkey3
    2aeb:	85 c0                	test   eax,eax
    2aed:	75 0f                	jne    2afe <funcHP48+0xae>
    2aef:	bf 03 00 00 00       	mov    edi,0x3
    2af4:	e8 b6 fe ff ff       	call   29af <phase_4+0xed>
    2af9:	e9 be 00 00 00       	jmp    2bbc <funcHP48+0x16c>

                                    ; now, if the token was not one of the three monkeys, we check
                                    ; whether it is an operator. remember that * is addition and + is
                                    ; multiplication.

                                    ; we first check for "+".
    2afe:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]
    2b02:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    2b05:	3c 2b                	cmp    al,0x2b                ; 0x2b = '+' (multiplication)
    2b07:	75 32                	jne    2b3b <funcHP48+0xeb>
    2b09:	b8 00 00 00 00       	mov    eax,0x0
                                    ; if the operator is "+", pop two monkeys and
                                    ; perform a multiplication.
    2b0e:	e8 f1 fe ff ff       	call   2a04 <phase_4+0x142>
    2b13:	48 89 45 f0          	mov    QWORD PTR [rbp-0x10],rax
    2b17:	b8 00 00 00 00       	mov    eax,0x0
    2b1c:	e8 e3 fe ff ff       	call   2a04 <phase_4+0x142>
    2b21:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    2b25:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    2b29:	48 0f af 45 f0       	imul   rax,QWORD PTR [rbp-0x10]

                                    ; push the result of the multiplication to the stack
    2b2e:	48 89 c7             	mov    rdi,rax
    2b31:	e8 79 fe ff ff       	call   29af <phase_4+0xed>
    2b36:	e9 81 00 00 00       	jmp    2bbc <funcHP48+0x16c>

                                    ; repeat the above for subtraction.
    2b3b:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]
    2b3f:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    2b42:	3c 2d                	cmp    al,0x2d                 ; 0x2d = '-' (subtraction)
    2b44:	75 2e                	jne    2b74 <funcHP48+0x124>
    2b46:	b8 00 00 00 00       	mov    eax,0x0
    2b4b:	e8 b4 fe ff ff       	call   2a04 <phase_4+0x142>
    2b50:	48 89 45 f0          	mov    QWORD PTR [rbp-0x10],rax
    2b54:	b8 00 00 00 00       	mov    eax,0x0
    2b59:	e8 a6 fe ff ff       	call   2a04 <phase_4+0x142>
    2b5e:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    2b62:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    2b66:	48 2b 45 f0          	sub    rax,QWORD PTR [rbp-0x10]
    2b6a:	48 89 c7             	mov    rdi,rax
    2b6d:	e8 3d fe ff ff       	call   29af <phase_4+0xed>
    2b72:	eb 48                	jmp    2bbc <funcHP48+0x16c>

                                    ; repeat the above for * (addition).
    2b74:	48 8b 45 d8          	mov    rax,QWORD PTR [line_addr]
    2b78:	0f b6 00             	movzx  eax,BYTE PTR [rax]
    2b7b:	3c 2a                	cmp    al,0x2a                 ; 0x2a = '*' (addition)

                                    ; if none of the monkeys or operators matched,
                                    ; we have an unknown operator. explode!
    2b7d:	75 31                	jne    2bb0 <funcHP48+0x160>
    2b7f:	b8 00 00 00 00       	mov    eax,0x0
    2b84:	e8 7b fe ff ff       	call   2a04 <phase_4+0x142>
    2b89:	48 89 45 f0          	mov    QWORD PTR [rbp-0x10],rax
    2b8d:	b8 00 00 00 00       	mov    eax,0x0
    2b92:	e8 6d fe ff ff       	call   2a04 <phase_4+0x142>
    2b97:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    2b9b:	48 8b 55 f8          	mov    rdx,QWORD PTR [rbp-0x8]
    2b9f:	48 8b 45 f0          	mov    rax,QWORD PTR [rbp-0x10]
    2ba3:	48 01 d0             	add    rax,rdx
    2ba6:	48 89 c7             	mov    rdi,rax
    2ba9:	e8 01 fe ff ff       	call   29af <phase_4+0xed>
    2bae:	eb 0c                	jmp    2bbc <funcHP48+0x16c>

    2bb0:	48 8d 3d 5c 09 00 00 	lea    rdi,[unkown_op_str_addr] ; "unknown operator"
    2bb7:	e8 ce fd ff ff       	call   298a <phase_4+0xc8>      ; explode with the msg

                                    ; we successfully handled a token. get next token
    2bbc:	48 8b 45 e8          	mov    rax,QWORD PTR [delim_addr]
    2bc0:	48 89 c6             	mov    rsi,rax
    2bc3:	bf 00 00 00 00       	mov    edi,0x0
    2bc8:	e8 b3 e5 ff ff       	call   1180 <strtok@plt>

    2bcd:	48 89 45 d8          	mov    QWORD PTR [line_addr],rax
                                    ; keep count of the number of tokens read such that later, we
                                    ; can determine whether the expression is "complex" enough.
    2bd1:	83 45 e4 01          	add    DWORD PTR [token_count_addr],0x1

                                    ; loop back around if strtok result is not a NULL pointer.
    2bd5:	48 83 7d d8 00       	cmp    QWORD PTR [line_addr],0x0  ; if (strtok(input, " ") != NULL)
    2bda:	0f 85 aa fe ff ff    	jne    2a8a <funcHP48+0x3a>       ;   goto 2a8a

                                    ; if at this point the stack contains a single value, we
                                    ; are done evaluating the expression!! jump to end and
                                    ; assert expression is "complex" enough.
    2be0:	0f b7 05 f9 47 00 00 	movzx  eax,WORD PTR [stack_size_addr]
    2be7:	66 83 f8 01          	cmp    ax,0x1
    2beb:	74 0c                	je     2bf9 <funcHP48+0x1a9>

                                    ; if we read all of the string but have more than one value
                                    ; left on the stack, explode!
    2bed:	48 8d 3d 31 09 00 00 	lea    rdi,[stack_leftover_str_addr]
    2bf4:	e8 91 fd ff ff       	call   298a <phase_4+0xc8>    ; print "stack leftover"; explode

                                    ; assert that the valid expression contains >= 5 tokens, ie.
                                    ; it is an expression containing at least 3 values and 2
                                    ; operators. if not, explode.
    2bf9:	83 7d e4 04          	cmp    DWORD PTR [token_count_addr],0x4
    2bfd:	7f 0c                	jg     2c0b <funcHP48+0x1bb>
    2bff:	48 8d 3d 2f 09 00 00 	lea    rdi,[low_complexity_str_addr]
    2c06:	e8 7f fd ff ff       	call   298a <phase_4+0xc8>  ; print "complexity too low"; explode

    2c0b:	b8 00 00 00 00       	mov    eax,0x0
    2c10:	e8 ef fd ff ff       	call   2a04 <phase_4+0x142>
    2c15:	c9                   	leave
    2c16:	c3                   	ret

0000000000002c17 <phase_5>:

    ; void phase_5(char *line) {
    ;   if (funcHP48(line) != 0xface)
    ;     explode();
    ; }
    2c17:	55                   	push   rbp
    2c18:	48 89 e5             	mov    rbp,rsp
    2c1b:	48 83 ec 10          	sub    rsp,0x10
    2c1f:	48 89 7d f8          	mov    QWORD PTR [line_addr],rdi
    2c23:	66 c7 05 b4 47 00 00 	mov    WORD PTR [stack_size_addr],0x0        # 73e0 <node5+0x420>
    2c2a:	00 00
    2c2c:	48 8b 45 f8          	mov    rax,QWORD PTR [line_addr]
    2c30:	48 89 c7             	mov    rdi,rax
                                    ; call funcHP48 with the given line and
                                    ; check that the result is 0xface. simple!
    2c33:	e8 18 fe ff ff       	call   2a50 <funcHP48>
    2c38:	48 3d ce fa 00 00    	cmp    rax,0xface
    2c3e:	74 0a                	je     2c4a <phase_5+0x33>
    2c40:	b8 00 00 00 00       	mov    eax,0x0
    2c45:	e8 ed e6 ff ff       	call   1337 <explode>
                                    ; say("phase 5 defused");
    2c4a:	48 8d 3d f7 08 00 00 	lea    rdi,[rip+0x8f7]        # 3548 <phase_6+0x767>
    2c51:	e8 63 e6 ff ff       	call   12b9 <say>
    2c56:	90                   	nop
    2c57:	c9                   	leave
    2c58:	c3                   	ret

0000000000002c59 <func_chebyshev>:

    ; func_chebyshev is some sort of recursively defined mathematical computation.
    ; I have not gone very far into understanding the intricacies of the function,
    ; since it isn't necessary to complete phase 5.5, but below is a decompilation
    ; of the function, and a somewhat sparsely commented disassembly.
    ;
    ; int64_t func_chebyshev(int64_t a, int64_t b) {
    ;   int64_t r13 = 0, r12 = 1;
    ;
    ;   while (1) {
    ;     if (a == 0)
    ;       return r12 + r13;
    ;     else if (a == 1)
    ;       return b * r12 + r13;
    ;
    ;     int64_t rec_res = func_chebyshev(a - 1, b);
    ;
    ;     r13 += b * r12 * rec_res * 2;
    ;
    ;     r12 = -r12;
    ;
    ;     a -= 2;
    ;   }
    ; }
    2c59:	41 55                	push   r13
    2c5b:	45 31 ed             	xor    r13d, r13d
    2c5e:	41 54                	push   r12
    2c60:	41 bc 01 00 00 00    	mov    r12d,0x1
    2c66:	55                   	push   rbp
    2c67:	48 89 fd             	mov    rbp,rdi
    2c6a:	53                   	push   rbx
    2c6b:	48 89 f3             	mov    rbx,rsi
    2c6e:	51                   	push   rcx

                                    ; loop_head:
                                    ; if (a == 0)
                                    ;   // return r12 + r13;
                                    ;   goto loop_exit1;
    2c6f:	48 85 ed             	test   rbp,rbp
    2c72:	74 2f                	je     2ca3 <func_chebyshev+0x4a>

                                    ; else if (a == 1)
                                    ;   // return b * r12 + r13;
                                    ;   goto loop_exit2;
    2c74:	48 83 fd 01          	cmp    rbp,0x1
    2c78:	74 2e                	je     2ca8 <func_chebyshev+0x4f>

                                    ; rec_res = func_chebyshev(a - 1, b);
                                    ; a -= 2;
    2c7a:	48 8d 7d ff          	lea    rdi,[rbp-0x1]
    2c7e:	48 89 de             	mov    rsi,rbx
    2c81:	48 83 ed 02          	sub    rbp,0x2
    2c85:	e8 cf ff ff ff       	call   2c59 <func_chebyshev>
    2c8a:	49 89 c0             	mov    r8,rax

                                    ; r13 += b * r12 * rec_res * 2;
    2c8d:	48 89 d8             	mov    rax,rbx
    2c90:	49 0f af c4          	imul   rax,r12
                                    ; r12 = -r12;
    2c94:	49 f7 dc             	neg    r12
    2c97:	49 0f af c0          	imul   rax,r8
    2c9b:	48 01 c0             	add    rax,rax
    2c9e:	49 01 c5             	add    r13,rax

                                    ;  goto loop_head;
    2ca1:	eb cc                	jmp    2c6f <func_chebyshev+0x16>

                                    ;  loop_exit:
                                    ;  rbx = 1;
    2ca3:	bb 01 00 00 00       	mov    ebx,0x1
                                    ;  loop_exit2:
                                    ;  return rbx * r12 + r13;
    2ca8:	49 0f af dc          	imul   rbx,r12
    2cac:	5a                   	pop    rdx
    2cad:	4a 8d 04 2b          	lea    rax,[rbx+r13*1]
    2cb1:	5b                   	pop    rbx
    2cb2:	5d                   	pop    rbp
    2cb3:	41 5c                	pop    r12
    2cb5:	41 5d                	pop    r13
    2cb7:	c3                   	ret



0000000000002cb8 <phase_55>:

    ; phase 5.5 is probably the second easiest phase of all, once you realize
    ; that you don't have to decompile or understand func_chebyshev in order to
    ; solve phase_55.

    ; anyway, the important points are:
    ; 1) the phase reads 2 long ints, a and b, from the input line, and explodes
    ;      on less than 2 vals read
    ; 2) the bomb explodes if the first value, a, is <= 9
    ; 3) the phase calls func_chebyshev(a - 1, a - 2) and checks the result
    ;      against the given b. if they do not match, the bomb explodes.
    ;
    ; hence all we need to do to solve the phase is to choose values for a and
    ; b, read out the result using gdb, and re-run with the same a and the
    ; result substituted for b. easy!

    2cb8:	48 83 ec 28          	sub    rsp,0x28
    2cbc:	48 8d 35 9f 08 00 00 	lea    rsi,[rip+0x89f]        # 3562 <phase_6+0x781>
    2cc3:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    2cca:	00 00
    2ccc:	48 89 44 24 18       	mov    QWORD PTR [rsp+0x18],rax
    2cd1:	31 c0                	xor    eax,eax
    2cd3:	48 8d 4c 24 10       	lea    rcx,[b_addr]
    2cd8:	48 8d 54 24 08       	lea    rdx,[a_addr]

                                    ; uint64_t a, b; // a at a_addr, b at b_addr.
                                    ; int n_read = sscanf(input, "%ld %ld", &a, &b);
                                    ; if (n_read <= 1)
                                    ;   explode();
    2cdd:	e8 6e e4 ff ff       	call   1150 <__isoc99_sscanf@plt>
    2ce2:	ff c8                	dec    eax
    2ce4:	7f 07                	jg     2ced <phase_55+0x35>
    2ce6:	31 c0                	xor    eax,eax
    2ce8:	e8 4a e6 ff ff       	call   1337 <explode>

                                    ; else if (a <= 9)
                                    ;   explode();
    2ced:	48 83 7c 24 08 09    	cmp    QWORD PTR [a_addr],0x9
    2cf3:	77 07                	ja     2cfc <phase_55+0x44>
    2cf5:	31 c0                	xor    eax,eax
    2cf7:	e8 3b e6 ff ff       	call   1337 <explode>

                                    ; res = func_chebyshev(a - 1, a - 2);
    2cfc:	48 8b 7c 24 08       	mov    rdi,QWORD PTR [a_addr]
    2d01:	48 8d 77 fe          	lea    rsi,[rdi-0x2]
    2d05:	48 ff cf             	dec    rdi
    2d08:	e8 4c ff ff ff       	call   2c59 <func_chebyshev>

                                    ; if (res != b)
                                    ;   explode();
    2d0d:	48 3b 44 24 10       	cmp    rax,QWORD PTR [b_addr]
    2d12:	74 07                	je     2d1b <phase_55+0x63>
    2d14:	31 c0                	xor    eax,eax
    2d16:	e8 1c e6 ff ff       	call   1337 <explode>
                                    ; else
                                    ;   say("phase 5.5 defused!");
    2d1b:	48 8d 3d 48 08 00 00 	lea    rdi,[rip+0x848]        # 356a <phase_6+0x789>
    2d22:	e8 92 e5 ff ff       	call   12b9 <say>
    2d27:	48 8b 44 24 18       	mov    rax,QWORD PTR [rsp+0x18]
    2d2c:	64 48 33 04 25 28 00 	xor    rax,QWORD PTR fs:0x28
    2d33:	00 00
    2d35:	74 05                	je     2d3c <phase_55+0x84>
    2d37:	e8 44 e3 ff ff       	call   1080 <__stack_chk_fail@plt>
    2d3c:	48 83 c4 28          	add    rsp,0x28
    2d40:	c3                   	ret



0000000000002d41 <foo6>:
    ; void foo6(void *a, void (*b)(void*, foo_type), foo_type c) {
    ;   if (a == 0)
    ;     return;
    ;   foo6(*a, b, c);
    ;   b(c, a);
    ;   // [*a + (**a) * 4 + 4] = b + 16;
    ;   // (*a)++
    ;   foo6(a[1], b, c);
    ; }
    ; char **a : array of arrays (hence the 8 increment)
                                    ; void foo6(char **a, func_p b, bar c)
    2d41:	55                   	push   rbp
    2d42:	48 89 e5             	mov    rbp,rsp
    2d45:	48 83 ec 20          	sub    rsp,0x20
    2d49:	48 89 7d f8          	mov    QWORD PTR [addr_a],rdi
    2d4d:	48 89 75 f0          	mov    QWORD PTR [addr_b],rsi
    2d51:	48 89 55 e8          	mov    QWORD PTR [addr_c],rdx
    2d55:	48 83 7d f8 00       	cmp    QWORD PTR [addr_a],0x0
                                    ; if (a == NULL)
                                    ;   return;
    2d5a:	74 4b                	je     2da7 <phase_55+0xef>

                                    ; rdi = *a;
                                    ; rsi = b;
                                    ; rdx = c;
    2d5c:	48 8b 45 f8          	mov    rax,QWORD PTR [addr_a]
    2d60:	48 8b 00             	mov    rax,QWORD PTR [rax]
    2d63:	48 8b 55 e8          	mov    rdx,QWORD PTR [addr_c]
    2d67:	48 8b 4d f0          	mov    rcx,QWORD PTR [addr_b]
    2d6b:	48 89 ce             	mov    rsi,rcx
    2d6e:	48 89 c7             	mov    rdi,rax

                                    ; foo6(*a, b, c)
    2d71:	e8 cb ff ff ff       	call   2d41 <phase_55+0x89>

                                    ; rdi = c;
                                    ; rcx = b;
                                    ; rsi = a;
                                    ; b(c, a); // call function pointed to by b (func6?)
    2d76:	48 8b 55 f8          	mov    rdx,QWORD PTR [addr_a]
    2d7a:	48 8b 45 e8          	mov    rax,QWORD PTR [addr_c]
    2d7e:	48 8b 4d f0          	mov    rcx,QWORD PTR [addr_b]
    2d82:	48 89 d6             	mov    rsi,rdx
    2d85:	48 89 c7             	mov    rdi,rax
    2d88:	ff d1                	call   rcx

                                    ; rdi = *((char*)a + 8);
                                    ; rsi = b
                                    ; rdx = c
                                    ; foo6(a[1], b, c);
    2d8a:	48 8b 45 f8          	mov    rax,QWORD PTR [addr_a]
    2d8e:	48 8b 40 08          	mov    rax,QWORD PTR [rax+0x8]
    2d92:	48 8b 55 e8          	mov    rdx,QWORD PTR [addr_c]
    2d96:	48 8b 4d f0          	mov    rcx,QWORD PTR [addr_b]
    2d9a:	48 89 ce             	mov    rsi,rcx
    2d9d:	48 89 c7             	mov    rdi,rax
    2da0:	e8 9c ff ff ff       	call   2d41 <phase_55+0x89>
    2da5:	eb 01                	jmp    2da8 <phase_55+0xf0>
    2da7:	90                   	nop
    2da8:	c9                   	leave
    2da9:	c3                   	ret

0000000000002daa <func6>:
                                    ; void func6(void *c, a)
    2daa:	55                   	push   rbp
    2dab:	48 89 e5             	mov    rbp,rsp
    2dae:	48 89 7d e8          	mov    QWORD PTR [addr_a],rdi
    2db2:	48 89 75 e0          	mov    QWORD PTR [addr_b],rsi

                                    ; rax = c;
                                    ; rax = *rax;
                                    ; -> rax = *c;
    2db6:	48 8b 45 e8          	mov    rax,QWORD PTR [addr_a]
    2dba:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    2dbe:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    2dc2:	8b 00                	mov    eax,DWORD PTR [rax]
                                     
                                    ; rcx = rax + 1 = *c + 1;
                                    ; -> rcx = *c + 1;
    2dc4:	8d 48 01             	lea    ecx,[rax+0x1]

                                    ; rdx = c;
                                    ; *rdx = ecx
                                    ; -> *c = *c + 1;
    2dc7:	48 8b 55 f8          	mov    rdx,QWORD PTR [rbp-0x8]
    2dcb:	89 0a                	mov    DWORD PTR [rdx],ecx

                                    ; rdx = a;
                                    ; ecx = a + 16;
                                    ; rdx = c;
    2dcd:	48 8b 55 e0          	mov    rdx,QWORD PTR [addr_b]
    2dd1:	8b 4a 10             	mov    ecx,DWORD PTR [rdx+0x10]
    2dd4:	48 8b 55 f8          	mov    rdx,QWORD PTR [rbp-0x8]

    2dd8:	48 98                	cdqe
                                    ; [c + (*c) * 4 + 4] = a + 16;
    2dda:	89 4c 82 04          	mov    DWORD PTR [rdx+rax*4+0x4],ecx
    2dde:	90                   	nop
    2ddf:	5d                   	pop    rbp
    2de0:	c3                   	ret

0000000000002de1 <phase_6>:
    2de1:	55                   	push   rbp
    2de2:	48 89 e5             	mov    rbp,rsp
    2de5:	48 83 ec 50          	sub    rsp,0x50
    2de9:	48 89 7d b8          	mov    QWORD PTR [rbp-0x48],rdi
    2ded:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
    2df4:	00 00
    2df6:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
    2dfa:	31 c0                	xor    eax,eax
    2dfc:	48 c7 45 d0 00 00 00 	mov    QWORD PTR [rbp-0x30],0x0
    2e03:	00
    2e04:	48 c7 45 d8 00 00 00 	mov    QWORD PTR [rbp-0x28],0x0
    2e0b:	00
    2e0c:	48 c7 45 e0 00 00 00 	mov    QWORD PTR [rbp-0x20],0x0
    2e13:	00
    2e14:	c7 45 e8 00 00 00 00 	mov    DWORD PTR [rbp-0x18],0x0
    2e1b:	48 8b 45 b8          	mov    rax,QWORD PTR [rbp-0x48]
    2e1f:	48 8d 15 ea 3c 00 00 	lea    rdx,[rip+0x3cea]        # 6b10 <node6+0x10>
    2e26:	52                   	push   rdx
    2e27:	48 8d 15 a2 41 00 00 	lea    rdx,[rip+0x41a2]        # 6fd0 <node5+0x10>
    2e2e:	52                   	push   rdx
    2e2f:	4c 8d 0d 7a 41 00 00 	lea    r9,[rip+0x417a]        # 6fb0 <node4+0x10>
    2e36:	4c 8d 05 b3 3c 00 00 	lea    r8,[rip+0x3cb3]        # 6af0 <node3+0x10>
    2e3d:	48 8d 0d 8c 3c 00 00 	lea    rcx,[rip+0x3c8c]        # 6ad0 <node2+0x10>
    2e44:	48 8d 15 45 41 00 00 	lea    rdx,[rip+0x4145]        # 6f90 <node1+0x10>
    2e4b:	48 8d 35 36 07 00 00 	lea    rsi,[rip+0x736]        # 3588 <phase_6+0x7a7>
    2e52:	48 89 c7             	mov    rdi,rax
    2e55:	b8 00 00 00 00       	mov    eax,0x0
    2e5a:	e8 f1 e2 ff ff       	call   1150 <__isoc99_sscanf@plt>
    2e5f:	48 83 c4 10          	add    rsp,0x10
    2e63:	83 f8 05             	cmp    eax,0x5
    2e66:	7f 0a                	jg     2e72 <phase_6+0x91>
    2e68:	b8 00 00 00 00       	mov    eax,0x0
    2e6d:	e8 c5 e4 ff ff       	call   1337 <explode>
    2e72:	48 8d 45 d0          	lea    rax,[rbp-0x30]
    2e76:	48 89 c2             	mov    rdx,rax
    2e79:	48 8d 35 2a ff ff ff 	lea    rsi,[func6_addr]        # 2daa <func6>
    2e80:	48 8d 3d 79 3c 00 00 	lea    rdi,[rip+0x3c79]        # 6b00 <node6>

    2e87:	e8 b5 fe ff ff       	call   2d41 <foo6> <phase_55+0x89>

                                    ; int i, *addr_i = &i;
                                    ;
                                    ; for (int i = 0; i <= 5; i++) {
                                    ;   ...
    ; 0x2e8c - 0x2de1
    2e8c:	c7 45 cc 00 00 00 00 	mov    DWORD PTR [addr_i],0x0
    2e93:	eb 1c                	jmp    2eb1 <phase_6+0xd0>

                                    ; eax = i;
    2e95:	8b 45 cc             	mov    eax,DWORD PTR [addr_i]
    2e98:	48 98                	cdqe
                                    ; // eax = *(rbp + (char*) i * 4 - 68);
                                    ; eax = *(rbp + i - 17);
                                    ; if (rbp[i - 17] != i) { // rbp[17 - i] ?
                                    ;   explode();
                                    ; }
    2e9a:	8b 44 85 d4          	mov    eax,DWORD PTR [rbp+rax*4-0x2c]
    2e9e:	39 45 cc             	cmp    DWORD PTR [addr_i],eax

    2ea1:	74 0a                	je     2ead <phase_6+0xcc>
    2ea3:	b8 00 00 00 00       	mov    eax,0x0
    2ea8:	e8 8a e4 ff ff       	call   1337 <explode>

    2ead:	83 45 cc 01          	add    DWORD PTR [addr_i],0x1
    2eb1:	83 7d cc 05          	cmp    DWORD PTR [addr_i],0x5
    2eb5:	7e de                	jle    2e95 <phase_6+0xb4>
                                    ; // end for loop

    2eb7:	48 8d 3d e2 06 00 00 	lea    rdi,[rip+0x6e2]        # 35a0 <phase_6+0x7bf>
    2ebe:	e8 f6 e3 ff ff       	call   12b9 <say>
    2ec3:	90                   	nop
    2ec4:	48 8b 45 f8          	mov    rax,QWORD PTR [rbp-0x8]
    2ec8:	64 48 33 04 25 28 00 	xor    rax,QWORD PTR fs:0x28
    2ecf:	00 00
    2ed1:	74 05                	je     2ed8 <phase_6+0xf7>
    2ed3:	e8 a8 e1 ff ff       	call   1080 <__stack_chk_fail@plt>
    2ed8:	c9                   	leave
    2ed9:	c3                   	ret
