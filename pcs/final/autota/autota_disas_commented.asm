00000000004011f6 <main>:

  ; int main() {
  ;   int secret_key_to_get_help_from_autota = 0;
  ;   char user_input_buf[112];
  ;   scanf("%[^\n]", user_input_buf);
  ; 
  ;   // various checks for the contents of user_input_buf go here ...
  ;   // ...
  ;   // but the one we are interested in is this one:
  ;   if (strcmp(user_input_buf, "Please, give me a hint") == 0) {
  ;     puts("no");   
  ;     return 0;
  ;   }
  ; }
  4011f6:  f3 0f 1e fa            endbr64
  4011fa:  53                     push   rbx

  ; allocate 128 bytes of stack space.
  4011fb:  48 83 c4 80            add    rsp,0xffffffffffffff80

  ; ...

  ; // here we set a shady local variable to 0. later we check if it's changed,
  ; // and if it has, we get help from autota! but it comes before our buffer,
  ; // so we can't overwrite it using simply an overflow attack. still fun tho!

  ; int secret_key_to_get_help_from_autota = 0;
  ; puts("hi, how can i help?");
  401210:  c7 44 24 0c 00 00 00   mov    DWORD PTR [rsp+0xc],0x0
  401218:  48 8d 3d e5 0d 00 00   lea    rdi,[rip+0xde5]        # 402004 <_IO_stdin_used+0x4>
  40121f:  e8 7c fe ff ff         call   4010a0 <puts@plt>

  ; // here we allocate the user input buffer and call scanf.
  ; // the buffer starts at [rsp + 16], meaning it is probably 112 bytes long,
  ; // since we allocated 128 bytes of stack and nothing comes after.
  ; char user_input_buf[112];
  ; scanf("%[^\n]", user_input_buf);
  401224:  48 8d 5c 24 10         lea    rbx,[rsp+0x10]
  401229:  48 89 de               mov    rsi,rbx
  40122c:  48 8d 3d e5 0d 00 00   lea    rdi,[rip+0xde5]        # 402018 <_IO_stdin_used+0x18>
  401233:  b8 00 00 00 00         mov    eax,0x0
  401238:  e8 a3 fe ff ff         call   4010e0 <__isoc99_scanf@plt>

  ; ... various checks on the user input

  ; // here comes the interesting check:
  ; if (strcmp(user_input_buf, "Please, give me a hint") == 0)
  ; // if it is true, we are on track to the `ret` instruction
  401265:  48 8d 3d c8 0d 00 00   lea    rdi,[rip+0xdc8]        # 402034 <_IO_stdin_used+0x34>
  40126c:  f3 a6                  repz cmps BYTE PTR ds:[rsi],BYTE PTR es:[rdi]
  40126e:  0f 97 c0               seta   al
  401271:  1c 00                  sbb    al,0x0
  401273:  84 c0                  test   al,al
  401275:  0f 84 9a 00 00 00      je     401315 <main+0x11f>

  ; ...
  ; ...
  ; ...

  ; this is the address we jump to once we get RIP control
  4012f0:  48 8d 3d 96 0d 00 00   lea    rdi,[rip+0xd96]        # 40208d <_IO_stdin_used+0x8d>
  4012f7:  e8 c4 fd ff ff         call   4010c0 <system@plt>
  4012fc:  eb 0c                  jmp    40130a <main+0x114>

  ; ...

  40130a:  b8 00 00 00 00         mov    eax,0x0
  40130f:  48 83 ec 80            sub    rsp,0xffffffffffffff80
  401313:  5b                     pop    rbx
  401314:  c3                     ret

  ; puts("no") followed by a jump to return 0
  401315:  48 8d 3d 15 0d 00 00   lea    rdi,[rip+0xd15]        # 402031 <_IO_stdin_used+0x31>
  40131c:  e8 7f fd ff ff         call   4010a0 <puts@plt>
  401321:  eb e7                  jmp    40130a <main+0x114>

  ; rest of main
  ; ...
  ; ...
  ; ...
