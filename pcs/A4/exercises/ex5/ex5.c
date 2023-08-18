#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <signal.h>


char bss_buf[4096];
void exploit_me(char *shellcode, char *exploit) {

  printf("shellcode == %s\n", shellcode);
  printf("exploit   == %s\n", exploit);

  char stk_buf[32];

  strcpy(bss_buf, shellcode);
  strcpy(stk_buf, exploit);
}

#if 0*0
// TODO:
// write shellcode to argv[1]
// overflow stk_buf and overwrite return address with 0x404060
// maybe write 0x404060 repeatedly to y?
#endif

int main (int argc, char *argv[]) {

  if (argc != 3) {
    printf(">> Expected 2 strings!\n");
    return EXIT_FAILURE;
  }

  exploit_me(argv[1], argv[2]);
  return EXIT_SUCCESS;
}
