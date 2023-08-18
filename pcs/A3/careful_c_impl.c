#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

uint8_t exename[] = {
  46, 47, 104, 97, 110, 100, 108, 101, 95, 119, 105, 116, 104, 95, 99, 97, 114, 101, 
  1,
};
uint8_t username[] = {
  78, 117, 109, 98, 101, 114, 119, 97, 110, 103,
  1,
};
uint8_t password[] = {
  80, 67, 83, 95, 80, 65, 83, 83, 87, 68, 61, 104, 117, 110, 116, 101, 114, 50,
  1,
};

int main() {


  int a = 0xdeafbeef;
  a ^= a;
  exename[18]  = a;
  username[10] = a;
  password[18] = a;

  uint64_t _argv[] = {
    (uint64_t) exename,
    (uint64_t) username,
    1,
  };
  uint64_t _envp[] = {
    (uint64_t) password,
    1,
  };

  _argv[2] = a;
  _envp[1] = a;

  execve((void*) exename, (void*) _argv, (void*) _envp);
  perror("execve");
}
