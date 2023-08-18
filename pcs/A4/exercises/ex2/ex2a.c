#include <stdlib.h>
#include <string.h>
#include <stdio.h>


void exploit_me(char *arg) {
  // return address
  struct {
    int is_admin;
    char buf[32];
  } v;

  v.is_admin = 0;
  strcpy(v.buf, arg);

  if (v.is_admin) {
    printf("Good day, master\n");
  }
}

int main (int argc, char *argv[]) {
  exploit_me(argv[1]);
  return EXIT_SUCCESS;
}
