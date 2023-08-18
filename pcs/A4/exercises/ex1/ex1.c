#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define BUF_SIZE 32
void exploit_me(char *arg) {
  int is_admin;
  char buf[BUF_SIZE];

  is_admin = 0;
  size_t n = strlen(arg);
  if (n > BUF_SIZE) {
    printf("Warning: arg too long! Max arg len = %d, but got %ld.\n", BUF_SIZE, n);
  }
  strncpy(buf, arg, BUF_SIZE);

  if (is_admin)
    printf("Good day, master\n");

}

int main (int argc, char *argv[]) {
  exploit_me(argv[1]);
  return EXIT_SUCCESS;
}
