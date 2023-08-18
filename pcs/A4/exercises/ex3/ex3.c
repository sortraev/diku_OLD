#include <stdlib.h>
#include <stdio.h>

void exploit_me(void) {
 struct {
   char buf[16];
   int is_admin;
    // your favorite definition from exercise 2
  } v;

  v.is_admin = 0;
  scanf("%[^\n]", v.buf); // The same as `gets(buf)`

  if (v.is_admin)
    printf("Good day, master\n");
}

int main (int argc, char *argv[]) {
  exploit_me();
  return EXIT_SUCCESS;
}
