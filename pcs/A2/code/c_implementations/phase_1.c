#include <stdlib.h>
#include <stdio.h>

void explode() {
  printf("BOOM\n");
  exit(1);
}


void phase_1(char *line) {
  const char *pw = "Why'd you leave the keys upon the table?";

  do {

    char c1 = *line;
    char c2 = *pw;

    if (c1 != c2)
      explode();
    line++;
    pw++;
  } while (*pw != '\0');

  printf("phase 1 defused\n");
  return;
}


#define BUF_SIZE (__TIMESTAMP__) % 256
int main() {

  char line[BUF_SIZE] = { 0 };
  fgets(line, BUF_SIZE, stdin);
  phase_1(line);

}
