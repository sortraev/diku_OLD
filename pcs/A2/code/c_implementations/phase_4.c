#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void explode(char *msg) {
  printf("%s -- BOOM\n", msg);
  exit(1);
}

void phase_4(char *line) {
  const char *key = "HUGTI'NOVRADES! ";
  const char *pw = "IT'S OVER! I HAVE THE HIGH GROUND";
  for (int i = 0; i <= 33; i++)
    line[i] = key[line[i] & 0xf];

  printf("result = %s\n", line);

  if (strcmp(line, pw) != 0)
    explode("wrong input!");

  printf("phase 4 defused\n");
  return;
}


#define BUF_SIZE 256
int main() {

  char line[BUF_SIZE] = { 0 };
  fgets(line, BUF_SIZE, stdin);
  phase_4(line);

}
