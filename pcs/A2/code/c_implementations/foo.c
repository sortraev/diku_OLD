#include <stdlib.h>
#include <stdio.h>


#define BUF_SIZE 256
int main() {

  char line[BUF_SIZE] = { 0 };
  fgets(line, BUF_SIZE, stdin);
  printf("%s\n", line);

}
