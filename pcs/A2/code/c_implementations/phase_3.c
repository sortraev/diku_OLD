#include <stdio.h>
#include <stdlib.h>

void explode() {
  printf("BOOM\n");
  exit(1);
}

void phase_3(char *line) {
  long long int x, y;
  int n_read = sscanf(line, "%lld %lld", &x, &y);
  if (n_read <= 1)
    explode();
  if (x <= 7999999999)
    explode();
  if (y <= 4999999999)
    explode();
  if ((x & (x - 1)) != 0)
    explode();
  if ((y & (y - 1)) != 0)
    explode();
  printf("phase 3 defused\n");
  return;
}

#define BUF_SIZE 252
int main() {

  char line[BUF_SIZE] = { 0 };
  fgets(line, BUF_SIZE, stdin);
  phase_3(line);

}
