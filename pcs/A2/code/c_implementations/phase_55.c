#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void explode(char *msg) {
  printf(msg);
  exit(1);
}

int64_t func_chebyshev(int64_t a, int64_t b) {

  int64_t r13 = 0;
  int64_t r12 = 1;

  while (1) {
    if (a == 0)
      return r12 + r13;
    else if (a == 1)
      return b * r12 + r13;

    int64_t rec_res = func_chebyshev(a - 1, b);

    r13 += b * r12 * rec_res * 2;

    r12 = -r12;

    a -= 2;
  }
}

void phase_5(char *line) {
  int64_t a, b;
  int n_read = sscanf(line, "%ld %ld", &a, &b);

  // if (n_read)

  if (a <= 9)
    explode("BOOM (a <= 9)\n");

  int64_t res = func_chebyshev(a - 1, a - 2);

  printf("(a, b, res) = (%ld, %ld, %ld)\n", a, b, res);

  if (res != b)
    explode("BOOM (res != b)\n");

  return;

}

#define BUF_SIZE 256
int main() {

  char line[BUF_SIZE];
  fgets(line, BUF_SIZE, stdin);

  phase_5(line);
}
