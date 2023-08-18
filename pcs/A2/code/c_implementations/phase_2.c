#include <stdlib.h>
#include <stdio.h>
#include <math.h>

float max(float a, float b) {
  if (isnan(a) || isnan(b))
    return b;
  return a > b ? a : b;
}

void explode() {
  printf("BOOM\n");
  exit(1);
}



void phase_2(char *line) {
  int weird_val = 0x803f0000;

  float a, b;

  int n_read = sscanf(line, "%f %f", &a, &b);
  if (n_read <= 1)
    explode();

  float max1 = max(b, a);
  float x = (*(int*) &max1) ^ weird_val;


  float max2 = max(a, b);
  float y = (*(int*) &max2) ^ weird_val;

  if (x == y)
    explode();
  printf("Phase 2 defused\n");
  return;
}


#define BUF_SIZE 256
int main() {

  char line[BUF_SIZE] = { 0 };
  fgets(line, BUF_SIZE, stdin);
  phase_2(line);

}
