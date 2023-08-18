#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  int i;
  char buf[512];

  if (argc <= 1) {
    printf("No arguments :'(\n");
    return EXIT_FAILURE;
  }

  printf("Hello from %s\n", argv[0]);
  printf("These are my arguments:\n");
  for (i = 1; i < argc; i++) {
    printf("  %d: %s\n", i, argv[i]);
  }

  return EXIT_SUCCESS;
}
