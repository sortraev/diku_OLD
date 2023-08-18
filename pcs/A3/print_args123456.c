#include <stdio.h>
#include <stdlib.h>
int main(int argc, char **argv, char **envp) {
  if (argc == 1)
    printf("NO ARGS GIVEN!!\n");
  for (int i = 0; i < argc; i++)
    printf("arg %d = %s\n", i, argv[i]);

  char *pw1 = getenv("PCS_PASSWD");
  if (pw1)
    printf("PCS_PASSWD = \"%s\"\n", pw1);
  else
    printf("PCS_PASSWD NOT SET!!\n");

  if (pw2)
    printf("VAR_PCS = \"%s\"\n", pw2);
  else
    printf("VAR_PCS NOT SET!!\n");
}
