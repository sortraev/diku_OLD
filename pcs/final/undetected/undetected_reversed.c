#include <stdlib.h>     // malloc, free, exit, etc
#include <sys/random.h> // getrandom
#include <string.h>     // memcpy, strlen, strncmp
#include <stdio.h>      // puts, printf, __printf_chk, scanf, fgets

void win() {
  printf("YOU WIN\n");
}


void input(char *buf_out) {
  char buf[420];
  size_t n_read;
  if (fgets(buf, 420, stdin) == 0) {
    puts("no input. :crying_cat_face\n");
    exit(1);
  }
  n_read = strlen(buf);
  memcpy(buf_out, buf, n_read);
}

char *super_secret() {
  char *buf = malloc(32);
  for (int i = 0; i < 32; i++)
    buf[i] = 0;

  if (getrandom(buf, 32, 0) < 32)
    __fprintf_chk(stderr, 1, "running out of randomness\n");

  char *p = buf;
  char *buf_end = buf + 32;
  do {
    int c = *p;
    if (c == '\0' || c == '\n')
      *p = 1;
  } while (++p != buf_end);

  return buf;
}

int main() {

  for (int i = 42; i > 0; i--) {
    puts("try to leak my secret. come on, I dare you");

    char *user_buf = malloc(32);
    char *secret    = super_secret();

    __printf_chk(1, "btw do you know a good riddle? ");
    input(user_buf);

    __printf_chk(1, "I see. so your riddle is: ");
    puts(user_buf);
    __printf_chk(1, "anyway. what's my secret? ");


    char user_guess[33];
    if (fgets(user_guess, 33, stdin) == 0) {
      puts("choosing not to play, I see. You still lose.\n");
      exit(1);
    }

    if (strncmp(secret, user_guess, 32) != 0) {
      puts("you failed. as expected. :smirk:");
      exit(1);
    }

    puts("wow. I didn't expect that");
    puts("ok, so what is the answer to your riddle?");

    if (scanf("%[^\n]%*c", user_buf) != 1) {
      puts("no input. :crying_cat_face:.");
      exit(1);
    }

    puts("hmm. I'm not sure I appreciate that kind of humour.");
    free(secret);
    free(user_buf);
    puts("Let's try again");
  }
  puts(":flustered: well, I didn't expect that");
  win();
}
