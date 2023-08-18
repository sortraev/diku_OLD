#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#define MONKEY1 "\xf0\x9f\x99\x88"
#define MONKEY2 "\xf0\x9f\x99\x89"
#define MONKEY3 "\xf0\x9f\x99\x8a"

#define MONKEY1 "2"
#define MONKEY2 "-1"
#define MONKEY3 "3"

#define BUF_SIZE 256
#define MAX_LEN 19
static int stack[MAX_LEN + 1] = { 0 };
static int i = 0;

void explode(char *msg) {
  printf("%s -- BOOM\n", msg);
  exit(1);
}

void push_monkey(int monkey) {
  if (i > MAX_LEN)
    explode("TOO HIGH");
  stack[i++] = monkey;
}

int pop_monkey() {
  if (i == 0)
    explode("I'll never sink so low\n");
  return stack[--i];
}

int funcHP48(char *line) {
  int num_tokens = 0;

  const char *delim = " ";
  char       *token = strtok(line, delim);

  while (token != NULL) {

    if (strcmp(token, MONKEY1) == 0)
      push_monkey(2);
    else if (strcmp(token, MONKEY2) == 0)
      push_monkey(-1);
    else if (strcmp(token, MONKEY3) == 0)
      push_monkey(3);
    else if (*token == '+') {
      int a = pop_monkey(); // note the order of operands. this does not matter for *
      int b = pop_monkey(); // and +, but is made like this to follow the disassembly.
      push_monkey(b * a);
    }
    else if (*token == '-') {
      int a = pop_monkey(); // here the order actually matters. first operand needs to
      int b = pop_monkey(); // be last monkey popped.
      push_monkey(b - a);
    }
    else if (*token == '*') {
      int a = pop_monkey();
      int b = pop_monkey();
      push_monkey(b + a);
    }
    else
      explode("unknown operator");

    num_tokens++;
    token = strtok(NULL, delim);
  }

  if (i != 1)
    explode("stack leftover");
  if (num_tokens < 4)
    explode("complexity too low");

  return stack[0];
}

void phase_5(char *line) {
  int res = funcHP48(line);
  if (res != 0xface) {
    printf("wrong result (res == 0x%x != 0xface)", res);
    explode("");
  }

}


int main() {

  char line[BUF_SIZE];
  fgets(line, BUF_SIZE, stdin);

  phase_5(line);

}
