#include <ctype.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct {
  uintptr_t (*action)();
  char *kuid;
} student_t;

char choice;
student_t *student;

#define FLAGSIZE 64
void winner_function() {
  char buf[FLAGSIZE];
  FILE *f = fopen("flag.txt", "r");
  if (f == NULL) {
    printf("Please create 'flag.txt' in this directory with your "
           "own debugging flag.\n");
    exit(EXIT_SUCCESS);
  }

  if (fgets(buf, FLAGSIZE, f) == NULL) {
    perror("Error reading flag.txt");
    exit(EXIT_FAILURE);
  }
  puts(buf);
  fflush(stdout);
  exit(EXIT_SUCCESS);
}

char *slurp(void) {
  getchar();
  char *line = malloc(98), *linep = line;
  size_t lenmax = 98, len = lenmax;
  int c;
  if (line == NULL)
    return NULL;
  for (;;) {
    c = fgetc(stdin);
    if (c == EOF)
      break;
    if (--len == 0) {
      len = lenmax;
      char *linen = realloc(linep, lenmax *= 2);

      if (linen == NULL) {
        free(linep);
        return NULL;
      }
      line = linen + (line - linep);
      linep = linen;
    }

    if ((*line++ = c) == '\n')
      break;
  }
  *line = '\0';
  return linep;
}

void doAction(student_t *obj) {
  (*obj->action)();
}

void s() {
  printf("OOPS! Leaking secrets...%p\n", winner_function);
  puts("Thanks for subscribing! I really recommend becoming a premium student member!");
}

void p() {
  puts("Payment pending...");
}

void m() {
  puts("Account created.");
}

void leaveMessage() {
  puts("I only read premium student member messages but you can try anyways: ");
  char *msg = (char *)malloc(8);
  if (read(0, msg, 8) <= 0) {
    puts("Error while reading message");
    exit(EXIT_FAILURE);
  }
}

void i() {
  char response;
  puts("You're leaving already(Y/N)?");
  if (scanf(" %c", &response) != 1) {
    puts("I don't understand you");
    exit(EXIT_FAILURE);
  }
  if (toupper(response) == 'Y') {
    puts("Bye!");
    free(student);
  } else {
    puts("OK. Remember to sign up for courses before deadline");
  }
}

void printMenu() {
  puts("");
  puts("Welcome to New Absalon");
  puts("======================");
  puts("(S)ubscribe to a course");
  puts("(I)nquire about graduating");
  puts("(M)ake an Absalon account");
  puts("(P)ay for higher grades");
  puts("(L)eave a message (with or without logging in)");
  puts("(E)xit");
}

void processInput() {
  if (scanf(" %c", &choice) != 1) {
    puts("I don't understand you");
    exit(EXIT_FAILURE);
  }
  choice = toupper(choice);
  switch (choice) {
  case 'S':
    if (student) {
      student->action = (void *) s;
    } else {
      puts("Not logged in!");
    }
    break;
  case 'I':
    student->action = (void*) i;
    break;
  case 'M':
    student->action = (void*) m;
    puts("====================================");
    puts("Registration: Welcome to New Absalon");
    puts("Enter your KU ID: ");
    student->kuid = slurp();
    break;
  case 'P':
    student->action = (void *) p;
    break;
  case 'L':
    leaveMessage();
    break;
  case 'E':
    exit(EXIT_SUCCESS);
  default:
    puts("Invalid option!");
    exit(EXIT_FAILURE);
    break;
  }
}

void setup() {
  // Maybe use _IONBF
  setvbuf(stdin,  NULL, _IOLBF, 0);
  setvbuf(stdout, NULL, _IOLBF, 0);
  setvbuf(stderr, NULL, _IOLBF, 0);
}

int main() {
  setup();
  student = (student_t *) malloc(sizeof(student));
  while (1) {
    printMenu();
    processInput();
    // if (student) {
    doAction(student);
    //}
  }
  return EXIT_SUCCESS;
}
