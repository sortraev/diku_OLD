
#define _GNU_SOURCE

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/mman.h>
#include <unistd.h>

#define LINE_COMMENT_PATCH 1
#define FIX_JMP_BUG        0
#define DEBUG_SHELLCODE    0
#define OUTPUT_BYTES_PATCH 1

// DASH(A, imm32) -> mov eax, imm32
// DASH(B, imm32) -> mov ebx, imm32
#define DASH           "\xF0\x9F\x92\xA8" // 💨

// POINT_UP(_, imm32) -> add eax, imm32
#define POINT_UP       "\xF0\x9F\x91\x86" // 👆

// POINT_DOWN(_, imm32) -> sub eax, imm32
#define POINT_DOWN     "\xF0\x9F\x91\x87" // 👇

// CLAPPING_HANDS ->
// add ax, bx
// nop, nop, nop
#define CLAPPING_HANDS "\xF0\x9F\x91\x8F" // 👏

// LIBRA(A, imm32) -> cmp eax, imm32
// LIBRA(_, _)     -> error!
#define LIBRA          "\xE2\x99\x8E"     // ♎

// BACK(A, imm8) -> mov eax, [r12 + imm8]
// BACK(_, imm8) -> mov ebx, [r12 + imm8]
#define BACK           "\xF0\x9F\x94\x99" // 🔙

// SOON(A, imm8) -> mov [r12 + imm8], eax
// SOON(_, imm8) -> mov [r12 + imm8], ebx
#define SOON           "\xF0\x9F\x94\x9C" // 🔜

// RUNNER n -> unconditional jump to instruction n
#define RUNNER         "\xF0\x9F\x8F\x83" // 🏃

// THUMBS_UP(n) -> jump to instruction n if zero flag = 1
#define THUMBS_UP      "\xF0\x9F\x91\x8D" // 👍

// THUMBS_DOWN(n) -> jump to instruction n if zero flag = 0
#define THUMBS_DOWN    "\xF0\x9F\x91\x8E" // 👎

// DIRECT_HIT -> ret
#define DIRECT_HIT     "\xF0\x9F\x8E\xAF" // 🎯


void die(char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(EXIT_FAILURE);
}

typedef struct {
  void *text;
  void *data;
} jit_t;

int jit_init(jit_t* jit) {
  long page_size = sysconf(_SC_PAGESIZE);
  int prot = PROT_READ | PROT_WRITE;
  int flags = MAP_ANONYMOUS | MAP_PRIVATE;

  void* pages[2] = {NULL, NULL};
  for (int i = 0; i < 2; i++) {
    void* mmapped = mmap(NULL, page_size, prot, flags, -1, 0);
    pages[i] = mmapped;
  }

  jit->text = pages[0];
  jit->data = pages[1];
  return 0;
}

int32_t parse_int(const char* inp) {
  int_fast8_t sign = 1;
  if (*inp == '-' || *inp == '+') {
    sign = (*inp == '-') ? -1 : 1;
    inp++;
  }
  int_fast64_t res = 0;
  for (; '0' <= *inp && *inp <= '9'; inp++) {
    res = res * 10 + *inp - '0';
    if (sign * res < INT32_MIN || res > INT32_MAX) {
      fprintf(stderr, "%ld? Seriously?\n", sign*res);
      die("Constant absolutely out of bounds");
    }
  }
  return sign * res;
}


void compile_instruction(const char* cmd, char* out, int instrno) {

  if (strncmp(cmd, DASH, sizeof(DASH) - 1) == 0) {
    out[0] = cmd[sizeof(DASH)] == 'A' ? 0xb8 : 0xbb;
    *(int*)(out + 1) = parse_int(cmd + sizeof(DASH) + 3);
  }
  else if (strncmp(cmd, POINT_UP, sizeof(POINT_UP) - 1) == 0) {
    out[0] = 0x05;
    *(int*)(out + 1) = parse_int(cmd + sizeof(POINT_UP) + 3);
  }
  else if (strncmp(cmd, POINT_DOWN, sizeof(POINT_DOWN) - 1) == 0) {
    out[0] = 0x2d;
    *(int*)(out + 1) = parse_int(cmd + sizeof(POINT_DOWN) + 3);
  }
  else if (strncmp(cmd, CLAPPING_HANDS, sizeof(CLAPPING_HANDS) - 1) == 0) {
    *(int*)out = 0x9090d801;
    out[4] = (char) 0x90;
  }
  else if (strncmp(cmd, LIBRA, sizeof(LIBRA) - 1) == 0) {
    if (cmd[sizeof(LIBRA)] != 'A')
      die("Invalid instruction, can only compare with A");

    out[0] = (char) 0x3d;
    *(int*)(out + 1) = parse_int(cmd + sizeof(LIBRA) + 3);

  }
  else if (strncmp(cmd, BACK, sizeof(BACK) - 1) == 0) {
    out[0] = (char) 0x41;
    out[1] = (char) 0x8b;
    out[2] = (char) (cmd[sizeof(BACK)] == 'A' ? 0x44 : 0x5c);
    out[3] = (char) 0x24;
    out[4] = (char) 4 * parse_int(cmd + sizeof(BACK) + 3);
  }
  else if (strncmp(cmd, SOON, sizeof(SOON) - 1) == 0) {
    out[0] = (char) 0x41;
    out[1] = (char) 0x89;
    out[2] = (char) (cmd[sizeof(SOON)] == 'A' ? 0x44 : 0x5c);
    out[3] = (char) 0x24;
    out[4] = (char) 4 * parse_int(cmd + sizeof(SOON) + 3);
  }
  else if (strncmp(cmd, RUNNER, sizeof(RUNNER) - 1) == 0) {
    *(int*)out = 0xebc301e2;
    int32_t jmp_line_offs = parse_int(cmd + sizeof(RUNNER)) - instrno;
#if FIX_JMP_BUG
    if (!(jmp_line_offs >= -24 && jmp_line_offs <= 26))
      die("invalid line number in argument to RUNNER!");
#endif
    out[4] = (char) (jmp_line_offs - 1) * 5;

  }
  else if (strncmp(cmd, THUMBS_UP, sizeof(THUMBS_UP) - 1) == 0) {
    *(int*)out = 0x74c301e2;
    int32_t jmp_line_offs = parse_int(cmd + sizeof(RUNNER)) - instrno;
#if FIX_JMP_BUG
    if (!(jmp_line_offs >= -24 && jmp_line_offs <= 26))
      die("invalid line number in argument to THUMBS_UP!");
#endif
    out[4] = (char) (jmp_line_offs - 1) * 5;

  }
  else if (strncmp(cmd, THUMBS_DOWN, sizeof(THUMBS_DOWN) - 1) == 0) {
    *(int*)out = 0x75c301e2;
    int32_t jmp_line_offs = parse_int(cmd + sizeof(RUNNER)) - instrno;
#if FIX_JMP_BUG
    if (!(jmp_line_offs >= -24 && jmp_line_offs <= 26))
      die("invalid line number in argument to THUMBS_UP!");
#endif
    out[4] = (char) (jmp_line_offs - 1) * 5;

  }
  else if (strncmp(cmd, DIRECT_HIT, sizeof(DIRECT_HIT) - 1) == 0) {
    out[0] = (char) 0xc3; // ret
    *(int*)(out + 1) = (int) 0x90909090;
  } else {
    fprintf(stderr, "Unknown instr: %s\n", cmd);
    exit(EXIT_FAILURE);
  }
}


struct program {
  size_t length;
  char **lines;
};

#if OUTPUT_BYTES_PATCH
int run(struct program prog, int output_bytes) {
#else
int run(struct program prog) {
#endif
  jit_t jit;
  jit_init(&jit);
  for (int i = 0; i < prog.length; i++) {
    // Each emojinstruction is compiled to exactly five bytes
    compile_instruction(prog.lines[i], ((char*)jit.text) + 5 * i, i);
  }

#if OUTPUT_BYTES_PATCH
  if (output_bytes) {
    write(1, jit.text, prog.length * 5);
    return 0;
  }
#endif
  if (mprotect(jit.text, 4096, PROT_READ|PROT_EXEC) != 0) {
    die("Cannot make the compiled code executable");
  }
  int64_t res = 0;
  __asm(
        "\n\tpush %%r12"
        "\n\tpush %%rbx"
        "\n\tmov $10000, %%rcx" // Maximum number of jumps, infinite loop protection
        "\n\tmov %1, %%rax"
        "\n\tmov %2, %%r12"
#if DEBUG_SHELLCODE
        "\n\tint3"
#endif
        "\n\tcall *%%rax"
        "\n\tpop %%rbx"
        "\n\tpop %%r12"
        "\n\tmov %%rax, %0"
        : "=r"(res)
        : "r"(jit.text), "r"(jit.data)
        : "rax", "rcx", "cc", "memory"
       );
  return (int)res;
}

const char* example[] = { // Computes the 11'th Fibonacci number
  DASH"(A, 10)",
  SOON"(A, 1)",
  DASH"(A, 0)",
  DASH"(B, 1)",
  POINT_UP"(A, 1)",
  SOON"(A, 2)",
  SOON"(B, 3)",
  BACK"(A, 2)",
  BACK"(B, 3)",
  CLAPPING_HANDS,
  SOON"(B, 2)",
  SOON"(A, 3)",
  BACK"(A, 1)",
  POINT_DOWN"(A, 1)",
  SOON"(A, 1)",
  LIBRA"(A, 0)",
  THUMBS_UP"(18)",
  RUNNER" 7",
  BACK"(A, 2)",
  DIRECT_HIT,
};


#if LINE_COMMENT_PATCH
int is_whitespace(char c) {
  return c == ' ' || c == '\t' || c == '\r' || c == '\v' || c == '\n' || c == '\0';
}

int line_is_comment(char *line, size_t n) {
  int i = 0;
  while (line[i]) {
    if (line[i] == '#')
      return 1;
    else if (!is_whitespace(line[i]))
      return 0;
    i++;
  }
  return 1;
}
#endif

struct program read_program(char *filename) {
  struct program p;
  p.length = 0;
  p.lines = calloc(800, sizeof(char *));
  FILE *file = fopen(filename, "r");

  if(!file) {
    die("Cannot open file");
  }

  char *line = NULL;
  size_t n = 0;
  ssize_t line_len = 0;
  while ((line_len = getline(&line, &n, file)) != -1) {

#if LINE_COMMENT_PATCH
    if (line_is_comment(line, n))
      free(line);
    else
#endif
    {
      if (++p.length > 799) {
        die("Program too long");
      }
      p.lines[p.length-1] = line;
    }

    line = NULL;
  }

  fclose(file);

  return p;
}

int main(int argc, char **argv) {

  struct program p;
  if (argc == 1) {
    printf("USAGE: %s <option> \n\n", argv[0]);
    printf("OPTIONS:\n"
           "  -run <file>   Compile and run the program in file\n"
           "  -hint         Give some hints\n"
#if OUTPUT_BYTES_PATCH
           "  -p            Output compiled instruction bytes\n"
#endif
          );
  } else if (argc == 2 && strcmp(argv[1], "-hint") == 0) {
    struct program p =
      { .length = sizeof(example)/sizeof(example[0]),
        .lines = (char **) example };

    printf("You need a hint? Sure, here is sample program:\n\n");
    for (int i = 0; i < p.length; ++i)
      printf("%s\n", p.lines[i]);

    printf("\nIt should evaluate to %d (better check that you agree, before running ahead)\n\n", run(p, 0));

  } else if (argc == 3 && strcmp(argv[1], "-run") == 0) {
    struct program p = read_program(argv[2]);
#if OUTPUT_BYTES_PATCH
    int res = run(p, 0);
#else
    int res = run(p);
#endif
    printf("res = %d\n", res);
#if OUTPUT_BYTES_PATCH
  } else if (argc == 3 && strcmp(argv[1], "-p") == 0) {
    struct program p = read_program(argv[2]);
    run(p, 1);
#endif
  } else {
    printf("Unknown options\n");
  }

  return EXIT_SUCCESS;
}
