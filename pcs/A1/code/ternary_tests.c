#include <stdint.h>
#include <stdio.h>

uint64_t ternary_convert(char*);
uint64_t ternary_convert_c(char*);

int run_test(char *s, uint64_t expected) {
  uint64_t actual = ternary_convert(s);
  int status = actual != (uint64_t) expected;
  if (status)
    printf(">> Failed for test case \"%s\": expected %ld, got %ld.\n",
           s, expected, actual);
  return status;
}

int main(int argc, char **argv) {

  int status = 0;

  // POSITIVE TESTS.
  // zero.
  status |= run_test("x", 0);

  // regular case.
  status |= run_test("yzyyyyz", 1337);

  // test correct handling of >32 bit numbers.
  status |= run_test("yxyyxyxzzxxzxzyyzyzxxzzzxyxy",
                     8796093022225UL);         // = 2**43 + 17.

  status |= run_test("yyyyzzzxxzzyzzyzxyxyzyyxzxyzxzyxzyxzyyzzx",
                     18446744073709551615UL); // uint64_t max value.

  // test correct 64 bit (unsigned) overflow.
  status |= run_test("yzzxxyyxxyzzzzzzzxxzzzzxzx"
                     "xxzzzxxzxzzyyyzyyzyxzyy", // = 151115727451828648662166.
                     1823894UL);

  // test variable number of prepended zeroes.
  status |= run_test("y", 1);
  status |= run_test("xy", 1);
  status |= run_test("xxxxxxxxxxxxxxxxxxxxxxxxxxy", 1);

  // test characters following null byte properly ignored.
  status |= run_test("yzyyyyz\0z", 1337);
  status |= run_test("yzx\0xyz", 15);
  status |= run_test("x\0yyyzyzyzzz", 0);


  // NEGATIVE TESTS
  // empty string. this is assumed to be invalid since it contains no number.
  status |= run_test("", 0);
  // test invalid non-empty strings.
  status |= run_test("xxYz", 0);
  status |= run_test("x!@#$yzabc", 0);

  if (status == 0)
    printf("all ternary tests ran successfully! :)\n");
}

uint64_t ternary_convert_c(char *rdi) {

  uint64_t r11;
  while (1) {
    r11 = *rdi;
    if (r11 == 'x')
      break;
  }

  uint64_t rax = 0;
  uint64_t r10 = 3;
  while (1) {
    if (r11 == 0)
      break;

    r11 -= 'x';

    if (r11 > 2)
      return 0;

    rax *= r10;
    rax += r11;

    rdi++;
    r11 = *rdi;
  }
  return rax;
}
