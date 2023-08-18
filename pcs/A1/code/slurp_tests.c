#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MAX_LINE 126
#define BUF_SIZE (MAX_LINE + 2)

uint64_t slurp(FILE*, char*);
uint64_t slurp2(int, char*);
uint64_t slurp_c(FILE*, char*);


char *readable(char *s) {
  int n = strlen(s);
  char *out = malloc(n * sizeof(char));
  int i = 0;
  while (s[i] != '\0') {
    out[i] = s[i] == '\n' ? '$' : s[i];
    i++;
  }
  return out;
}

int slurp_test(char *s, char *s_expected, int n_expected) {
  char *buf = malloc(BUF_SIZE * sizeof(char));
  memset(buf, 0, BUF_SIZE);

  // write the test string to a temporary file.
  FILE *f_tmp = tmpfile();
  fwrite(s, sizeof(char), strlen(s), f_tmp);
  rewind(f_tmp);

  uint64_t n_actual = slurp(f_tmp, buf);
  // uint64_t n_actual = slurp2(fileno(f_tmp), buf);
  fclose(f_tmp);


  int assert_num_read  = n_actual != n_expected;
  int assert_output    = strcmp(buf, s_expected) != 0;
  int assert_null_byte = *(strchr(buf, '\n') + 1) != '\0';
  int status = assert_num_read | assert_output | assert_null_byte;

  if (status) {
    printf(">> Failed for test case \"%s\":\n", s);
    if (assert_num_read)
      printf("  >> incorrect number of bytes read: expected %d, read %d.\n",
             n_expected, n_actual);
    if (assert_output) {
      char *_buf = readable(buf),
           *_s_expected = readable(s_expected);
      printf("  >> incorrect output: expected \"%s\", read \"%s\".\n",
             _s_expected, _buf);
      free(_s_expected);
      free(_buf);
    }
    if (assert_null_byte)
      printf("  >> incorrect (or missing) null byte.\n");
  }
  free(buf);

  return status;
}

int main(int argc, char **argv) {

  int status = 0;

  status |= slurp_test("Cpebs = best pebs :DDD",
                       "Cpebs = best pebs :DDD\n", 22);
  // POSITIVE TESTS
  // simple cases.

  status |= slurp_test("foo",   "foo\n", 3);
  status |= slurp_test("foo\n", "foo\n", 4);


  // empty line.
  status |= slurp_test("", "\n", 0);
  // just a newline.
  status |= slurp_test("\n", "\n", 1);

  // 126 character line.
  status |= slurp_test("qwjEIOcja sldcj wEjcl283-kqjwEl_c3jouiah5-"
                       "uCQOIHU242738Y778y64172@4394huqw#hj_kwechj"
                       "kasdyuqioweH$&172389124ha819203845684ajsk_",
                       "qwjEIOcja sldcj wEjcl283-kqjwEl_c3jouiah5-"
                       "uCQOIHU242738Y778y64172@4394huqw#hj_kwechj"
                       "kasdyuqioweH$&172389124ha819203845684ajsk_\n",
                       126);

  // 126 character line with a newline on the end.
  status |= slurp_test("qwjEIOcja sldcj wEjcl283-kqjwEl_c3jouiah5-"
                       "uCQOIHU242738Y778y64172@4394huqw#hj_kwechj"
                       "kasdyuqioweH$&172389124ha819203845684ajsk\n",
                       "qwjEIOcja sldcj wEjcl283-kqjwEl_c3jouiah5-"
                       "uCQOIHU242738Y778y64172@4394huqw#hj_kwechj"
                       "kasdyuqioweH$&172389124ha819203845684ajsk\n",
                       126);
  // more than 126 characters.
  status |= slurp_test("qwjEIOcja sldcj wEjcl283-kqjwEl_c3jouiah5-"
                       "uCQOIHU242738Y778y64172@4394huqw#hj_kwechj"
                       "kasdyuqioweH$&172389124ha819203845684ajsk_foobar? nope!",
                       "qwjEIOcja sldcj wEjcl283-kqjwEl_c3jouiah5-"
                       "uCQOIHU242738Y778y64172@4394huqw#hj_kwechj"
                       "kasdyuqioweH$&172389124ha819203845684ajsk_\n",
                       126);

  if (status == 0)
    printf("all slurp tests ran successfully! :)\n");
}


uint64_t slurp_c(FILE *f, char *buf) {
  int i = 0;
  int c;

  while (1) {
    if (i >= MAX_LINE || (c = fgetc(f)) == EOF)
      goto on_eol;

    buf[i++] = (char) c;

    if (c != '\n')
      continue;

    buf[i] = '\0';
    goto end;
  }

on_eol:
  buf[i]     = '\n';
  buf[i + 1] = '\0';
end:
  return i;
}
