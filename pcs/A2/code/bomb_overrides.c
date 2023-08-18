#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
#include <stdarg.h>

#define OVERRIDE(ret_t, f_name, args, body) \
  ret_t (f_name) args {\
    ret_t (*_##f_name) args =\
      dlsym(RTLD_NEXT, #f_name);\
    _##f_name = _##f_name;\
    body;\
  }


// disable sleep timer on printing.
OVERRIDE(int, nanosleep, (),
  {
    return 0;
  }
)


// ignore SIGINT handling.
OVERRIDE(void, signal, (int sig, void (*f)(int)),
  {
      if (sig == 2)
        printf(">> ignoring SIGINT handler\n");
      // else if (sig == 14)
      //   printf (">> ignoring SIGALRM handler\n");
      else
        _signal(sig, f);
  }
)


OVERRIDE(int, srand, (),
  {
    int seed = 2;
    printf(">> setting rand seed = %d\n", seed);
    return _srand(seed);
  }
)


#ifdef ENABLE_CHEATS
OVERRIDE(int, sscanf, (const char *restrict str,
                       const char *restrict fmt, ...),
  {
    va_list args;
    va_start(args, fmt);
    float *a = va_arg(args, float*);
    float *b = va_arg(args, float*);
    va_end(args);
    int res = _sscanf(str, fmt, a, b);

    printf(">> sscanf(%s, %s, ...) = %d\n", str, fmt, res);
    return res;
  }
)




OVERRIDE(int, strcmp, (const char *s1, const char *s2),
  {
    int res = _strcmp(s1, s2);
    printf(">> strcmp(%s, %s) = %d\n", s1, s2, res);
    return res;
  }
)


OVERRIDE(char*, strtok, (char *restrict str, const char *restrict delim),
  {
    char *res = _strtok(str, delim);
    printf(">> strtok(%s, \"%s\") = %s\n", str, delim, res);
    return res;
  }
)
#endif
