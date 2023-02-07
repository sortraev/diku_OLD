#include <stdlib.h>
#include <stdint.h>

#define ABS(x) ((x) < 0 ? -(x) : (x))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

int main() {

  // first, let's allocate some memory ...
  size_t   request_size = 256 * sizeof(int);
  int      *my_int_arr  = malloc(request_size);
  unsigned *my_uint_arr = malloc(request_size);

  // compute the distance between the two allocations.
  int difference = my_int_arr - my_uint_arr;
  int distance   = ABS(difference);

  // to verify success of one of the allocations, assert that the
  // distance between the two arrays is at least the size of the request.
  int allocation_successful = distance >= request_size;

  return allocation_successful;
}
