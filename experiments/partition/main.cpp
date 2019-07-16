#include <stdlib.h>
#include <assert.h>
#include "pragma.h"

int main(int argc, char **argv) {
  ap_int<32> iterations = 1000;

  kernel(iterations);

  return 0;
}
