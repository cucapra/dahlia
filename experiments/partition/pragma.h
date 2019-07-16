#ifdef __SDSVHLS__
#include "ap_int.h"
#else
template <int N> using ap_int = int;
#include <iostream>
#endif

void kernel(ap_int<32> iterations);
