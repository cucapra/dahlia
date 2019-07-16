// Avoid using `ap_int` in "software" compilation.
#include "../pragma.h"


void kernel(ap_int<32> iterations) {


  #pragma HLS ARRAY_PARTITION variable=A factor=2 dim=1
  #pragma HLS ARRAY_PARTITION variable=A factor=2 dim=2
  ap_int<32> A[32][32];

  #pragma HLS ARRAY_PARTITION variable=B factor=2 dim=1
  #pragma HLS ARRAY_PARTITION variable=B factor=2 dim=2
  ap_int<32> B[32][32];
  ap_int<32> res;


  while((iterations > 0)) {
    res = 0;
    for(int i = 0; i < 32; i++) {
      for(int j = 0; j < 32; j++) {
        A[i][j] = 0;
        B[i][j] = (i + j);
      }
    }
    //---
    for(int i = 0; i < 32; i++) {
      for(int j = 0; j < 32; j++) {
        A[i][j] = (B[i][j] + 1);
      }
    }
    //---
    for(int i = 0; i < 32; i++) {
      for(int j = 0; j < 32; j++) {
        ap_int<32> x = B[i][j];
        // combiner:
        res += x;
      }
    }
    iterations = (iterations - 1);
  }

  #ifndef __SDSVHLS__
  std::cout << res << std::endl;
  #endif
}
