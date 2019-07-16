#include "../pragma.h"

ap_int<32> A_0_0[16][16];
ap_int<32> A_0_1[16][16];
ap_int<32> A_1_0[16][16];
ap_int<32> A_1_1[16][16];

ap_int<32> B_0_0[16][16];
ap_int<32> B_0_1[16][16];
ap_int<32> B_1_0[16][16];
ap_int<32> B_1_1[16][16];

ap_int<32> res = 0;

// gadget for reading A
ap_int<32> read_a(ap_int<32> i, ap_int<32> j) {
  if (i >= 0 && i < 16 && j >= 0 && j < 16) {
    return A_0_0[i][j];
  }
  else if (i >= 0 && i < 16 && j >= 16 && j < 32) {
    return A_0_1[i][j - 16];
  }
  else if (i >= 16 && i < 32 && j >= 0 && j < 16) {
    return A_1_0[i - 16][j];
  }
  else if (i >= 16 && i < 32 && j >= 16 && j < 32) {
    return A_1_1[i - 16][j - 16];
  }
  else {
    return -1;
  }
}

// gadget for writing to A
void write_a(ap_int<32> i, ap_int<32> j, ap_int<32> v) {
  if (i >= 0 && i < 16 && j >= 0 && j < 16) {
    A_0_0[i][j] = v;
  }
  else if (i >= 0 && i < 16 && j >= 16 && j < 32) {
    A_0_1[i][j - 16] = v;
  }
  else if (i >= 16 && i < 32 && j >= 0 && j < 16) {
    A_1_0[i - 16][j] = v;
  }
  else if (i >= 16 && i < 32 && j >= 16 && j < 32) {
    A_1_1[i - 16][j - 16] = v;
  }
}

// gadget for read b
ap_int<32> read_b(ap_int<32> i, ap_int<32> j) {
  if (i >= 0 && i < 16 && j >= 0 && j < 16) {
    return B_0_0[i][j];
  }
  else if (i >= 0 && i < 16 && j >= 16 && j < 32) {
    return B_0_1[i][j - 16];
  }
  else if (i >= 16 && i < 32 && j >= 0 && j < 16) {
    return B_1_0[i - 16][j];
  }
  else if (i >= 16 && i < 32 && j >= 16 && j < 32) {
    return B_1_1[i - 16][j - 16];
  }
  else {
    return -1;
  }
}

// gadget for writing to b
void write_b(ap_int<32> i, ap_int<32> j, ap_int<32> v) {
  if (i >= 0 && i < 16 && j >= 0 && j < 16) {
    B_0_0[i][j] = v;
  }
  else if (i >= 0 && i < 16 && j >= 16 && j < 32) {
    B_0_1[i][j - 16] = v;
  }
  else if (i >= 16 && i < 32 && j >= 0 && j < 16) {
    B_1_0[i - 16][j] = v;
  }
  else if (i >= 16 && i < 32 && j >= 16 && j < 32) {
    B_1_1[i - 16][j - 16] = v;
  }
}

void kernel(ap_int<32> iterations) {

  while((iterations > 0)) {
    res = 0;
    for(int i = 0; i < 32; i++) {
      for(int j = 0; j < 32; j++) {
        write_a(i, j, 0);
        write_b(i, j, (i + j));
      }
    }
    //---
    for(int i = 0; i < 32; i++) {
      for(int j = 0; j < 32; j++) {
        write_a(i, j, read_b(i, j) + 1);
      }
    }
    //---
    for(int i = 0; i < 32; i++) {
      for(int j = 0; j < 32; j++) {
        ap_int<32> x = read_b(i, j);
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
