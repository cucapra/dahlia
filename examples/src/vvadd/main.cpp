#include <iostream>
#include <stdlib.h>
#include <stdint.h>

#include "madd.h"

#if SDX == 1
#include "sds_lib.h"
#include "ap_int.h"
#else
#include "time.h"
#endif

#ifndef NUM_TESTS
#define NUM_TESTS 1024
#endif

class perf_counter
{
public:
     uint64_t tot, cnt, calls;
     perf_counter() : tot(0), cnt(0), calls(0) {};
     inline void reset() { tot = cnt = calls = 0; }
#if SDX == 1
     inline void start() { cnt = sds_clock_counter(); calls++; };
     inline void stop() { tot += (sds_clock_counter() - cnt); };
#else
     inline void start() { cnt = clock(); calls++; };
     inline void stop() { tot += (clock() - cnt); };
#endif
     inline uint64_t avg_cpu_cycles() { return ((tot+(calls>>1)) / calls); };
};

static void init_arrays(float *A,  float *B, float *C, float *C_sw)
{
     for (int i = 0; i < N; i++) {
          for (int j = 0; j < N; j++) {
               A[i * N + j] = 1+i*N+j;
               B[i * N + j] = rand() % (N * N);
               C[i * N + j] = 0.0;
               C_sw[i * N + j] = 0.0;
          }
     }
}

void madd_golden(float *A, float *B, float *C)
{
     for (int row = 0; row < N; row++) {
          for (int col = 0; col < N; col++) {
               C[row*N+col] = A[row*N+col] + B[row*N+col];
          }
     }

}

static int result_check(float *D, float *D_sw)
{
     for (int i = 0; i < N * N; i++) {
          if (D_sw[i] != D[i]) {
               std::cout << "Mismatch: data index=" << i << "d=" << D_sw[i] 
                        << ", dout=" << D[i] << std::endl;
               return 1;
          }
     }
     return 0;
}

int vvadd_test(float *A,  float *B, float *C, float *C_sw)
{
     std::cout << "Testing " << NUM_TESTS << " iterations of " << N << "x" << N 
               << " floating point vvadd..." << std::endl;

     perf_counter hw_ctr, sw_ctr;
     
     for (int i = 0; i < NUM_TESTS; i++) 
     {
          init_arrays(A, B, C, C_sw);

          sw_ctr.start();
          madd_golden(A, B, C_sw);
          sw_ctr.stop();

          hw_ctr.start();
          madd(A, B, C);
          hw_ctr.stop();

          if (result_check(C, C_sw))
               return 1;
     }

     uint64_t sw_cycles = sw_ctr.avg_cpu_cycles();
     uint64_t hw_cycles = hw_ctr.avg_cpu_cycles();
     double speedup = (double) sw_cycles / (double) hw_cycles;

     std::cout << "Average number of CPU cycles running mmultadd in software: "
               << sw_cycles << std::endl;
     std::cout << "Average number of CPU cycles running mmultadd in hardware: "
               << hw_cycles << std::endl;
     std::cout << "Speed up: " << speedup << std::endl;

     return 0;
}

int main(int argc, char* argv[]){
     int test_passed = 0;
     float *A, *B, *C, *C_sw;

#if SDX == 1
     A = (float *)sds_alloc(N * N * sizeof(float));
     B = (float *)sds_alloc(N * N * sizeof(float));
     C = (float *)sds_alloc(N * N * sizeof(float));
     C_sw = (float *)malloc(N * N * sizeof(float));

     if (!A || !B || !C || !C_sw) {
          if (A) sds_free(A);
          if (B) sds_free(B);
          if (C) sds_free(C);
          if (C_sw) free(C_sw);
          return 2;
     }
#else
     A = (float *)malloc(N * N * sizeof(float));
     B = (float *)malloc(N * N * sizeof(float));
     C = (float *)malloc(N * N * sizeof(float));
     C_sw = (float *)malloc(N * N * sizeof(float));

     if (!A || !B || !C || !C_sw) {
          if (A) free(A);
          if (B) free(B);
          if (C) free(C);
          if (C_sw) free(C_sw);
          return 2;
     }
#endif

     test_passed = vvadd_test(A, B, C, C_sw);

     std::cout << "TEST " << (test_passed ? "FAILED" : "PASSED") << std::endl;

#if SDX == 1
     sds_free(A);
     sds_free(B);
     sds_free(C);
     free(C_sw);
#else     
     free(A);
     free(B);
     free(C);
     free(C_sw);
#endif

     return (test_passed ? -1 : 0);
}

