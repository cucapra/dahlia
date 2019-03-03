#include <iostream>
#include <stdlib.h>
#include <stdint.h>

#include "gemm.h"

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

static void init_arrays(TYPE *m1,  TYPE *m2, TYPE *prod, TYPE *prod_sw)
{
     for (int i = 0; i < row_size; i++) {
          for (int j = 0; j < col_size; j++) {
               m1[i * row_size + j] = 1+i*row_size+j;
               m2[i * row_size + j] = rand() % (N);
               prod[i * row_size + j] = 0.0;
               prod_sw[i * row_size + j] = 0.0;
          }
     }
}

void gemm_golden(TYPE *m1, TYPE *m2, TYPE *prod)
{

     for (int row = 0; row < row_size; row++) {
          for (int col = 0; col < col_size; col++) {
               for (int red = 0; red < row_size; red++) {
                    prod[row * row_size + col] += m1[row * row_size + red] * m2[red * row_size + col];
               }
          }
     }
}

static int result_check(TYPE *D, TYPE *D_sw)
{
     for (int i = 0; i < N; i++) {
     	for (int j = 0; j < N; j++) {
        	if (D_sw[i * row_size + j] != D[i * row_size + j]) {
               		std::cout << "Mismatch: data index=" << i << ", d=" << D_sw[i * row_size + j] 
                        	  << ", dout=" << D[i * row_size + j] << std::endl;
               		return 1;
          	}
	}
     }
     return 0;
}

int gemm_test(TYPE *m1,  TYPE *m2, TYPE *prod, TYPE *prod_sw)
{
     std::cout << "Testing " << NUM_TESTS << " iterations of " << row_size << "x" << col_size 
               << "float" << " MachSuite ncubed gemm..." << std::endl;

     perf_counter hw_ctr, sw_ctr;
     
     for (int i = 0; i < NUM_TESTS; i++) 
     {
          init_arrays(m1, m2, prod, prod_sw);
          
          sw_ctr.start();
          gemm_golden(m1, m2, prod_sw);
          sw_ctr.stop();

          hw_ctr.start();
          gemm(m1, m2, prod);
          hw_ctr.stop();

          if (result_check(prod, prod_sw))
               return 1;
     }

     uint64_t sw_cycles = sw_ctr.avg_cpu_cycles();
     uint64_t hw_cycles = hw_ctr.avg_cpu_cycles();
     double speedup = (double) sw_cycles / (double) hw_cycles;

     std::cout << "Average number of CPU cycles running in software baseline for gemm: "
               << sw_cycles << std::endl;
     std::cout << "Average number of CPU cycles running MachSuite gemm in hardware: "
               << hw_cycles << std::endl;
     std::cout << "Speed up: " << speedup << std::endl;

     return 0;
}

int main(int argc, char* argv[]){
     int test_passed = 0;
     TYPE *m1, *m2, *prod, *prod_sw;

#if SDX == 1     
     m1 = (TYPE *)sds_alloc(N * sizeof(TYPE));
     m2 = (TYPE *)sds_alloc(N * sizeof(TYPE));
     prod = (TYPE *)sds_alloc(N * sizeof(TYPE));
     prod_sw = (TYPE *)malloc(N * sizeof(TYPE));

     if (!m1 || !m2 || !prod || !prod_sw) {
          if (m1) sds_free(m1);
          if (m2) sds_free(m2);
          if (prod) sds_free(prod);
          if (prod_sw) free(prod_sw);
          return 2;
     }
#else
     m1 = (TYPE *)malloc(N * sizeof(TYPE));
     m2 = (TYPE *)malloc(N * sizeof(TYPE));
     prod = (TYPE *)malloc(N * sizeof(TYPE));
     prod_sw = (TYPE *)malloc(N * sizeof(TYPE));

     if (!m1 || !m2 || !prod || !prod_sw) {
          if (m1) free(m1);
          if (m2) free(m2);
          if (prod) free(prod);
          if (prod_sw) free(prod_sw);
          return 2;
     }
#endif

     test_passed = gemm_test(m1, m2, prod, prod_sw);

     std::cout << "TEST " << (test_passed ? "FAILED" : "PASSED") << std::endl;

#if SDX == 1     
     sds_free(m1);
     sds_free(m2);
     sds_free(prod);
     free(prod_sw);
#else
     free(m1);
     free(m2);
     free(prod);
     free(prod_sw);
#endif

     return (test_passed ? -1 : 0);
}

