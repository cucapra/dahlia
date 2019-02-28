#include <iostream>
#include <stdlib.h>
#include <stdint.h>

#include "stencil.h"

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

static void init_arrays(TYPE *orig, TYPE *filter, TYPE *sol, TYPE *sol_sw)
{
     for (int i = 0; i < row_size; i++) {
          for (int j = 0; j < col_size; j++) {
               orig[i * row_size + j]   = rand() % (input_size);
               sol[i * row_size + j]    = 0.0;
               sol_sw[i * row_size + j] = 0.0;
          }
     }
     
     for (int k = 0; k < f_size; k++) {
          filter[k] = rand() % (f_size);
     }
}

void stencil_golden (TYPE *orig, TYPE *filter, TYPE *sol){
    int r, c, k1, k2;
    TYPE temp, mul;

    for (r=0; r<row_size-2; r++) {
        for (c=0; c<col_size-2; c++) {
            temp = (TYPE)0;
            for (k1=0;k1<3;k1++){
                for (k2=0;k2<3;k2++){
                    mul = filter[k1*3 + k2] * orig[(r+k1)*col_size + c+k2];
                    temp += mul;
                }
            }
            sol[(r*col_size) + c] = temp;
        }
    }
}

static int result_check(TYPE *D, TYPE *D_sw)
{
     for (int i = 0; i < row_size; i++) {
     	for (int j = 0; j < col_size; j++) {
        	if (D_sw[i * row_size + j] != D[i * row_size + j]) {
               		std::cout << "Mismatch: data index=" << i << "d=" << D_sw[i * row_size + j] 
                        	  << ", dout=" << D[i * row_size + j] << std::endl;
               		return 1;
          	}
	}
     }
     return 0;
}

int stencil_test(TYPE *orig,  TYPE *filter, TYPE *sol, TYPE *sol_sw)
{
     std::cout << "Testing " << NUM_TESTS << " iterations of " << row_size << "x" << col_size 
               << " MachSuite stencil 2D..." << std::endl;

     perf_counter hw_ctr, sw_ctr;
     
     for (int i = 0; i < NUM_TESTS; i++) 
     {
          init_arrays(orig, filter, sol, sol_sw);
          
          sw_ctr.start();
          stencil_golden(orig, filter, sol_sw);
          sw_ctr.stop();

          hw_ctr.start();
          stencil(orig, filter, sol);
          hw_ctr.stop();

          if (result_check(sol, sol_sw))
               return 1;
     }

     uint64_t sw_cycles = sw_ctr.avg_cpu_cycles();
     uint64_t hw_cycles = hw_ctr.avg_cpu_cycles();
     double speedup = (double) sw_cycles / (double) hw_cycles;

     std::cout << "Average number of CPU cycles running in software baseline for stencil: "
               << sw_cycles << std::endl;
     std::cout << "Average number of CPU cycles running MachSuite stencil in hardware: "
               << hw_cycles << std::endl;
     std::cout << "Speed up: " << speedup << std::endl;

     return 0;
}

int main(int argc, char* argv[]){
     int test_passed = 0;
     TYPE *orig, *filter, *sol, *sol_sw;

#if SDX == 1
     orig   = (TYPE *)sds_alloc(input_size * sizeof(TYPE));
     filter = (TYPE *)sds_alloc(f_size * sizeof(TYPE));
     sol    = (TYPE *)sds_alloc(input_size * sizeof(TYPE));
     sol_sw = (TYPE *)malloc(input_size * sizeof(TYPE));

     if (!orig || !filter || !sol || !sol_sw) {
          if (orig) sds_free(orig);
          if (filter) sds_free(filter);
          if (sol) sds_free(sol);
          if (sol_sw) free(sol_sw);
          return 2;
     }
#else
     orig   = (TYPE *)malloc(input_size * sizeof(TYPE));
     filter = (TYPE *)malloc(f_size * sizeof(TYPE));
     sol    = (TYPE *)malloc(input_size * sizeof(TYPE));
     sol_sw = (TYPE *)maalloc(input_size * sizeof(TYPE));

     if (!orig || !filter || !sol || !sol_sw) {
          if (orig)   free(orig);
          if (filter) free(filter);
          if (sol)    free(sol);
          if (sol_sw) free(sol_sw);
          return 2;
     }
#endif

     test_passed = stencil_test(orig, filter, sol, sol_sw);

     std::cout << "TEST " << (test_passed ? "FAILED" : "PASSED") << std::endl;

#if SDX == 1
     sds_free(orig);
     sds_free(filter);
     sds_free(sol);
     free(sol_sw);
#else
     free(orig);
     free(filter);
     free(sol);
     free(sol_sw);
#endif

     return (test_passed ? -1 : 0);
}

