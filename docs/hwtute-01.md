vector-vector add on host processor
--------

### Hardware apporach

Our first attempt will be to create a host program. A host program is essentially a control program is to manage a heterogeneous system. It so happens, most control programs are very sequential. Therefore, host program is a sequential program, written with a common (high-level) language and running on a general purpose processor. Since these programs are inherently sequential, parallel architectures cannot exploit them. And general purpose processors are specialized at running these programs fast. 

We will run a very simple application on our host processor. This program would create data structures which we'll be using for our vector-vector add application. This would create some arrays, initialize them and send them to a vector-vector add function. In this first program, we will create a software vector-vector add function in our host and run it to get some baseline results.


### How to write this program

In the future, we expect to make seashell capable of writing this program too. For now, we will write this in C, and use an HLS library for C to generate hardware-software interfaces.

```
#include <iostream>
#include <stdlib.h>
#include <stdint.h>
#include "sds_lib.h"

#define N 32 // Number of elements in our vectors
#define NUM_TESTS 1024 // Number of iterations run to average out results

int main(int argc, char* argv[]){
     std::cout << "TEST FOR VECTOR-VECTOR ADD" << std::endl;
     int test_passed = 0;
     float *A, *B, *C_sw;

     A = (float *)malloc(N * N * sizeof(float));
     B = (float *)malloc(N * N * sizeof(float));
     C_sw = (float *)malloc(N * N * sizeof(float));

     if (!A || !B || !C_sw) {
          if (A) free(A);
          if (B) free(B);
          if (C_sw) free(C_sw);
          return 2;
     }

     test_passed = vvadd_test(A, B, C_sw);

     std::cout << "TEST " << (test_passed ? "FAILED" : "PASSED") << std::endl;

     free(A);
     free(B);
     free(C_sw);

     return (test_passed ? -1 : 0);
}
```

Our `vvadd_test` will initialize the arrays, run it on software function, check the results and return a pass or fail.

```
int vvadd_test(float *A,  float *B, float *C_sw)
{
     std::cout << "Testing " << NUM_TESTS << " iterations of " << N << "x" << N
               << " floating point vvadd..." << std::endl;

     perf_counter sw_ctr;

     for (int i = 0; i < NUM_TESTS; i++)
     {
          init_arrays(A, B, C_sw);

          sw_ctr.start();
          vvadd_sw(A, B, C_sw);
          sw_ctr.stop();

          if (result_check(C_sw))
               return 1;
     }

     uint64_t sw_cycles = sw_ctr.avg_cpu_cycles();

     std::cout << "Average number of CPU cycles running mmultadd in software: "
               << sw_cycles << std::endl;

     return 0;
}
```

Array initilization is a simple functions adding random values to our inputs
```
static void init_arrays(float *A,  float *B, float *C_sw)
{
     for (int i = 0; i < N*N; i++) {
               A[i] = rand() % (N*N);
               B[i] = rand() % (N*N);
               C_sw[i] = 0.0;
     }
}

static int result_check(float *D_sw)
{
     float sum =0.0;
     for (int i = 0; i < N * N; i++) {
         sum = sum + D_sw[i];
     }
     
     if (sum == 0.0) {
         std::cout << "Assuming all values in A and B are not 0.0, vector addition has failed" << std::endl;
         return 1;
     }
     
     return 0;
}
```

`perf_counter` is a hardware performance counter measured in host cpu clock cycles. This class is the only reason for adding SDx libraries for this hosp program.

```
class perf_counter
{
public:
     uint64_t tot, cnt, calls;
     perf_counter() : tot(0), cnt(0), calls(0) {};
     inline void reset() { tot = cnt = calls = 0; }
     inline void start() { cnt = sds_clock_counter(); calls++; };
     inline void stop() { tot += (sds_clock_counter() - cnt); };
     inline uint64_t avg_cpu_cycles() { return ((tot+(calls>>1)) / calls); };
};
```

And finally, our primitive vector-vector add software function
```
void vvadd_sw(float *A, float *B, float *C)
{
     for (int i = 0; i < N*N; i++) {
          C[i] = A[i] + B[i];

     }

}
```

::::todo
This needs to be verified after running. Buildbot cannot run it as it will look for a sea file. Not sure if it's a good idea to change it, as we might provide host program too in a sea file.
::::

### How not to write this program
