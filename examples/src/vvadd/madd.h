#ifndef _MADD_H_
#define _MADD_H_

#define N 32

static int ELE = N;

//#pragma SDS data access_pattern(A:RANDOM, B:RANDOM, C:RANDOM)
//#pragma SDS data copy(A[0:N*N], B[0:N*N], C[0:N*N])
//#pragma SDS data zero_copy(A[0:N*N], B[0:N*N], C[0:N*N])
void madd(float *A, float *B, float *C);

#endif /* _MADD_H_ */


