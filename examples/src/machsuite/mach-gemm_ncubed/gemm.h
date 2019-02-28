#ifndef _GEMM_H_
#define _GEMM_H_

//#define SDX

//Standard Libraries
//#include <stdio.h>
//#include <stdlib.h>

//Define compute data type
//#define TYPE double
#define TYPE float

//Specify row/column sizes
#define row_size 64
#define col_size 64
#define N row_size*col_size

//Define the input range to operate over
#define MIN 0.
#define MAX 1.0

//Set number of iterations to execute
#define MAX_ITERATION 1

//#pragma SDS data copy(m1[0:row_size][0:col_size], m2[0:row_size][0:col_size])
//#pragma SDS data zero_copy(prod[0:row_size][0:col_size])
//#pragma SDS data access_pattern(m1:SEQUENTIAL) //, m2:SEQUENTIAL)
//#pragma SDS data data_mover(m1:AXIFIFO, m2:AXIFIFO)
void gemm(TYPE *m1, TYPE *m2, TYPE *prod);

#endif /* _GEMM_H_ */
