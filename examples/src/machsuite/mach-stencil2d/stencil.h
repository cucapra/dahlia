#ifndef _STENCIL_H_
#define _STENCIL_H_

//Define input sizes
#define col_size 64
#define row_size 128
#define f_size 9
#define input_size col_size*row_size

//Data Bounds
#define TYPE int
#define MAX 1000
#define MIN 1

//Set number of iterations to execute
#define MAX_ITERATION 1

//#pragma SDS data copy(orig[0:input_size], sol[0:input_size])
void stencil( TYPE *orig, TYPE *filter, TYPE *sol);

#endif /*_STENCIL_H_*/
