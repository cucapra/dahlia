# Seashell Examples

This is a list of Seashell programs that showcase useful FPGA programming behavior.

## Usage

    make all # Generate HLS programs (will be placed in new folder out/)
    make clean # Remove HLS programs
    
## Example List

| # | Example | Creator | Purpose | Status
| --- | --- | --- | --- | --- |  
| 1 | [vsadd](https://github.com/cucapra/seashell/blob/master/examples/docs/vsadd.md) | SA | Banking and unrolling | **Passes.** Typechecks and emits expected code. |  
| 2 | [vsadd_nrl](https://github.com/cucapra/seashell/blob/master/examples/docs/vsadd.md) | SA | Explicit access with banks | **Passes.** Typechecks and emits expected code. | 
| 3 | [vvadd](https://github.com/cucapra/seashell/blob/master/examples/docs/vvadd.md) | SA | Banking and unrolling | **Passes.** Typechecks and emits expected code. |  
| 4 | [matadd(vvadd_nested)](https://github.com/cucapra/seashell/blob/master/examples/docs/vvadd.md) | SA | Check nested loops and multi loop unroll | **Fails.** Emits some code and typechecks, but we have not decided on type rules for MD access. |   
| 5 | float | TB | Simple usage of float type | **Passes.** Typechecks and emits expected code. |    
| 6 | typedefs | TB | Simple usage of type alias | **Passes.** Typechecks and emits expected code. |    
| 7 | [multaccess](https://github.com/cucapra/seashell/blob/master/examples/docs/multaccess.md) | SA | Check multi-write access to same element | **Fails.** Need to implement. | 
| 8 | [matadd_1dfullnrl/(vvadd_fullnrl)](https://github.com/cucapra/seashell/blob/master/examples/docs/vvadd.md) | SA | Check explicit access with variable if loop full unrolled | **Passes.** Typechecks and emits expected code. |  
| 9 | [matmul](https://github.com/cucapra/seashell/blob/master/examples/docs/matmul.md) | SA | Compute-reduce, transposed loop access | **Fails.** Need to implement. |  
| 10 | convolution | SA | Multiple loops and designs with different loop unrolls | **Fails.** |  
| 11 | mini_convolution | TB | Interactions between index types and integers. Physical and logical access. | **Passes.** Typechecks and emits code, but perhaps need to clarify operations with index types. |  
