# Seashell Examples

This is a list of Seashell programs that showcase useful FPGA programming behavior.

## Usage

    make all   # Generate HLS programs (will be placed in new folder out/)
    make clean # Remove HLS programs

## Example List

| # | Example | Creator | Purpose | Status as of 2018/09/04
| --- | --- | --- | --- | --- |
| 1 | [vsadd](https://github.com/cucapra/seashell/blob/master/examples/docs/vsadd.md) | SA | Banking and unrolling | **Passes.** Typechecks and emits expected code. |
| 2 | [vsadd_nrl](https://github.com/cucapra/seashell/blob/master/examples/docs/vsadd.md) | SA | Explicit access with banks | **Fails.** Bank accessor must be static. (Seems explicit access is throwing an error) |
| 3 | [vvadd](https://github.com/cucapra/seashell/blob/master/examples/docs/vvadd.md) | SA | Banking and unrolling | **Passes.** Typechecks and emits expected code. |
| 4 | [matadd(vvadd_nested)](https://github.com/cucapra/seashell/blob/master/examples/docs/vvadd.md) | SA | Check nested loops and multi loop unroll | **Fails.** Index must contain static information (Since we have added the multi-d rule, this should work) |
| 5 | float | TB | Simple usage of float type | **Passes.** Typechecks and emits expected code. |
| 6 | typedefs | TB | Simple usage of type alias | **Fails.** Type error: can't apply operator '+' to number and number. |
| 7 | [multaccess](https://github.com/cucapra/seashell/blob/master/examples/docs/multaccess.md) | SA | Check multi-write access to same element | **Fails.** Index must contain static information |
| 8 | [matadd_1dfullnrl/(vvadd_fullnrl)](https://github.com/cucapra/seashell/blob/master/examples/docs/vvadd.md) | SA | Check explicit access with variable if loop full unrolled | **Fails.** Bank accessor must be static (This format may no longer be supported.)|
| 9 | [matmul](https://github.com/cucapra/seashell/blob/master/examples/docs/matmul.md) | SA | Compute-reduce, transposed loop access | To be implemented. |
| 10 | convolution | SA | Multiple loops and designs with different loop unrolls | To be implemented. |
| 11 | mini_convolution | TB | Interactions between index types and integers. Physical and logical access. | **Fails.** Bank accessor must be static |

What are following sea files?
    * illegal_op.sea
