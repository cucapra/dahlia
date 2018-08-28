Tutorial on hardware programming
-------

This tutorial is targeted to introduce hardware programming to C programmers or hardware programmers interested in moving into high level synthesis. This tutorial will use:
 * Seashell, which is a type overlay to ensure hardware safety of your program and porgram synthesis to C/C++
 * Vivado HLS, which is a C/C++ to Verilog/VHDL translator on top of Vivado synthesis tool flow
 * Xilinx Zedboard SoC development board, which has an arm CPU and a Zync FPGA with memory and interconnections which have some configurability
 
We'll be going through the following examples to get familiarized with the tools and to use the infrastructure to create interesting programs.

1. vector-vector add on host processor
   Contrast between C compilers and sdscc, Familiarize with tool flow, Run a program on FPGA development board's host processor
2. vector-vector add on FPGA controlled by the host processor
   Design and interface hardware module in HLS, Familiarize with hardware synthesis, Run a CPU-FPGA program on the development board
3. optimized vector-vector add on FPGA controlled by the host processor
   Introduce to commonly used hardware optimizations, array partition, loop unroll and pipelining
4. matrix multiplication
   Usage and optimization of nested loops, Using local variables, 
5. convolution
   Data transfer methods, Using DDR memory, 
