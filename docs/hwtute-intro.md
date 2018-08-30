Tutorial on hardware programming
-------

This tutorial is targeted to introduce hardware programming in high-level languages to C programmers or hardware programmers interested in moving into high level synthesis. This tutorial will use:
 * Seashell, which is a type overlay to ensure hardware safety of your program and porgram synthesis to C/C++
 * Buildbot, which is infrastructure created to submit a seashell program and receive hardware results
 
Buildbot itself hides away some other tools that can be replaced with different tools,
 * Vivado HLS, which is a C/C++ to Verilog/VHDL translator on top of Vivado synthesis tool flow
 * Xilinx Zedboard SoC development board, which has an arm CPU and a Zync FPGA with memory and interconnections which have some configurability
 
We'll be going through the following examples to get familiarized with the tools and to use the infrastructure to create interesting programs.

1. [vector-vector add on host processor](hwtute-01.html)  
   - Run a program on heterogeneous platform host processor
2. [vector-vector add on reconfigurable hardware controlled by the host processor](hwtute-02.html)  
   - Design and interface hardware module in Buildbot, Familiarize with Seashell, Run a CPU-FPGA program on a heterogeneous platform
3. [optimized vector-vector add on reconfigurable hardware controlled by the host processor](hwtute-03.html)  
   - Introduce to commonly used hardware optimizations, array partition, loop unroll and pipelining
4. [matrix multiplication]()  
   - Usage and optimization of nested loops, Using local variables, 
5. [convolution]()  
   - Data transfer methods, Using memory, 
