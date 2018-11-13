vector-vector add on hardware
------------

### Hardware approach

Now that we have a software version running on the development board, next step is to put down a design into the reconfigurable FPGA. We will write the same example we used to run on software and run a comparison between the two. This will also be the introduction to Seashell.

During the host program tutorial, we discussed that the CPU is better suited to play the managerial role in a program. The host program would do the initializations, load data to the memory and would now handover a part of the program that can be parallelized to the FPGA. Once the FPGA does this segment of computation, CPU host would do the wind-down writing the data to the memory and executing the program.

This handover is the major tradeoff for the benefit of offloading compute to a parallel hardware. CPU has to transmit the data out of the chip or handover the data, and once the compute is done the reverse should happen. Depending on the computation we do, this interface between different processing fabrics would differ. However, for this example High-level synthesis would create these interfaces automatically for us.

In high-level synthesis, we can simply write a function constrained by some guidelines, and this would get translated to hardware. We can communicate to this generated hardware by a regular function call. Note that we use some data structures as arguments to this function, and rather than simply passing a value or an address we are actually creating channel to our hardware module. High-level synthesis provides us 

### How to write this program

### How not to write this program

