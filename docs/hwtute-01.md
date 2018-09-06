vector-vector add on host processor
--------

Our first attempt will be to create a host program. A host program is a regular program, written with a regular language and running on a general purpose processor. However, general purpose processor are great at running sequential programs. These programs are inherently sequential, so parallel architectures cannot exploit them. ANd general purpose processors are specialized at running these programs fast. It so happens, most control programs are very sequential. And a control program is essential to manage a heterogeneous system.

We will run a very simple application on our host processor, which would create some arrays, initialize them and send them to a vector-vector add function. Then we can print these values to a
