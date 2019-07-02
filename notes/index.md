---
title: Notes About Seashell
---

::: todo
These notes might be out of date. Refer to the up to date [documentation](https://capra.cs.cornell.edu/fuse/).
:::

Read some notes about aspects of Seashell's design:

* [An introduction to Seashell.](seashellintro.html)
  We discuss some basic Seashell programs that showcase the capabilities of the language's type system.

* [Index types.](indextype.html)
  This is the Seashell type system's main mechanism for enforcing safe access to banked memories from unrolled loops (i.e., parallelized hardware).

* [Banking in Hardware.](banking.html)
  Banking strategies used in hardware and their mathematical implications.

* [Logical memory access.](logicalmemoryaccess.html)
  Seashell supports *logical access* to multidimensional arrays implemented as banked hardware memories. We discuss the relationship between this logical view and the physical layout in banks.

* [Views](view.html)
  This is a proposal to add two new constructs to make various parallel array
  iterations safe.

* [Combine blocks.](combine.html)
  This is Seashell's construct for allowing reductions and other
  imperfectly-parallel patterns in parallel `for` loops.

* [Appendix](appendix.html)

* [Spatial Notes](spatial.html)
  Some notes about spatial.

Tutorial on hardware programming with Seashell:

* [An introduction to Seashell.](tutorial/)

Older discussions about Seashell design:

* [Older discussions](old/)
