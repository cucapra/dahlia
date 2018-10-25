---
title: Notes About Seashell
---
Try out seashell demo:

* [Online REPL](../)

Read some notes about aspects of Seashell's design:

* [An introduction to Seashell.](seashellintro.html)
  We discuss some basic Seashell programs that showcase the capabilities of the language's type system.

* [Index types.](indextype.html)
  This is the Seashell type system's main mechanism for enforcing safe access to banked memories from unrolled loops (i.e., parallelized hardware).

* [Banking in Hardware.](banking.html)
  Banking strategies used in hardware and their mathematical implications.

* [Logical memory access.](logicalmemoryaccess.html)
  Seashell supports *logical access* to multidimensional arrays implemented as banked hardware memories. We discuss the relationship between this logical view and the physical layout in banks.

* [Proposal: Views](views.html)
  This is a proposal to add two new constructs to make various parallel array
  iterations safe.

* [Appendix](appendix.html)

Tutorial on hardware programming with Seashell:

* [An introduction to Seashell.](tutorial/)

Older discussions about Seashell design:

* [Older discussions](old/)
