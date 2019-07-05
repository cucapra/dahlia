---
id: overview
title: Overview
---

Fuse is a programming language for designing hardware accelerators.  It
provides abstractions that guarantee hardware realizability after type
checking.

The goal of this project is to build an end-to-end pipeline for compiling
high level programming languages into performant hardware designs. Instead
of targeting unrestricted programming languages like C or C++ or building
domain specific languages, we're building an imperative programming language
that leverages an affine type system to constrain programs to
only represent valid hardware designs.

The current state of the art in High Level Synthesis (HLS) tools take unconstrained
programming languages like C/C++ or various subsets thereof and compile them
down to hardware designs. The compilation process is imprecise and depends
heavily on a **scheduling pass**. Scheduling is a catch-all term for the various
dependency analysis passes (such as alias analysis) and hardware module
instantiation passes that an HLS tools must to extract static designs from
C programs.

Furthermore, while HLS tools claim to transparently compile C/C++ programs,
such codebases do not result in performant hardware designs. To circumvent
this, the HLS toolchains provide static annotations (pragmas) for C programs
that guide the toolchain in making decisions such as memory paritioning and
loop pipelining. The complicated interactions between these source level
pragmas and the scheduling passes result in unexpected hardware designs, complicated
developer workflows, and manual effort spent optimizing the designs. All of
this stands in opposition to the claim that HLS tools improve productivity and
produce usable designs.

Instead of using these ad hoc techniques, we aim to develop a precise semantics
and compiler toolchain and leverage modern programming language techniques (such
as affine type systems) to build modular and agile HLS flows. We do this by treating
scheduling as a first class problem in the langugage.

The intellectual
goal of this project is to study HLS as a language design and compilers problem
and build open source tools that help programming language and architecture
researchers to explore this problem.

### Status

Fuse is still early in its development. The current reference compiler can
generate Vivado HLS C code using fuse code. We have several ongoing projects:

[**Fuse**][repo]: The language constructs and the reference compiler. We are
focusing our efforts on building precise semantics and compiling
down to various HLS backends to study their properties.

[**FuTIL**][futil-repo] (Fuse Temporal Intermediary Language): An intermediary language for compiling high level
languages down to RTL. Distinct from IRs like [FIRRTL][firrtl] which target the lower level problem of optimizing RTL code.

**Fuse-to-RTL**: Ongoing project to directly generate RTL code from Fuse source
programs using FuTIL.

### Open Source

In the spirit of free and open research, **all** of our research
prototypes are open source under a permissive license and we welcome any
collaborations from industry and academic research groups.

[repo]: https://github.com/cucapra/seashell
[futil-repo]: https://github.com/sgpthomas/futil
[firrtl]: https://github.com/freechipsproject/firrtl
