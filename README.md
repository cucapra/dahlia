# Seashell

Under development! Seashell is a language that provides a type system that makes programming FPGAs easier.

## Setup/Use

Build Seashell:

  $ jbuilder build bin/ex.bc 

Transpile a program:

  $ cat my_program | jbuilder exec bin/ex.bc -- -m t

Interpret a program:

  $ cat my_program | jbuilder exec bin/ex.bc -m i

Build/run the test cases:

  $ jbuilder build test/test.bc
  $ jbuilder exec test/test.bc

Just typecheck :

  $ cat my_program | jbuilder exec bin/ex.bc -m n -nt
