# Seashell

Under development! Seashell is a language that provides a type system that makes programming FPGAs easier.

## Setup/Use

Build Seashell:
``` jbuilder build bin/ex.bc ```

Transpile a program:
``` cat my_program | jbuilder exec bin/ex.bc ```

Interpret a program:
``` cat my_program | jbuilder exec bin/ex.bc -- -tr ```

Turn off type checking with ``` -nt ```.
