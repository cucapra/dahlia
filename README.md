# Seashell

Under development! Seashell provides a type system that makes programming FPGAs easier.

## Setup/Use

Requires Dune and Menhir (in addition to OCaml and opam):

	$ opam install jbuilder
	$ opam install menhir

Build Seashell:

	$ jbuilder build bin/ex.bc 

Generate an HLS program from your Seashell program:

	$ cat my_program | jbuilder exec bin/ex.bc
