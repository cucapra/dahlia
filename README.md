# Seashell

Under development! Seashell provides a type system that makes programming FPGAs easier.

## Setup/Use

Requires Dune and Menhir (in addition to OCaml and opam):

	$ opam install dune
	$ opam install menhir

Generate an HLS program from your Seashell program:

	$ cat my_program | dune exec bin/ex.bc
