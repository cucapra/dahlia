# Seashell

[![build status](https://circleci.com/gh/cucapra/seashell.svg?style=shield)](https://circleci.com/gh/cucapra/seashell)

Seashell provides a type system that makes programming FPGAs easier. To learn about the syntax and behavior, check out the [docs](https://github.com/cucapra/seashell/wiki/Seashell-Syntax).

## Setup/Use

Requires [Dune](https://github.com/ocaml/dune) and [Menhir](http://gallium.inria.fr/~fpottier/menhir/) (in addition to OCaml and OPAM):

	$ opam install dune
	$ opam install menhir

Generate an HLS program from your Seashell program:

	$ cat my_program | dune exec bin/ex.bc
