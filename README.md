# Seashell

[![CircleCI](https://circleci.com/gh/cucapra/seashell.svg?style=svg)](https://circleci.com/gh/cucapra/seashell)

Seashell provides a type system that makes programming FPGAs easier. To learn about how Seashell works and where it's headed, check out these links:
  - [Notes about Seashell](https://capra.cs.cornell.edu/seashell/docs/index.html)
  - [Internal Seashell Wiki](https://github.com/cucapra/seashell/wiki)
  - [Seashell Roadmap](https://github.com/cucapra/seashell/wiki/Project-Roadmap)
  - [Vim Syntax Highlighting](https://github.com/tedbauer/seashell.vim)

## Setup/Use

Install dependencies (in addition to OCaml and OPAM) and update the OPAM environment:

	$ opam install dune menhir core
	$ eval $(opam env)

Then, build and install `seac` compiler:

    $ dune build
    $ dune install

(You can also just type `make` to do the same thing.)
Now you can generate HLS programs from your Seashell programs.
For example:

    $ seac < examples/float.ss > float.c
