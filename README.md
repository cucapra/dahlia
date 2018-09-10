# Seashell

[![CircleCI](https://circleci.com/gh/cucapra/seashell.svg?style=svg)](https://circleci.com/gh/cucapra/seashell)

Seashell provides a type system that makes programming FPGAs easier. To learn about how Seashell works and where it's headed, check out these links:
  - [Notes about Seashell](https://capra.cs.cornell.edu/seashell/docs/index.html)
  - [Internal Seashell Wiki](https://github.com/cucapra/seashell/wiki)
  - [Seashell Roadmap](https://github.com/cucapra/seashell/wiki/Project-Roadmap)
  - [Vim Syntax Highlighting](https://github.com/tedbauer/seashell.vim)

## Setup/Use

### Requirements
The following are required to build the seashell compiler.
  - [Opam](https://opam.ocaml.org/blog/opam-2-0-0-rc4/).
  - [Dune](https://github.com/ocaml/dune)

### Installation

Install dependencies (in addition to OCaml and OPAM) and update the OPAM environment:

    $ eval "$(dune external-lib-deps --missing @install 2>&1 | grep 'opam' | sed 's/.*\(opam .*\)/\1/')" -y

Then, run the following commands to install the `seac` CLI tool:

    $ dune build
    $ dune install

(You can also just type `make` to do the same thing.)
Now you can generate HLS programs from your Seashell programs.
For example:

    $ seac examples/float.sea > float.c

For more information about the CLI options, run:

    $ seac --help
