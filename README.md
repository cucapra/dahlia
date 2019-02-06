# Fuse

[![CircleCI](https://circleci.com/gh/cucapra/seashell.svg?style=svg)](https://circleci.com/gh/cucapra/seashell)

Fuse is a programming language for designing hardware accelerators.
It provides abstractions that guarantee hardware realizability after type checking.
For more details, see [the documentation][docs].

[docs]: https://capra.cs.cornell.edu/seashell/docs/index.html


## Set It Up

The compiler is written in [Scala][].
To get things running, you will need a Java runtime, Scala itself, and [sbt][].
Here's what you need to do:

- Get Java if you don't already have it. On macOS with [Homebrew][], for example, you can use `brew cask install adoptopenjdk`.
- Install Scala and sbt. On macOS, use `brew install scala sbt`.

Now you can compile the compiler by typing `sbt compile`.
Use `sbt test` to run the tests.
Type `sbt assembly` to generate a [fat jar][] for command-line use and distribution.

If you're working on the compiler, you probably want to use the sbt console instead (it's faster for repeated builds).
Run `sbt` alone to get the console, where you can type commands like `compile` and `test`.

[scala]: https://www.scala-lang.org/
[sbt]: https://scala-sbt.org
[homebrew]: https://brew.sh
[fat jar]: https://stackoverflow.com/questions/19150811/what-is-a-fat-jar


## Use It

Type `sbt assembly` to package up a [fat jar][] for command-line use.
The short `fuse` shell script here invokes the built jar to run the compiler.
To compile a simple test, for example, run:

    $ ./fuse -f src/test/resources/should-compile/matadd.sea

The compiler produces HLS C source code on its standard output.
