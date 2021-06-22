---
id: installation
title: Installation
---

Here's how to get and run the compiler.

## Set It Up

The compiler is written in [Scala][].
To get things running, you will need a Java runtime, Scala itself, and [sbt][].
Here's what you need to do:

- Get Java if you don't already have it. On macOS with [Homebrew][], for example, you can use `brew cask install adoptopenjdk`.
- Install Scala and sbt. On macOS, use `brew install scala sbt`. For debian/Ubuntu see [this](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html#Ubuntu+and+other+Debian-based+distributions).

Now you can compile the compiler by typing `sbt compile`.
Use `sbt test` to run the tests.
Type `sbt assembly` to generate a [fat jar][] for command-line use and distribution.

[scala]: https://www.scala-lang.org/
[sbt]: https://scala-sbt.org
[homebrew]: https://brew.sh
[fat jar]: https://stackoverflow.com/questions/19150811/what-is-a-fat-jar

## Compiler development

If you're working on the compiler, you probably want to use the sbt console instead (it's faster for repeated builds).
Run `sbt` alone to get the console, where you can type commands like `compile`, `test`, and `run [args]`.

Adding the prefix `~` (such as `~compile`) makes `sbt` go into watch mode, i.e., it will re-run the command every time a dependency changes. Use `~assembly` to continuously update `./fuse` or `~test` to continuously test the changes.

If you want to execute a sequence of `sbt` commands without starting `sbt` console, you can type `sbt "; cmd1; cm2 ..."`. For example, `sbt "; test; assembly"` will run `sbt test` followed by `sbt assembly`.

## Use It

Type `sbt assembly` to package up a [fat jar][] for command-line use.
The short `fuse` shell script here invokes the built jar to run the compiler.
To compile a simple test, for example, run:

    $ ./fuse src/test/should-compile/matadd.fuse

The compiler produces HLS C source code on its standard output.
