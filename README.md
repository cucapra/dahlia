# Fuse

[![CircleCI](https://circleci.com/gh/cucapra/seashell.svg?style=svg)](https://circleci.com/gh/cucapra/seashell)

Fuse is a modern programming language for designing Hardware Accelerators. It provides
abstractions that guarantee hardware realizability after type checking. For more
information, read the [notes](https://capra.cs.cornell.edu/seashell/docs/index.html)

## Setup/Use

### Requirements
The following are required to build the Fuse compiler
  - Open JDK 8
  - [Sbt](https://scala-sbt.org) >= 1.2.8

### Running and Testing

- Run `sbt compile` to compile the project.
- Run `sbt test` to run the tests.

### Using the CLI tool

To use the provided `fuse` script, run:
```
sbt assembly
```

to generate a [fat jar](https://stackoverflow.com/questions/19150811/what-is-a-fat-jar<Paste>).

Use the `./fuse` script to run Fuse programs and use the `./target/scala-2.12/fuse.jar`
for standalone redistribution of the compiler
