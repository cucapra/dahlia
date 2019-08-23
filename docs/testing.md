---
id: testing
title: Testing Fuse
---

We use `sbt`'s built-in testing facilities to run tests. There are four test
suites for the language:

## Parsing Tests

Parsing tests simply check to see if language constructs are parsable or not.
They do not perform any type checking

## Type Checker Spec

These is the most comprehensive set of example to document the behavior of
all language constructs with respect to the type checker. We use the
[FunSpec](http://www.scalatest.org/getting_started_with_fun_spec) model to
specify tests:

- Create a `describe` for the new language construct. If there are too many
  tests for one construct, consider splitting them using logical groups. For
  example, "capabilities w/ simple contexts" and "capabilities w/ complex contexts"

- In each describe, use the `it` function to give the test a descriptive name
  and use the `typeCheck` method to write the test. Use Scala's [multiline
  strings](https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch01s03.html)
  to specify long programs.

## File tests

The file test harness simply pulls in tests under `src/test/{should-compile,should-fail}`.
As the names suggest, the first directory contains tests that should compile with
the type checker while the second contains programs that should fail.

Simply add a Fuse program under the directory to get them to run.

## Run tests

This harness is used to check the functional correctness of the emitted code.
It creates a temporary directory `_test` to store intermediary C++ files.

For every `*.fuse` program under the `src/test/should-run` directory, the
harness does the following:

1. Call `fuse run <src> -o ./_test/<src>.cpp`
2. Call `./_test/<src>.cpp.o src/test/should-run/<src>.data.json`

### Adding tests

To add a new test, create a fuse program `<src>` and import the `cassert` header:

```
import "cassert" {
  def assert(cond: bool);
}
```

Add a `gold` argument to the kernel using a `decl`. At the end of the program,
compare the result of the computation with the `gold` value:

```
decl gold: ...
...
assert(result == gold);
```

> `==` only works correctly for primitive types. To make `gold` a memory type,
> define a function that walks over the two arrays and checks if each element
> is equal.

Next, create a file with the name `<src>.data.json` and add a json file with
the arguments to the kernel as specified in ["Generating executables"](cpp-runnable.md).

Now run `sbt test` to check if the test passes.

> Running Run tests while development can be slow. Instead of `sbt test` or
> `sbt ~test`, use `sbt ~testOnly -- -l "fuselang.tag.SlowTest` to exclude the
> run tests.
