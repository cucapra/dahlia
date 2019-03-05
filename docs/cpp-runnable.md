---
title: Generating Executables
id: cpp-runnable
---

The `fuse` CLI provides the `run` subcommand for generating executables that
can be used to test Fuse code without going throught a synthesis workflow.

To build an executable, first run the following command from the project root:

```
sbt assembly
```

This will rebuild the compiler and download any headers required for building
the executable.

## Example

As an example, we'll be using the following fuse code:

```C
decl a: float[2][2];
decl b: float[2][2];
decl c: float[2][2];

for (let i = 0..2) {
  for (let j = 0..2) {
    c[i][j] := a[i][j] + b[i][j];
  }
}
```

Add this code to a file titled `matadd.fuse`.

Next, run the command:

```
fuse run matadd.fuse -o matadd.cpp
```

the command will something similar to the following:

```sh
g++ --std=c++11 -Wall -Isrc/main/resources/headers matadd.cpp -o matadd.cpp.o
```

When the command succeeds, it will add two new files to the directory --
`matadd.cpp` and `matadd.cpp.out`. Remember the output from above, we'll use it
in a later step.

Create another file titled `data.json` with the following contents:

```json
{
  "a": [[1, 1], [1, 1]],
  "b": [[1, 1], [1, 1]],
  "c": [[0, 0], [0, 0]]
}
```

and run the command:

```
./matadd.cpp.out data.json
```

You should see no output indicating that the program ran successfully. However,
this is not very useful. To see an output, open `matadd.cpp` and add the following
line after the call to `kernel`:

```C
int main {
  ...
  kernel(a, b, c); // Already present
  print_vector(c); // Provided by picojson
}
```

and the run the `g++` command produced by `fuse run` earlier followed by
```
./matadd.cpp.out data.json
```

which will produce the desired output from adding the two matrices `a` and `b`.
