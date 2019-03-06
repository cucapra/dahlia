---
title: Generating Executables
id: cpp-runnable
---

The `fuse` CLI provides the `run` subcommand for generating executables that
can be used to test Fuse code without going throught a synthesis workflow.

## Example

As an example, we'll be using the following fuse code:

```C
def extern print_vector(c: float[4]);

decl a: float[2][2];
decl b: float[2][2];
decl c: float[2][2];

for (let i = 0..2) {
  for (let j = 0..2) {
    c[i][j] := a[i][j] + b[i][j];
  }
}
---
print_vector(c);
```

Add this code to a file titled `matadd.fuse`. The `print_vector` extern is
provided by the parsing library.

> If you're running the `fuse` command outside the compiler repository, the
> command will also create a folder called `_headers` and unpack the required
> header files into it.

Next, run the command:

```
fuse run matadd.fuse -o matadd.cpp
```

the command will something similar to the following:

When the command succeeds, it will add two new files to the directory --
`matadd.cpp` and `matadd.cpp.out`.

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

which will produce the desired output from adding the two matrices `a` and `b`.

## Compiler Configuration

The `fuse run` command is a wrapper that essentially calls two commands:

```
fuse matadd.fuse -o matadd.cpp
$(CXX) -I <headers_loc>/ matadd.cpp -o matadd.cpp.o
```

The headers include a small parsing library written in C++ that uses the
[picojson](https://github.com/kazuho/picojson) header library to parse in
arguments to the function.

The `run` mode also automatically generates a `main` method to parse in
arguments and align their types. This mode is meant for allow for quick functional
testing of the kernel code while development.

## Larger Projects

For larger kernels or projects, we recommend creating a Makefile. The Makefile
is meant to give finer grained control over the compilation parameters and
linking.

This code can be used as a starter for the Makefile:

```make
.PHONY: all

SRCS:=$(wildcard *.cpp)
OBJS:=$(SRCS:.cpp=.out)
CPP=g++ --std=c++11 -Wall

all: $(OBJS)

%.cpp: %.fuse
	fuse $< -o $@

%.o: %.cpp
	$(CPP) $< -o $@

clean:
	rm -rf *.o
```
