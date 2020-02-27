---
title: Generating Executables
id: cpp-runnable
---

The `fuse` CLI provides the `run` subcommand for generating executables that
can be used to test Dahlia code without going through a synthesis workflow.

## Limitation

The backend is meant to be used for functional testing and as such has a few
important limitations.

1. Exact bitwidth integer computation is not supported. Sized integers get
   compiled to C++ `int`. This affects the semantics of programs that relay on wrap around behavior for sized ints.
2. `unroll`s do not create any form of parallelism. The annotation is compiled
   away after typechecking. This does not affect the semantics, only the performance.
3. Functions are not inlined. This does not affect the semantics, only the performance.

## Example

As an example, we'll be using the following Dahlia code:

```dahlia
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
There will be no output because we aren't printing anything out.

We can define a C++ function to print the output of an array and `import`
it into the fuse file:

```C++
void print_vector(vector<int> vec, int size) {
  for (int i = 0; i < size; i++) {
    std::cout << vec[i] << std::endl;
  }
}

```

and import it using:

```
import "printer.cpp" {
  def print_vector(vec: bit<32>[10], size: bit<32>);
}

```

Assuming that the C++ code was saved in a file called `printer.cpp`. Once
imported, the function can be used as a normal function and used to see
the output.

## Compiler Configuration

The `fuse run` command is a wrapper that essentially calls two commands:

```
fuse matadd.fuse -o matadd.cpp
$(CXX) -I <headers_loc>/ matadd.cpp -o matadd.cpp.o
```

The headers include a small parsing library written in C++ that uses the
[nlohmann json](https://github.com/kazuho/picojson) header library to parse in
arguments to the function.

The `run` mode also automatically generates a `main` method to parse in
arguments and align their types. This mode is meant for allow for quick functional
testing of the kernel code while development.

## Data Format

We use the [JSON](https://www.json.org/) format to pass in data to the compiled
executable. The types in Dahlia directly map to JSON data elements:

- **Numeric Types**: While the JSON standard only allows for `double`s, the
    parsing library correctly casts double into input types. As noted before,
    numbers are not bit precise.
- **Booleans**: Maps to a JSON boolean.
- **N-Dimensional Arrays**: Maps to arrays of arrays in JSON. We require that
   the array elements have the same type. The parsing library can handle
   arbitrary `n`-dimensional arrays. However, note that we use a template based
   implementation for flattening arrays which might increase the *compile* time
   with the C++ compiler.
- **Records**: Records simply map to JSON objects. The record will be parsed
  into the type specified by the declaration.

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
