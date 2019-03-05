---
id: cli-compile
title: Compilation Options
---

The CLI tools can be built using the [installation instructions](install.md#use-it).
It provides the following options:

## Kernel Name

The `-n` or `--name` option provides control over the name of the top level
kernel function generated to wrap the `decl`s. The default kernel name is
`kernel`.

## Output file

The `-o` or `--out` option tells the compiler to place the output in a new
file `<out>` instead of printing to STDOUT.

## Backend

The `-b` or `--backend` allows changing the backend used for generating code.
Support backends are:

1. `vivado`: Generates code compatible with Vivado HLS.
2. `c++`: Generate code that can be compiled into an executable. See [this](cpp-runnable.md).

## `run`

The subcommand `run` requires the `-o` to be present. It automatically generates
a CPP file and links it with the json data parser. See [this](cpp-runnable.md)
for more information.
