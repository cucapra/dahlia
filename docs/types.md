---
id: types
title: Types
---

At the heart of the Dahlia compiler is the type checker that checks for various
constraints that make programs hardware realizable.

## Booleans

Boolean constants and the result of comparisons are typed as `boolean`.

```dahlia
let x = true // x is boolean
let z = x >= y // y is boolean
```

## Sized Integers

Bitwidth optimizations are important for building fast hardware. Dahlia allows
programs to specify an exact bit width specification for all constants
and variables.

A bare `let`-binding automatically infers bit widths for constants. The
inference is conservative and finds the minimum number bits needed to store
the constant:

```dahlia
let x = 1; // x is bit<1>
```

If the program needs to store a constant with a larger bitwidth (for use in
a future computation), simply add an explicit type annotation:

```dahlia
let x: bit<10> = 1;
```

## Floats & Doubles

Dahlia provides a simple float type:

```dahlia
let f = 1.0; // f is float
let f: double = 2.0;
```

**Note**: Automatic inferences always infers the type `float` for floating
point literals.

Fixed point floating numbers are **not yet implemented**.

## Binary Operations

Binary operations over `booleans` and `floats` are straightforward.

```dahlia
let a = true || false // a is boolean
let b = 1 || 2 // Error! Only booleans expressions can be used with boolean operators
let f = 1.0 + 2.0 // f is float
```

Binary operations over sized integers are more subtle. They automatically
infer the largest bitwidth between the operands and uses that for the
result:

```dahlia
let x: bit<10> = 10;
let y: bit<32> = 20;
let z = x + y; // z is bit<32>
```
