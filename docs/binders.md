---
id: binders
title: Variable Binders
---

Variables in Fuse can be bound in four ways.

## Let bindings

A `let` binding can be used in three ways:

```
let <iden> = <expr>
let <iden>: <type> = <expr>
let <iden>: <type>;
```

The first allows simple variable declarations that automatically infer the
type of the identifer from the expression:

```C
let x = true; // x is boolean
```

The second form simply checks the type of the expression and makes sure it
can be represented as the explicit type.

```
let x: bit<10> = 1;
```

> The right hand side has the type `bit<1>` which can be zero extended to be
> represented using 10 bits.

The final form allows defining uninitialized variables. Reading from uninitialized
memories returns undefined garbage values stored in the memory.

```
let x: bit<10>;
let y = x; // returns garbage value stored in memory
```

## Declarations

The `decl` allow declarations without initalization:

```
decl x: bit<10>
```

However, `decl`s always get compiled into the top level kernel function.
This Fuse program:

```
decl x: bit<10>;
decl y: bit<10>;
x + y
```

turns into the following in the default Vivado backend:

```C++
void kernel(ap_int<10> x, ap_int<10> y) {
  x + y;
}
```

## Functions

Function definitions can be used to bind variables into a new scope:

```
def foo(x: bit<10>, y: bit<10>) {
  x + y
}
```

External functions, such as functions provided by header files, can be
declared using the `extern` keyword.

```
def extern print_vector(c: float[4]);
```

Backends can either provide their implementations for `extern`s (for example
to implement printing when running on CPUs) or erase the calls completely.

For more details on the exact hardware semantics see here **TODO**.
