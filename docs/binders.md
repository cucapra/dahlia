---
id: binders
title: Variable Binders
---

Variables in Dahlia can be bound in four ways.

## Let bindings

A `let` binding can be used in three ways:

```
let <iden> = <expr>;
let <iden>: <type> = <expr>;
let <iden>: <type>;
```

The first allows simple variable declarations that automatically infer the
type of the identifer from the expression:

```dahlia
let x = true; // x is boolean
```

The second form simply checks the type of the expression and makes sure it
can be represented as the explicit type.

```dahlia
let x: bit<10> = 1;
```

> The right hand side has the type `bit<1>` which can be zero extended to be
> represented using 10 bits.

The final form allows defining uninitialized variables. Reading from uninitialized
memories returns undefined garbage values stored in the memory.

```dahlia
let x: bit<10>;
let y = x; // returns garbage value stored in memory
```

## Declarations

The `decl` allow declarations without initalization:

```dahlia
decl x: bit<10>
```

However, `decl`s always get compiled into the top level kernel function.
This Dahlia program:

```dahlia
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

```dahlia
def foo(x: bit<10>, y: bit<10>) {
  x + y
}
```

Imported function also bind function in the global scope:

```dahlia
import "printer.h" {
  def print_vector(c: float[4]);
}
```

Import functions are described in detail [here](imports.md).
