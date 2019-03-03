---
id: binders
title: Variable Binders
---

Variables in Fuse can be bound in three ways.

## Let bindings

A `let` binding can be used in two ways:

```
let <iden> = <expr>
let <iden>: <type> = <expr>
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

The right hand side has the type `bit<1>` which can be zero extended to be
represented using 10 bits.

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

For more details on the exact hardware semantics see here **TODO**.
