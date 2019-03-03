---
id: cheatsheet
title: Cheatsheet
---

The next few sections will cover these in details. Feel free to skip this if
nothing makes sense yet!

## Constants

```
1; // numbers
true; false; // booleans
1.2; // floating point
```

## Let bindings

```C
let x = 1;
// Can optionally provide type
let x: bit<32> = 1;
```

## Declarations

```C
decl a: bit<32>; // number
```

## Arrays

```C
decl arr: bit<32>[10] // 10 elements each of which is bit<10>
decl barr: bit<32>[10 bank 2] // An array with two banks
decl marr: bit<32>[10 bank 2][8 bank 4] // multi-dimensional arrays are supported
```

## Records

### Definitions

```C
// Cannot contain arrays
record point {
  x: bit<32>;
  y: bit<32>;
}
// Can contain other records
record rect {
  lb: point;
  rt: point;
}
```

### Literals

Record literals can only defined in a let binder and need an explicit type.

```C
let p: point = { x = 1; y = 2 }
```

## Binary operations

```C
1 + 2;
a[i] << 1;
```

## Assignment

```C
x := 1;
a[i] := 10;
```

## Conditionals (if)

```C
// Can omit the else
if (x) {
  a[i] := 1
}

if (y) {
  true;
} else {
  false;
}
```

## While loops

```C
while (i > 0) {
  a[i] := 1;
  i := i - 1;
}
```

## For loops

```C
for (let i = 0..10) {
  a[i] := 1;
}
// With unrolling factor
for(let i = 0..10) unroll 2 {
  a[i] := 1;
}
```

## Combine blocks and reductions

```C
let sum = 0;
for (let i = 0..10) unroll 2 {
  let x = a[i];
} combine {
  sum += x; // += is a reductions
}
```

## Sequential composition

```C
a[0] := 1;
---
a[1] := 10;
```

## Functions

Functions cannot return values. They can only modify buffers.

### Definitions

```C

def foo(a: bit<32>[10], b: bit<32>[10]) {
  a[0] := b[0]
}
```

### Application

```C
foo(arr, barr)
```

## Views

Once created, views are accessed transparently as arrays.

### Shrink views

Shrink views must have the same step and width parameters.

```C
decl a: bit<32>[8 bank 4];
view v = shrink a[2 * i : 2];
```

## Program Structure

A fuse program has the following structure:
```C
<func defs or record defs>
<decls>
<commands>
```

Any one of these may be omitted but the order between them must be maintained.
