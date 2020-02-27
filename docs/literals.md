---
id: literals
title: Literals
---

The set of literals in Dahlia largely overlaps with those in C with some
conveniences.

## Numerical constants

Simple unsigned numbers can be defined using:

- Decimal notation: `10, 20, 0`
- Octal notation: `070, 010`
- Hexadecimal notation: `0x16, 0xdfa`

Signed numbers are written with the unary operator `-`.

## Floating point constants

Floating point numbers are defined using C syntax:

```C
1.0
0.123
```

## Booleans

Unlike C, Dahlia treats booleans differently from numbers.

```
true
false
```

Most C/C++ backends will simply compile these down to `1` and `0` respectively.
