---
title: Views in Seashell
---

This document presents _Views_ which are Seashell's zero-cost abstraction for
structured iteration. The Seashell type system provides a safe access
guarantee, i.e., a program that typechecks never tries to access the same
memory bank in the same cycle. To provide this guarantee, the type system
needs the programmer to expose the iteration pattern they are using on a
logical array in the program. _Views_ provide a mechanism to expose the iteration
pattern to the type system without having to pay the cost of creating a new memory
bank. _Views_ are a _zero-cost abstraction_ because they only exist in Seashell
and are compiled into simple array accesses.

## Creating views

A _view_, as it's name suggests, is a logical view over a pre-existing array.
To create a _view_ we use the following syntax:

```
int[8 bank(2)] a;
v_a = view[w=2, s=1, off=0] a;
```

![](./img/row-slice.png)