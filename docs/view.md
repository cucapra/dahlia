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
v_a = view[w=2, off=0] a;
```

![A width 2 view over an array](./img/row-view.png)

This creates a _view_ `v_a` on array `a` that has width (`w`) of 2. The `off`
represents the starting point of the _view_. A _view_ expression
also _consumes_ array `a` from the current context which means `a` can no
longer be accessed in the current scope. The variable `v_a` created from this
view expression simply acts as an array. For example, we can write:

```
for (i = 0..2) {
    v_a[i] := 10
}
```

However, the _view_ in this example isn't very interesting since it remains
static. In order to specify an iteration strategy using a _view_, we can use
a dynamic `off` value:

```
int [8 bank(2)] a;
for (i = 0..3) {
    v_a = view[w=2, off=i];
}
```

which creates the following iteration pattern:

![](./img/row-view-seq.png)

And again, since `v_a` just acts as a simple array, we can do the following:

```
for (i = 0..3) {
    v_a = view[w=2, off=i];
    for (j = 0..2) {
        v_a[j] := 1;
    }
}
```

While this syntax can express simple block-based iteration strategies, it is
not expressive enough to specify iterations where we skip some elements. We
extend the notation of _views_ to add a _stride_ operator:

```
int[8 bank(2)] a;
v_a = view[w=2, s=2, off=0];
```

which creates the following _view_:

![](./img/row-view-stride.png)

When omitted, stride is set to $1$, which creates a block.
