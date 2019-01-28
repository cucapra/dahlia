---
title: Spatial Experience
---

Spatial provides `ForEach` as a map operator over memory. You can write:

```
Foreach(1 until 16 par 1){i => s(i) = s(i - 1) + 1}
```

which is equivalent to the following in Seashell

```
for (let i = 1..16) unroll 1 {
  s[i] := s[i - 1] + 1;
}
```
Note that this seashell program doesn't typecheck, but is compiled by Spatial.
However, Spatial doesn't stop the programmer from writing:

```
Foreach(1 until 16 par 3){i => s(i) = s(i - 1) + 1}
```

and naively the following when `s` is all `0`:

```
0 1 1 1 2 1 1 2 1 1 2 1 1 2 1 1
```

This shouldn't happen in Seashell. The larger theorem we want is:

::: formula

**Theorem**: Given a program $P$, we create a new program $P_u$ by adding
`unroll`s to it. If $\text{typecheck}(P_u) \implies \text{eval}(P) =
\text{eval}(P_u)$.  Similarly, going from $P_u$ to $P$ doesn't change the final
evalution of the program.

:::
