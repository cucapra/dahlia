---
title: Index Types in Seashell
---
This note expands on the meaning and capabilities of *index types* in Seashell,
which are our way of handling loop unrolling and banked memories by combining
static and dynamic information about sets of indices.

Syntax & Semantics
------------------

We use the notation $l..h$ to mean the interval of numbers
$\{ n \in \mathbb{N} ~|~ l \le n < h \}$ and $|l..h|$ to mean the size of the
set (i.e. $h - l$).

An *index type* consists of two intervals: a static interval and a dynamic interval.
We write type as $\text{idx}\langle l_s .. h_s, l_d .. h_d \rangle$.
A value of an index type consists of a *single* dynamic number $d \in l_d..h_d$.
For any $d$, the value corresponds to a *set* of indices $s + |l_s..h_s| \times d$
for every $s \in l_s..h_s$.  To restate this notion in set notation, a given
dynamic value $d \in l_d..h_d$ for the type $\text{idx}\langle l_s .. h_s, l_d .. h_d \rangle$
represents this set of numbers:

$$\{ s + |l_s..h_s| \times d ~|~ s \in l_s..h_s\}$$

**Example.**
Consider the type
$\text{idx}\langle 0 .. 5, 0 .. 2 \rangle$.
There are two values of this type: $d=0$ and $d=1$.
Each value represents a set of five indices, because $|0..5| = 5$.
The first set, for $d=0$, is
$\{ s + |0..5| \times 0 ~|~ s \in 0..5\} = \{s + 5 \times 0 ~|~ s \in 0..5\} = 0..5$.
The second set, for $d=1$, is
$\{s + 5 \times 1 ~|~ s \in 0..5\} = 6..10$.

**Special cases.**
Index types generalize `int` and `static int`, i.e., the types for ordinary dynamic integers and for static integers.
For example, $\text{idx}\langle 42..43, 0..1 \rangle$ has a single value that represents a single number, 42.
And $\text{idx}\langle 0..1, 0..2^{64} \rangle$ is the type of 64-bit unsigned integers.
So a core language might have *only* index types, where `int` is syntactic sugar for the latter type.
Put differently, index types can describe a fluid amount of static "knowledge" about sets of numbers, including "no static information" and "full static information" and points in between.

**Value space.**
Index types have sets of values, each of which represents a set of indices. So
there's a sort of two-level set organization going on here.  It's worth
considering the *aggregate* set of indices represented by *all* values in a
given index.  For a given type $\text{idx}\langle l_s .. h_s, l_d .. h_d \rangle$,
this set is given by:

$$
\begin{aligned}
&\phantom{=}
\bigcup_{d ~\in~ l_d..h_d}
\{ s + |l_s..h_s| \times d ~|~ s \in l_s..h_s\}
\\
&=
\{ s + |l_s..h_s| \times d ~|~ s \in l_s..h_s, d \in l_d..h_d\}
\end{aligned}
$$

For Loops
---------

Seashell's syntax for unrolled `for` loops looks roughly like this:

    for i in l..h unroll k:
        <body>

The loop executes $\frac{|l..h|}{k}$ times.
In each execution, $i$ represents a set of $k$ indices.
Therefore,
the type of $i$ in the loop body is
$\text{idx}\langle 0 .. k, \frac{l}{k} .. \frac{h}{k} \rangle$.

**Constraints.**
These fractions imply that we must
constrain $k$ to be a factor of $l$, $h$, and, as a consequence, $|l..h|$.
(Intuitively, $l$ and $h$ must be "$k$-aligned.")
That is,
unless we eventually want to deal with partially unrolled iterations, which can get messy.

**Special cases.**
It's worth considering two special cases: no unrolling, where $k=1$, and full
unrolling, where $k=|l..h|$.  In the former, the type degenerates to
$\text{idx}\langle 0 .. 1, l .. h \rangle$, i.e., the type of plain integers in
$l..h$.  In the latter, it degenerates to $\text{idx}\langle 0 .. k, l .. (l+1)
\rangle$, i.e., the *static* type denoting a single set of integers in
$l..(l+k)$.

**Iteration space.**
Consider the value space of $i$, which is:

$$
\begin{aligned}
S_u &= \{ s + |l_s..h_s| \times d \mid s \in l_s..h_s, d \in l_d..h_d\} \\
    &= \left\{ s + k \times d \mid s \in 0..k, d \in \frac{l}{k}..\frac{h}{k}\right\}
\end{aligned}
$$

which is equal to $l..h$.

**Proof.** For unrolling, we want to show that $S_u = l..h$. This ensures that
an unrolled loop accesses exactly the elements that the original loop did. The
proof shows that there are exactly $n = |l..h|$ distinct natural numbers in
$S_u$ with $l$ and $h$ being the lower and upper bounds. Since there are only
$n-1$ natural numbers in $l..h$, this implies that $S_u = l..h$.

$$
\begin{aligned}
S_u &= \left\{ s + k \times d \mid s \in 0..k, d \in \frac{l}{k}..\frac{h}{k}\right\} \\
    &= \bigcup_{d \in \frac{l}{k}..\frac{h}{k}}\{ s + k \times d \mid s \in 0..k \}
\end{aligned}
$$

Let each of the smaller subsets be $S_i$ where $i$ is equal to the value of
$d$.  Therefore, we have $S_u = \cup S_i$. Next, we show that each $S_i$ has
exactly $k$ elements and that $\max(S_i) \leq min(S_{i+1})$. The first
assertion is trivially true since $s$ ranges from $0$ to $k-1$. For the second
assertion, we have

$$
\begin{aligned}
\max(S_i) &= k - 1 + k \times i = k \times (i + 1) - 1 \\
\min(S_{i+1}) &= 0 + k \times (i + 1) = k \times (i + 1)
\end{aligned}
$$

Since $k$ and $i$ are both positive natural numbers, $\max(S_i) < \min(S_{i+1})$.
Therefore, the union of $S_i$ has $(\frac{h}{k} - \frac{l}{k}) \times k =
|l..h|$ elements. (This also implies that each set is disjoint.)

Finally, we show that $0$ and $n-1$ form the lower and upper bounds. Since $s$,
$k$, and $d$ are positive, the $\min(S_u) = 0 + k \times \frac{l}{k} = l$ and
$\max(S_u) = k + k \times \frac{h - k}{k} = h$. Therefore, $S_u = l..h$.

**Multidimensional accesses**. This reasoning can be easily extended to
multiple dimensions. The expression $s + k \times d$ repeates for each
dimension's configration.  The elements of $S_u$ can be represented as a
n-tuple. Since each dimension has a separate and independent component in the
tuple, the reasoning above can be applied to each element.

Operations
----------

We can support arithmetic operations on index types.
Consider variables $i$ of type
$\text{idx}\langle l_s .. h_s, l_d .. h_d \rangle$
and $i'$ of type
$\text{idx}\langle l_s' .. h_s', l_d' .. h_d' \rangle$.
We want to determine the type of expressions involving these variables, such as $i + i'$, such that the type of the result has values that represent the sum of values of the operands.
(This notion could be made more formal.)

For example, we would intuitively like to support addition by a constant, so that $i + 5$
has type
$\text{idx}\langle (l_s + 5) .. (h_s + 5), l_d .. h_d \rangle$.
Each value of this type still represents $|l_s..h_s| = |(l_s+5)..(h_s+5)|$ distinct indices, just beginning at a different dynamic location.
The dynamic value of this type is a "no-op": at run time, no addition needs to occur to get the appropriate value of $i+5$.
(To see this, observe that the dynamic component of the $i+5$ value is identical to the dynamic component of $i$; only the static component has changed which can be computed at compile time.)

We would like to come up with a general strategy for determining the type of expressions like $i+i'$ and $i \times i'$, given constraints on the types.
This is forthcoming.
