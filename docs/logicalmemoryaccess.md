---
title: Logical Memory Accesses
---

[it]: indextype.html

Conventional HLS tools limit the usage of mutlidimensional arrays and usually
require programmers to map a logical mutlidimensional arrays onto a single
dimensional array. Seashell allows programmers to specify banking structures
of mutlidimensional arrays and use them with ease. This document outlines
the syntax of multidimensional arrays and how maps multidimensional banked
arrays onto a single dimensional array.

There are two ways we refer to arrays in this document:

- A **logical array** is the array abstraction that is exposed to a programming
  in seashell. This can be mutlidimensional and banked.
- A **physical array** is the representation of arrays accepted by the hardware.
  This is single dimensional and can be banked.

Furthermore, this document also proves that following safety theorem about
the Seashell type system:

::: formula
**Theorem.** Given a seashell program that typechecks, all array accesses are
*safe*, i.e., they only access distinct memory banks.
:::

Index Types
-----------

Seashell uses [index types][it] for loop iterators. Index types describe the
set of locations that an index value will access. For example:

    for i in l..h unroll k:
        access a[i]

The variable $i$ has the type $\text{idx}\langle l_s .. h_s, l_d .. h_d
\rangle$. This type consists of a *static component*, $l_s .. h_s$, and a
*dynamic component*, $l_d .. h_d$. According to the semantics of index types,
a value used as an index type is a (dynamic) number $d \in l_d .. h_d$. For
any such $d$, the value represents the following set of locations being accessed:

$$\tag{1}
\{ s + |l_s .. h_s| \times d ~|~ s \in l_s .. h_s \}
$$

Given an array access with index types, this set representation allows us to
precisely identify which indices (and subsequently memory banks) are
accessed.

Mapping Multi-Dimensional Arrays to Hardware
----------------------------------

Hardware memories are inherently one dimensional---they map a linear range of
addresses to values. You can loosely think of banked memories as two
dimensional, where the first dimension is the bank number.

However, high-level languages usually employ logically higher dimensional
arrays to simplify programs. HLS compilers typically only support logically
one-dimensional arrays corresponding to the physical memories, and higher
dimensional arrays are manually rolled out to one dimension. In Seashell,
however, we want to support logically multi-dimensional arrays and map them
to banked memories.

We'll write array declarations like this:

$$
\text{a} : t[\sigma_0][\sigma_1] \dots [\sigma_n]
$$

where $\text{a}$ is the name of the array, $t$ is the type of elements in
$\text{a}$, and $\sigma_i$ is the size of the $i$th dimension.

During compilation, this multi-dimensional array is translated to a
one-dimensional array. The one dimensional array, which we'll call
$\text{a}_f$, has a size equal to the product of our Seashell array's
dimensions:

$$
\text{a}_f : t \left[
    \prod_{i=0}^{n} \sigma_i
\right]
$$

Consider a logical access to an element in our array:

$$\text{a}[i_0][i_1] \dots [i_n]$$

Here, each $i_j$ is a plain old integer, and not an index type.
This logical access corresponds to a physical access in $\text{a}_f$ that we can write as $\text{a}_f[i_f]$, where $i_f$ is defined as:

$$\tag{2}
i_f = \sum_{j=0}^{n} \left[i_j \times \left(\prod_{j'=j+1}^{n} \sigma_{j'} \right) \right]
$$

**Example.**
Consider a two-dimensional array $\text{a}$ defined like this:

$$
\text{a} : t[4][2]
$$

The flattened version, $\text{a}_f$, has size $8$. The logical access to
$\text{a}[3][1]$ corresponds to a physical access $\text{a}_f[3 \times 2 +
1]$ i.e., $\text{a}_f[7]$.

Typing array accesses with Index types
-------------------------------

We now give the semantics of logical accesses using Seashell's index types.
We want to determine the *set* of elements accessed in an expression of the
form $\text{a}[i_0] \dots [i_n]$ where each $i_j$ is of an index type
$\text{idx}\langle l_{s_j} .. h_{s_j}, l_{d_j} .. h_{d_j} \rangle$.

::: todo
Do we need to make this simplifying assumption (that static ranges start at zero) yet? Maybe it doesn't hurt, but it does seem possible to do without it.
If we *do* make the simplification immediately, I propose doing the simplification *first* to Equation 1, where $|0..k|$ can just become $k$.
--A
:::

::: todo
We've attempted to do without the simplification right up to the type rule. Maybe even then it's not needed, but I'm struggling a little to comprehend what it means to have arbitrary boundaries for static and dynamic part.
--S
:::

We can get the set of physical locations for this access by combining
Equation 1, which describes the meaning of index types, with Equation 2,
which describes the logical-to-physical mapping. For a given set of dynamic
values $d_j$ for each index type, we substitute each $i_k$ in Equation 2 with
the indices given in Equation 1:

$$\tag{3}
I_f = \left\{
    \sum_{j=0}^{n} \left[ (s_j + |l_{s_j}..h_{s_j}| \times d_j) * \left( \prod_{j'=j+1}^{n}{\sigma_{j'}} \right) \right]
    \;\middle|\;
    j \in 0..n, s_j \in l_{s_j} .. h_{s_j}
\right\}
$$

**Example.** Consider this program:

    for i in 0..4 unroll 2
        for j in 0..2 unroll 2
            access a[i][j]

The types of $i$ and $j$ are:

$$
\begin{aligned}
i &: \text{idx}\langle 0 .. 2, 0 .. 2 \rangle \\
j &: \text{idx}\langle 0 .. 2, 0 .. 1 \rangle
\end{aligned}
$$

The physical index set $I_f$ for a given
$d_0 \in 0 .. 2$ and $d_1 \in 0 .. 1$ is:

$$
I_f = \{
(s_0 + |0..2| \times d_0)*2 +
(s_1 + |0..2|*d_1)
\mid
s_0 \in 0 .. 2, s_1 \in 0 .. 2
\}
$$

For the loop iteration where $d_0=1$ and $d_1=0$, this set contains these indices:

$$
\begin{aligned}
(0+2*1)*2 + (0+2*0) &= 4 \\
(0+2*1)*2 + (1+2*0) &= 5 \\
(1+2*1)*2 + (0+2*0) &= 6 \\
(1+2*1)*2 + (1+2*0) &= 7 \\
\end{aligned}
$$

A 3-D array example is also provided in the [appendix](https://capra.cs.cornell.edu/seashell/docs/appendix.html#d-array-examples-to-visualize-multi-dimensional-access).

Safety of array accesses
------------------------

To represent the banking of an array $\text{a}$ with banking factor $b$,
we'll extend our array notation:

$$
\text{a}: t[\sigma_0\text{ bank }(b_0)] \dots [\sigma_n\text{ bank }(b_n)]
$$

The notation naturally extends the original array notation to mean that the
$i$th dimension is banked with a banking factor of $b_i$. When mapping
thismapping this logical array to a physical one, we get the following
single-dimensional array:

$$
\text{a}_f : t \left[
    \prod_{i=0}^{n} \sigma_i \text{ bank } \left(
        \prod_{i=0}^n b_i
    \right)
\right]
$$

To reiterate, we are trying to prove the following safety property for Seashell:

::: formula
**Theorem.** Given a seashell program that typechecks, all array accesses are
*safe*, i.e., they only access distinct memory banks.
:::

**Proof.** Given an array with the structure $a[\sigma_1\text{ bank
}(b_1)]\ldots[\sigma_n\text{ bank }(b_n)]$ and array indices $i_1\ldots i_n$,
prove that $a[i_1]\ldots[i_n]$ is safe.

First, assume that for all indices $i$, we have:

$$
    \Gamma \vdash i_j : \text{idx}\langle ls_j..hs_j, ld_j..hd_j \rangle
$$

Now, we're going to use the functions defined in the document on [banking](banking.html)
to construct the set of banks accessed by $a[i_1]\ldots[i_n]$. For any index
$i$, the corresponding term generated by $\mathcal{B}_t$ will be: