---
title: Logical Accesses to Multidimensional Arrays in Banked Memories
---
Index Type Recap
----------------------

[Index types](https://capra.cs.cornell.edu/seashell/docs/indextype.html) allow us to combine static and dynamic information about the indices we're accessing in unrolled loops. For instance, consider the following example: 

    for i in l..h unroll k
        access a[i]

The variable $\text{i}$ accessing array $\text{a}$ has type $\text{idx}\langle 0 .. k, \frac{l}{k} .. \frac{h}{k} \rangle$. This type is comprised of a *static component*, $0 .. k$, and a *dynamic component*, $\frac{l}{k} .. \frac{h}{k}$. This means that for every dynamic value $d$ that $\text{i}$ takes on, $\text{i}$ represents the following set of indices:

$$
\{ s + |l_s..h_s| \times d ~|~ s \in 0..k, d \in \frac{l}{k}..\frac{h}{k}\}
$$

**Example.**
Index types will allow us to make more expressive accesses on higher-dimensional arrays (decoupling matrix logic and banking structure); however, before we dive into a more involved example, it might be a good sanity check to try out a simpler example on a one-dimensional array.  Consider an array $\text{a}$ of size 30. We'd like to access $\text{a}$ 5 times in parallel:

    int a[30 bank(5)]
    for i in 0..30 unroll 5
        access a[i]

Here, $\text{i}$ takes on type $\text{idx}\langle 0 .. 5, 0 .. 6 \rangle$. Using our set interpretation of the index type, for each $d \in 0 .. 6$, $\text{i}$ represents:

$$\{ s + |0 .. 5| \times d ~|~ s \in 0..5 \} \rightarrow \{ s + 5 \times d ~|~ s \in 0..5 \}$$

So, for $d=0$, we'd be simultaneously accessing the indices $\{0, \dots, 4 \}$; for $d=1$ we'd have $\{5, \dots, 9 \}$; and so on and so forth until for $d=5$ we'd be finally be accessing $\{25, \dots, 29 \}$.

Multi dimensional arrays
------------------------

Let's consider a multi dimensional array,
$a:t[\sigma_0][\sigma_1]..[\sigma_n] bank b$
where $i$ would refer to index variable for a dimension
$\sigma$ refers to the size of a dimension(starting from $0$ this would be the range of $i$)
We would use the notation $a[i_0][i_1]..[i_n]$ to access an element in this array.

We can unpack such a multidimensional array into a single dimensional array,
$a:t[\sigma_0][\sigma_1]..[\sigma_n] \equiv a':t[\sigma_N]$

given the above we can derive the expression
$$ i' = \sum_{k=0}^{n} (i_k \prod_{k'=k+1}^{n} \sigma_k')$$

**Example** 

$$a:t[2][5][3]$$
$$a':t[30] \text{ bank} (5)$$

$$a[1][4][2] = a'[29]$$

in order to check which bank and offset this element occupies, we can use the same approach as in a single dimension,

$$ \text{bank} \rightarrow i' \bmod b $$
$$ \text{offset} \rightarrow i' / b $$

As an aside, this assumes interleaved memory banking (cyclic partitioning). Block partitioning (chunking) would simply require us to swap these,
i.e.

$$ \text{bank} \rightarrow i'/b $$
$$ \text{offset} \rightarrow i' \bmod b$$

**Example**

$$ \text{bank} \rightarrow 29 \bmod 5 = 4 $$
$$ \text{offset} \rightarrow 29 / 5 = 5$$


