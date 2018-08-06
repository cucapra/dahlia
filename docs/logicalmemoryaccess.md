---
title: Logical Memory Accesses
---

In this document, we'll describe how Seashell typechecks logical array accesses using [index types](https://capra.cs.cornell.edu/seashell/docs/indextype.html), and we'll argue that our methods are valid.

Index Type Recap
----------------------

Index types allow us to combine static and dynamic information about the indices we're accessing in unrolled loops. For instance, consider the following example: 

    for i in l..h unroll k
        access a[i]

The variable $\text{i}$ accessing array $\text{a}$ has type $\text{idx}\langle 0 .. k, \frac{l}{k} .. \frac{h}{k} \rangle$. This type is comprised of a *static component*, $0 .. k$, and a *dynamic component*, $\frac{l}{k} .. \frac{h}{k}$. The set representation of this index type is:

$$
\{ s + |0..k| \times d ~|~ s \in 0..k, d \in \frac{l}{k}..\frac{h}{k}\}
$$

**Usage.** For Seashell, it's important to know exactly which indices are being used given a particular array access, which may be inside an unrolled loop. Seashell's typechecker uses index types to determine these indices. Seashell allows for two styles of array accesses: *implicit* and *explicit*. For the latter, which do not appear inside unrolled loops, the programmer specifies a statically known bank number and a potentially dynamic index offset into that bank. An index type representation of such an access would have trivial single-value static and dynamic components (rather than ranges), as such an access represents only a single value.

For the purposes of this document, we're concerned with the index types used to represent the indices involved in *implicit* accesses.  

(I feel it might be useful to show this in the mathy sense and how the type checker can ensure safe access)  

Logical Multi-Dimensional Arrays
------------------------

As far as we're concerned, HLS only supports the use of one-dimensional arrays. (Maybe worthwhile to say FPGAs have (and most memory architectures) one dimensional arrays.) With Seashell, we'd like to offer some abstractions to make expressing logical computations on multi-dimensional matrices easier. We'll define Seashell $n$-dimensonal arrays like this:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \text{ bank}(b)
$$

Here, $\text{a}$ is the name of our array; the contents following the colon tell us that $t$ is some arbitrary type ($\text{int}$, etc) for the array elements; and $\sigma_i$ represents the size of a dimension $i$. $\text{bank}(b)$ tells us that this array is banked by a factor of $b$: in the future we'd like to impose flexible banking structures on each dimension, but for now, we're going to simply bank the entire array - that is, the entire flattened array, which we'll talk about in just a moment.

Under the hood (that is, when we translate our Seashell program to HLS C), this multi-dimensional array is translated to a one-dimensional array. This flattened array (which we'll call $\text{a}_f)$ has size equal to the product of our Seashell array dimensions:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \text{ bank}(b) \equiv \text{a}_f:t[{(\prod_{i=0}^{n} \sigma_i)} ] \text{ bank}(b)
$$

Logical accesses to a Seashell multi-dimensonal array look like this: $\text{a}[i_0][i_1]..[i_n]$. We'd like to access these higher-dimensional arrays with our Seashell index types, but to first examine how working with these arrays might work, it would be useful to first consider $i_0..i_n$ as plain old integers. To compute what our flattened index $i_f$ would be based on our logical indices $i_0..i_n$, we could use the following method: 

$$
i_f = \sum_{k=0}^{n} \left[i_k * \left(\prod_{k'=k+1}^{n} \sigma_{k'} \right) \right]
$$

**Example.** Consider a three-dimensional array $\text{a}$ defined like this:

$$a:t[2][5][3] \text{ bank} (5)$$

The flattened version, $\text{a}_f$, would have size $N=30$. Say we make an access $\text{a}[1][4][2]$. Using our formula we defined, we'd access $\text{a}_f$ with $i_f=29$.

Logical Access with Index Types
------------------------

Now, we'd like to talk about which indices are represented when we make a logical array access using our index types. Consider an access to array $a$ (that we defined earlier in this document):

$$ \text{a}[i_0]..[i_n]$$

Here, $i_0$..$i_n$ are index types, where index $i_x$ is $\text{idx}\langle 0 .. k_x, l_x .. h_x  \rangle$. We'd like to determine the set of flattened indices $I_f$. Define $\tau(i)$ to be the static component of index type $i$, and $\delta(i)$ to be the dynamic component of $i$. Then we can write this set as:

(I think it's useful to highlight we substitute $i$ in the above equation. Have numbers on the prior equations and show this?)  

$$ I_f = \left\{ \sum_{x=0}^{n} \left[ (s_x + |0 .. k_x| \times d_x) * \left( \prod_{x'=x+1}^{n}{\sigma_{x'}} \right) \right] ~|~ s_x \in \tau(i_x), d_x \in \delta(i_x) \right\} $$

It's a bit of a headache to look at, but basically it's capturing the idea of taking every set of literal index integers $a_0..a_n$ that are represented by our index types, and then applying our index-flattening formula, and then taking the union of each of these results to produce one set of flattened indices.

**Example.** Consider this program:

    int a[2][2] bank(4)

    for x in 0..2 unroll 2
        for y in 0..2 unroll 2
            access a[x][y]

Here are the types of $x$ and $y$:

 - $x : \text{idx}\langle 0 .. 2, 0 .. 1 \rangle$
 - $y : \text{idx}\langle 0 .. 2, 0 .. 1 \rangle$

So we can compute $I_f$ like this:

$$
\{ (0*2 + 0*1), (0*2 + 1*1), (1*2 + 0*1), (1*2 + 1*1) \} = \{ 0, 1, 2, 3 \}
$$

(I prefer the uneven sized dimension example because it's clear, and also feel the implicit access expression is important to highlight)

Array Banking Strategies
------------------------

TODO: make more involved example in previous section, then use same array in these banking examples.

We are interested in the indices being used to access $\text{a}_f$, so we can restrict the banks that a Seashell programmer can access. However, which banks the programmer accesses is influenced by the array banking strategy. Here are a few ways we could bank $\text{a}_f$. 

**Bank Interleaving.** We could interleave the elements of $\text{a}_f$ among its banks, like this (each rectangle represents a bank):

(I want to introduce the term cyclic too, which might be widely used)  

| 0 5 10 15 20 25 | 1 6 11 16 21 26 | 2 7 12 17 22 27 | 3 8 13 18 23 28 | 4 9 14 19 24 29 | 
| --- | --- | --- | --- | --- |

Then, given an index $i_f$ into $\text{a}_f$, we could determine the bank being accessed with $i_f \bmod b$. The index offset into the bank would be $i_f / b$.

**Bank Chunking.** We could simply divide $a_f$ into banks, like this:  

(I want to introuce the term block too, which is probably the widely used term)  

| 0 1 2 3 4 5 | 6 7 8 9 10 11 | 12 13 14 15 16 17 | 18 19 20 21 22 23 | 24 25 26 27 28 29 |  
| --- | --- | --- | --- | --- |  

Then, we could use $i_f / b$ to find the relevant bank, and $i_f \bmod b$ to find the index within the bank. 

Both have use cases, but we won't go into those here. For the remainder of this document we'll assume we're using bank interleaving, but we could have similar results with chunking.

Typechecking Array Accesses
------------------------

In these document, we've been describing methods of determining the banks that array accesses make. Now, we'd like to expand on how this might be of use in the Seashell type system. In general, the problem we're trying to solve is the following: restrict programs such that banks of memories can only be accessed once, to reflect the fact that in actual hardware, these memories have limited access ports. One way we might be able to do this is by tracking a set of banks that are available for use in accessing an array. When an access with a particular bank occurs, we mark the bank unreachable after that point. So, for any array $\text{a}$ in our typing context, associate it with some set $B$ of unconsumed banks, and when we access some bank $b \in B$, the set of banks associated with $B$ becomes $B \setminus b$.  

Now, we need a way to determine which accesses are being used and consumed when we use an index type. One way we could accomplish this is by generating every single index that our index types can represent, and then determine every single bank they access, using the methods we've described. However, we'd like to simplify this process. We'd like to come up with some simpler type rules that enforce memory access safety.

### Type Rules for One-Dimensional Access
First, we'll make the assumption that any static component of an index type will start at $0$, that is, any static component will be some range $0 .. k$. This will make reasoning about the indices we're accessing a little bit easier. 

**Type Rule 1.** For any one-dimensional array access into array $\text{a}$ with index type $i$, we will only consider it valid if the size of the static component of $i$ divides into the banking factor of $\text{a}$.

**Type Rule 2.** Allow only one legal usage of index type $i$ to access $\text{a}$, if rule (1) holds.  

(I don't quite follow rule 2)

We'd now like to discuss some arguments that show our rules only allow valid array accesses. Consider the following example:

    int a[s bank(m*k)]
    for i in 0..n unroll k
        access a[i]

Here, $\text{i}$ has type $\text{idx}\langle 0 .. k, 0 .. \frac{n}{k}\rangle$. For any $d \in 0 .. \frac{n}{k}$, $\text{i}$ represents:

$$
\{ s + k * d ~|~ s \in 0 .. k \}
$$

Note that we're operating on a 1-D array, so this set already represents our "flattened indices". This is a special case of the $I_f$ definition we provided earlier.  

(It's special because 1-D logical and the flattened representation are equivalent?)  

We mentioned earlier that assuming an interleaved banking style, we can determine the bank an index accesses with $i \bmod b$, where $i$ is some integer and $b$ is the number of banks. So the banks that the set above accesses would be:

$$
\{ ((s \bmod mk) + (kd \bmod mk)) \bmod mk ~|~ s \in 0 .. k \}
$$

We know a couple things that help us rewrite this set:

  - $s < mk$
  - $kd \bmod mk = k (d \bmod m)$ [[proof](https://imgur.com/a/9cEQHGr)]

So then, we have:

$$
\{ s + k (d \bmod m) ~|~ s \in 0 .. k \}
$$

We can then express this as a range:

$$
k (d \bmod m) .. \left( k (d \bmod m) + k \right)
$$

This range shows us that our index type would be accessing $k$ distinct banks at any time, and we'd never access some non-existent bank. For $d=0$ we'd access $0..k$, and for $d=(m-1)$ we'd access $(mk-k)..mk$. Allowing accesses that follow rule (1) would guarantee this. Then, because we can't know what $d$ is statically, we can follow rule (2) and conservatively disallow any further accesses, preventing banks from being accessed multiple times.  

(still think it's nicer to consider the trivial case of unroll factor = banking factor, especially as it can justify both the rules (if I'm understanding rule 2 correctly))

### Type Rules for Multi-Dimensional Access

The next step is to determine rules for when we access an array $\text{a}$ with multiple index types.

