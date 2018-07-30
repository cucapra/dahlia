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
Index types will allow us to make more expressive accesses on higher-dimensional arrays (decoupling matrix logic and banking structure); however, before we dive into a more involved example, it might be a good sanity check to review a simpler example with a one-dimensional array.  Consider an array $\text{a}$ of size 30. We'd like to access $\text{a}$ 5 times in parallel:

    int a[30 bank(5)]
    for i in 0..30 unroll 5
        access a[i]

Here, $\text{i}$ takes on type $\text{idx}\langle 0 .. 5, 0 .. 6 \rangle$. Using our set interpretation of the index type, for each $d \in 0 .. 6$, $\text{i}$ represents:

$$\{ s + |0 .. 5| \times d ~|~ s \in 0..5 \} \rightarrow \{ s + 5 \times d ~|~ s \in 0..5 \}$$

So, for $d=0$, we'd be simultaneously accessing the indices $\{0, \dots, 4 \}$; for $d=1$ we'd have $\{5, \dots, 9 \}$; and so on and so forth until for $d=5$ we'd be finally be accessing $\{25, \dots, 29 \}$.

**Typechecking.** For Seashell, it is of interest to find the banks that accesses use, to restrict illegal operations that access the same bank multiple times. So in this previous example, we can compute the banks that each index $i'$ of the set represented by $i$ access, with $i' \bmod b$ - assuming an interleaved banking style. With a chunked banking style we can simply use $i / b$.

Multi Dimensional Arrays
------------------------

As far as we're concerned, HLS only supports the use of one-dimensional arrays. With Seashell, we'd like to offer some abstractions to make expressing logical computations on multi-dimensional matrices easier. We'll define Seashell $n$-dimensonal arrays like this:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \text{ bank}(b)
$$

Here, $\text{a}$ is the name of our array; the contents following the colon tell us that $t$ is some arbitrary type ($\text{int}$, etc); and $\sigma_i$ represents the size of a dimension $i$. $\text{bank}(b)$ tells us that this array is banked by a factor of $b$: in the future we'd like to impose flexible banking structures on each dimension, but for now, we're going to simply bank the entire array - that is, the entire flattened array, which we'll talk about in just a moment.

Under the hood (that is, when we translate our Seashell program to HLS C), this multi-dimensional array is translated to a one-dimensional array. This flattened array (which we'll call $\text{a}_f)$ has size equal to the product of our Seashell array dimensions:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \equiv \text{a}_f:t[\sigma_{(\prod_{i=0}^{n} \sigma_i)} ]
$$

Testing comments
Line from local setup
$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \text{ bank}(b)
$$
\todo{Here's a comment in the margin!}

Logical accesses to a Seashell multi-dimensonal array look like this: $\text{a}[i_0][i_1]..[i_n]$. We'd like to access these higher-dimensional arrays with our Seashell index types, but to first examine how working with these arrays might work, it would be useful to first consider $i_0..i_n$ as plain old integers. So now, for the purposes of typechecking our array accesses, we'd like to know exactly which indices we're using to access this flattened array when we make our logical accesses. So to compute what our flattened index $i_f$ would be based on our logical indices $i_0..i_n$, we could use the following method: 

$$
i_f = \sum_{k=0}^{n} (i_k \prod_{k'=k+1}^{n} \sigma_k)
$$

The general intuition behind the above formula is that when accessing the $i$th element of dimension $k$, we need to skip over i sections of $\text{a}_f$ that have size equal to the product of the remainder of the $n-k$ logical dimensions. (TODO: more intuition?)

**Example.** Consider a three-dimensional array $\text{a}$ defined like this:

$$a:t[2][5][3] \text{ bank} (5)$$

The flattened version, $\text{a}_f$, would have size $N=30$. Say we make an access $\text{a}[1][4][2]$. Using our formula we defined, we'd access $\text{a}_f$ with $i_f=29$.

**Typechecking.** Because we're using this flattened array, computing which bank is being accessed from $i_f$ can simply be accomplished with $i \bmod b$ or $i / b$, depending on banking structure - just like in the earlier one-dimensional example.

Typechecking Array Accesses
------------------------

In these document, we've been describing methods of determining the banks that array accesses make. Now, we'd like to expand on how this might be of use in the Seashell type system. In general, the problem we're trying to solve is the following: restrict programs such that banks of memories can only be accessed once, to reflect the fact that in actual hardware, these memories have limited access ports. One way we might be able to do this is by tracking a set of indices that are available for use in accessing an array. When an access with a particular index occurs, we mark it unreachable after that point. So, for any array $\text{a}$ in our typing context, associate it with some set $\text{I}$ of unconsumed indices, and when we access some index $i \in \text{I}$, the set of indices associated with $\text{a}$ becomes $\text{a} \setminus i$.


Now, we need a way to determine which accesses are being used and consumed when we use an index type. One way we could accomplish this is by generating every single index that our index types can represent, and then determine every single bank they access, using the methods we've described. However, we can simplify this process with the help of a few simplifying assumptions.

**Assumption 1.** Our index types, as we've defined them, allow for static components to be any integer range $l_s .. h_s$. We can restrict this so that $l_s=0$, which is all we really need for practical purposes: if a loop has an unroll factor $k$, then the static component of an index variable for this loop would certainly just be $0 .. k$. This just makes reasoning about which indices are being accessed a little bit easier.

**Assumption 2.** If we restrict our programs so that we only allow for array accesses where the index type static component length matches the unroll factor, then typechecking becomes much easier. Consider the following example, where $\text{a}$ is an array with length $s$ and banking factor $k$:

    int a[s bank(k)]
    for i in 0..n unroll k
        access a[i

Here, $\text{i}$ has type $\text{idx}\langle 0 .. k, 0 .. \frac{n}{k}\rangle$. For any $d \in 0 .. \frac{n}{k}$, $\text{i}$ represents:

$$
\{ s + k * d ~|~ s \in 0 .. k\}
$$

So, for $d=0$, the indices in use are:

$$
\{ s ~|~ s \in 0 .. k \} \rightarrow 0..k
$$

and for $d=\frac{n}{k}$, the indices would be:

$$
\{ s + k*(\frac{n}{k}-1)  ~|~ s \in 0 .. k\} \rightarrow \{ s + n - k ~|~ s \in 0..k\} \rightarrow (n-k)..n
$$

and indeed for any $d=m$, we'd have a $k$-sized chunk $(0+mk)..(k+mk)$; there would be $\frac{m}{k}$ of these chunks, which are (hopefully) disjoint - so the union of all of these sets would probably be $0..n$. (TODO: actual proof instead of this). 

So, from this we can conclude that when the banking factor matches the unroll factor of an index type, we access every index of a loop. In other words, if an index type has a static component $l_s..h_s$, and it's indexing into some array $\text{a}$ with banking factor $b$, then if $|l_s..h_s|=b$, the type system consumes every index of $\text{a}$, disallowing any further accesses

**Assumption 2, weakened.** We might want to relax this assumption to allow situations where the unroll factor divides into the banking factor.
