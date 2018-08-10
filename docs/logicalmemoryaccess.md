---
title: Logical Memory Accesses
---

In this document, we'll describe how Seashell typechecks logical array accesses using [index types](https://capra.cs.cornell.edu/seashell/docs/indextype.html), and we'll argue that our methods are valid.

Logical access with index types
-------------------------------

Index types allow us to combine static and dynamic information about the indices we're accessing in unrolled loops. For instance, consider the following example: 

    for i in l..h unroll k
        access a[i]

The variable *i* accessing array $\text{a}$ has type, $\text{idx}\langle 0 .. k, \frac{l}{k} .. \frac{h}{k} \rangle$. This type is comprised of a *static component*, $0 .. k$, and a *dynamic component*, $\frac{l}{k} .. \frac{h}{k}$. As described in the [semantics of index types](https://capra.cs.cornell.edu/seashell/docs/indextype.html#syntax-semantics), a value of an index type corresponds to a dynamic number $d \in \frac{l}{k}..\frac{h}{k}$. For any such $d$ type idx represents the set:

$$\tag{1}
\{ s + |0..k| \times d ~|~ s \in 0..k \}
$$

**Usage.** For Seashell, it's important to know exactly which indices are being used given a particular array access, which may be inside an unrolled loop. Seashell's typechecker uses index types to determine these indices. Seashell allows for two styles of array accesses: *implicit* and *explicit*. For the latter, which do not appear inside unrolled loops, the programmer specifies a statically known bank number and a potentially dynamic index offset into that bank. An index type representation of such an access would have trivial single-value static and dynamic components (rather than ranges), as such an access represents only a single value. Therefore, for logical accesses we're concerned with the index types used to represent the indices involved in *implicit* accesses.  

Logical Multi-Dimensional Arrays
--------------------------------

As far as we're concerned, hardware we are dealing with (especially FPGAs) have physically one-dimensional arrays. Thereby, HLS only supports the use of one-dimensional arrays. With Seashell, we'd like to offer some abstractions to make expressing logical computations on multi-dimensional matrices easier. We'll define Seashell $n$-dimensonal arrays like this:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \text{ bank}(b)
$$

Here, $\text{a}$ is the name of our array; the contents following the colon tell us that $t$ is some arbitrary type ($\text{int}$, etc) for the array elements; and $\sigma_i$ represents the size of a dimension $i$. $\text{bank}(b)$ tells us that this array is banked by a factor of $b$: in the future we'd like to impose flexible banking structures on each dimension, but for now, we're going to simply bank the entire array - that is, the entire flattened array, which we'll talk about next.

Under the hood (that is, when we translate our Seashell program to HLS C), this multi-dimensional array is translated to a one-dimensional array. This flattened array (which we'll call $\text{a}_f)$ has size equal to the product of our Seashell array dimensions:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \text{ bank}(b) \equiv \text{a}_f:t[{(\prod_{i=0}^{n} \sigma_i)} ] \text{ bank}(b)
$$

Logical accesses to a Seashell multi-dimensonal array look like this: $\text{a}[i_0][i_1]..[i_n]$. We'd like to access these higher-dimensional arrays with our Seashell index types, but to first examine how working with these arrays might work, it would be useful to first consider $i_0..i_n$ as plain old integers. To compute what our flattened index $i_f$ would be based on our logical indices $i_0..i_n$, we could use the following method: 

$$\tag{2}
i_f = \sum_{k=0}^{n} \left[i_k * \left(\prod_{k'=k+1}^{n} \sigma_{k'} \right) \right]
$$

**Example 1** Consider a two-dimensional array $\text{a}$ defined like this:

$$a:t[4][2] \text{ bank} (4)$$

The flattened version, $\text{a}_f$, would have size $N=8$. Say we make an access $\text{a}[3][1]$. Using our formula we defined, we'd access $\text{a}_f$ with $i_f=7$.

Note that the banking factor is not used in logical access.

Multi-dimensional logical Access with Index Types
-------------------------------------------------

Now, we'd like to talk about which indices are represented when we make a logical array access using our index types. Consider an access to array $a$:

    for i in 0..L unroll x
      for j in 0..M unroll y
        for k in 0..N unroll z
          access a[i][j][k]

To generalize, we can represent such an access as:
$$ \text{a}[i_0]..[i_n]$$

Here, $i_0$..$i_n$ are index types, where index $i_x$ is $\text{idx}\langle 0 .. k_x, l_x .. h_x  \rangle$. Following our discussion with one-dimensional accesses, value each for all of these index type variables correspond to a set of dynamic numbers $\{\forall x \in 0 .. n, d_x \in l_x .. h_x\}$. We can derive the set type indices would represent for any such set of $d$s, substituting $i_k$ in Equation 2 with Equation 1:

$$\tag{3}
I_f = \left\{ \sum_{x=0}^{n} \left[ (s_x + |0..k_x| \times d_x) * \left( \prod_{x'=x+1}^{n}{\sigma_{x'}} \right) \right] ~|~ \forall x \in 0..n, s_x \in 0..k_x \right\}
$$

To put this into words, our index type can formalize the set of elements accessed when using multi-dimensional access into an array.  

**Example 2.** Consider this program:

    int a[4][2] bank(4)

    for i in 0..4 unroll 2
        for j in 0..2 unroll 2
            access a[i][j]

The types of $i$ and $j$ would then be:

 - $i : \text{idx}\langle 0 .. 2, 0 .. 2 \rangle$
 - $j : \text{idx}\langle 0 .. 2, 0 .. 1 \rangle$


With these index type indices we can compute the elements of $I_f$. We do this by computing the following, $\forall$ $d_0 \in 0 .. 2$, $d_1 \in 0 .. 1$:

$$
\{(s_0 + |0..2|*d_0)*\sigma_1 + (s_1 + |0..2|*d_1)*1 ~|~ \forall s_0 \in 0 .. 2, s_1 \in 0 .. 2 \}
$$

where $$ \sigma_1 = 2 $$

For a pair of $\langle d_0,d_1 \rangle = \langle 1,0 \rangle$:

  - $(0+2*1)*2 + (0+2*0)=4$
  - $(0+2*1)*2 + (1+2*0)=5$
  - $(1+2*1)*2 + (0+2*0)=6$
  - $(1+2*1)*2 + (1+2*0)=7$

Note that the banks should be arranged with an interleaved strategy. Alternatively, if we unroll the [outer loop by 4](https://capra.cs.cornell.edu/seashell/docs/appendix.html#example-2.2) then the memory arrangement should be block-wise. We will discuss further about banking in the next section. A 3-D array example is also provided in the [appendix](https://capra.cs.cornell.edu/seashell/docs/appendix.html#d-array-examples-to-visualize-multi-dimensional-access).

Bank access with index types
----------------------------

Now that we have established what a logical array access in index types would represent, we can use it to formally represent unrolling. However, in order to create interesting type checking rules to prevent unsafe memory accesses, we need to identify which bank/ banks each access would attempt to reach. This discussion would use common approaches for memory banking, namely interleaving and block-wise strategies. To read more about banking strategies [here](https://capra.cs.cornell.edu/seashell/docs/appendix.html#array-banking-strategies-with-2-d-example).  

We will mainly proceed with the interleaving strategy, as that would allow consecutive elements to be accessed in parallel. Since our index types could provide us with the logical one dimensional integer index $I_f$, we can use the same strategy as intuitively figuring out the bank.  
  - with an interleaving strategy, we can use $i_f \bmod b$   
  - with a chunking strategy, we can use $i_f / b$  

Typechecking Array Accesses
---------------------------

We've been describing the method of determining which banks an array accesse would make. Now, we'd like to expand on how this might be of use in the Seashell type system. In general, the problem we're trying to solve is the following: restrict programs such that banks of memories can only be accessed once, to reflect the fact that in actual hardware, these memories have limited access ports. One way we might be able to do this is by tracking a set of banks that are available for use in accessing an array. When an access with a particular bank occurs, we mark the bank unreachable after that point. So, for any array $\text{a}$ in our typing context, associate it with some set $B$ of unconsumed banks, and when we access some bank $b \in B$, the set of banks associated with $B$ becomes $B \setminus b$.  

Now, we need a way to determine which accesses are being used and consumed when we use an index type. One way we could accomplish this is by generating every single index that our index types can represent, and then determine every single bank they access, using the methods we've described. However, we'd like to simplify this process. We'd like to come up with some simpler type rules that enforce memory access safety.

### Type Rules for One-Dimensional Access
We'll make the assumption that the static component of an index type will start at $0$, that is, any static component will be some range $0 .. k$. This will make it easier to reason about the indices we're accessing in a real seashell example. However, the generality should hold to our index type definition.   

#### Type Rule 1
For any one-dimensional array access into array $\text{a}$ with index type $i$, we will only consider it valid if the size of the static component of $i$ divides into the banking factor of $\text{a}$.

#### Type Rule 2
Allow only one legal usage of index type $i$ to access $\text{a}$, if rule (1) holds. That is, if we have some access $\text{a}[i]$ in a program, then after that point in the program we cannot access $\text{a}$ again.

It is evident from these rules, that our type checker is conservative about the design space of safe accesses. Our attempt through this document is to formally prove our accesses are safe, and not derive the complete design space of safe accesses. We hope to convince our index types are rigorous and flexible enough to attempt proofs with other type rules, which would make the type checker gradually less conservative.  

We'd now like to demonstrate some proofs that show our rules only allow valid array accesses. Consider the following cases:  

**banked array access with unroll factor = banking factor**

where $l > n$   

    int a[l bank(k)]
    for i in 0..n unroll k
        access a[i]

Here, $i$ has type $\text{idx}\langle 0 .. k, 0 .. \frac{n}{k}\rangle$. For any $d \in 0 .. \frac{n}{k}$, $\text{i}$ represents:

$$
\{ s + k * d ~|~ s \in 0 .. k \}
$$

We mentioned earlier that assuming an interleaved banking style, we can determine the bank an index accesses with $i \bmod b$, where $i$ is some integer and $b$ is the number of banks. So the banks that the set above accesses would be:

$$
\{ s + k * d ~|~ s \in 0 .. k \} \bmod k
$$

Using the following expansion with modulus:
$(a + b) \bmod c =  (a \bmod c + b \bmod c) \bmod c$

$$
\{ ((s \bmod k) + (k*d \bmod k)) \bmod k ~|~ s \in 0 .. k \}
$$

$$
\{ s \bmod k ~|~ s \in 0 .. k \}
$$

We know the following about $s$,

  - $s < k$

which helps us express this set as a range:

$$
\{ 0 .. k \}
$$

This range shows us that our index type would be accessing all $k$ distinct banks by any access, and we'd never access some non-existent bank or not access an existent bank. Allowing accesses that follow rule (1) would guarantee this access. Rule(2) would disallow any further accesses, preventing banks from being accessed multiple times.  

**banked array access with unroll factor factor of banking factor**

where $l > n$ and $m \in N$  

    int a[l bank(m*k)]
    for i in 0..n unroll k
        access a[i]

Here, $i$ has type $\text{idx}\langle 0 .. k, 0 .. \frac{n}{k}\rangle$. For any $d \in 0 .. \frac{n}{k}$, $\text{i}$ represents:

$$
\{ s + k * d ~|~ s \in 0 .. k \}
$$

We mentioned earlier that assuming an interleaved banking style, we can determine the bank an index accesses with $i \bmod b$, where $i$ is some integer and $b$ is the number of banks. So the banks that the set above accesses would be:

$$
\{ ((s \bmod m*k) + (k*d \bmod m*k)) \bmod m*k ~|~ s \in 0 .. k \}
$$

We know a couple things that help us rewrite this set:

  - $s < mk$
  - $kd \bmod mk = k (d \bmod m)$ [proof](https://capra.cs.cornell.edu/seashell/docs/appendix.html#modulus-proof), [image](https://imgur.com/a/9cEQHGr)

So then, we have:

$$
\{ s + k (d \bmod m) ~|~ s \in 0 .. k \}
$$

We can then express this as a range:

$$
k (d \bmod m) .. \left( k (d \bmod m) + k \right)
$$

This range shows us that our index type would be accessing $k$ distinct banks at any time, and we'd never access some non-existent bank. For $d=0$ we'd access $0..k$, and for $d=(m-1)$ we'd access $(mk-k)..mk$. Allowing accesses that follow rule (1) would guarantee this. Then, because we can't know what $d$ is statically, we can follow rule (2) and conservatively disallow any further accesses, preventing banks from being accessed multiple times.  

**banked array access with an unroll factor of 1**

where $l > n$ and $k > 1$  

    int a[l bank(k)]
    for i in 0..n unroll 1
        access a[i]

This is clearly a special case of the case above. Here, $i$ has type $\text{idx}\langle 0 .. 1, 0 .. k \rangle$. For any $d \in 0 .. k$, $\text{i}$ represents:

$$
\{ s + 1 * d ~|~ s \in 0 .. 1 \}
$$

We can determine the banks we access from:


$$
\{ ((s \bmod k) + (d \bmod k)) \bmod k ~|~ s \in 0 .. 1 \}
$$

Since we know that $s$ can take only the value $0$  

We have:

$$
\{ d \bmod k \}
$$

This shows us that our index type would be accessing a single bank at any time, and we'd never access some non-existent bank. For $d=0$ we'd access $0$, and for $d=k-1$ we'd access $k-1$. Allowing accesses that follow rule (1) would allow this. We can follow rule (2) and conservatively disallow any further accesses, preventing banks from being accessed multiple times. But this also limits accessing other banks, which would of course be a safe operation.  

**Note- ** We're operating on a 1-D array, so this set already represents our "flattened indices" for multi-dimensional access. In fact, we can argue 1-D as a special case of the $I_f$ definition we provided earlier. We will proceed in the next section to arrive at primitive access rules for multi-dimensional access, and gradually improve on them.  

### Type Rules for Multi-Dimensional Access

Now we'd like to describe some type rules for when we use multiple index types to access an array.

**Type Rule 1.** The product of the unroll factors influencing the types $i_0..i_n$ must equal the banking factor of the array they're accessing.


    int a[x][y] bank(b1*b2)

    for i in range 0..x unroll b1
        for j in range 0..y unroll b2
	    access[i][j]


Intuitively, we unroll this loop $b=b_1*b_2$ times, so we're simultaneously accessing $b$ values, and there better be $b$ banks we can access. Our index types are the following types:

 - $\text{idx}\langle 0 .. b_1, 0 .. \frac{x}{b1} \rangle$
 - $\text{idx}\langle 0 .. b_2, 0 .. \frac{y}{b2} \rangle$

Then we can compute $I_f$ for all $d\in 0 .. \frac{x}{b}$, 

$$ I_f = \left\{ \sum_{x=0}^{n} \left[ (s_x + |0..k_x| \times d_x) * \left( \prod_{x'=x+1}^{n}{\sigma_{x'}} \right) \right] ~|~ s_0 \in 0..b_0, s_1 \in 0.. b_1 \right\} $$

The banks accessed would be:

