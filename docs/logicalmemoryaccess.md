---
title: Logical Memory Accesses
---

This document describes Seashell's *logical access* expressions, which are a way of accessing memories using [index types][it].
The idea is to let programs access the logical structure of an array, even a multidimensional array, while preserving information about the underlying physical banks being accessed.
The key result is a set of sound typing rules that determine which banks in a memory are accessed when using a logical access expression---which determines how our type system "consumes" banks from the typing context.

[it]: indextype.html


Logical Access with Index Types
-------------------------------

Seashell uses [index types][it] for loop induction variables.
Index types describe what we know statically about the set of locations that an index value will access.
In a loop like this, for example:

    for i in l..h unroll k:
        access a[i]

The variable $i$ has the type
$\text{idx}\langle 0 .. k, \frac{l}{k} .. \frac{h}{k} \rangle$.
This type consists of a *static component*, $0 .. k$, and a *dynamic component*, $\frac{l}{k} .. \frac{h}{k}$.
According to the [semantics of index types](https://capra.cs.cornell.edu/seashell/docs/indextype.html#syntax-semantics), a value of an index type is a (dynamic) number $d \in \frac{l}{k}..\frac{h}{k}$.
For any such $d$, the value represents this set of locations:

$$\tag{1}
\{ s + |0..k| \times d ~|~ s \in 0..k \}
$$

Given an array access with our index types, this set representation allows us to precisely identify which indices (and subsequently memory banks) are accessed. This is important for the Seashell typechecker, which needs this information to enforce memory access safety.

::: todo
How does the implicit vs. explicit terminology differ from logical vs. physical? It sounds like it might be the same thing.
And here, I'm not sure we need a discussion of physical accesses---unless we're going to go into more detail about how physical accesses work, we can stick to the logical view (and make the above paragraph shorter).
--A
:::


Accessing Multi-Dimensional Arrays
----------------------------------

Hardware memories are inherently one dimensional---they map a linear range of addresses to values.
You can loosely think of banked memories as two dimensional, where the first dimension is the bank number.

However, high-level languages usually employ logically higher dimensional arrays to simplify programs. HLS compilers typically only support logically one-dimensional arrays corresponding to the physical memories, and higher dimensional arrays are manually rolled out to one dimension. In Seashell, however, we want to support logically multi-dimensional arrays and map them to banked memories.

We'll write array declarations like this:

$$
\text{a} : t[\sigma_0][\sigma_1] \dots [\sigma_n]
$$

where $\text{a}$ is the name of the array, $t$ is the element type (`int`, for example), and each $\sigma_i$ is the size of a dimension.

Under the hood (i.e. when we translate our Seashell program to HLS C), this multi-dimensional array is translated to a one-dimensional array.
The flattened array, which we'll call $\text{a}_f$, has a size equal to the product of our Seashell array's dimensions:

$$
\text{a}_f : t \left[
    \prod_{i=0}^{n} \sigma_i
\right]
$$

::: todo
I removed the $\equiv$ here---I think it's a little more readable if we just show the definition of $\text{a}_f$. (If the reader wants the type of $\text{a}$, they can just look upward a couple of lines. :)
--A
:::

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

The flattened version, $\text{a}_f$, has size $8$.
The logical access to $\text{a}[3][1]$ corresponds to a physical access $\text{a}_f[3 \times 2 + 1]$ or $\text{a}_f[7]$.

::: todo
Because banking factors aren't relevant for Equation 2, which is the main point of this section, perhaps they shouldn't be introduced here?
--A
:::


Logical Access with Index Types
-------------------------------

We now consider the meaning of logical accesses using Seashell's index types.
We want to determine the *set* of elements accessed in an expression of the form 
$\text{a}[i_0] \dots [i_n]$
where each $i_j$ is of an index type
$\text{idx}\langle l_{s_j} .. h_{s_j}, l_{d_j} .. h_{d_j} \rangle$.

::: todo
Do we need to make this simplifying assumption (that static ranges start at zero) yet? Maybe it doesn't hurt, but it does seem possible to do without it.
If we *do* make the simplification immediately, I propose doing the simplification *first* to Equation 1, where $|0..k|$ can just become $k$.
--A
:::

We can get the set of physical locations for this access by combining Equation 1, which describes the meaning of index types, with Equation 2, which describes the logical-to-physical mapping.
For a given set of dynamic values $d_j$ for each index type, we substitute each $i_k$ in Equation 2 with the indices given in Equation 1:

$$\tag{3}
I_f = \left\{
    \sum_{j=0}^{n} \left[ (s_j + |l_{s_j}..h_{s_j}| \times d_j) * \left( \prod_{j'=j+1}^{n}{\sigma_{j'}} \right) \right]
    \;\middle|\;
    j \in 0..n, s_j \in l_{s_j} .. h_{s_j}
\right\}
$$

::: todo
For intelligibility, maybe it would be nice to make the subscript consistent between here and Equation 2 (that one uses $k$, and we use $x$ here for obvious reasons)?
$j$ might work in both places, for example.
--A
:::

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
\{
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

::: todo
I don't quite understand the note in the last paragraph. We haven't talked about banking at all yet, and we haven't even defined what "interleaved" or "block-wise" mean.
Since $I_f$ just talks about the flat, bankless physical addresses, it's hard to see how banking comes in here.
--A
:::


Banking in logical arrays
-------

::: todo
I changed the title of this section from "Bank access with index types" because it doesn't seem to have to do with index types at all---it just introduces the concept of banking and how to map flattened offsets to bank/offset pairs.
Also, this might be a good place to move the above discussion of $\text{bank}(b)$ that seemed premature.
--A
:::

::: todo
I agree it's a little off-putting. I wanted to show that determining the bank with index types is as same as with integer types, and note on 'banking' to be in the appendix. Would it make a difference to make the title 'banking in logical arrays'?
--S
:::

To define Seashell's typing rules for logical accesses, we need to know which banks each access employs. Since we can convert logical access to a physical access with equation 3, we specifically need to know how to map any flattened index $i_f$ to a bank number and an offset within that bank. With this one-dimensional representation, we can simply use intuitions from one-dimensional array bank access.

There are two common approaches to banking: interleaving and block-wise.
(See more details [in the appendix][app-banking].)
We focus on interleaving in this document, where for a given flattened index $i_f$:

- The bank number is $i_f \bmod b$.
- The bank offset is $i_f \div b$ (using integer division).

In block-wise banking, the two are reversed.

### Banking in Seashell

To represent the banking of an array $\text{a}$ with banking factor $b$, we'll extend our array notation:

$$
\text{a}: t[\sigma_0] \dots [\sigma_n] \text{ bank} (b)
$$

We'll actually define this to mean the division of memory $\text{a}_f$ into $b$ banks. In the future, we'd perhaps like to support finer banking across each dimension of a multi-dimensional array, but for now we'll stick with this simpler scheme.

**Example.** 

    memory a: int[4][2] bank(4)

[app-banking]: appendix.html#array-banking-strategies-with-2-d-example


Typechecking Array Accesses
---------------------------

We've been describing methods of determining the indices represented by index types in array accesses. Now we'd like to use this information to determine the banks that are accessed, given these indices. We can describe the _bank set_ of a particular array access, given the simplifying assumption:

  * We can assume a particular banking strategy. In particular for this section, we'll assume we're using bank interleaving; we can produce a similar set with other strategies as well.

we can define this bank set $B$ like this:

$$\tag{4}
B = \left\{
    \left( \sum_{j=0}^{n} \left[ (s_j + |l_{s_j}..h_{s_j}| \times d_j) * \left( \prod_{j'=j+1}^{n}{\sigma_{j'}} \right) \right] \right) \bmod b
    \;\middle|\;
    j \in 0..n, s_j \in l_{s_j} .. h_{s_j}
\right\}
$$

::: todo
Instead of jumping right into defining the type rules, I recommend inserting a section here first that derives the *bank set* for a given access (i.e., the set of banks that the access will touch).
Then you can use the bank set to justify the rules.
--A
:::

Now, we need a way to determine which accesses are being used and consumed when we use an index type. One way we could accomplish this is by generating every single index that our index types can represent, and then determine every single bank they access, using the methods we've described. However, we'd like to simplify this process. We'd like to come up with some simpler type rules that enforce memory access safety.

Therefore, we can expand on how this might be of use in the Seashell type system. In general, the problem we're trying to solve is the following: restrict programs such that banks of memories can only be accessed once, to reflect the fact that in actual hardware, these memories have limited access ports. One way we might be able to do this is by tracking a set of banks that are available for use in accessing an array. When an access with a particular bank occurs, we mark the bank unreachable after that point. So, for any array $\text{a}$ in our typing context, associate it with some set $B$ of unconsumed banks, and when we access some bank $b \in B$, the set of banks associated with $B$ becomes $B \setminus b$.  

### Type Rules for One-Dimensional Access

One dimensional arrays are a special case of array accesses, but has significance due to actual hardware being one dimensional and multi-dimensional arrays can be expanded to one dimension by the programmer.

$$\tag{5}
B = \left\{ 
    ( s + |l_{s}..h_{s}| \times d ) \bmod b ~|~ s \in l_{s}..h_{s} 
    \right\}
$$

**banked array access with unroll factor divides banking factor**

Loop unrolling we currently express do not have an offset prior to the static portion. Therefore, we can consider following assumption,

::: todo
I'm tripping myself here, I cannot visualize why static portion would start from an arbitrary value than 0. dynamic portion also start from 0 in the proof. :(
--S
:::

* We can assume that static components of index types start at $0$, i.e. an index type will always have type $\text{idx}\langle 0 .. h_s, l_d, h_d \rangle$; Seashell's unrolled loops imply this.
  
Our attempt now, is to determine the set of banks accessed given the unroll factor divides banking factor.

We would write such a loop as follows:

where $l > n$ and $m \in N$  

    int a[l bank(m*k)]
    for i in 0..n unroll k
        access a[i]

Here, $i$ has type $\text{idx}\langle 0 .. k, 0 .. \frac{n}{k}\rangle$. For any $d \in 0 .. \frac{n}{k}$, $\text{i}$ represents:

$$
\{ s + k * d ~|~ s \in 0 .. k \}
$$

From equation 5, the corresponding set of banks accessed would be:

$$
\{ s + k * d ~|~ s \in 0 .. k \} \bmod m * k
$$

Using the following expansion with modulus:
$(a + b) \bmod c =  (a \bmod c + b \bmod c) \bmod c$

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

This range shows us that our index type would be accessing $k$ distinct banks at any time, and we'd never access some non-existent bank. For $d=0$ we'd access $0..k$, and for $d=(m-1)$ we'd access $(mk-k)..mk$. Allowing accesses that follow our condition of unroll factor dividing into the bank factor would guarantee this.

::: todo
The below assumption seems to have already been made way at the beginning of the document.
But I like the idea of introducing the assumption here (after deriving the bank set) instead!
If it doesn't make the math too messy.
--A
:::

### Type Rules

#### Type Rule 1

For any one-dimensional array access into array $\text{a}$ with index type $i$, we will only consider it valid if the size of the static component of $i$ divides into the banking factor of $\text{a}$.

::: todo
I think this part of the type rules---the notion that "consuming" an array index means you can't access it again---belongs somewhere else.
For this purposes of this document, which is about finding out where logical accesses "go," we can just say "this access consumes these banks of the array."
The meaning of *consumes* can be left to another discussion that dives into our affine types.
--A
:::

::: todo
Moved to appendix under basic type rules we need
--S
:::

It is evident from this rule, that our type checker is conservative about the design space of safe accesses. Our attempt through this document is to formally prove our accesses are safe, and not derive the complete design space of safe accesses. We hope to convince our index types are rigorous and flexible enough to attempt proofs with other type rules, which would make the type checker gradually less conservative.  

::: todo
I'm not 100% sure we need all three of these cases.
It seems like the second one is the most general, and the other two are special cases.
Maybe it would be clearer to just go into the second one in detail---and leave it parameterized on $m$.
This gets you a general type rule.
Then you can give further intuition by examining the *consequences* of this rule for when $m=1$ and when $k=1$.
Having three proofs ends up pretty repetitive.
--A
:::

::: todo
b=mk case moved higher, others moved to appendix 
--S
:::

**Note- ** We're operating on a 1-D array, so this set already represents our "flattened indices" for multi-dimensional access. In fact, we can argue 1-D as a special case of the $I_f$ definition we provided earlier. We will proceed in the next section to arrive at primitive access rules for multi-dimensional access, and gradually improve on them.  

### Type Rules for Multi-Dimensional Access

::: todo
This looks like a good start, but it may not be necessary to separate the single-dimensional and multi-dimensional cases.
I kind of expect that the math here won't get too wild, and it will end up resulting in conclusions that look really similar to the single-dimensional case.
If we do the general math up front, then describing the consequences for the single-dimensional special case will be easy.
--A
:::

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

