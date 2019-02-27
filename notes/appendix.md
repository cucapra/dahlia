---
title: Appendix
---
::: todo
These examples use the old banking notation. Update these or remove these.
:::

3-D array examples to visualize multi-dimensional access
--------------------------------------------------------

##### Example 1
Consider a three-dimensional array $\text{a}$ defined like this:

$$a:t[2][5][3] \text{ bank} (5)$$

The flattened version, $\text{a}_f$, would have size $N=30$. Say we make an access $\text{a}[1][4][2]$. Using our formula we defined, we'd access $\text{a}_f$ with $i_f=29$.

Note that the banking factor is not used in this example.

##### Example 2a
Let's consider an access to the array we considered earlier.
$$a:t[2][5][3] \text{ bank} (5)$$

Since the size of dimension 2 fits nicely with the banking factor, let's say we bank in terms of dimension 2. i.e., we put each set of elements in dim 2 in a different bank.

Let's try accessing the same element as we tried before, $a[1][4][2]$. In index type indexing, knowing banks represent dim 2, we can translate this to $a[\langle 0,1 \rangle][\langle 4,0 \rangle][\langle 0,2 \rangle]$.

Using the equation we derived,

$$
(0 + 1 \times 1) \times 15 + (4 + 5 \times 0) \times 3 + (0 + 1 \times 2) \times 1 = 29
$$

##### Example 2b
Let's try to access the same array as before with the dynamic index set $d=\{1,0,2\}$

From the equation,

$$
\{ (s_0 + 1 \times 1) \times 15 + (s_1 + 5 \times 0) \times 3 + (s_2 + 1 \times 2) \times 1 ~|~ s_0 \in 0..1, s_1 \in 0..5, s_2 \in 0..1 \} = \{17,20,23,26,29\}
$$

##### Example 2c
Consider this program:

    int a[2][5][3] bank(5)

    for x in 0..2 unroll 1
        for y in 0..5 unroll 5
            for z in 0..3 unroll 1
                access[x][y][z]

The types of $x$, $y$ and $z$ would then be:

 - $x : \text{idx}\langle 0 .. 1, 0 .. 2 \rangle$
 - $y : \text{idx}\langle 0 .. 5, 0 .. 1 \rangle$
 - $z : \text{idx}\langle 0 .. 1, 0 .. 3 \rangle$

Finally we compute the elements of $I_f$. We do this by computing the following, for all $s_0 \in 0 .. 1$, $s_1 \in 0 .. 5$, $s_2 \in 0 .. 1$, $d_0 \in 0 .. 2$, $d_1 \in 0 .. 1$, $d_2 \in 0 .. 3$:

$$
(s_0 + |0..k_0|*d_0)*\sigma_1*\sigma_2 + (s_1 + |0..k_1|*d_1)*\sigma_2 + (s_2 + |0..k_2|*d_2)*1
$$

2-D array examples to visualize multi-dimensional access
--------------------------------------------------------

##### Example 1
Consider a two-dimensional array $\text{a}$ defined like this:

$$a:t[4][2] \text{ bank} (4)$$

The flattened version, $\text{a}_f$, would have size $N=8$. Say we make an access $\text{a}[3][1]$. Using our formula we defined, we'd access $\text{a}_f$ with $i_f=7$.

Note that the banking factor is not used.

##### Example 2.1
Consider this program:

    int a[4][2] bank(4)

    for x in 0..4 unroll 2
        for y in 0..2 unroll 2
            access[x][y]

The types of $x$ and $y$ would then be:

 - $x : \text{idx}\langle 0 .. 2, 0 .. 2 \rangle$
 - $y : \text{idx}\langle 0 .. 2, 0 .. 1 \rangle$


Finally we compute the elements of $I_f$. We do this by computing the following, for all $s_0 \in 0 .. 2$, $s_1 \in 0 .. 2$, $d_0 \in 0 .. 2$, $d_1 \in 0 .. 1$:

$$
(s_0 + |0..k_0|*d_0)*\sigma_1*\sigma_2 + (s_1 + |0..k_1|*d_1)*\sigma_2
$$

Here are the computed elements:

  - $(0+2*0)*2*1 + (0+2*0)*1=0$
  - $(0+2*0)*2*1 + (1+2*0)*1=1$
  - $(1+2*0)*2*1 + (0+2*0)*1=2$
  - $(1+2*0)*2*1 + (1+2*0)*1=3$
  - $(0+2*1)*2*1 + (0+2*0)*1=4$
  - $(0+2*1)*2*1 + (1+2*0)*1=5$
  - $(1+2*1)*2*1 + (0+2*0)*1=6$
  - $(1+2*1)*2*1 + (1+2*0)*1=7$


##### Example 2.2
Consider this program:

    int a[4][2] bank(4)

    for i in 0..4 unroll 4
        for j in 0..2 unroll 1
            access a[i][j]

The types of $i$ and $j$ would then be:

 - $i : \text{idx}\langle 0 .. 4, 0 .. 1 \rangle$
 - $j : \text{idx}\langle 0 .. 1, 0 .. 2 \rangle$


With these index type indices we can compute the elements of $I_f$. We do this by computing the following, $\forall$ $d_0 \in 0 .. 1$, $d_1 \in 0 .. 2$:

$$
\{(s_0 + |0..4|*d_0)*\sigma_1 + (s_1 + |0..1|*d_1)*1 ~|~ \forall s_0 \in 0 .. 4, s_1 \in 0 .. 1 \}
$$

where $$ \sigma_1 = 2 $$

For a pair of $\langle d_0,d_1 \rangle = \langle 0,1 \rangle$:

- $(0+4*0)*2 + (0+1*1)=1$
- $(1+4*0)*2 + (0+1*1)=3$
- $(2+4*0)*2 + (0+1*1)=5$
- $(3+4*0)*2 + (0+1*1)=7$

Modulus proof
-------------

$$ab \bmod ac = ab - \lfloor \frac{ab}{ac} \rfloor ac
            = a (b - \lfloor \frac{b}{c} \rfloor c )
            = a (b \bmod c)  $$

Index type array access proofs
-------------

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

Some basic type rules we depend on
-----------

#### Type Rule 2

Allow only one legal usage of index type $i$ to access $\text{a}$, if rule (1) holds. That is, if we have some access $\text{a}[i]$ in a program, then after that point in the program we cannot access $\text{a}$ again.

#### Array out of bound rule


## Old section on banked accesses

we can define this bank set $B$ like this:

$$\tag{4}
B = \left\{
    \left( \sum_{j=0}^{n} \left[ (s_j + |l_{s_j}..h_{s_j}| \times d_j) * \left( \prod_{j'=j+1}^{n}{\sigma_{j'}} \right) \right] \right) \bmod b
    \;\middle|\;
    j \in 0..n, s_j \in l_{s_j} .. h_{s_j}
\right\}
$$

With the knowledge of which bankes we access, we can expand on how this might
be of use in the Seashell type system. In general, the problem we're trying
to solve is the following: restrict programs such that banks of memories can
only be accessed once, to reflect the fact that in actual hardware, these
memories have limited access ports. One way we might be able to do this is by
tracking a set of banks that are available for use in accessing an array.
When an access with a particular bank occurs, we mark the bank unreachable
after that point. So, for any array $\text{a}$ in our typing context,
associate it with some set $B$ of unconsumed banks, and when we access some
bank $b \in B$, the set of banks associated with $B$ becomes $B \setminus b$.
However, we'd like to simplify this process. We'd like to come up with some
simpler type rules that enforce memory access safety.

One dimensional arrays are a special case of array accesses, but has
significance due to actual hardware being one dimensional and
multi-dimensional arrays can be expanded to one dimension by the programmer.

$$\tag{5}
B = \left\{
    ( s + |l_{s}..h_{s}| \times d ) \bmod b ~|~ s \in l_{s}..h_{s}
    \right\}
$$

This also implies that **banked array access with unroll factor divides
banking factor**.

::: todo
I'm tripping myself here, I cannot visualize why static portion would start from an arbitrary value than 0. dynamic portion also start from 0 in the proof. :(

To remove
Loop unrolling we currently express do not have an offset prior to the static portion. Therefore, we can consider following assumption,

- We can assume that static components of index types start at $0$, i.e. an index type will always have type $\text{idx}\langle 0 .. h_s, l_d, h_d \rangle$; Seashell's unrolled loops imply this.

--S
:::

Our attempt now, is to determine the set of banks accessed given the unroll
factor divides banking factor.

We can write such a loop as follows:

where $~|~ l..h | = n$, $len > n$ and $m \in N$

    int a[len bank(m*k)]
    for i in l..h unroll k
        access a[i]

Here, $i$ has type $\text{idx}\langle l_s .. h_s, l_d .. h_d \rangle$.

Since $~|~ l_s .. h_s | = k$ For any $d \in l_d .. h_d$, $\text{i}$ represents:

$$
\{ s + k * d ~|~ s \in l_s .. h_s \}
$$

From equation 5, the corresponding set of banks accessed would be:

$$
\{ s + k * d ~|~ s \in l_s .. h_s \} \bmod m * k
$$

Using the following expansion with modulus:
$(a + b) \bmod c =  (a \bmod c + b \bmod c) \bmod c$

$$
\{ ((s \bmod m*k) + (k*d \bmod m*k)) \bmod m*k ~|~ s \in l_s .. h_s \}
$$

We know a couple things that help us rewrite this set:

- $~|~ l_s .. h_s | < mk$

Therefore, $s \bmod mk$ is unique

- $kd \bmod mk = k (d \bmod m)$ [proof](https://capra.cs.cornell.edu/seashell/docs/appendix.html#modulus-proof), [image](https://imgur.com/a/9cEQHGr)

So then, we have:

$$
\{ s \bmod mk + k (d \bmod m) ~|~ s \in l_s .. h_s \}
$$

We can then express this as a range:

$$
\left( l_s \bmod mk + k (d \bmod m) \right) .. \left( h_s \bmod mk + k (d \bmod m) \right)
$$

This range shows us that our index type would be accessing $k$ distinct banks at any time, and we'd never access some non-existent bank. If $l=0$, then for $d=0$ we'd access $0..k$, and for $d=(m-1)$ we'd access $(mk-k)..mk$. Allowing accesses that follow our condition of unroll factor dividing into the bank factor would guarantee this.

::: todo
The below assumption seems to have already been made way at the beginning of the document.
But I like the idea of introducing the assumption here (after deriving the bank set) instead!
If it doesn't make the math too messy.
--A
:::

### Type Rules (originally in logicalmemoryaccess.md)

#### Type Rule 1

For any one-dimensional array access into array $\text{a}$ with index type
$i$, we will only consider it valid if the size of the static component of
$i$ divides into the banking factor of $\text{a}$.

It is evident from this rule, that our type checker is conservative about the
design space of safe accesses. Our attempt through this document is to
formally prove our accesses are safe, and not derive the complete design
space of safe accesses. We hope to convince our index types are rigorous and
flexible enough to attempt proofs with other type rules, which would make the
type checker gradually less conservative.

**Note.** We're operating on a 1-D array, so this set already represents our
"flattened indices" for multi-dimensional access. In fact, we can argue 1-D
as a special case of the $I_f$ definition we provided earlier. We will
proceed in the next section to arrive at primitive access rules for
multi-dimensional access, and gradually improve on them.

### Type Rules for Multi-Dimensional Access
::: todo
This section needs to be updated with the new banking syntax. - R
:::

::: todo
This looks like a good start, but it may not be necessary to separate the single-dimensional and multi-dimensional cases.
I kind of expect that the math here won't get too wild, and it will end up resulting in conclusions that look really similar to the single-dimensional case.
If we do the general math up front, then describing the consequences for the single-dimensional special case will be easy.
--A
:::

Now we'd like to describe some type rules for when we use multiple index types to access an array.

**Type Rule 1.** The product of the unroll factors influencing the types $i_0..i_n$ must equal the banking factor of the array they're accessing.

    int a[x][y] bank(b)

    for i in range 0..x unroll k1
        for j in range 0..y unroll k2
            access[i][j]

Intuitively, we unroll this loop $b$ times, i.e. simultaneously accessing $b$ values, and there better be $b$ banks we can access. Our index types are the following types:

 - $\text{idx}\langle 0 .. k_1, 0 .. \frac{x}{k_1} \rangle$
 - $\text{idx}\langle 0 .. k_2, 0 .. \frac{y}{k_2} \rangle$

Then we can compute $I_f$ for all $d\in 0 .. \frac{x}{b}$,

$$
I_f = \left\{
    \sum_{j=0}^{n} \left[ (s_j + |l_{s_j}..h_{s_j}| \times d_j) * \left( \prod_{j'=j+1}^{n}{\sigma_{j'}} \right) \right]
    \;\middle|\;
    j \in 0..n, s_j \in l_{s_j} .. h_{s_j}
\right\}
$$

The banks accessed would be:

$$
B = \left\{
    \left( \sum_{j=0}^{n} \left[ (s_j + |l_{s_j}..h_{s_j}| \times d_j) * \left( \prod_{j'=j+1}^{n}{\sigma_{j'}} \right) \right] \right) \bmod b
    \;\middle|\;
    j \in 0..n, s_j \in l_{s_j} .. h_{s_j}
\right\}
$$

We consider the bank factor to be the multiplication of all unroll factors, $b = k_1 \times .. \times k_n$

The case where $j=n-1$, $~|~ l_{s_n-1} .. h_{s_n-1} | = k_{n-1} < b$,
which gives us a unique value and a constant offset in the form of

$$
\{ s \bmod b + k_{n-1} (d \bmod b/k_{n-1}) ~|~ s \in l_s .. h_s \}
$$

Therefore, each should access a different bank.

The case where $j<n-1$,
Since $k_j$ divides $\sigma_{j}$,

$$
\left\{ \left[ (s_j + |l_{s_j}..h_{s_j}| \times d_j) * \left( \prod_{j'=j+1}^{n}{\sigma_{j'}} \right) \right] \bmod b \right\}
$$

$\left( \prod_{j'=j+1}^{n}{k_{j'}} \right) = \Theta$ and $\left( \prod_{j'=j+1}^{n}{|l_{d_j}..h_{d_j}|_{j'}} \right) = \Delta$

$$
\left\{ \Theta \left( \left[ (s_j + k_j \times d_j) *  \Delta \right] \bmod b \right) \right\}
$$

$~|~ l_{s_j} .. h_{s_j} | = k_j < b$,
which gives us a unique value and a constant offset, offsetted further by a multiplicant.

::: todo
But adding unique values don't ensure resulting in a unique value. The offset has to make sure the range of unique values each produce are different.
--S
:::