---
title: Appendix
---

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
