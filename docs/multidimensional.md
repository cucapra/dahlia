---
title: Logical Accesses to Multidimensional Arrays in Banked Memories
---
Index Type Recap
----------------------

[Index types](https://capra.cs.cornell.edu/seashell/docs/indextype.html) allow us to combine static and dynamic information about the indices we're accessing in unrolled loops. For instance, consider the following example: 

    for i in l..h unroll k
        access a[i]

The variable $\text{i}$ accessing array $\text{a}$ has type $\text{idx}\langle 0 .. k, \frac{l}{k} .. \frac{h}{k} \rangle$. This type is comprised of a *static component*, $0 .. k$, and a *dynamic component*, $\frac{l}{k} .. \frac{h}{k}$. Following set represents this set of indices,

$$
\{ s + |0..k| \times d ~|~ s \in 0..k, d \in \frac{l}{k}..\frac{h}{k}\}
$$

**Example.** Consider a loop with $l=0$ and $h=30$ with an unroll factor $5$. The set of indeces can be expressed in index types as,


$$
\{ s + |0..5| \times d ~|~ s \in 0..5, d \in \frac{0}{5}..\frac{30}{5}\}
$$

$$
\{ s + 5 \times d ~|~ s \in 0..5, d \in 0..6\}
$$

Since the static component should be unique at runtime, for every dynamic value $d$ that $\text{i}$ takes on, $\text{i}$ represents the following set of indices:

$$
\{ s + |0..k| \times d ~|~ s \in 0..k\}
$$

This set is very important in our subsequent discussion (in any index type discussion). When we access an $i$th element $a[i]$, we would be accessing this set rather than a single element. Our type checking would work on this observation to check for interesting qualities such as safety. 

[//]: # (not entirely happy with this bit)

**Example.** We can create a non-unrolled loop with explicit array accessing using index types. This means we can use the dynamic component dynamcally (of course!), but static component should be explicit. If we consider the same loop as before, for a given dynamic index, we can access the following set of array indices,

$$
\{ s + 5 \times d ~|~ s \in 0..5\}
$$

$if d=5$
$$
\{ 25,26,27,28,29 \}
$$

Just as when accessing with array indices, we can explicitly specify a single element using index types by providing both static and dynamic components $\langle s,d \rangle$,

$$
\{ s + |0..k| \times d \}
$$

**Example.** Using the same array as before, if we access element $\langle 4,5 \rangle$,

$$
\{ 4 + 5 \times 5\} = \{29\}
$$

Multi Dimensional Arrays
------------------------

As far as we're concerned, HLS only supports the use of one-dimensional arrays. With Seashell, we'd like to offer some abstractions to make expressing logical computations on multi-dimensional matrices easier. We'll define Seashell $n$-dimensonal arrays like this:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \text{ bank}(b)
$$

Here, $\text{a}$ is the name of our array; the contents following the colon tell us that $t$ is some arbitrary type ($\text{int}$, etc) for the array elements; and $\sigma_i$ represents the size of a dimension $i$. $\text{bank}(b)$ tells us that this array is banked by a factor of $b$: in the future we'd like to impose flexible banking structures on each dimension, but for now, we're going to simply bank the entire array - that is, the entire flattened array, which we'll talk about in just a moment.

Under the hood (that is, when we translate our Seashell program to HLS C), this multi-dimensional array is translated to a one-dimensional array. This flattened array (which we'll call $\text{a}_f)$ has size equal to the product of our Seashell array dimensions:

$$
\text{a}:t[\sigma_0][\sigma_1]..[\sigma_n] \equiv \text{a}_f:t[{(\prod_{i=0}^{n} \sigma_i)} ]
$$

Logical accesses to a Seashell multi-dimensonal array look like this: $\text{a}[i_0][i_1]..[i_n]$. We'd like to access these higher-dimensional arrays with our Seashell index types, but to first examine how working with these arrays might work, it would be useful to first consider $i_0..i_n$ as plain old integers. So now, for the purposes of typechecking our array accesses, we'd like to know exactly which indices we're using to access this flattened array when we make our logical accesses. So to compute what our flattened index $i_f$ would be based on our logical indices $i_0..i_n$, we could use the following method: 

$$
i_f = \sum_{k=0}^{n} (i_k \prod_{k'=k+1}^{n} \sigma_(k'))
$$

The general intuition behind the above formula is that when accessing the $i$th element of dimension $k$, we need to skip over i sections of $\text{a}_f$ that have size equal to the product of the remainder of the $n-k$ logical dimensions. (TODO: more intuition?)

[//]: # (What is missing here?)

**Example.** Consider a three-dimensional array $\text{a}$ defined like this:

$$a:t[2][5][3] \text{ bank} (5)$$

The flattened version, $\text{a}_f$, would have size $N=30$. Say we make an access $\text{a}[1][4][2]$. Using our formula we defined, we'd access $\text{a}_f$ with $i_f=29$.

**Typechecking.** Because we're using this flattened array, computing which bank is being accessed from $i_f$ can simply be accomplished with $i \bmod b$ or $i / b$, depending on banking structure. 

In one-dimensional arrays we can arrange banks in two ways,  
	- 1. cyclic partitioning/ interleaving (adjacent elements in different banks)

| 0 5 10 15 20 25 | 1 6 11 16 21 26 | 2 7 12 17 22 27 | 3 8 13 18 23 28 | 4 9 14 19 24 29 | 
| --- | --- | --- | --- | --- |

We can use $i \bmod b$ to find the relevant bank for this variant. $i / b$ gives the index within the bank.  
	- 2. block partitioning/ chunking (adjacent elements in the same bank) 

| 0 1 2 3 4 5 | 6 7 8 9 10 11 | 12 13 14 15 16 17 | 18 19 20 21 22 23 | 24 25 26 27 28 29 |  
| --- | --- | --- | --- | --- |  

We can use $i / b$ to find the relevant bank for this variant. $i \bmod b$ gives the index within the bank. 

A detailed note how these are used and why they are needed can be found at [status log](https://github.com/cucapra/seashell/wiki/Test-status-log#array-partitioning-seems-to-need-different-types-of-partitioning)

**Example.** For the same array we considered with $i_f=29$, 
bank is $29 \bmod 5 = 4$ and bank index is $29 / 5 = 5$. 
i.e., we would access the 5th element in the 4th bank.  

Typechecking Array Accesses
------------------------

In these document, we've been describing methods of determining the banks that array accesses make. Now, we'd like to expand on how this might be of use in the Seashell type system. In general, the problem we're trying to solve is the following: restrict programs such that banks of memories can only be accessed once, to reflect the fact that in actual hardware, these memories have limited access ports. One way we might be able to do this is by tracking a set of indices that are available for use in accessing an array. When an access with a particular index occurs, we mark it unreachable after that point. So, for any array $\text{a}$ in our typing context, associate it with some set $\text{I}$ of unconsumed indices, and when we access some index $i \in \text{I}$, the set of indices associated with $\text{a}$ becomes $\text{a} \setminus i$.  

[//]: # (What does this mean?)  

Now, we need a way to determine which accesses are being used and consumed when we use an index type. One way we could accomplish this is by generating every single index that our index types can represent, and then determine every single bank they access, using the methods we've described. However, we can make this process easier with the help of a few simplifying assumptions.

**Assumption 1.** Our index types, as we've defined them, allow for static components to be any integer range $l_s .. h_s$. We can restrict this so that $l_s=0$, which is all we really need for practical purposes: if a loop has an unroll factor $k$, then the static component of an index variable for this loop would certainly just be $0 .. k$. This just makes reasoning about which indices are being accessed a little bit easier.

**Assumption 2.** If we restrict our programs so that we only allow for array accesses where the index type static component length matches the unroll factor, then typechecking becomes much easier. Consider the following example, where $\text{a}$ is an array with length $s$ and banking factor $k$:

    int a[s bank(k)]
    for i in 0..n unroll k
        access a[i]

Here, $\text{i}$ has type $\text{idx}\langle 0 .. k, 0 .. \frac{n}{k}\rangle$. For any $d \in 0 .. \frac{n}{k}$, $\text{i}$ represents:

$$
\{ s + k * d ~|~ s \in 0 .. k \}
$$

We could then compute the banks being accessed like this (assuming the interleaved banking structure):

$$
\{ s \bmod b + (k * d) \bmod b ~|~ s \in 0 .. k \}
$$

And because of our assumption (that is, that $b=k$), we have:

$$
\{ s \bmod k + (k * d) \bmod k ~|~ s \in 0 .. k \} \rightarrow 0 .. k
$$

So, from this we can conclude that when the banking factor matches the unroll factor of an index type, we access every bank when we use an index type for accessing an array. In other words, if an index type has a static component $l_s..h_s$, and it's indexing into some array $\text{a}$ with banking factor $b$, then if $|l_s..h_s|=b$, the type system consumes every bank of $\text{a}$, disallowing any further accesses.

**Assumption 2, weakened.** We might want to relax this assumption to allow situations where the unroll factor divides into the banking factor. Consider the following example: 

    int a[s bank(m*k)]
    for i in 0..n unroll k
        access a[i]

Similar to our previous example, $\text{i}$ has type $\text{idx}\langle 0 .. k, 0 .. \frac{n}{mk} \rangle$. The set interpretation for a particular $d \in 0..\frac{n}{mk}$ would be: 

$$
\{ s + mk * d ~|~ s  \in 0 .. k \} 
$$


Using index types for multi dimensional array accesses
------------------------------------------------------

So far we have recapped index types in a single dimensional sense and we have explored how logical accessing in multi-dimensional arrays are translated to a single dimension in hardware, thereby making it easy to type check for banking.  

However, it'll be nice to use the same type (index type) for both single and multi dimensions. Moreover, using index types in multi-dimensions may allow us to finely bank such an array in different dimensions (we can only do block and cyclic in single dimension. However, nested loops with unroll factors need more flexible banking strutures.)  

So, let's try to extend index types to a multi-dimensional array indices.  

from the recap,  
 
$$
i = \{ s + |0..k| \times d \}
$$

from multi-dimensional access,

$$
i_f = \sum_{j=0}^{n} (i_j \prod_{j'=j+1}^{n} \sigma_(j'))
$$

therefore,  
$$
i_f = \sum_{j=0}^{n} (\{ s_j + |0..k_j| \times d_j \} \prod_{j'=j+1}^{n} \sigma_(j'))
$$

where $sigma_j$ is $|0..k| \times |\frac{l}{k}..\frac{h}{k}|$  

(maybe this is multiplication of index type variables? But the order may matter, as it is not associative)  

**Example.** Let's consider an access to the array we considered earlier. 
$$a:t[2][5][3] \text{ bank} (5)$$

Since the size of dimension 2 fits nicely with the banking factor, let's say we bank in terms of dimension 2. i.e., we put each set of elements in dim 2 in a different bank.  

Let's try accessing the same element as we tried before, $a[1][4][2]$. In index type indexing, knowing banks represent dim 2, we can translate this to $a[\langle 0,1 \rangle][\langle 4,0 \rangle][\langle 0,2 \rangle]$.  

Using the equation we derived,  

$$
(0 + 1 \times 1) \times 15 + (4 + 5 \times 0) \times 3 + (0 + 1 \times 2) \times 1 = 29
$$

As we noted before, we are more interested in the set of indices we can access with $a[i]$ than accessing a single element with index types. Therefore, we can similarly extend the equation we derived earlier,


$$
\{ s + |0..k| \times d ~|~ s \in 0..k\}
$$

$$
i_f = \{ \sum_{j=0}^{n} ( [ s_j + |0..k_j| \times d_j ] \prod_{j'=j+1}^{n} \sigma_(j')) ~|~ s_0 \in 0..k_0, s_1 \in 0..k_1, .. , s_n \in 0..k_n \}
$$

Note that rather than using $s_j \in 0..k_j$ with a union operation, I have written all the static component sets. This way just felt more intuitive, but expressing with a union might be better. 

**Example.** Let's try to access the same array as before with the dynamic index set $d=\{1,0,2\}$  

[//]: # (Is this an accurate representation?)

From the equation,  

$$
\{ (s_0 + 1 \times 1) \times 15 + (s_1 + 5 \times 0) \times 3 + (s_2 + 1 \times 2) \times 1 ~|~ s_0 \in 0..1, s_1 \in 0..5, s_2 \in 0..1 \} = \{17,20,23,26,29\}
$$

Case studies as a sanity check
------------------------------

**for a single dimension with no unrolling**

for $n=1$ and $k=1$
$$
i_f = \{ \sum_{j=0}^{n} ( [ s_j + |0..k_j| \times d_j ] \prod_{j'=j+1}^{n} \sigma_(j')) ~|~ s_0 \in 0..k_0, s_1 \in 0..k_1, .. , s_n \in 0..k_n \}
$$

$$
i_f = \{[s_0 + |0..1| \times d_0] \times 1 ~|~ s_0 \in 0..1\}
i_f = d_0
$$

$bank = d_0 \bmod b$

__*This shows we'll be filling in the banks cyclically, as $i \bmod b$ was intended to do.*__

**for banking factor = unroll factor**

We use  
proof for taking modulus inside an expression $(a+b) \bmod c = (a \bmod c + b \bmod c) \bmod c$ and  
proof for $ab \mod ac = a(b \bmod c)$  
for the following.  
 
Intuitively we know that if banking factor equals the unroll factor, in each access we'd be accessing different banks and also all the banks.  

for $n=1$ and $b=k$
$$
i_f = \{ \sum_{j=0}^{n} ( [ s_j + |0..k_j| \times d_j ] \prod_{j'=j+1}^{n} \sigma_(j')) ~|~ s_0 \in 0..k_0, s_1 \in 0..k_1, .. , s_n \in 0..k_n \}
$$

$$
i_f = \{[s_0 + |0..k| \times d_0] \times 1 ~|~ s_0 \in 0..k\}
$$

$bank = i_f \bmod b$

$$
= \{[s_0 + |0..k| \times d_0] \times 1 ~|~ s_0 \in 0..k\} \bmod k
$$

$$
= \{[(s_0 \bmod k) + (k \times d_0) \bmod k] \bmod k ~|~ s_0 \in 0..k\}
$$

$$ 
= \{[(s_0 \bmod k) + k \times (d_0 \bmod 1)] \bmod k ~|~ s_0 \in 0..k\}
$$

$$
= \{[(s_0 \bmod k) + k \times 0] \bmod k ~|~ s_0 \in 0..k\} 
$$

$$
= \{[s_0 \bmod k] ~|~ s_0 \in 0..k\}
$$

since $s_0$ is bounded by $k$, $s_0 \bmod k$ is always smaller than k
$$
bank = \{s_0 \in 0..k\} 
$$

__*This shows we'd access all the banks and each bank would be accessed only once*.__  

**for banking factor = constant * unroll factor**

We use if $a \in 0..b$ and $m$ is an integer, then $a \bmod mk = a \bmod k$ for the following.  

Intuitively we know that if we have more banks than the unroll factor, it should be possible to access different banks at a given time. For this case, where number of banks is a constant of multiple of unroll factor, we know that depending on the dynamic index we'd access a specific set of banks and that set would also have a unique set of banks.  

Given a dynamic component $d$, unroll factor $k$ and banking factor $m \times k$ where $m$ is the constant integer multiple: 

Since we'd be accessing using interleaved strategy, we'd put adjacent elements to different banks. Since we have $m$ banks represented by each static component, we'd access these $m$ banks cyclically depending on the dynamic component. As we'd be accessing different sets of banks of size of the unroll factor depending on the dynamic component, we can say this set is $d \bmod m$. And we'd be accessing all \{0..k\} banks in that set, we can write the banks we are accessing as,  

$$\{k \times (d \bmod m).. k \times (d \bmod m) + k\}$$

We can determine the banks we access from our derived equation
$for n=1 and b=mk

$$
i_f = \{ \sum_{j=0}^{n} ( [ s_j + |0..k_j| \times d_j ] \prod_{j'=j+1}^{n} \sigma_(j')) ~|~ s_0 \in 0..k_0, s_1 \in 0..k_1, .. , s_n \in 0..k_n \}
$$

$$
i_f = \{[s_0 + |0..k| \times d_0] \times 1 ~|~ s_0 \in 0..k\}
$$

$bank = i_f \bmod b$
$$
= \{[s_0 + |0..k| \times d_0] \times 1 ~|~ s_0 \in 0..k\} \bmod mk
$$

$$
= \{[(s_0 \bmod mk) + (k \times d_0) \bmod mk] \bmod mk ~|~ s_0 \in 0..k\} 
$$

$$
= \{[(s_0 \bmod k) + k \times (d_0 \bmod m)] \bmod mk ~|~ s_0 \in 0..k\}
$$

we can expand this to a range,
$$
bank = 0 + k \times (d_0 \bmod m) .. k + k \times (d_0 \bmod m)
= k \times (d \bmod m) .. k \times (d \bmod m ) + k
$$

so our equation allows us to come to the same conclusion as the intuition.  

[//]: # (I think it does not need to be generalizable as we are referring to the interleaved case only and hopefully my write up is sufficient)

__*This shows us that index type expressions we derived show the set of banks we access when the bank factor is $m$ times the unroll factor.*__   

