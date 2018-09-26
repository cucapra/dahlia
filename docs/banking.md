---
title: Banking in hardware
---

This document outlines the various banking strategies used in HLS tools when
banking hardware memory and defines some mathematical relationships between
the original structure of the array and a banked array.

## Interleaving banking
Interleaving banking allows programmers to access logically consecutive
elements parallely. This is done by moving consecutive elements into separate
banks.

**Example.**
Consider the following array:

| 1 2 3 4 5 6 7 8 |
| --------------- |

If the array is banked using 4 banks and an interleaving scheme, we get the
following physical representation:

| 1 5 | 2 6 | 3 7 | 4 8 |
| --- | --- | --- | --- |

Now if the user wants, they can access first four, or the last four logically
consecutive elements in one cycle.

For an array element at index $i$ in an array of length $l$ and banking
factor $b$, we have the following:

- bank number: $i \bmod b$
- index in the bank: $\lfloor i \div b \rfloor$

### Generalization to $n$ dimensions

Since seashell allows banking in mutliple dimensions, we need a function that
maps a banking structure and indices to a specific bank. So, given an array
$a[\sigma_1\text{ bank }(b_1)]\ldots[\sigma_n\text{ bank }(b_n)]$, we define the
function $\mathcal{B}$ as

$$\tag{1}
\mathcal{B}(i_1, \ldots, i_n) =
\sum_{j=1}^n \left ( (i_j \bmod b_j) \times \prod_{k=i+1}^n b_k \right )
$$

## Block based banking
Block based banking puts logically consecutive elements in an array into the
same bank. An intuitive way of thinking about this is choping up an array in
to $b$ parts where $b$ is the banking factor.

**Example.**
Consider the following array:

| 1 2 3 4 5 6 7 8 |
| --------------- |

If the array is banked using 4 banks and an interleaving scheme, we get the
following physical representation:

| 1 2 | 3 4 | 5 6 | 7 8 |
| --- | --- | --- | --- |

For an array element at index $i$ in an array of length $l$, banking
factor $b$ and $n=l/b$, we have the following:

- bank number: $\lfloor i \div n \rfloor$
- index in the bank: $i \bmod n$