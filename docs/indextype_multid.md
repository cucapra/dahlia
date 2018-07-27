A recap of index types
----------------------

An array with an index type where $s$ refers to the static part and $d$ to the dynamic part, represents a possible set of indeces,

$$\\{ s + |l_s..h_s| \times d ~|~ s \in l_s..h_s, d \in l_d..h_d\\}$$

**Example**  
consider $a'$ an array with the size 30 and we attempt to access it 5 times in parallel (unrolled 5 times). 

The set of possible indeces are,
$$\\{ s + 5 \times d ~|~ s \in 0..5, d \in 0..6 }\\$$ $$

i.e from $(0,0)$ to $(4,5)$

Since accessing the same static part would result in a safety violation, for a given $d$, valid accesses would be,

$$\\{ s + |l_s..h_s| \times d ~|~ s \in l_s..h_s\\}$$

**Example**
For above instance, that set would be $0..5$

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
$a:t[2][5][3]$
$a':t[30] bank 5$

$ a[1][4][2] = a'[29]$

in order to check which bank and offset this element occupies, we can use the same approach as in a single dimension,
$bank \rightarrow i'%b$
$offset \rightarrow i'/b$

As an aside, this assumes interleaved memory banking (cyclic partitioning). Block partitioning (chunking) would simply require us to swap these,
i.e.
$bank \rightarrow i'/b$
$offset \rightarrow i'%b$

**Example**

$ bank is 29 % 5 = 4 $
$ offset is 29 / 5 = 5$


