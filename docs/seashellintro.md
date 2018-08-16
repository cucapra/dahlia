# An Introduction to Seashell

::: todo
Will delete this outline; just for organizational purposes.
:::

 1. Why Seashell is useful
 2. Summary of the document, listing the features that will be covered.
 3. How to define functions (brief discussion of how host code should work
    in relation to the hardware functions)
 4. Memories, banking; just short discussion
 5. Accessing memories in loops.
    - Simple 1D loop example
      + Non-unrolled, with banked array access; show that multiple bank access
        is illegal
      + Unrolled example, with array example; show legal programs with different
        unroll factors (= to BF, or factor of BF); discuss why they're legal;
	show illegal program, why it's illegal.

    - Multidimensional example
      + Discuss a legal program, and why it's legal.
      + Discuss an illegal program, and why it's illegal.
 6. Usages of muxes.

::: todo
General paragraph about Seashell.
:::

::: todo
General paragraph summarizing document.
:::

## Functions 

::: todo
Section about functions.
:::

## Memories

::: todo
Section about memories.
:::

## Accessing Memories in Loops

Seashell supports a couple of ways to access memories. In each method, Seashell enforces _memory access safety_: that is, when the programmer accesses a memory bank, Seashell prevents the programmer from doing so again. This reflects the fact that memory banks have limited access ports.

### Banked Memory Access

Here, we can use Seashell's _banked memory access_, which follows this format:

    array_name{bank}[index_into_bank]

Here's an example:

::: todo
Other styling for concrete Seashell programs, to distinguish from styling for pseudo-codey stuff?
:::

    func populate(a: int[10 bank(5)], b: int) {
	  for (let i = 0..2) {
	      a{0}[i] := b;
	      a{1}[i] := b;
	      a{2}[i] := b;
	      a{3}[i] := b;
	      a{4}[i] := b;
	  }
    }

Here, in each iteration of our loop, we access each bank **once**; therefore, this is a legal program.

Here's an illegal program:

    func illegal(a: int[10 bank(5)], b: int) {
	  for (let i = 0..2) {
	      let x = a{0}[i];
	      a{0}[i] := b;
	  }
    }

Here, we accessing bank $0$ of array $\text{a}$ twice: once when we read it and store its contents in variable $\text{x}$; and once when we write it with $\text{b}$. Seashell will reject this program. 

::: todo 
error msg? 
:::




