# An Introduction to Seashell

## Outline (will delete, just for organization):

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

