# An Introduction to Seashell

::: todo
Figure out how to put hl.js here, or something like that, -T
:::

In this document, we'll illustrate the capabilities of Seashell's type system by describing both legal and illegal Seashell programs.

## Memory Access

Seashell supports a couple of ways to access memories. In each method, Seashell enforces _memory access safety_: that is, when the programmer accesses a memory bank, Seashell prevents the programmer from doing so again. This reflects the fact that memory banks have limited access ports. Seashell also requires that the memory banks accessed are specified _statically_, so no multiplexer will be generated in the hardware. This is in effort to make Seashell programs more explicit: multiplexers should only appear when the programmer explicitly specifies it.

### Banked Memory Access

We can use Seashell's _banked memory access_, which follows this format:

    array_name{bank}[index_into_bank]

#### A Legal Banked Access

Here's a legal program, which accesses each memory bank only once, and the banks are specified statically:

    func populate(a: int[10 bank(5)], b: int) {
	  for (let i = 0..2) {
	      a{0}[i] := b;
	      a{1}[i] := b;
	      a{2}[i] := b;
	      a{3}[i] := b;
	      a{4}[i] := b;
	  }
    }

#### Illegal Banked Access: Non-Static Bank Accessor

The following program is illegal, because the banks being accessed are determined dynamically with the variable $\text{i}$:

    func illegal_nonstatic(a: int[10 bank(5)], b: int) {
	  for (let i = 0..2) {
	      a{i}[0] := b;
	  }
    }

Seashell rejects the program with the following error message:

    [Type error] Bank accessor must be static

#### Illegal Banked Access: Multiple Bank Access

Here's an illegal program, because we access bank $0$ of memory $\text{a}$ twice:

    func illegal_multaccess(a: int[10 bank(5)], b: int) {
	  for (let i = 0..2) {
	      let x = a{0}[i];
	      a{0}[i] := b;
	  }
    }

Seashell will reject the program with this error message:

    [Type error] memory "a" illegal access: bank 0

Here, we accessing bank $0$ of array $\text{a}$ once when we read it and store its contents in variable $\text{x}$; and once when we write it with $\text{b}$.

### Normal Memory Access

Specifying a bank for each array access would become quite tedious in larger programs, particularly with multidimensional arrays. Seashell supports a _normal memory access_ that offers a little abstraction away from bank specification, and looks like this:

    array_name[i0]...[in]

$i_0 \dots i_n$ are _index types_, which represent both static and dynamic information. See the notes on [index types](indextype.html) and [logical memory access](logicalmemoryaccess.html) to learn more about how they work.

where $\text{array\_name}$ is $n$-dimensional.

#### A Legal Memory Access

The following example is legal:

    func madd(a: float[1024 bank(32)], b: float, c: float[1024 bank(32)]) {
      for (let i = 0..1023) unroll 32 {
	    c[i] := a[i] + b;
      }
    }

Here, we unroll the loop $32$ times, which influences the type of variable $\text{i}$, which has type $\text{idx}\langle 0 .. 32, 0 .. 32 \rangle$. By using $\text{i}$ to access $\text{c}$ and $\text{a}$, we are statically accessing $32$ memory bank values in each iteration of this loop, that is, the values $0$ through $31$. This precisely matches the banking structure we've defined, so this is a legal Seashell program.
