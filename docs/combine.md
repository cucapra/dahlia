Combiners in Seashell
=====================

Seashell's unrollable `for` loops are typically "DOALL" loops---they are trivially parallel and rule out all cross-iteration dependencies.
This restriction is critical for implementing our intended semantics, i.e., a loop unrolled $k$ times generates $k$ copies of the hardware for the loop body to execute in lockstep.
But it's also clearly too restrictive; even simple accelerator designs need basic cross-iteration dependencies to support patterns like reductions.

*Combiner* blocks allow a restricted form of serial, dependency-laden computation by separating it from the perfectly parallel "main" part of a loop.
Combiners can support reductions but are more general.
The anatomy of a Seashell `for` loop is:

    for (let i = /* iteration space */) unroll k {
      /* body (parallel) */
    } combine {
      /* combiner (serial) */
    }

The *body* part of the loop gets duplicated $k$ times in the emitted architecture, i.e., it enjoys $k$-way parallelism.
The *combiner* part runs once for every $k$ iterations of the body,
:::todo
"iterations" is a bit confusing? # of iterations are I/k.
:::
i.e., the compiler only instantiates one copy of its hardware and it runs serially.
:::todo
What does running serially mean here? Different expressions separated by `;` run serially or the operator itself run serially? Latter would make sense why there's no multiple memory access here. But it also would mean the adders happen serially?
:::
Because the combiner is serial, cross-iteration dependencies are allowed there (where they are still *not* allowed in the body).

The combiner runs after the $k$ copies of the body finish executing and---here's the important part---it receives the resulting values from all $k$ body copies.
If the body defines a variable `x` of type `float`, then the combiner can refer to `x` also---but `x` is not a single `float` value; instead, it represents a group of $k$ `float`s, one from each copy of the `body` hardware.

Imagine for a moment that, in the combiner, `x[i]` referred to the value of `x` from the $i$th "copy" of the body ($i < k$).
(This is not the final version of our language constructs!)
Here's how you might use this syntax in a combiner to implement a simple map/reduce pattern in Seashell:

    let acc = 0.0;
    for (let i = 0..32) unroll 4 {
      let x = f(a[i]);
    } combine {
      acc = acc + x[0] + x[1] + x[2] + x[3];
    }

The cross-iteration dependency through `acc` is allowed in the combiner where it wouldn't be allowed in the body.
::: todo
I see. What's the hardware mapping though? Is the body of the for loop one PE with control for interations? Do we have another layer of control which enforces this serialization of a chain of sub-PEs?
:::

::: todo Does cross-iteration dependency include aloowing accesses like `a[i+1] := a[i]`?
:::

This version of our reducer, however, is not terribly convenient and, worse, it does not meet Seashell's erasure goal---that the program should have standard serial semantics if you ignore all the Seashell-specific annotations, including `combine`.
Expressions like `x[3]` don't make sense in the erased version of the loop:

    let acc = 0.0;
    for (let i = 0..32) {
      let x = f(a[i]);
      acc = acc + x[0] + x[1] + x[2] + x[3];  // Huh? x is a scalar!
    }

So we also introduce special reduction syntax that works on entire vectors of values at once:

    let acc = 0.0;
    for (let i = 0..32) unroll 4 {
      let x = f(a[i]);
    } combine {
      acc = acc + x;
    }

Here, `+` is overloaded to work on a `float` (`acc`) and a vector of `float`s (`x`).
I'm not sure this is the best syntax, but it's what we've got for now and it certainly has a serial semantics.

Combiners generalize reductions because you can do anything you like with the values in the vector `x`---you don't need to perform a commutative operator on them. Doing so probably won't have sequential semantics, but I'm curious what else---other than a straightforward reduction---we might want to do with this more general power.
::: todo
How does it differ from using serialization to solve this problem? It feels like `combine` is actually `serialize` and the complexity is avoided by serializing everything? Where as reduction feels like maintaining parallelism as much as possible by possibly using a bunch of memory elements but doing read access only once?
:::
