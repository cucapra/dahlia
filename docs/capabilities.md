---
title: "Proposal: Capabilities"
---

This is a proposal to replace index types with simpler types and explicit
capabilities. This has the effect of the separating the two concerns that an
index type is trying to verify: safe index access and safe bank access.

### Introduction to capabilities

In a simple programming language, a value can be read or written to with minimal
constraints (usually none). In a language with type-level capabilities, we
can control the access and mutation of values using capabilities. A capability
can be thought of as a labelled key that gives the program permission to
use a value in a certain way.

A simple version of this exists in the seashell core language which the type checker
works with. In seashell core, each array access is associated with a capability.
For example, to read an array value, the following is needed:

```
read a[i] as a1; // array access consumes linear capability from array
let x = a1; // Use the named capability to get the value in a[i]
```

Similarly, for an array write, we use

```
write a[i] as a1;
a1 := 10
```

Unlike read capabilities, write capabilities are affine, i.e., they can be used
at most once which helps us enforce the invariant that writes to physical
wires can only occur once in a clock cycle.

(Since it is tedious to write capabilities explicitly, seashell also infers
these capabilities automatically. In this document, we explicitly write
capabilities.)

### Banks as a capabilities

In the current formulation, seashell assigns [Index types](indextype.html) to
values and uses those to check invalid bank accesses. Since the index type
encodes both the static and the dynamic information about the, formalizing
them is non-trivial.

Instead of encoding these two constraints into one type, we suggest separating
them out into capabilities and types. Concretely, this means that any array
access expression will only type check when the context has the required
capabilities to access the bank and an in-bounds type for access expression.

**Example**. For an access expression of the form `a{b}[i]`, the context must
prove that $\mathcal{C}(a) \leq b$ and $\Gamma(i) = \text{int}$ where $\mathcal{C}$
is the capability context and $\Gamma$ is the type context. The $\mathcal{C}_1 \leq
\mathcal{C}_2$ states that $\mathcal{C}_2$ has at least as many capabilities
as $\mathcal{C}_1$.

This change also has the effect of simplifying the formalization for index type
addition. Since both the bank access expression $b$ and access expression $i$
are numbers, arithmetic operations are simply defined as bitvector arithmetic.

#### Polymorhphism using capabilities

The separation of capabilities can potentially have a big impact in
simplifying the polymorphic encoding for index types. Instead of having to
formalize the polymorphism over index types, we can simply reuse work from
capability polymorphism.

#### Capability regions

Once we define a sequential composition operator for seashell, we'll have to
change the current implementation of the read/write capability inference to
full blown capability inference. If we choose to switch to explicit capabilities,
we can use this already needed extension to implement those.

### Typing rules

Our core language has the following forms:

```
v ::= n | true | false
e ::= v
    | op2 e e
    | if e e e
    | let x = e
    | c e as x
    | for (let x = e) e
    | func id (x ...) e
    | id(e ...)
```

where $x$ is metavariable representing identifiers and $\text{n} \in \mathbb{N}$.
We also define the typing forms:

```
t ::= bit<n> | bool | t -> t
```

Finally, we define the capability forms

```
c ::= read n
    | write n
```

A typing judgement for language is of the form $\Gamma, \kappa, e \vdash \Gamma', \kappa', t$
where $\Gamma$ is the typing context and $\kappa$ is the capability context.

::: formula
**Note**(rachit). I am fairly new to capabilities. If something doesn't sound
right about my characterization, please point it out.
:::