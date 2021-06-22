---
id: access-visualizer
title: Array Access Visualizer
---

We have written a small library in Racket to provide visualization facilities
for array accesses. This tool can be used while developing Dahlia programs
that make use of complicated array access patterns and views.

The library defines the wrapper `let/matrix` which allows defining matrices
and defines C-like array access notation for those arrays. The `let/matrix`
context also defines `---` that corresponds to the sequencing operator in
Dahlia.

To play with examples, open `tools/array-access-visualizer/examples.rkt`
in [DrRacket](https://download.racket-lang.org) and click run. This will generate three sequences of
images that corresponds to the two examples.

For the first example, we have:

```lisp
(let/matrix [ (A[2][4]) ]
  (A[0][1])
  (---)
  (A[1][2]))
```

The first sequence in `let/matrix` defines a matrix `A` with dimensions `2` and
`4`. The remaining expressions in `let/matrix` simply access the array. The
`(---)` tells the visualizer that the two access are sequential, not parallel.
Try removing it to see the visualization generated.

The second example is a bit more interesting:

```lisp
(let/matrix [ (A[2][4])
              (B[1][8]) ]
 (for ([i (in-range 1)]
       [j (in-range 4)])
   (A[i][j])
   (A[(+ i 1)][j])
   (B[0][j])))
```

Here we define two matrices `A` and `B` and iterate over them using the `for`
construct. While we can use any arbitrary iterator (like recursive functions or
`while` loops), the `for` form is special in that it implicitly adds a `(---)`
at the end of each iteration. This means that for each iteration, the `A[i][j]`
and `A[(+ i 1)][j]` accesses are in parallel but the cross iteration accesses
are sequential.

Also note that this `let/matrix` generates *two* images. In general, for
`n` matrix declarations, the visualizer will generate `n` sequences of array
accesses.

In the `let/matrix` form's array definitions, we can also specify the banking
factor for each dimension:

```lisp
(let/matrix [ (A[1][8 #:bank 4])) ])
```

which generates the array:

![1 dimensional banked array](assets/banked-1d.png)

The values produced by `let/matrix` are first class racket lists that can be
manipulated like other values:

```lisp
(define-value (A-access B-access)
  (let/matrix ([A 2 2]
               [B 2 2])
    (A[1][1])
    (B[1][1])))

(map (lambda (img) (scale img 3.5)) A-access)
```
