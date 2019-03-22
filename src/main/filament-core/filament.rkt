#lang racket
(require redex)
(define-language fuse
  (v variable
     number
     boolean
  )
  (bop +
       -
       /
       *
       modulo
       =
       <
       <=
       >
       >=
       arithmetic-shift
       bitwise-not
       bitwise-or
       bitwise-and)
  (e v
     (bop e e)
     (deref e)
     (alloc e))
  (c (c semi c)
     (let variable assgn e)
     (if e e e)
     (while (e) c))
 )
     