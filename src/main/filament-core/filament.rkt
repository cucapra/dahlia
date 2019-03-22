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
     (bop e e))
 )
     