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
       %
       ==
       !=
       <
       <=
       >
       >=
       >>
       <<
       and
       or
       b-and
       b-or)
  (e v
     (bop e e)
     (deref e)
     (alloc e))
  (c (c semi c)
     (let variable = e)
     (if e e e)
     (while (e) c))
 ;------------------------------
  (loc number)
  (store ((loc v) ...))
 )
    