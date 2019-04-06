#lang racket
(require redex)
(define-language fuse
  (v variable-not-otherwise-mentioned
     number
     boolean
  )
  (bop + - / * % == != < <= > >= >> << and or b-and b-or)
  (e v
     (bop e e)
     (deref e)
     (alloc e))
  (c (c semi c)
     (let variable-not-otherwise-mentioned = e)
     (if e e e)
     (while (e) c))
 ;------------------------------
  (loc number)
  (store ((loc v) ...))
 )

(define red
  (reduction-relation
   fuse
   #:domain (e store)
   (--> (term ((+ num1 num2) store))
        (,(apply + (term (num1 num2))) store)
        "+")
  )
)
