#lang racket/base

(require "let-matrix.rkt")

;; For a given split view access v[i][j] where view has
;; B_v banks and the underlying array has B_a banks, returns
;; the index access on the corresponding underlying array.
(define (split i j B_a B_v)
  (+ (* i (/ B_a B_v)) (modulo j B_v) (* B_a (floor (/ j B_v))) ))

(define-values (A V)
  (let ([B_a 4]
        [B_v 2])
    (let/matrix [ (A[1][8 #:bank B_a] )
                  (V[2 #:bank 2][4 #:bank 2]) ]
                (for ([i (in-range 2)]
                      [j (in-range 4)])
                  (V[i][j])
                  (A[0][(split i j B_a B_v)])))))

(require pict)
(scale
 (for/fold ([acc (blank 0)])
           ([e1 V]
            [e2 A])
   (hc-append 10 acc (vc-append 4 e1 e2)))
 1.7)