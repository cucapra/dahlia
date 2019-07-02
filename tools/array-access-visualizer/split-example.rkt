#lang racket/base

(require "let-matrix.rkt")

;; For a given split view access v[i][j] where view has
;; B_v banks and the underlying array has B_a banks, returns
;; the index access on the corresponding underlying array.
(define (split i j B_a B_v)
  (+ (* i B_v) (modulo j B_v) (* B_a (floor (/ j B_v)))))

(define-values (A V)
  (let* ([B-a 6]
         [B-v 3]
         [L-a (* B-a 2)])
    (let/matrix [ (A[1][L-a #:bank B-a] )
                  (V[B-v #:bank B-v][(/ L-a B-v) #:bank (/ B-a B-v)]) ]
                (for ([i (in-range B-v)]
                      [j (in-range (/ L-a B-v))])
                  (V[i][j])
                  (A[0][(split i j B-a (/ B-a B-v))])))))

(require pict)
(scale
 (for/fold ([acc (blank 0)])
           ([e1 V]
            [e2 A])
   (hc-append 10 acc (vc-append 4 e1 e2))) 0.8)