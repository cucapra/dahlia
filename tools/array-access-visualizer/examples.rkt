#lang racket/base

(require "let-matrix.rkt")

;; "tests"
(let/matrix
 ([A 2 4])
 (A[0][1])
 (---)
 (A[1][2]))

(let/matrix
 ([A 2 4]
  [B 1 8])
 (for ([i (in-range 1)]
       [j (in-range 4)])
   (A[i][j])
   (A[(+ i 1)][j])
   (B[0][j])))
