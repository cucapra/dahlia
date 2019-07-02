#lang racket/base

(require "let-matrix.rkt")

;; "tests"
(println "simple sequential access")
(let/matrix
 [(A[2][4])]
 (A[0][1])
 (---)
 (A[1][2]))

(println "parallel access with for loop")
(let/matrix
 [(A[2 #:bank 2][4 #:bank 2])
  (B[1][8])]
 (for ([i (in-range 1)]
       [j (in-range 4)])
   (A[i][j])
   (A[(+ i 1)][j])
   (B[0][j])))

(println "sequential access with while loop in banked array")
(let/matrix
 [(A[1][8 #:bank 4])]
 (define x 0)
 (while (< x 8)
        (A[0][x])
        (set! x (+ x 1))))