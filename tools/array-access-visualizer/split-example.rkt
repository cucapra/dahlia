#lang racket/base

(require racket/list)

(require "let-matrix.rkt"
         "image-gen-helpers.rkt")
(require pict)

(define (merge-imgs A V)
  (zip-with
      (lambda (e1 e2) (vc-append 4 e1 e2))
      A V))

;; For a given split view access v[i][j] where view has
;; B_v banks and the underlying array has B_a banks, returns
;; the index access on the corresponding underlying array.
(define (split i j B_a B_v)
  (+ (* i B_v) (modulo j B_v) (* B_a (floor (/ j B_v)))))

; Show what splitting looks like
(define-values (A-simpl V-simpl)
  (let* ([B-a 4]
         [B-v 2]
         [L-a (* B-a 2)])
    (let/matrix [ (A[1][L-a #:bank B-a] )
                  (V[B-v #:bank B-v][(/ L-a B-v) #:bank (/ B-a B-v)]) ])))

(save-pict
  (scale (car (merge-imgs A-simpl V-simpl)) 2) "split.png" 'png)

; Show what splitting and parallelizing the inner loop looks like
(define-values (A V)
  (let* ([B-a 4]
         [B-v 2]
         [L-a (* B-a 2)])
    (let/matrix [ (A[1][L-a #:bank B-a] )
                  (V[B-v #:bank B-v][(/ L-a B-v) #:bank (/ B-a B-v)]) ]
      (for ([j (in-range (/ L-a B-v))])
        (for* ([i (in-range B-v)])
          (V[i][j])
          (A[0][(split i j B-a (/ B-a B-v))]))))))

; Drop last element of the list
(define img-lst
  (let ([tmp-lst (merge-imgs A V)])
    (take tmp-lst (sub1 (length tmp-lst)))))

(parameterize ([max-elems (* arr-elem-len 16)]
               [separator? #f])
  (save-pict (scale (smart-layout img-lst) 2) "split-outer.png" 'png))

; Show what splitting and parallelizing the outer loop looks like
#|(define-values (A-outr V-outr)
  (let* ([B-a 4]
         [B-v 2]
         [L-a (* B-a 2)])
    (let/matrix [ (A[1][L-a #:bank B-a] )
                  (V[B-v #:bank B-v][(/ L-a B-v) #:bank (/ B-a B-v)]) ]
      (for ([i (in-range B-v)])
        (for* ([j (in-range (/ L-a B-v))])
          (V[i][j])
          (A[0][(split i j B-a (/ B-a B-v))]))))))

; Drop last element of the list
(define img-lst-outr
  (let ([tmp-lst (merge-imgs A-outr V-outr)])
    (take tmp-lst (sub1 (length tmp-lst)))))

(save-pict (scale (smart-layout img-lst-outr) 2) "split-outer.png" 'png)|#
