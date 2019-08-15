#lang racket

(require "let-matrix.rkt"
         "image-gen-helpers.rkt")
(require pict)

(define image-scale 5)

(define (merge-imgs A V)
  (zip-with
      (lambda (e1 e2) (vc-append 4 e1 e2))
      A V))

#|============== bank-1d.pdf =================|#
(let-values ([(A) (let/matrix [ (A[1][8 #:bank 4] ) ])])
  (save-pict (scale (car A) image-scale) "bank-1d.pdf"))

#|============== bank-2d.pdf =================|#
(let-values ([(A) (let/matrix [ (V[4 #:bank 2][4 #:bank 2]) ])])
  (save-pict
    (scale (car A) image-scale)
    "bank-2d.pdf"))

#|================ split.pdf ==================|#
;; For a given split view access v[i][j] where view has
;; B_v banks and the underlying array has B_a banks, returns
;; the index access on the corresponding underlying array.
(define (split i j B_a B_v)
  (+ (* i B_v) (modulo j B_v) (* B_a (floor (/ j B_v)))))

; Show what splitting looks like
(let-values
  ([(A-simpl V-simpl)
    (let* ([B-a 4]
           [B-v 2]
           [L-a (* B-a 2)])
      (let/matrix [ (A[1][L-a #:bank B-a] )
                   (V[B-v #:bank B-v][(/ L-a B-v) #:bank (/ B-a B-v)]) ]))])

  (save-pict
    (scale (car (merge-imgs A-simpl V-simpl)) image-scale) "split.pdf"))

#|================ SPLIT-OUTER.PDF ==================|#
; Show what splitting and parallelizing the inner loop looks like.
(let-values
  ([(A V)
    (let* ([B-a 4]
           [B-v 2]
           [L-a (* B-a 2)])
      (let/matrix [ (A[1][L-a #:bank B-a] )
                   (V[B-v #:bank B-v][(/ L-a B-v) #:bank (/ B-a B-v)]) ]
                  (for ([j (in-range (/ L-a B-v))])
                    (for* ([i (in-range B-v)])
                      (V[i][j])
                      (A[0][(split i j B-a (/ B-a B-v))])))))])

  ; Drop last element of the list
  (define img-lst
    (let ([tmp-lst (merge-imgs A V)])
      (take tmp-lst (sub1 (length tmp-lst)))))

  (parameterize ([max-elems (* arr-elem-len 16)]
                 [separator? #f])  ; Remove the ugly red line separator.
    (save-pict (scale (smart-layout img-lst) image-scale) "split-outer.pdf")))

#|===================== Suffix.pdf =================|#
(let-values
  ([(A)
    (let* ([B-a 2]
           [L-a 8])
      (let/matrix [ (A[1][L-a #:bank B-a] ) ]
                  (for ([j (add1 (/ L-a B-a))])
                    (for* ([i (* j B-a)])
                      (A[0][(- L-a i 1)])))))])
  (parameterize ([max-elems (* arr-elem-len 16)]
                 [separator? #f])
    (save-pict
      (scale
        (smart-layout (cdr (take A (sub1 (length A)))))
        image-scale)
      "suffix.pdf")))
