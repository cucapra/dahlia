#lang racket

(require "let-matrix.rkt"
         "image-gen-helpers.rkt")
(require pict)

(define image-scale 5)

(define (merge-imgs A V [len arr-elem-len])
  (zip-with
   (lambda (e1 e2) (hc-append len e1 e2))
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

  (define image-scale 3.8)

  ; Drop last element of the list
  (define img-lst
    (let* ([tmp-lst
            (map (lambda (img) (scale img image-scale)) (merge-imgs A V))]
           [final-len (sub1 (length tmp-lst))]
           [it-lst (build-list final-len
                               (lambda (idx) (scale
                                              (cc-superimpose
                                               (rectangle 40 15)
                                               (text (~a "i = " idx)
                                                     (cons 'italic null)))
                                              (/ image-scale 2))))]
           [new-tmp-lst (merge-imgs it-lst tmp-lst (* image-scale arr-elem-len))])

      (take new-tmp-lst final-len)))

  (parameterize ([max-elems (* arr-elem-len image-scale 20)]
                 [separator? #f])  ; Remove the ugly red line separator.
    (save-pict (smart-layout img-lst) "split-outer.pdf")))

#|===================== Suffix.pdf =================|#
(let-values
    ([(A)
      (let* ([B-a 2]
             [L-a 6])
        (let/matrix [ (A[1][L-a #:bank B-a] ) ]
                    (for ([j (add1 (/ L-a B-a))])
                      (for* ([i (in-range (* j B-a) L-a)])
                        (A[0][i])))))])
  (parameterize ([max-elems (* arr-elem-len 6)]
                 [vspace 2]
                 [separator? #f])
    (save-pict
     (scale
      (smart-layout (take A (sub1 (length A))))
      image-scale)
     "suffix.pdf")))
