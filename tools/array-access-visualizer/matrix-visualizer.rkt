#lang racket/base

(require racket/set)
(require pict)

;; Define a sickly orange color for visualizing array accesses.
(define creamsicle (list 255 153 83))

;; Define what a bare array element looks like
(define elem-size 12)
(define arr-elem
  (rectangle elem-size elem-size))

;; Define what a marking for an array element looks like
(define marking
  (let ([small-elem (- elem-size 2)])
    (colorize (rectangle small-elem small-elem) creamsicle)))

;; Marks the given element
(define (mark-elem elem)
  (cc-superimpose elem marking))

;; Calculate the bank number where [[idx]] is stored where [[len]]
;; is the total number of banks in the array.
(define (bank idx len)
  (modulo idx len))

;; Two dimensional version of bank
(define (matrix-bank row col row-bank col-bank)
  (+ (bank col col-bank) (* row-bank (bank row row-bank))))

;; Two dimensional array with dimensions [[rows]] and [[cols]] and
;; mark element in the list [[marks]].
(define (marked-matrix-with-banks rows cols
                                  #:row-banks [row-banks 1]
                                  #:col-banks [col-banks 1]
                                  #:marks [marks '()])
  (for/fold ([row-canvas (blank 0)])
            ([cur-row (in-range rows)])
    (vc-append
     row-canvas
     (for/fold ([col-canvas (blank 0)])
               ([cur-col (in-range cols)])
       (hc-append
        col-canvas
        (cc-superimpose
        (if (set-member? marks (list cur-row cur-col))
            (mark-elem arr-elem)
            arr-elem)
        (scale (text (number->string (matrix-bank cur-row cur-col row-banks col-banks))) 0.6)))))))

;; Generate a sequence of [[marked-matrix]] using the given paramter
;; [[seq]] which is a list of sets.
(define (matrix-seq row col seq)
  (map
   (lambda (marks) (marked-matrix-with-banks row col
                                  #:marks marks)) seq))

(provide matrix-seq)