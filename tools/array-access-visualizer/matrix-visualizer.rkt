#lang racket/base

(require racket/set
         racket/match
         racket/class
         racket/draw)
(require pict)

;; Define a sickly orange color for visualizing array accesses.
(define creamsicle
  (make-object color% 255 153 83))

;; Define what a bare array element looks like
(define elem-size 12)
(define arr-elem
  (filled-rectangle elem-size elem-size #:color "white"))

;; Define what a marking for an array element looks like
(define marking
  (let ([elem-size (- elem-size 2)])
    (rectangle elem-size elem-size #:border-color creamsicle #:border-width 2)))

;; Marks the given element
(define (mark-elem elem)
  (cc-superimpose elem marking))

;; Calculate the bank number where [[idx]] is stored where [[len]]
;; is the total number of banks in the array.
(define (bank idx len)
  (modulo idx len))

;; Two dimensional version of bank
(define (matrix-bank row col row-bank col-bank)
  (+ (bank col col-bank) (* col-bank (bank row row-bank))))

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
(define (matrix-seq dims banks seq)
  (match (list dims banks)
    [(list (list row-len col-len) (list row-bank col-bank))
     (map
      (lambda (marks) (marked-matrix-with-banks row-len col-len
                                                #:row-banks row-bank
                                                #:col-banks col-bank
                                                #:marks marks)) seq)]
    [_ raise-argument-error 'matrix-seq "list with two elements" (list dims banks)]))

(provide matrix-seq
         arr-elem ;; Used by smart layout.
         )