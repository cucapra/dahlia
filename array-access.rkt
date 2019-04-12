#lang slideshow

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

;; Two dimensional array with dimensions [[rows]] and [[cols]] and
;; mark element in the list [[marks]].
(define (marked-matrix rows cols marks)
  (for/fold ([row-canvas (blank 0)])
            ([cur-row (in-range rows)])
    (vc-append
     row-canvas
     (for/fold ([col-canvas (blank 0)])
               ([cur-col (in-range cols)])
       (hc-append
        col-canvas
        (if (set-member? marks (list cur-row cur-col))
            (mark-elem arr-elem)
            arr-elem))))))


;; Generate a sequence of [[marked-matrix]] using the given paramter
;; [[seq]] which is a list of sets.
(define (matrix-seq row col seq)
  (map (lambda (marks) (marked-matrix row col marks)) seq))

;; ============ Syntax for the views language ====================
(require racket/syntax)

(define-syntax let/matrix
  (syntax-rules ()
    [(let/matrix [(id rows cols) ...] body)
     (let ([id '()] ...)
       (begin
         body
         (values (matrix-seq rows cols (reverse id)) ...)))]))

(define-syntax-rule (get arr row col)
  (set! arr (cons (mutable-set (list row col)) arr)))

(define (get-par arr row col)
  (set-add! (first arr) (list row col)))

(define-syntax-rule (for ([id init] ...) body ...)
  (begin (define-syntax-rule (get-seq arr row col) 'hi)
         (for* ([id init] ...)
           body ...)))
   
(let/matrix
 ([A 2 4]
  [B 1 8])
 (for ([i (in-range 1)]
       [j (in-range 4)])
   (get A i j)
   (get-par A i (+ j 1))
   (get B 0 j)))

;; "tests"
(define (split-access i j)
  (list 0 (+ (* i 2) (* 2 (floor (/ j 2))) (modulo j 2))))