#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         racket/class
         racket/list
         racket/math
         racket/draw
         pict)

(require (only-in "matrix-visualizer.rkt"
                  arr-elem-len))

(provide layout
         smart-layout
         vspace
         save-pict
         zip-with
         max-elems
         separator?)

(define max-elems (make-parameter (* arr-elem-len 96)))

(define separator? (make-parameter #t))

(define vspace (make-parameter 4))

;; Save a given pict with the name and a kind.
;; Usage: (save-pict pict "foo.pdf")
(define (save-pict the-pict path)
  (define bm (pict->bitmap the-pict))
  (define pdf (new pdf-dc% [output path]
                           [interactive #f]
                           [width (pict-width the-pict)]
                           [height (pict-height the-pict)]))
  (send* pdf
    (start-doc "starting doc")
    (start-page)
    (draw-bitmap bm 0 0)
    (end-page)
    (end-doc))
  the-pict)

;; Given a list, create a list of lists, each of
;; size n. If the length of the list is not divisible by n, throws an error.
(define (split-list lst n)
  (define (split-list-helper lst n)
    (if (= (length lst) 0)
        '()
        (cons (take lst n) (split-list-helper (drop lst n) n))))
  (cond
    [(not (= (modulo (length lst) n) 0))
     (raise-argument-error 'split-list "List length not divided by splitting factor" (length lst))]
    [else (split-list-helper lst n)]))

;; Generates an hline for separating two rows
(define (separator len)
  (if (separator?)
    (colorize (hline len 8) "Red")
    (blank 0)))

;; Layout a given list of images by placing [[factor]] of them in each row
;; and creating as many vertically stacked rows as needed.
(define (layout img-lst factor)
  (define remainder (modulo (length img-lst) factor))
  (define split
    (cond
      [(not (= remainder 0))
       (split-list (append img-lst (build-list (- factor remainder) (lambda (_) (blank 0)))) factor)]
      [else (split-list img-lst factor)]))

  (for/fold ([row-acc (blank 0)])
            ([row split])
    (define row-res
      (for/fold ([col-acc (blank 0)])
                ([img row])
        (hc-append 10 col-acc img)))
    (define sep (separator (pict-width row-res)))
    (vc-append (vspace) row-acc sep row-res)))

;; Layout that automatically tried to figure out how many list elements
;; can be places in one row.
(define (smart-layout img-lst)
  ;; Length of one array as defined in matrix-visualizer library
  (let* (;; Length of the current image. Assumes that widthds of all elems is
         ;; the same.
         [width (pict-width (car img-lst))]
         [factor (exact-floor (/ (max-elems) width))])
    (if (zero? factor)
        (raise-result-error 'smart-layout
                            "Image element too big to be smart layout. Use layout instead"
                            factor)
        (layout img-lst factor))))

;; Zips up an arbitrary sequence of list with a give function.
;; Usage: (zip-with (lambda (x y z) (list x y z)) (list 1 2 3) (list 'a 'b 'c) (list 4 5 6))
(define-syntax (zip-with stx)
  (syntax-parse stx
    [(_ f:expr l1:expr l2:expr ...)
     (with-syntax ([(e2 ...) (generate-temporaries #'(l2 ...))])
       #'(for/fold [(acc (list)) #:result (reverse acc)]
                   [(e1 l1)
                    (e2 l2) ...]
           (cons (f e1 e2 ...) acc)))]))
