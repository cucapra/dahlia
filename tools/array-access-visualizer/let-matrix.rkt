#lang racket/base

(require (for-syntax racket/base)
         racket/syntax
         racket/stxparam
         racket/contract
         racket/set
         "matrix-visualizer.rkt")

;; Defines `get` that adds the given index to the current set of accesses
;; made by the array.
(define/contract (get arr idxs)
  (-> (listof (set/c #:kind 'mutable (listof number?))) (non-empty-listof number?) void)
  (set-add! (car arr) idxs))

;; Add a new sequence to the array access.
(define-syntax-rule (seq-arr arr ...)
  (begin
    (set! arr (cons (mutable-set) arr))
    ...))

;; Turn instances of the syntax (id[i0][i1]...) to (get id i0 i1 ...).
;; Also defines id to be an error anywhere else.
;; (... ...) is used to escape the level two ... in the internal macro.
(define-syntax-rule (define-accessor-syntax id id-access)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(id (e1) (e) (... ...))
       #'(get id-access (list e1 e (... ...)))]
      [id (raise-syntax-error #f "can only used as an array access (arr[i][j]) and requires at least one accessor." stx)])))

;; Define (---) to be an error outside let/matrix contexts
(define-syntax-parameter ---
  (lambda (stx)
    (raise-syntax-error #f "can only be used inside let/matrix" stx)))

;; Define wrapper context that can tracks accesses into matrices and
;; generates a visualization for them. The matrice identifiers bound by the
;; context can only be used with the get syntax and are defined to be an
;; error everywhere else (TODO).
(define-syntax (let/matrix stx)
  (syntax-case stx ()
    [(_ [(id rows cols) ...] body ...)
     ;; Generate temporary names for structures that keep track of array accesses.
     (with-syntax ([(id-access ...) (generate-temporaries #'(id ...))])
       ;; Define structures to keep track to accesses into matrices
       #'(let ([id-access (list (mutable-set))] ...)
           ;; Define --- operator for this let/matrix context
           (syntax-parameterize ([--- (syntax-rules () [(_) (seq-arr id-access ...)])])
             ;; Define syntactic sugar M[i][j]... for array accesses
             (define-accessor-syntax id id-access) ...
             ;; Run the body
             body ...
             ;; Generate visualization for all accesses
             (values (matrix-seq rows cols (reverse id-access)) ...))))]))

;; Wrapper around for* that terminates each execution of the body with
;; a ---.
(define-syntax-rule (for ([id init] ...) body ...)
  (for* ([id init] ...)
    body
    ...
    (---)))

(provide let/matrix
         for
         ---
         (all-from-out "matrix-visualizer.rkt"))