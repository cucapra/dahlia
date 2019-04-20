#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
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
;; error everywhere else.
(define-syntax (let/matrix stx)

  ;; Syntax class for array dimension specification for arrays.
  ;; If the optional parameter #:bank is not specified, the default
  ;; is set to 1.
  ;; Fails if the bank factor does not divide the length.
  (define-syntax-class array-dimension
    #:description "array dimension in definiton"
    (pattern (len:expr (~optional (~seq #:bank bank-opt:expr)))
             #:with bank (if (attribute bank-opt) #'bank-opt #'1)))

  (syntax-parse stx
    [(_ [(id:id dims:array-dimension ...) ...] body:expr ...)
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
             (values (matrix-seq (list dims.len ...) (list dims.bank ...) (reverse id-access)) ...))))]))

;; Wrapper around for* that terminates each execution of the body with
;; a ---.
(define-syntax-rule (for ([id init] ...) body ...)
  (for* ([id init] ...)
    body
    ...
    (---)))

;; Simple while loops without support for break or continue.
(define-syntax-rule (while test body ...)
  (begin
    (define (loop)
      (cond
        [test body ... (---) (loop)]
        [else #f]))
    (loop)))

(provide let/matrix
         for
         while
         ---
         (all-from-out "matrix-visualizer.rkt"))
