#lang racket/base
(require redex)
(require (only-in racket/pretty
                  pretty-format))

(define-language Filament
  (e ::=
     n
     (let (x n e) e)
     (set! x e e) ;; x[e] := e
     (get x e) ;; x[e]
     (bop op e e)
     (if0 e e e)
     (seq e e ...)
     (par e e ...)
     (while e e))
  (x ::= variable-not-otherwise-mentioned)
  (n ::= natural)
  (op ::= + - / * % == != < <= > >= >> << and or b-and b-or))

;; Recognize terms in the Filament language
(define filament?
  (redex-match? Filament e))

;; Macro for multiple let-bindings for one body
(define-metafunction Filament
  let* : ((x n e) ...) e e -> e
  [(let* ([x n e] [x_1 n_1 e_1] ...) e_b)
   (let (x n e) (let* ([x_1 n_1 e_1] ...) e_b))]
  [(let* ([x n e]) e_b)
   (let (x n e) e_b)])

;; Define extended language for values, evaluation contexts, and stores.
(define-extended-language Filament-ev Filament
  (e ::= .... v)
  (v ::= n void)
  (sto ::= ((x n v) ...))
  (E ::=
     hole
     (let (x n E) e)
     (set! x E e)
     (set! x v E)
     (get x E)
     (bop op E e)
     (bop op v E)
     (if0 E e e)
     (par e ... E e ...) ;; This is non-determinstic!!
     (seq E e ...))
     (while E e))

;; Helper functions for stores
;; Lookup : sto -> x -> index -> v
(define-metafunction Filament-ev
  lookup : any x n -> any
  [(lookup ((x_1 n_1 any_1) ... (x n any_t) (x_2 n_2 any_2) ...) x n)
   any_t
   (side-condition (not (member (term (x n)) (term ((x_1 n_1) ...)))))]
  [(lookup any_1 x n)
   ,(error 'lookup "not found: ~e in: ~e" (term (x n)) (term any_1))])

;; add : sto -> x -> index -> v -> sto
;; Errors out if the declaration is already in the store
(define-metafunction Filament-ev
  add : sto x n v -> sto
  [(add ((x_1 n_1 any_1) ... (x n v) (x_2 n_2 any_2) ...) x n v)
   ,(error 'add "~e already in: ~e" (term (x n)) (term sto))]
  [(add ((x_1 n_1 any_1) ...) x n v)
   ((x n v) (x_1 n_1 any_1) ...)
   (side-condition (not (member (term (x n)) (term ((x_1 n_1) ...)))))])

;; update : sto -> x -> index -> v -> sto
;; Errors out if the declaration is not in the store
(define-metafunction Filament-ev
  update : sto x n v -> sto
  [(update ((x_1 n_1 any_1) ... (x n any_2) (x_2 n_2 any_3) ...) x n v)
   ((x_1 n_1 any_1) ... (x n v) (x_2 n_2 any_3) ...)]
  [(update ((x_1 n_1 any_1) ...) x n v)
   ,(error 'update "not found: ~e in: ~e" (term (x n)) (term sto))])

;; Metafunction to help apply operators
(define-metafunction Filament-ev
  binop-app : op n n -> n
  [(binop-app + n_1 n_2) ,(+ (term n_1) (term n_2))]
  [(binop-app * n_1 n_2) ,(* (term n_1) (term n_2))]
  [(binop-app - n_1 n_2) ,(- (term n_1) (term n_2))]
  [(binop-app / n_1 n_2) ,(/ (term n_1) (term n_2))])

;; Reduction relation over the language
(define filament-->>
  (reduction-relation
    Filament-ev
    #:domain (e sto)
    (--> [(in-hole E (get x n)) sto]
         [(in-hole E (lookup sto x n)) sto]
         "get")
    (--> [(in-hole E (set! x n v)) sto]
         [(in-hole E void) (update sto x n v)]
         "set!")
    (--> [(in-hole E (let (x n v) e)) sto]
         [(in-hole E e) (add sto x n v)]
         "let-add")
    (--> [(in-hole E (bop op n_1 n_2)) sto]
         [(in-hole E (binop-app op n_1 n_2)) sto]
         "binop")
    (--> [(in-hole E (if0 0 e_1 e_2)) sto]
         [(in-hole E e_1) sto]
         "if-true")
    (--> [(in-hole E (if0 n e_1 e_2)) sto]
         [(in-hole E e_2) sto]
         (where #false ,(= (term n) 0))
         "if-false")
    (--> [(in-hole E (seq v e_1 e ...)) sto]
         [(in-hole E (seq e_1 e ...)) sto]
         "seq-step")
    (--> [(in-hole E (seq v)) sto]
         [(in-hole E v) sto]
         "seq-end")
    (--> [(in-hole E (while 0 e)) sto]
         [(in-hole E void) sto]
         "while-false")
    (--> [(in-hole E (while n e)) sto]
         [(in-hole E (seq e (while n e)) sto)]
         (where #false ,(= (term n) 0))
         "while-true")))
  
;; Helper function to show the trace of a program
;; starting with the empty store.
(define (filament-apply-reduction reducer prog)
  (if (not (filament? prog))
      (error 'filament-trace "~e is not a valid Filament program" prog)
  (reducer filament-->>
          (term (,prog ())))))

(define (filament-trace prog)
  (filament-apply-reduction traces prog))

(define (filament-step prog)
  (filament-apply-reduction stepper prog))

;;================== TESTS ===========================

;; Helper terms
(define add-A-0
  (term (bop + (get A 0) (get A 0))))

(define simple-get-test
  (term
    (let [A 0 1] ,add-A-0)))

(define simple-set
  (term
    (let [A 0 0]
      (seq
        (set! A 0 10)
        ,add-A-0))))

(define parallel-set
  (term
    (let [A 0 0]
      (par
        (set! A 0 10)
        ,add-A-0))))


;; Playing with the language
;; Use (filament-traces prog) to generate ALL possible reductions for the give program (Uncomment line below)
;; (filament-trace parallel-set)

;; Use (filament-step prog) to open GUI for single step exploring the language reductions (Uncomment line below)
;; (filament-step parallel-set)