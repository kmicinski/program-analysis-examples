;; k-CFA for a direct-style lambda calculus
;;
;; This analysis includes the following features
;; - Direct-style lambda calculus
;; - Partitioned into Eval and Return states for evaluation
;; - P4F-style continuation allocation
;; - k-CFA ala instrumentation
;; - Alphatizes expressions via labeling each with a unique tag
;;
;; Kris Micinski, Fall 2019. Please report bugs: krismicinski@gmail.com
#lang racket

;; Parameter for k-CFA
(define cfa-k (make-parameter 3))

;; Language
(define (expr? e)
  (match e
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [(? symbol? x) #t]
    [`(lambda (,x) ,(? expr? body)) #t]
    [else #f]))

(define (labeled-expr? e)
  (match e
    [`(,(? symbol? n) ,(? labeled-expr? e0) ,(? labeled-expr? e1)) #t]
    [`(,(? symbol? n) ,(? symbol? x)) #t]
    [`(,(? symbol? n) lambda (,x) ,(? labeled-expr? body)) #t]
    [else #f]))

(define (label-of e)
  (match e
    [`(,(? symbol? n) ,(? labeled-expr? e0) ,(? labeled-expr? e1)) n]
    [`(,(? symbol? n) ,(? symbol? x)) n]
    [`(,(? symbol? n) lambda (,x) ,(? labeled-expr? body)) n]
    [else #f]))

(define (expr->labeled-expr e)
  (match e
    [`(,(? expr? e0) ,(? expr? e1)) `(,(gensym) ,(expr->labeled-expr e0) ,(expr->labeled-expr e1))]
    [(? symbol? x) `(,(gensym) ,x)]
    [`(lambda (,x) ,(? expr? body)) `(,(gensym) lambda (,x) ,(expr->labeled-expr body))]))

(define (label? l)
  (match l
    ['• #t]
    [(? symbol? s) #t]))

;; Instrumentation is lists of length l where each element is a label
(define (instr? l) (list? l))

;; Addresses are either halt, continuations, or variables paired with
;; contours.
(define (addr? a)
  (match a
    ;; halt
    ['• #t]
    ;; P4F style continuation allocation. This continuation allocator
    ;; follows the formulation in "Pushdown for Free" by Gilray
    ;; et. al.
    [`(,(? labeled-expr? e) ,(? env? ρ) ,(? instr?)) #t]
    ;; Value addresses are variables (and their labels) along with a
    ;; contour of length k. We store both variable and label to
    ;; disambiguate multiple instances of variables with the same
    ;; syntactic name.
    [`(,(? symbol? x) ,(? instr?)) #t]
    [else #f]))

;; Continuations are either halt, argument, or apply
(define (kont? k)
  (match k
    [`(ar ,(? labeled-expr? e) ,(? env? ρ) ,(? addr? ka) (? instr?)) #t]
    [`(fn ,(? value? clo) ,(? addr? ka) ,(? instr?)) #t]
    ['halt #t]))

;; Environments map variables to addresses
(define (env? ρ)
  (and (andmap symbol? (hash-keys ρ))
       (andmap addr? (hash-values ρ))))

;; Stores map addresses to values
(define (sto? σ)
  (and (andmap addr? (hash-keys σ))
       (andmap (or/c value? kont?) (hash-values σ))))

;; Value allocation
(define (alloc ς x)
  (match ς
    [`(E ,_ ,_ ,_ ,_ ,i) `(,x ,i)]
    [`(T ,_ ,_ ,_ ,i) `(,x ,i)]))

(define (kalloc e ρ ς)
  (define i (match ς
              [`(E ,_ ,_ ,_ ,_ ,i) i]
              [`(T ,_ ,_ ,_ ,i) i]))
  `(,e ,ρ ,i))

(define (value? v)
  (match v
    [`(clo (lambda (,x) (? labeled-expr? body)) ,(? env? ρ)) #t]
    [else #f]))

;; We have two types of states: eval states and return states. Return
;; states are used to avoid having to put values in the control
;; position of the machine.
(define (state? state)
  (match state
    [`(E ,(? labeled-expr?) ,(? env?) ,(? sto?) ,(? addr?) ,(? instr?)) #t]
    [`(T ,(? value? v) ,(? sto?) ,(? addr?) ,(? instr?)) #t]
    [else #f]))

;; Inject the initial state
(define (inj e)
  `(E ,e ,(hash) ,(hash '• 'halt) • ,(map (lambda (i) '•) (range (cfa-k)))))

(define (store-update σ a v)
  (hash-set σ a (set-add (hash-ref σ a (set)) v)))

(define (step ς)
  (displayln "Stepping...")
  (pretty-print ς)
  (match ς
    ;; Handle a syntactic variable
    [`(E (,_ ,x) ,ρ ,σ ,ka ,i)
     ;; Transition to return frame w/ same instrumentation
     (list->set (foldl (lambda (v acc) (set-add acc `(T ,v ,σ ,ka ,i)))
                       (set)
                       (set->list (hash-ref σ (hash-ref ρ x)))))]
    ;; Handle a syntactic lambda
    [`(E (,l lambda (,x) ,e-body) ,ρ ,σ ,ka ,i)
     (set `(T (clo (,l lambda (,x) ,e-body) ,ρ) ,σ ,ka ,i))]
    ;; Evaluate an application
    [`(E (,_ ,e0 ,e1) ,ρ ,σ ,ka ,i)
     (let* ([ka1 (kalloc e0 ρ ς)]
            [k `(ar ,e1 ,ρ ,ka ,i)]
            [i-next (cons (label-of e0) (reverse (cdr (reverse i))))]
            [σ1 (store-update σ ka1 k)])
       (set `(E ,e0 ,ρ ,σ1 ,ka1 ,i-next)))]
    ;; Handle a return
    [`(T ,v ,σ ,ka ,i)
     ;; Lookup the continuations in the store
     (foldl
      (lambda (k resultant-states)
        (match k
          [`(ar ,e-next ,ρ ,ka ,i)
           (let* ([ka1 (kalloc e-next ρ ς)]
                  [k `(fn ,v ,ka ,i)]
                  [i-next (cons (label-of e-next) (reverse (cdr (reverse i))))]
                  [σ1 (store-update σ ka1 k)])
             (set `(E ,e-next ,ρ ,σ1 ,ka1 ,i-next)))]
          [`(fn ,clo ,ka ,i)
           (match-let* ([`(clo (,l lambda (,x) ,e) ,ρ) clo]
                        [α (alloc ς x)]
                        [σ1 (store-update σ α v)]
                        [i-next (cons l (reverse (cdr (reverse i))))]
                        [ρ-next (hash-set ρ x α)])
             (set `(E ,e ,ρ-next ,σ1 ,ka ,i-next)))]))
      (set)
      (set->list (hash-ref σ ka)))]))

(define ex0 `((lambda (x) (x x)) (lambda (x) (x x))))

;; Iterate an exression to a frontier
(define (iter expr)
  (define initial-hash (hash (inj (expr->labeled-expr ex0)) (set)))
  (define (loop states-hash)
    (let ([new (foldl (lambda (state states-hash)
                        (let* ([next-states (step state)]
                               [next-hash (hash-set states-hash state (set-union (hash-ref states-hash state (set))
                                                                                 next-states))])
                          (foldl
                           (lambda (next-state next-hash)
                             (hash-set next-hash next-state (set-union (hash-ref next-hash next-state (set)) (set))))
                           next-hash
                           (set->list next-states))))
                      states-hash
                      (hash-keys states-hash))])
      (if (equal? new states-hash) states-hash (loop new))))
  ;; Iterate to a frontier
  (loop initial-hash))

(iter ex0)
