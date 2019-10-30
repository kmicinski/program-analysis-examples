;; Abstract CESK* machine for lambda calculus
#lang racket

(provide (all-defined-out))

;; Predicates for CESK*
(define (expr? e)
  (match e
    [(? symbol? var) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [`(lambda (,x) ,(? expr?)) #t]
    [else #f]))

(define (addr? α) #t)

(define (env? e)
  (and (andmap (lambda (key) (symbol? key)) (hash-keys e))
       (andmap addr? (hash-values e))))

(define (kont? k)
  (match k
    ['mt #t]
    [`(ar ,(? expr?) ,(? env?) ,(? addr?)) #t]
    [`(fn (lambda (,x) ,(? expr?)) ,(? env?) ,(? addr?)) #t]))

(define (storable? v)
  (match v
    [`(clo (lambda (,x) ,(? expr?)) ,(? env?)) #t]
    [(? kont?) #t]))

(define (store? e)
  (and (andmap addr? (hash-keys e))
       (andmap storable? (hash-values e))))

(define (cesk*-state? state)
  (match state
    [`(,(? expr? c) ,(? env? ρ) ,(? store? σ) ,(? addr? ka)) #t]
    [else #f]))

;; Create a CESK* state from e
(define (inject e)
  `(,e ,(hash) ,(hash '• (set 'mt)) •))

;; σ ⊔ {a ↦ v} = σ' such that σ'(x) = σ(x) if x <> a
;;                            σ'(x) = {v} ∪ σ(x) if x = a
(define (store-extend σ a v)
  (hash-set σ a (set-add (hash-ref σ a (set)) v)))

;; Examples
(define id-id '((lambda (x) x) (lambda (x) x)))
(define omega '((lambda (x) (x x)) (lambda (x) (x x))))

;; Step relation
;; ς^ → set(ς')
(define (step state)
  (match state
    ;; Variable lookup
    [`(,(? symbol? x) ,ρ ,σ ,a)
     (let ([all-closures+continuations (hash-ref σ (hash-ref ρ x))])
       (foldl (lambda (closure-or-continuation output-states)
                (match closure-or-continuation
                  [`(clo (lambda (,x) ,e) ,ρ-prime)
                   (set-add output-states `((lambda (,x) ,e) ,ρ-prime ,σ ,a))]
                  [else
                   output-states]))
              (set)
              (set->list all-closures+continuations)))]
    ;; Application
    [`((,e0 ,e1) ,ρ ,σ ,a)
     (let ([b `(,e0 ,e1)])
       (set `(,e0 ,ρ ,(store-extend σ b `(ar ,e1 ,ρ ,a)) ,b)))]

    ;; Lambdas...
    [`(,v ,ρ ,σ ,a)
     (let ([all-closures+continuations (hash-ref σ a)])
       (foldl (lambda (closure-or-continuation output-states)
                (match closure-or-continuation
                   [`(ar ,e ,ρ-prime ,c)
                    (let ([b e])
                      (set-add output-states `(,e ,ρ-prime ,(store-extend σ b `(fn ,v ,ρ ,c)) ,b)))]
                   [`(fn (lambda (,x) ,e) ,ρ-prime ,c)
                    (let* ([α x]
                           [ρ-prime-prime (hash-set ρ-prime x α)]
                           [σ-prime (store-extend σ α `(clo ,v ,ρ))])
                      (set-add output-states `(,e ,ρ-prime-prime ,σ-prime ,c)))]
                   [else
                    output-states]))
              (set)
              (set->list all-closures+continuations)))]))

(define (iterate state)
  (displayln "Iterating state...")
  (pretty-print state)
  (let ([next-state (step state)])
    (if (equal? next-state state)
        ;; Done
        (displayln "Done w/ evaluation.")
        (iterate next-state))))

(define (repl)
  (displayln "Type an expression...")
  (display "> ")
  (let ([input (read)])
    ;; Execute the expression
    (iterate (inject input))
    (repl)))

;(repl)

