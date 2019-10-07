;; CEK Machine
#lang racket

(provide
 expr?
 env?
 value?
 continuation?
 state?
 inj
 step
 step*)

;; Define builtin
(define (builtin? δ)
  (member δ '(+ - * / =)))

;; Define C
(define (expr? c)
  (match c
    [(? symbol? x) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e-body)) #t]
    [`(call/cc (lambda (,(? symbol? x)) ,(? expr? e-body))) #t]
    [(? number? n) #t]
    [`#t #t]
    [`#f #t]
    [`(if ,(? expr? e-guard) ,(? expr? e-true) ,(? expr? e-false)) #t]
    [`(,(? builtin? δ) ,e₀ ,e₁) #t]
    [else #f]))

;; Define E
(define empty-env (hash))
(define (extend-env ρ key value) (hash-set ρ key value))
(define (lookup-env ρ key) (hash-ref ρ key))

(define (env? ρ)
  (and (andmap symbol? (hash-keys ρ))
       (andmap value? (hash-values ρ))))

;; Define values
(define (value? v)
  (match v
    [`(clo (lambda (,x) ,e-body) ,(? env? ρ)) #t]
    [(? number? n) #t]
    [#t #t]
    [#f #t]
    [else #f]))

;; Define K
(define (continuation? k)
  (match k
    ['halt #t]
    [`(ar ,(? expr? e) ,(? env? ρ) ,(? continuation? k-prime)) #t]
    [`(fn ,(? value? v) ,(? continuation? k-prime)) #t]
    [else #f]))

;; Define states = C * E * K
(define (state? ς)
  (match ς
    [`(,(? expr? C) ,(? env? ρ) ,(? continuation? k)) #t]
    [else #f]))

;; Define inj : Expr -> State
(define (inj e)
  `(,e ,(hash) halt))

;; This is a correct but naive step function, because it repeats the
;; work of atomic evaluation.
(define (step-basic ς)
  (match ς
    ;; To apply e0 to e1, step to e0 then come back to e1
    [`((,e0 ,e1) ,ρ ,k)
     `(,e0 ,ρ (ar ,e1 ,ρ ,k))]

    ;; Done evaluating function, got x
    [`(,(? symbol? x) ,ρ (ar ,e₁ ,ρ₁ ,k₁))
     `(,e₁ ,ρ₁ (fn ,(lookup-env ρ x) ,k₁))]

    [`((lambda (,x) ,e-body) ,ρ (ar ,e₁ ,ρ₁ k₁))
     `(,e₁ ,ρ₁ (fn (clo (lambda (,x) ,e-body) ,ρ) k₁))]

    ;; Done evaluating argument, actually *apply* the function
    [`(,(? symbol? x) ,ρ (fn (clo (lambda (,y) ,e-body) ,ρ₁) ,k-prime))
     `(,e-body ,(extend-env ρ₁ y (lookup-env ρ x)) ,k-prime)]

    [`((lambda (,x) ,e-body) ,ρ (fn (clo (lambda (,y) ,e-body) ,ρ₁) ,k-prime))
     `(,e-body ,(extend-env ρ₁ y `(clo (lambda (,x) ,e-body) ,ρ)) ,k-prime)]))

;; "Atomic" expressions
(define (atom? ae)
  (match ae
    [(? symbol? x) #t]
    [`(lambda (,x) ,e-body) #t]
    [(? number? n) #t]
    [`#t #t]
    [`#f #t]
    [else #f]))

;; Turn an ae into a value (closure)
(define (aeval ae ρ)
  (match ae
    [(? symbol? x) (lookup-env ρ x)]
    [`(lambda (,x) ,e-body) `(clo ,ae ,ρ)]
    [(? number? n) n]
    [`#t #t]
    [`#f #t]))

;; A "smarter" version of step that uses aeval
(define (step ς)
  (match ς
    ;; To apply e0 to e1, step to e0 then come back to e1
    [`((,e0 ,e1) ,ρ ,k)
     `(,e0 ,ρ (ar ,e1 ,ρ ,k))]

    ;; Done evaluating function, got x
    [`(,(? atom? ae) ,ρ (ar ,e₁ ,ρ₁ ,k₁))
     `(,e₁ ,ρ₁ (fn ,(aeval ae ρ) ,k₁))]

    ;; Done evaluating argument, actually *apply* the function
    [`(,(? atom? ae) ,ρ (fn (clo (lambda (,y) ,e-body) ,ρ₁) ,k-prime))
     `(,e-body ,(extend-env ρ₁ y (aeval ae ρ)) ,k-prime)]))

;; Reflexive transitive closure of step
(define (step* ς)
  (define (h ς)
    (displayln (format "--> ~a" (pretty-format ς)))
    (match ς
      [`(,(? atom? ae) ,ρ halt) (aeval ae ρ)]
      [else
       (h (step ς))]))
  (displayln (format "Final answer: ~a" (pretty-format (h ς)))))

;; If I wanted to represent E as functions instead....
;;(define empty-env (lambda (x) (error "no such key")))
;;(define (extend-env ρ key value) (lambda (x) (if (equal? x key) value) (ρ x)))
;;(define (lookup-env ρ key) (ρ key))
