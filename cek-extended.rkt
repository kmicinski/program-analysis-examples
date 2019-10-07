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
    [(? atom? ae) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [`(call/cc (lambda (,(? symbol? x)) ,(? expr? e-body))) #t]
    [`(if ,(? expr? e-guard) ,(? expr? e-true) ,(? expr? e-false)) #t]
    [else #f]))

(define (lit? l)
  (match l
    [(? number? n) #t]
    [`#t #t]
    [`#f #t]
    [else #f]))

(define (prim? l)
  (member l '(+ - * / =)))

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
    [`(prim-rhs ,(? prim? δ) ,(? expr? e₁) ,(? env? ρ) ,(? continuation? k-prime)) #t]
    [`(apply-prim ,(? prim? δ) ,(? value? v) ,(? continuation? k-prime)) #t]
    [else #f]))

;; Define states = C * E * K
(define (state? ς)
  (match ς
    [`(,(? expr? C) ,(? env? ρ) ,(? continuation? k)) #t]
    [else #f]))

;; Define inj : Expr -> State
(define (inj e)
  `(,e ,(hash) halt))

;; "Atomic" expressions
(define (atom? ae)
  (match ae
    [`(,(? prim? δ) ,(? atom? ae₀) ,(? atom? ae₁)) #t]
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
    [(? lit? l) l]
    [`(,(? prim? δ) ,ae₀ ,ae₁) ((convert-builtin δ) (aeval ae₀ ρ) (aeval ae₁ ρ))]))

;; Look up the implementation of a builtin function
(define (convert-builtin δ)
  (hash-ref (hash '+ + '- - '* * '/ / '= equal?)))

;; A "smarter" version of step that uses aeval
(define (step ς)
  (match ς
    ;; Application of a prim
    [`((,(? prim? δ) ,e₀ ,e₁) ,ρ ,k)
     `(,e₀ ,ρ (prim-rhs ,δ ,e₁ ,ρ ,k))]

    ;; To apply e0 to e1, step to e0 then come back to e1
    [`((,e0 ,e1) ,ρ ,k)
     `(,e0 ,ρ (ar ,e1 ,ρ ,k))]

    ;; Done evaluating function, got x
    [`(,(? atom? ae) ,ρ (ar ,e₁ ,ρ₁ ,k₁))
     `(,e₁ ,ρ₁ (fn ,(aeval ae ρ) ,k₁))]

    ;; Done evaluating argument, actually *apply* the function
    [`(,(? atom? ae) ,ρ (fn (clo (lambda (,y) ,e-body) ,ρ₁) ,k-prime))
     `(,e-body ,(extend-env ρ₁ y (aeval ae ρ)) ,k-prime)]
    
    ;; Done evaluating a LHS, swap to start evaluating RHS
    [`(,(? atom? ae) ,ρ (prim-rhs ,δ ,e₁ ,ρ-prime ,k))
     `(,e₁ ,ρ-prime (apply-prim ,δ ,(aeval ae ρ) ,k))]

    [`(,(? atom? ae) ,ρ (apply-prim ,δ ,v ,ρ-prime
                                    k))
     (let ([ans ((convert-builtin δ) v (aeval ae ρ))])
       (match k
         [`(fn (clo (lambda (,y) ,e-body) ,ρ₁) ,k-prime)
          `(,e-body ,(extend-env ρ₁ y ans) ,k-prime)]
         [`(prim-rhs ,δ-prime ,e₁ ,ρ₂ ,k)
          `(apply-prim ,δ-prime ,ans ,k)]
         [`(apply-prim ,δ-prime ,v ,k)
          
          ]))]))

;; Reflexive transitive closure of step
(define (step* ς)
  (define (h ς)
    (displayln (format "--> ~a" (pretty-format ς)))
    (match ς
      [`(,(? atom? ae) ,ρ halt) (aeval ae ρ)]
      [else
       (h (step ς))]))
  (displayln (format "Final answer: ~a" (pretty-format (h ς)))))

;; Run a repl
(define (repl)
  (define (loop)
    (display "> ")
    (let ([expr (read)])
      (if (expr? expr) 
          (begin
            (displayln "Evaluating...")
            (step* (inj expr)))
          (displayln "Error: not an expression."))))
  (loop))

(repl)

;; If I wanted to represent E as functions instead....
;;(define empty-env (lambda (x) (error "no such key")))
;;(define (extend-env ρ key value) (lambda (x) (if (equal? x key) value) (ρ x)))
;;(define (lookup-env ρ key) (ρ key))
