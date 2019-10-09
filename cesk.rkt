;; CESK interpreter for the direct-style lambda calculus
#lang racket

(define lit? number?)

(define (expr? e)
  (match e
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [(? symbol? x) #t]
    [(? lit? l) #t]
    [`(lambda (,x) ,(? expr? e-body)) #t]
    [else #f]))

(define (addr? x) (or (number? x) (equal? x '•)))

(define (env? ρ)
  (and (andmap symbol? (hash-keys ρ))
       (andmap addr? (hash-values ρ))))

(define (clo? clo)
  (match clo
    [`(clo (lambda (,x) ,e) ,(? env? ρ)) #t]
    [else #f]))

(define (continuation? k)
  (match k
    [`halt #t]
    [`(ar ,(? expr? e) ,(? env? ρ) ,(? addr? k)) #t]
    [`(fn ,(? clo? clo) ,(? addr? k)) #t]
    [else #f]))

(define (store? σ)
  (and (andmap addr? (hash-keys σ))
       (andmap (lambda (x) (or (continuation? x) (clo? x))) (hash-values σ))))

(define (state? ς)
  (match ς
    [`(,(? expr? c) ,(? env? ρ) ,(? store? σ) ,(? addr? α)) #t]
    [else #f]))

(define (inj e)
  `(,e ,(hash) ,(hash '• 'halt) •))


(define num-allocs 0)
(define (alloc σ)
  (set! num-allocs (add1 num-allocs))
  num-allocs)

(define (aexpr? ae)
  (match ae
    [`(lambda (,x) ,(? expr? e)) #t]
    [(? symbol? x) #t]
    [else #f]))

(define (aeval ae ρ σ)
  (match ae
    [(? symbol? x) (hash-ref σ (hash-ref ρ x))]
    [`(lambda (,x) ,e) `(clo ,ae ,ρ)]))

(define (step ς)
  (match ς
    [`((,e₀ ,e₁) ,ρ ,σ ,kα)
     (let* ([kα₁ (alloc σ)]
            [σ₁ (hash-set σ kα₁ `(ar ,e₁ ,ρ ,kα))])
       `(,e₀ ,ρ ,σ₁ ,kα₁))]
    [`(,(? aexpr? ae) ,ρ ,σ ,kα)
     (match (hash-ref σ kα) 
       [`(ar ,e ,ρ₁ ,kα₁)
        (let* ([kα₂ (alloc σ)]
               [σ₁ (hash-set σ kα₂ `(fn ,(aeval ae ρ σ) ,kα₁))])
          `(,e ,ρ₁ ,σ₁ ,kα₂))]
       [`(fn (clo (lambda (,x) ,e) ,ρ₁) ,kα)
        (let* ([α (alloc σ)]
               [σ₁ (hash-set σ α (aeval ae ρ σ))]
               [ρ₂ (hash-set ρ₁ x α)])
          `(,e ,ρ₂ ,σ₁ ,kα))])]))

(define (halting ς)
  (match ς
    [`(,(? aexpr? ae) ,ρ ,σ ,kα)
     (equal? (hash-ref σ kα) 'halt)]
    [else #f]))

(define (step-n e n)
  (define (iter n ς)
    (if (or (halting ς) (= n 0)) 
        (begin
           (pretty-print ς)
           (displayln "Done!"))
        (begin
          (pretty-print ς)
          (display "→")
          (iter (sub1 n) (step ς)))))
  (iter n (inj e)))

(define (repl)
  (display "> ")
  (let ([e (read)])
    ;; Evaluate...
    (step-n e 100)
    (repl)))

(repl)
