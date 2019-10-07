;; CPS-based lambda calculus interpreter
#lang racket

(define (aexp? exp)
  (match exp
    [`(lambda (,(? symbol? x) ...) ,(? cexp? e-body)) #t]
    [(? symbol? x) #t]
    [else #f]))

(define (cexp? exp)
  (match exp
    [`(,(? aexp?) ,(? aexp?) ...) #t]
    [(? aexp?) #t]
    [else #f]))

(define (aeval aexp ρ)
  (match aexp
    [`(lambda (,x ...) ,e-body) `(clo ,aexp ,ρ)]
    [(? symbol? x) (hash-ref ρ x)]))

(define (step state)
  (match state
    [`((,ae0 ,aes ...) ,ρ)
     (match (aeval ae0 ρ)
       [`(clo (lambda (,xs ...) ,e-body) ,ρ-prime)
        (let ([ρ-new (foldl (lambda (x v ρ-acc) (hash-set ρ-acc x v)) ρ-prime xs (map (lambda (x) (aeval x ρ)) aes))])
          `(,e-body ,ρ-new))])]))

(define ex-0
  '((lambda (halt)
      ((lambda (k28398 z) (k28398 z z))
       (lambda (_28397 x) (halt x x))
       (lambda (k28399 y) (k28399 y y))))
    (lambda (k x) x)))

(define (done? st)
  (match st
    [`((? aexp? ae) ,ρ) #t]
    [else #f]))

(define (step-n term n)
  (define (helper state n)
    (pretty-print state)
    (pretty-print (done? state))
    (if (or (= n 0) (done? state))
        (begin
           (displayln "Done!")
           (pretty-print (aeval (car state) (car (cdr state)))))
        (match-let ([`(,e-next ,ρ-next) (step state)])
          (displayln "-->")
          (pretty-print `(,e-next ,ρ-next))
          (helper `(,e-next ,ρ-next) (- n 1)))))
  (define initial-state `(,term ,(hash)))
  (pretty-print initial-state)
  (helper initial-state n))

(step-n ex-0 100)
