;; CESK* machine for lambda calculus
#lang racket

;; Predicates for CESK*
(define (expr? e)
  (match e
    [(? symbol?) #t]
    [`(,(? expr?) ,(? expr?)) #t]
    [`(lambda (,x) ,(? expr?)) #t]
    [else #f]))

(define addr? number?)

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
    [`(,(? expr?) ,(? env?) ,(? store?) ,(? addr?)) #t]
    [else #f]))

;;
;; Store handling
;;
(define addr 0)

;; Return a new address
(define (fresh-addr)
  (set! addr (add1 addr))
  addr)

;; Create a CESK* state from e
(define (inject e)
  (let ([a0 (fresh-addr)])
    `(,e ,(hash) ,(hash a0 'mt) ,a0)))

;; Examples
(define id-id '((lambda (x) x) (lambda (x) x)))
(define omega '((lambda (x) (x x)) (lambda (x) (x x))))

;; Step relation
(define (step state)
  (match state
    ;; Variable lookup
    [`(,(? symbol? x) ,env ,sto ,a) 
     (match (hash-ref sto (hash-ref env x))
       [`(clo (lambda (,x) ,body) ,rho-prime)
        `((lambda (,x) ,body) ,rho-prime ,sto ,a)])]
    ;; Application
    [`((,e0 ,e1) ,ρ ,σ ,a)
     (let* ([b (fresh-addr)]
            [new-k `(ar ,e1 ,ρ ,a)]
            [new-σ (hash-set σ b new-k)])
       `(,e0 ,ρ ,new-σ ,b))]
    ;; Lambdas...
    [`(,v ,ρ ,σ ,a)
     (let ([k (hash-ref σ a)]
           [b (fresh-addr)])
       (match k
         [`(ar ,e ,ρ1 ,c)
          `(,e ,ρ1 ,(hash-set σ b `(fn ,v ,ρ ,c)) ,b)]
         [`(fn (lambda (,x) ,e) ,ρ1 ,c)
          `(,e ,(hash-set ρ1 x b) ,(hash-set σ b `(clo ,v ,ρ)) ,c)]
         [else state]))]))

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

(repl)
