;; Homework 1, CIS 700
#lang racket

(define lit? number?)

;
; ANF conversion -- This is done for you.
; 

; Flanagan, et al, 1993 (Essence of compiling with continuations)
(define (anf-convert e)
  (define (normalize-ae e k)
    (normalize e (lambda (anf)
                   (match anf
                     [(? number? n) (k n)]
                     [(? symbol? x)
                      (k x)]
                     [`(lambda ,xs ,e0)
                      (k `(lambda ,xs ,e0))]
                     [else
                      (define ax (gensym 'a))
                      `(let ([,ax ,anf])
                         ,(k ax))]))))
  (define (normalize-aes es k)
    (if (null? es)
        (k '())
        (normalize-ae (car es) (lambda (ae)
                                 (normalize-aes (cdr es)
                                                (lambda (aes)
                                                  (k `(,ae ,@aes))))))))
  (define (normalize e k)
    (match e
      [(? number? n) (k n)]
      [(? symbol? x) (k x)]
      [`(lambda (,x) ,e0) (k `(lambda (,x) ,(anf-convert e0)))]
      [`(if ,e0 ,e1 ,e2)
       (normalize-ae e0 (lambda (ae)
                          (k `(if ,ae ,(anf-convert e1) ,(anf-convert e2)))))]
      [`(,es ...)
       (normalize-aes es k)]))
  (normalize e (lambda (x) x)))

(define (aexpr? e)
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
