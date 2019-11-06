;; A tool to create input tables for use by 0cfa-flow.dl. Source
;; language is direct-style lambda calculus.
#lang racket

;; Tables
(define source-lambda (make-hash))
(define source-var-ref (make-hash))
(define source-application (make-hash))

;; Ids
(define var->id (make-hash))
(define/contract (lookup-or-new var)
  (any/c . -> . number?)
  (if (hash-has-key? var->id var)
      (car (hash-ref var->id var))
      (let ([id (newid)])
        (hash-set! var->id var `(,id ,var))
        id)))

(define x 0)
(define (newid) (let ([id x]) (set! x (add1 x)) id))

(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [`(lambda (,x) ,(? expr? e-body)) #t]))

;; Populate tables for a term 
(define (make-tables expr)
  (match expr
    [(? symbol? x)
     (let ([id (newid)])
       (hash-set! source-var-ref id `(,id ,(lookup-or-new x)))
       id)]
    [`(,e0 ,e1)
     (let* ([id (newid)]
            [fnid (make-tables e0)]
            [argid (make-tables e1)])
       (hash-set! source-application id `(,id ,fnid ,argid))
       id)]
    [`(lambda (,x) ,e-body)
     (let* ([id (newid)]
            [varid (lookup-or-new x)]
            [body-id (make-tables e-body)])
       (hash-set! source-lambda id `(,id ,varid ,body-id))
       id)]))

(define (dump-table table name)
  (for ([tuple (hash-values table)])
    (displayln (format "~a(~a)." name (string-join (map number->string tuple) ",")))))

;(make-tables `((lambda (x) x) (lambda (y) y)))
(make-tables `((lambda (x) (x x)) (lambda (y) (y y))))

(dump-table source-lambda "sourceLambda")
(dump-table source-var-ref "sourceVarRef")
(dump-table source-application "sourceApplication")
