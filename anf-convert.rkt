#lang racket

(provide (all-defined-out))

(define (fact n)
  (if (= 0 n)
      1
      (* n (fact (- n 1)))))

(define (cps-fact n k)
  (let ([c (= 0 n)])
    (if c
        (k 1)
        (let ([n-1 (- n 1)])
          (cps-fact n-1 (lambda (v)
                          (let ([n*v (* n v)])
                            (k n*v))))))))

(define (fib n)
  '(if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (cps-fib n k)
  (let ([c (<= n 1)])
    (if c
        (k n)
        (let ([n-1 (- n 1)])
          (cps-fib n-1 (lambda (v0)
                         (let ([n-2 (- n 2)])
                           (cps-fib n-2 (lambda (v1)
                                          (let ([sum (+ v0 v1)])
                                            (k sum)))))))))))


(define (anf-convert e)
  (define (normalize-ae e k)
    ...)
  (define (normalize-aes es k)
    ...)
  (define (normalize e k)
    (match e
      [(? number? n) (k n)]
      [(? symbol? x) (k x)]
      [`(lambda (,x) ,e0) (k `(lambda (,x) ,(anf-convert e0)))]
      [`(if ,e0 ,e1 ,e2)
       (normalize-ae e0
                     (lambda (ae)
                       (k `(if ,ae ,(anf-convert e1) ,(anf-convert e2)))))]
      [`(,es ...)
       (normalize-aes es k)]))
  (normalize e (lambda (x) x)))

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
