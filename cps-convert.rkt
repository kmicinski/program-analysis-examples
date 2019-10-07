;; Example CPS converter
#lang racket

(define (cps-convert e)
  (define (T-ae ae)
    (match ae
      [(? symbol? x) x]
      [`(lambda (,x) ,e0)
       (define k (gensym 'k))
       `(lambda (,k ,x) ,(T-e e0 k))]
      [else ae]))
  (define (T-e e cae)
    (match e
      [(? symbol? x)
       `(,cae ,x ,x)]
      [`(lambda . ,rest)
       `(,cae 0 ,(T-ae e))]
      [`(let ([,x ,e0]) ,e1)
       (define _x (gensym '_))
       (T-e e0 `(lambda (,_x ,x) ,(T-e e1 cae)))]
      [`(if ,ae ,e0 ,e1)
       `(if ,ae ,(T-e e0 cae) ,(T-e e1 cae))]
      [`(,aef ,aes ...)
       `(,(T-ae aef) ,cae ,@(map T-ae aes))]))
  (T-e e 'halt))

(cps-convert '(let ([x ((lambda (z) z) y)]) x))
