;; Constraint-based analysis for direct-style 0CFA
#lang racket

(provide (all-defined-out))

(define (constraint? constraint)
  (match constraint
    [`(⊆ ,s0 ,s1) #t]
    [`(∈ ,element ,st) #t]
    [`(← (⊆ ,s0 ,s1) ,(? constraint? bodies) ...) #t]
    [`(← (∈ ,e ,s) ,(? constraint? bodies) ...) #t]
    [else #f]))

(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e-body)) #t]
    [else #f]))

(define (solve-constraints constraints)
  (define (body-true body h)
    (match body
      [`(⊆ ,s0 ,s1) (subset? (hash-ref h s0 (set)) (hash-ref h s1 (set)))]
      [`(∈ ,e ,st) (set-member? (hash-ref h st (set)) e)]))
  (define (iter h)
    (let ([next
           (foldl
            (lambda (constraint h)
              (match constraint
                [`(∈ ,e ,s) (hash-set h s (set-add (hash-ref h s (set)) e))]
                [`(⊆ ,s0 ,s1) (hash-set h s1 (set-union (hash-ref h s0 (set)) (hash-ref h s1 (set))))]
                [`(← (⊆ ,s0 ,s1) ,(? constraint? bodies) ...)
                 (if (andmap (lambda (body) (body-true body h)) bodies)
                     (hash-set h s1 (set-union (hash-ref h s0 (set)) (hash-ref h s1 (set))))
                     h)]
                [`(← (∈ ,e ,s) ,(? constraint? bodies) ...)
                 (if (andmap (lambda (body) (body-true body h)) bodies)
                     (hash-set h s (set-add (hash-ref h s (set)) e))
                     h)]))
            h
            (set->list constraints))])
      (if (equal? next h) h (iter next))))
  (iter (hash)))

;; Example: solving set constraints
(solve-constraints (set `(∈ 1 s0) `(∈ 2 s0) `(⊆ s0 s1) `(∈ 3 s1) `(⊆ s1 s2) `(← (⊆ s2 s3) (∈ 1 s2))))

((lambda (x) x) (lambda (y) y))

(pretty-print
 (solve-constraints
  (set `(∈ (lambda (x) x) (flow-set (lambda (x) x)))
       `(∈ (lambda (y) y) (flow-set (lambda (y) y)))
       `(← (∈ (lambda (x) x) (flow-set x))
           (∈ (lambda (x) x) (flow-set (lambda (x) x)))
           (∈ (lambda (x) x) (flow-set (lambda (y) y))))
       `(← (∈ (lambda (y) y) (flow-set x))
           (∈ (lambda (x) x) (flow-set (lambda (x) x)))
           (∈ (lambda (y) y) (flow-set (lambda (y) y))))
       `(← (∈ (lambda (x) x) (flow-set y))
           (∈ (lambda (y) y) (flow-set (lambda (x) x)))
           (∈ (lambda (x) x) (flow-set (lambda (y) y))))
       `(← (∈ (lambda (y) y) (flow-set y))
           (∈ (lambda (y) y) (flow-set (lambda (x) x)))
           (∈ (lambda (y) y) (flow-set (lambda (y) y))))
       `(← (∈ (lambda (x) x) (flow-set ((lambda (x) x) (lambda (y) y))))
           (∈ (lambda (x) x) (flow-set (lambda (x) x)))
           (∈ (lambda (x) x) (flow-set x)))
       `(← (∈ (lambda (y) y) (flow-set ((lambda (x) x) (lambda (y) y))))
           (∈ (lambda (x) x) (flow-set (lambda (x) x)))
           (∈ (lambda (y) y) (flow-set x))))))
