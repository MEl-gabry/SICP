#lang scheme
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
        ((filter a) (iter (next a) (combiner (term a) result)))
        (else (iter (next a) result))))
  (iter a null-value))

(define (sum-prime a b)
  (filtered-accumulate (lambda (x y) (+ x y)) prime? 0 (lambda(x) x) a (lambda (x) (+ x 1)) b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-rel-prime n)
  (define (rel-prime k)
    (if (= (gcd n k) 1)
        true
        false))
  (filtered-accumulate (lambda (x y) (* x y)) rel-prime 1 (lambda(x) x) 1 (lambda (x) (+ x 1)) (- n 1)))

(product-rel-prime 7)

