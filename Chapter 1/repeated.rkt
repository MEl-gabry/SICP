#lang scheme

(define (square x)
  (* x x))

(define (even? n)
    (= (remainder n 2) 0))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter func k)
    (if (= k 1)
        func
        (iter (compose f func) (- k 1))))
  (iter f n))

((repeated square 2) 5)