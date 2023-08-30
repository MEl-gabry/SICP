#lang scheme

(define dx 0.7)

(define (repeated f n)
  (define (iter func k)
    (if (= k 1)
        func
        (iter (compose f func) (- k 1))))
  (iter f n))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold f n)
  ((repeated smooth n) f))
