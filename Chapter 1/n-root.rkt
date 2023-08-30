#lang scheme

(define (average x y)
  (/ (+ x y) 2))

(define (repeated f n)
  (define (iter func k)
    (if (= k 1)
        func
        (iter (compose f func) (- k 1))))
  (iter f n))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (n-root x n)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2)))) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(n-root 10000 4)