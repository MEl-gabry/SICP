#lang scheme

(define (iterative-improve good-enough improve)
  (define (try guess)
    (if (good-enough guess)
        guess
        (try (improve guess))))
  try)

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess)))) x))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- guess (f guess))) tolerance))
    (lambda (guess) (f guess))) first-guess))

(exact->inexact(sqrt 2))

(exact->inexact(fixed-point (lambda (y) (average y (/ 2 y)))
               1.0))
