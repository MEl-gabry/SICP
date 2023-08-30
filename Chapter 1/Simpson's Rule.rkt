#lang scheme
(define (even? n)
  (= (remainder n 2) 0))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simp f a b n)
  (define h (/ (- b a) n))
  (define (identity k)
    (define fk (f (+ a (* k h))))
    (cond ((or (= k 0) (= k n)) fk)
          ((even? k) (* 2 fk))
          (else (* 4 fk))))
  (* (sum identity 0 (lambda (x) (+ x 1)) n) (/ h 3)))

(simp cube 0 100 100)