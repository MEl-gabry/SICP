#lang scheme
(define (product-rec term a next b)
  (if (> a b)
      1
  (* (term a) (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (inc x)
  (+ x 1))

(define (wallis b)
  (define (identity n)
    (* (/ (* 2 n) (- (* 2 n) 1))
       (/ (* 2 n) (+ (* 2 n) 1))))
  (product-iter identity 1 inc b))

(define (factorial n)
  (product-iter (lambda (x) x) 2 inc n))

(exact->inexact(* (wallis 10000) 2))