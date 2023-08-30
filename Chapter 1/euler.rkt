#lang scheme

(define (cont-frac-rec n d k)
  (define (calc i)
    (cond ((= i 1) (/ (n i) (calc (+ i 1))))
          ((> i k) (d (- i 1)))
          (else (+ (d (- i 1)) (/ (n i) (calc (+ i 1)))))))
  (calc 1))

(define (cont-frac-iter n d k)
  (define (even? n)
    (= (remainder n 2) 0))
    (define (calc i result)
      (cond ((= i 0) result)
          ((even? i) (calc (- i 1) (+ result (d (/ i 2)))))
          (else (calc (- i 1) (/ (n (ceiling (/ i 2))) result)))))
  (calc (* 2 (- k 1)) (/ (n k) (d k))))

(define (euler k)
  (cont-frac-iter (lambda (x) 1) (lambda (x) (if (= (remainder x 3) 2) (+ 2 (* 2 (/ (- x 2) 3))) 1)) k))

(+ 2(exact->inexact(euler 1)))