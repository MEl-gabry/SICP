#lang scheme

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (iter p count)
    (if (= (remainder p 2) 0)
        (iter (/ p 2) (+ count 1))
        count))
  (iter z 0))

(define (cdr z)
  (define (iter p count)
    (if (= (remainder p 3) 0)
        (iter (/ p 3) (+ count 1))
        count))
  (iter z 0))

(display (cdr (cons 411 532)))