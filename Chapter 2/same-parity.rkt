#lang scheme

(define (same-parity x . y)
  (define (iter list)
    (cond ((null? list) null)
          ((= (remainder x 2) (remainder (car list) 2)) (cons (car list) (iter (cdr list))))
          (else (iter (cdr list)))))
  (iter (cons x y)))

(same-parity 2 3 4 5 6 7)