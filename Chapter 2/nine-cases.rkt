#lang scheme

(define (neg? x)
  (< x 0))

(define (n-neg? x)
  (>= x 0))

(define (make-interval a b) (cons a b))

(define (lower-bound z)
  (car z))

(define (upper-bound z)
  (cdr z))

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond
      ((and (n-neg? x1) (n-neg? x2) (n-neg? y1) (n-neg? y2)) (make-interval (* x1 y1) (* x2 y2)))
      ((and (n-neg? x1) (n-neg? x2) (neg? y1) (n-neg? y2)) (make-interval (* x2 y1) (* x2 y2)))
      ((and (n-neg? x1) (n-neg? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y1) (* x1 y2)))
      ((and (neg? x1) (n-neg? x2) (n-neg? y1) (n-neg? y2)) (make-interval (* x1 y2) (* x2 y2)))
      ((and (neg? x1) (n-neg? x2) (neg? y1) (n-neg? y2)) (make-interval (min (* x2 y1) (* x1 y2)) (max (* x2 y2) (* x1 y1))))
      ((and (neg? x1) (n-neg? x2) (neg? y1) (neg? y2)) (make-interval (* x2 y1) (* x1 y1)))
      ((and (neg? x1) (neg? x2) (n-neg? y1) (n-neg? y2)) (make-interval (* x1 y2) (* x2 y1)))
      ((and (neg? x1) (neg? x2) (neg? y1) (n-neg? y2)) (make-interval (* x1 y2) (* x1 y1)))
      (else (make-interval (* x2 y2) (* x1 y1))))))