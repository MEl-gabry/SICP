#lang scheme

(define (square x)
  (* x x))

(define (make-rect l w)
  (cons l w))

(define (get-length r)
  (segment-length (car r)))

(define (get-width r)
  (segment-length (cdr r)))

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (segment-x-length s)
  (- (x-point (end-segment s)) (x-point (start-segment s))))

(define (segment-y-length s)
  (- (y-point (end-segment s)) (y-point (start-segment s))))

(define (segment-length s)
  (sqrt (+ (square (segment-x-length s)) (square (segment-y-length s)))))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (rect-perm r)
  (+ (* 2 (get-length r)) (* 2 (get-width r))))

(define (rect-area r)
  (* (get-length r) (get-width r)))

(define rect
  (make-rect (make-segment (make-point -2 4) (make-point 4 4)) (make-segment (make-point -2 4) (make-point -2 12))))

(display (rect-perm rect))