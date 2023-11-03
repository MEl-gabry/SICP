#lang scheme

(define (square x)
  (* x x))

(define (make-rect a b c d)
  (cons (cons a b) (cons c d)))

(define (get-length r)
  (dist (car (car r)) (cdr (cdr r))))

(define (get-width r)
  (dist (car (car r)) (cdr (car r))))

(define (dist a b)
  (sqrt (+ (square (- (x-point b) (x-point a)))
           (square (- (y-point b) (y-point a))))))

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
  (make-rect (make-point -3 1) (make-point -5 -1) (make-point -1 -2) (make-point 1 0)))

(display (rect-area rect))