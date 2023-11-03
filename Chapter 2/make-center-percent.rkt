#lang scheme

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-interval a b) (cons a b))

(define (lower-bound z)
  (car z))

(define (upper-bound z)
  (cdr z))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (let ((c (center i))
        (x1 (lower-bound i)))
    (* (- 1 (/ x1 c)) 100)))

(define (make-center-percent c p)
  (let ((percent (/ p 100)))
      (make-interval (* c (- 1 percent)) (* c (+ 1 percent)))))


(define i
  (make-center-percent 6.8 10))

(display "[")
(display (lower-bound i))
(display ",")
(display (upper-bound i))
(display "]\n")

(display (percent i))
(display "%")