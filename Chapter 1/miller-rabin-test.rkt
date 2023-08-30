#lang scheme
(define (square x)
  (* x x))

(define (sqrt-test a n)
  (cond ((or (= a 1) (= a (- n 1))) a)
        ((= (remainder (square a) n) 1) 0)
        (else a)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (sqrt-test (expmod base (/ exp 2) m) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000000)
      (report-prime (- (current-inexact-milliseconds) start-time))
      0))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 2)
(timed-prime-test 3)
(timed-prime-test 5)
(timed-prime-test 7)
(timed-prime-test 11)
(timed-prime-test 1009)
(timed-prime-test 561)
(timed-prime-test 1105)
(timed-prime-test 1729)
(timed-prime-test 2465)
(timed-prime-test 2821)
(timed-prime-test 6601)