#lang scheme

; My solutions
#;(define (cont-frac-rec n d k)
  (define (calc i)
    (cond ((= i 1) (/ (n i) (calc (+ i 1))))
          ((> i k) (d (- i 1)))
          (else (+ (d (- i 1)) (/ (n i) (calc (+ i 1)))))))
  (calc 1))

#;(define (cont-frac-iter n d k)
  (define (even? n)
    (= (remainder n 2) 0))
    (define (calc i result)
      (cond ((= i 0) result)
          ((even? i) (calc (- i 1) (+ result (d (/ i 2)))))
          (else (calc (- i 1) (/ (n (ceiling (/ i 2))) result)))))
  (calc (* 2 (- k 1)) (/ (n k) (d k))))

; Better ones
(define (cont-frac-recur n d k)
  (define (recur i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ 1 i))))))
  (recur 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= 0 i)
        result
        (iter (sub1 i) (/ (n i) (+ result (d i))))))
  (iter (sub1 k) (/ (n k) (d k))))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           4)


