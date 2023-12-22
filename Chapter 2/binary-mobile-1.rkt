#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

(define (branch-weight b)
    (cond ((pair? (branch-structure b)) (total-weight (branch-structure b)))
          (else
           (branch-structure b))))

(define (total-weight m)
  (+ (branch-weight (left-branch m)) (branch-weight (right-branch m))))

(define (balanced? m)
  (define (torque b)
    (* (branch-weight b) (branch-length b)))
  (if (pair? m)
      (let ((left (left-branch m))
            (right (right-branch m)))
        (and (= (torque left) (torque right))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))
      true))