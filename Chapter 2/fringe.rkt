#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe items)
  (cond ((null? items) items)
        ((pair? items)
         (append (fringe (car items)) (fringe (cdr items))))
        (else (list items))))

(define x (list (list 1 2) (list 3 4)))

(fringe (list x x))