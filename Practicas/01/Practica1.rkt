#lang plai

;Ejercicio 6
(define (mconcat lst1 lst2) 
  (cond
    [(empty? lst1) lst2]
    [else (cons (car lst1) (mconcat (cdr lst1) lst2))]))

(test (mconcat '() '()) '()) ;The concation of two empty
(test (mconcat '() '(1 2 3)) '(1 2 3))
(test (mconcat '(1 2 3) '(1)) '(1 2 3 1))
(test (mconcat '(4 5 6) '()) '(4 5 6))
(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

;Function which takes a list 
;and give us the last element of it. (list) -> (list)
(define (getLast lst)
  (cond
    [(empty? lst) '()]
    [(empty? (cdr lst)) (car lst)]
    [else (getLast (cdr lst))]))
