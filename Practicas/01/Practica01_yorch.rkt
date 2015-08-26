#lang plai

(define (msum lst)
  (cond
      [(empty? lst) 0]
      [else (+ (car lst) (msum (cdr lst)))]))

(define (mreduce op lst)
  (cond
    [(empty? lst) '()]
    [(empty? (cdr lst)) (car lst)]
    [else (op (car lst) (mreduce op (cdr lst)))]))

;(test (mreduce + '()) '())
;(test (mreduce - '(100 50 25 12 6 3)) 66)
;(test (mreduce + '(1 2 3 4 5 6 7 8 9 10)) 55)

(define (mmap func lst)
  (cond
    [(empty? lst) '()]
    [else (cons (func (car lst)) (mmap func (cdr lst)))]))

(test (mmap add1 '(1 2 3 4)) '(2 3 4 5))
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))