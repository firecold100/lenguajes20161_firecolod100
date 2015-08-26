#lang plai

;Ejercicio 5

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


;Ejercicio 7

(define (mmap func lst)
  (cond
    [(empty? lst) '()]
    [else (cons (func (car lst)) (mmap func (cdr lst)))]))

;(test (mmap add1 '(1 2 3 4)) '(2 3 4 5))
;(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
;(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))
;(test (mmap car (cdr '((1 2 3) (4 5 6) (7 8 9)))) '(4 7))


;Ejercicio 9

(define (myany? pred lst)
  (cond
    [(empty? lst) (pred lst)]
    [(if (pred (car lst)) #t #f)]
    [else (myany? pred (cdr lst))]))

;(test (myany? number? '()) #f)
;(test (myany? number? '(a b c d 1)) #t)
;(test (myany? symbol? '(1 2 3 4)) #f)
;(test (myany? symbol? '(1 2 + 4)) #t)
;(test (myany? empty? '()) #t)


;Ejercicio 11

(define (mpowerset lst)
  (cond
    [(empty? lst) '()]
    [else (construye lst lst)]))

(define (construye lst1 lst2))

(test (mpowerset '(1 2)) '((1 2) (2) (1) ()))