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


;Ejercicio 6
(define (mconcat lst1 lst2) 
  (cond
    [(empty? lst1) lst2]
    [else (cons (car lst1) (mconcat (cdr lst1) lst2))]))

;(test (mconcat '() '()) '()) ;The concation of two empty
;(test (mconcat '() '(1 2 3)) '(1 2 3))
;(test (mconcat '(1 2 3) '(1)) '(1 2 3 1))
;(test (mconcat '(4 5 6) '()) '(4 5 6))
;(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))


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

(define (aux lst)
  (cond
    [(empty? (cdr lst)) (list lst)]
    [else (mconcat (list (list (car lst))) (aux (cdr lst)))]))

(define (mpowerset lst)
  (cond
    [(empty? lst) '(())]
    [(empty? (cdr lst)) (mconcat '(()) (list lst))]
    [else (mconcat (mconcat (mconcat '(()) (aux lst)) (const '() (car lst) (cdr lst))) (list lst))]))

(define (const pas elem pos)
  (cond
    [(empty? pos) '()]
    [else (mconcat (mconcat (pares (list elem) pos) (const (mconcat pas (list elem)) (car pos) (cdr pos))) (pares pas pos))]))

(define (pares elem lst)
  (cond
    [(empty? lst) '()]
    [else (mconcat (list (mconcat elem (list (car lst)))) (pares elem (cdr lst)))]))

;(test (mpowerset '(1)) '(() (1)))
;(mpowerset '(1 2))
;(test (mpowerset '(1 2 3)) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)))
;(mpowerset '(1 2 3 4))

(define (subcjtos lst)
  (cond
    [(empty? lst) '(())]
    [else (creacjtos '() (car lst) (cdr lst))]))

(define (creacjtos pas elem pos)
  (cond
    [(empty? pos) (mconcat (mconcat (list (list elem)) (list pas)) (list (mconcat pas (list elem))))]
    [else (mconcat (list (list elem)) (mconcat (pares (list elem) pos) (mconcat (list (mconcat pas pos)) (creacjtos (mconcat pas (list elem)) (car pos) (cdr pos)))))]))

(subcjtos '(1 2 3 4))