#lang plai

;Funciones auxiliares
(define (mlength lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (mlength (cdr lst)))]))

(define (sum lst)
  (cond
      [(empty? lst) 0]
      [else (+ (car lst) (sum (cdr lst)))]))


;Ejercicio 1)
(define (pow x n)
  (cond
    [(zero? n) 1]
    [else (* x (pow x (- n 1)))]))

(test (pow 2 3) 8)
(test (pow 10 0) 1)
(test (pow 10 2) 100)
(test (pow 9 2) 81)
(test (pow 8 6) 27)

;Ejercicio 2)
(define (average lst)
  (cond
    [(zero? (sum lst)) (error "¡Requiero una lista no vacia o que la suma de los numeros sea mayor que cero!")]
    [else (/ (sum lst) (mlength lst))]))

(test (average '(5)) 5)
(test (average '(3 2 6 2 1 7 2 1)) 3)
(test (average '(10 7 13)) 10)
(test (average '(1 1 1 1 1)) 2)
(test (average '(0))0)
