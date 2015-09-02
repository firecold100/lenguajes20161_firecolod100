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

;Function which takes a list 
;and give us the last element of it. (list) -> (list)
(define (getLast lst)
  (cond
    [(empty? lst) '()]
    [(empty? (cdr lst)) (car lst)]
    [else (getLast (cdr lst))]))

;Auxiliary function of the auxiliary function.
(define (mprimeaux n m)
  (cond
    [(= m 1) #t]
    [(= n m) (mprimeaux n (- m 1))]
    [(= (modulo n m) 0) #f]
    [else (mprimeaux n (- m 1))]))
    

;Auxiliary function
;returns if the given number is a prime or is not.
(define (mprime? n)
  (cond
    [(< n 2) #f]
    [else (mprimeaux n n)]
  ))

(define (loop i n)
   (if (= n  i) ;condicion
      '()  ; then
      (if(and (mprime? i) #t) ;elseif
         (cons i (loop (+ i 1) n))
      (loop (+ i 1) n)))) ;then

;Ejercicio 1)
(define (pow x n)
  (cond
    [(zero? n) 1]
    [else (* x (pow x (- n 1)))]))

(test (pow 2 3) 8)
(test (pow 10 0) 1)
(test (pow 10 2) 100)
(test (pow 9 2) 81)
(test (pow 1 6) 1)

;Ejercicio 2)
(define (average lst)
  (cond
    [(zero? (sum lst)) 0]
    [else (/ (sum lst) (mlength lst))]))

(test (average '())0)
(test (average '(0 0 0 0)) 0)
(test (average '(3 2 6 2 1 7 2 1)) 3)
(test (average '(10 7 13)) 10)
(test (average '(1 1 1 1 1)) 1)

;Ejercicio 3)
(define (primes n)
  (loop 1 (+ n 1)))

(test (primes 1) '())
(test (primes 9) '(2 3 5 7))
(test (primes 11) '(2 3 5 7 11))
(test (primes 13) '(2 3 5 7 11 13))
(test (primes 30) '(2 3 5 7 11 13 17 19 23 29))

;Ejercicio 4)
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))empty
  [cons (list (car lst1) (car lst2))
        (zip (cdr lst1) (cdr lst2))]))

(test (zip '() '())'())
(test (zip '(1 2) '())'())
(test (zip '(3 5) '(4 5)) '((3 4)(5 5)))
(test (zip '(3 4 5 6) '(1 2 3))'((3 1)(4 2)(5 3)))
(test (zip '(1) '(10))'((1 10)))

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

(test (mconcat '() '()) '()) ;The concation of two empty
(test (mconcat '() '(1 2 3)) '(1 2 3))
(test (mconcat '(1 2 3) '(1)) '(1 2 3 1))
(test (mconcat '(4 5 6) '()) '(4 5 6))
(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '() '()) '()) ;Testing the base case

;Ejercicio 7

(define (mmap func lst)
  (cond
    [(empty? lst) '()]
    [else (cons (func (car lst)) (mmap func (cdr lst)))]))

;(test (mmap add1 '(1 2 3 4)) '(2 3 4 5))
;(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
;(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))
;(test (mmap car (cdr '((1 2 3) (4 5 6) (7 8 9)))) '(4 7))

;Ejercicio 8

(define (mfilter predicate lst);We define a function which takes a predicate and a list, the predicate may be a lambda.
  (cond
    ;If the list is empty (base case) then we return the empty list
    [(empty? lst) '()] 
    ;If the predicate applied to the head of the list is true, then we return the first element of it 
    ;concatenated with the application to the rest of the list recursively.
    [(predicate (car lst)) (cons (car lst) (mfilter predicate (cdr lst))) ]
    ;Else, we return the application of the function of the rest of the list recursively.
    [else (mfilter predicate (cdr lst)) ]))

(test (mfilter (lambda (x) (not(zero? x))) '(2 0 1 4 0)) '(2 1 4))
(test (mfilter (lambda (n) (= (modulo n 3) 1)) '(4 7 10 8)) '(4 7 10))
(test (mfilter (lambda (y) (not (eq? "pikachu" y)) ) '("raichu" "ratatta" "charmeleon" ) ) '("raichu" "ratatta" "charmeleon" ))
(test (mfilter (lambda (u) (< u 5)) '(12 32 1 2 3 5)) '(1 2 3 ))
(test (mfilter (lambda (h) (eq? #t h)) '(#f #f #t #t #t)) '(#t #t #t))
(test (mfilter string? '()) '()) ;Testing the base case.

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


;Ejercicio 10

(define (mevery? predicate lst)
  (cond
    ;If the list is empty, then we return true by vacuity
    [(empty? lst) #t]
    ;If the predicate is true applied to the head of the list, then we continue recursively searching
    ;the element that doesn't returns true
    [(predicate (car lst)) (mevery? predicate (cdr lst))]
    ;Else we return false, because we found an element that doesn't meets the requeriments
    [else #f]))

(test (mevery? (lambda (x) (= (modulo x 3) 1)) '(1 4 7 10 13) ) #t)
(test (mevery? (lambda (x) (eq? "pikachu" x)) '("pikachu" "raichu") ) #f)
(test (mevery? number? '(1 2 3 4)) #t)
(test (mevery? (lambda (x) (empty? x)) '('() '() '())) #f) ;It's false because is not empty the list.
(test (mevery? string? '("charizard" "squirtle" "haunter" "combee")) #t)
(test (mevery? (lambda (x) (= (modulo x 3) 1)) '()) #t) ;Testing the base case.

;Ejercicio 11

(define (mpowerset lst)
  (cond
    [(empty? lst) (list '())]
    [else (agrega (mpowerset (cdr lst)) (car lst))]))

(define (agrega res elem)
  (cond
    [(empty? res)  '()]
    [else (cons (cons elem (first res)) (cons (car res) (agrega (cdr res) elem)))]))

;(test (mpowerset '()) '(()))
;(test (mpowerset '(1)) '((1) ()))
;(test (mpowerset '(1 2)) '((1 2) (2) (1) ()))
;(test (mpowerset '(1 2 3)) '((1 2 3) (2 3) (1 3) (3) (1 2) (2) (1) ()))
;(test (mpowerset '(1 2 3 4)) '((1 2 3 4) (2 3 4) (1 3 4) (3 4) (1 2 4) (2 4) (1 4) (4) (1 2 3) (2 3) (1 3) (3) (1 2) (2) (1) ()))