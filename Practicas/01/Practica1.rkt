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
(test (mconcat '() '()) '()) ;Testing the base case

;Function which takes a list 
;and give us the last element of it. (list) -> (list)
(define (getLast lst)
  (cond
    [(empty? lst) '()]
    [(empty? (cdr lst)) (car lst)]
    [else (getLast (cdr lst))]))

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
